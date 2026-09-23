000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6115500.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   92/07/02.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*    LÄSER FIL MED STATUSPOSTER FRÅN W6G3 OCH SUMMERAR VAD SOM            
001000*    PRODUCERATS IDAG OCH GENOMLOPPSTIDEN.                                
001100*                                                                         
001200*    LÄSER FIL FRÅN W6G1 MED ÖNSKADE GENOMLOPPSTIDER PER DC/AVD.          
001300*                                                                         
001400*    NOTE:                                                                
001500*    FIL FRÅN W6G1 SKALL VARA SORTERAD PÅ IDDC FÖR                        
001600*    RÄTT PROGRAMFUNKTION.                                                
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  TABELL FULL ELLER FEL FRÅN WORKDAY                      
002100*                                                                         
002200* CCID 6442199 -                                                          
002300*        FOLLOW-UP ON TEAM LEVEL                                          
002400* CCID 6628002 -                                                          
002500*        CORRECTION OF PRM ERROR                                          
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- STATUSPOSTER FRÅN W6G3                                     
003500     SELECT W61140                     ASSIGN TO W61155D1.                
003600     SKIP2                                                                
003700*          --- PARAMETRAR   FRÅN W6G1                                     
003800     SELECT W6115A                     ASSIGN TO W61155D2.                
003900     SKIP2                                                                
004000*          --- SUMMAFIL MED PRODUCERAT IDAG OCH GENOMLOPPSTID             
004100     SELECT W61155                     ASSIGN TO W61155D3.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W61140                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000     SKIP2                                                                
005100*01  -COPY W6114001      -L.                                              
005200     SKIP3                                                                
005300* CCID 6442199 -                                                          
005400 FD  W6115A                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700     SKIP2                                                                
005800*01  -COPY W6115A01      -L.                                              
005900     SKIP3                                                                
006000 FD  W61155                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300     SKIP2                                                                
006400*01  POST -COPY W6115501 -PRE  UT-  -L.                                   
006500     EJECT                                                                
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800*    -COPY WY2000W1                                                       
006900     SKIP3                                                                
007000 77  IDPGM                       PIC X(8)    VALUE 'W6115500'.            
007100 77  JA                          PIC X       VALUE 'J'.                   
007200 77  NEJ                         PIC X       VALUE 'N'.                   
007300 77  TIGLT-ADDED                 PIC X       VALUE 'N'.                   
007400                                                                          
007500 77  MAX-SUM-IX                  PIC S9(9)   VALUE +300 COMP SYNC.        
007600                                                                          
007700 77  MAX-MAAL-IX                 PIC S9(9)   VALUE +300 COMP SYNC.        
007800                                                                          
007900 77  DCIX                        PIC S9(9)   COMP-3 VALUE ZERO.           
008000                                                                          
008100*FIX                                                                      
008200 01  WS-TIREGDATFIX          PIC 9(6)        VALUE ZERO.                  
008300 01  FILLER REDEFINES WS-TIREGDATFIX.                                     
008400   05 WS-TIREGDATFIX-AA      PIC 9(2).                                    
008500   05 WS-TIREGDATFIX-MM      PIC 9(2).                                    
008600   05 WS-TIREGDATFIX-DD      PIC 9(2).                                    
008700*SLUTFIX                                                                  
008800                                                                          
008900 01  DC-SPARFAELT.                                                        
009000    03 FILLER OCCURS 27.                                                  
009100     05 SPAR-KDINLUPF           PIC X(4)       VALUE SPACE.               
009200     05 W-ADINLOMR              PIC X(4)       VALUE SPACE.               
009300     05 W-KDINLUPF              PIC X(4)       VALUE SPACE.               
009400     05 W-IDLEVNR               PIC X(5)       VALUE SPACE.               
009500     05 W-OLD-IDLOPNRM          PIC S9(9)      VALUE ZERO COMP-3.         
009600     05 W-TIGLT-MM              PIC S9(9)      VALUE ZERO COMP-3.         
009700     05 W-TOT-IDLOPNRM          PIC S9(9)      VALUE ZERO COMP-3.         
009800     05 W-TOT-KVART             PIC S9(5)      VALUE ZERO COMP-3.         
009900     05 W-TOT-KVART-DAG         PIC S9(5)      VALUE ZERO COMP-3.         
010000     05 W-TOT-KVART-KVALL       PIC S9(5)      VALUE ZERO COMP-3.         
010100     05 W-TOT-KVART-MAAL        PIC S9(5)      VALUE ZERO COMP-3.         
010200     05 W-TOT-KVART-MAAL-PRIO   PIC S9(5)      VALUE ZERO COMP-3.         
010300     05 W-TOT-KVRADER           PIC S9(5)      VALUE ZERO COMP-3.         
010400     05 W-TOT-KVRADER-DAG       PIC S9(5)      VALUE ZERO COMP-3.         
010500     05 W-TOT-KVRADER-KVALL      PIC S9(5)      VALUE ZERO COMP-3.        
010600     05 W-TOT-KVRADER-PRIO       PIC S9(5)      VALUE ZERO COMP-3.        
010700     05 W-TOT-KVRADER-PRIO-DAG   PIC S9(5)      VALUE ZERO COMP-3.        
010800     05 W-TOT-KVRADER-PRIO-KVALL PIC S9(5)      VALUE ZERO COMP-3.        
010900     05 W-TOT-SUBEL              PIC S9(9)V9(2) VALUE ZERO COMP-3.        
011000     05 W-TOT-VLARTNTO          PIC S9(13)V9(1) VALUE ZERO COMP-3.        
011100     05 W-TOT-SUBEL-PRIO         PIC S9(9)V9(2) VALUE ZERO COMP-3.        
011200     05 W-TOT-TIGLT-MM           PIC S9(9)      VALUE ZERO COMP-3.        
011300     05 W-TOT-TIGLT-MM-DAG       PIC S9(9)      VALUE ZERO COMP-3.        
011400     05 W-TOT-TIGLT-MM-KVALL     PIC S9(9)      VALUE ZERO COMP-3.        
011500     05 W-TOT-TIGLT-PRIO-MM      PIC S9(9)      VALUE ZERO COMP-3.        
011600     05 W-TOT-TIGLT-PRIO-MM-DAG  PIC S9(9)      VALUE ZERO COMP-3.        
011700     05 W-TOT-TIGLT-PRIO-MM-KVALL PIC S9(9)     VALUE ZERO COMP-3.        
011800                                                                          
011900                                                                          
012000 01  DC-SPARFAELT-CDC.                                                    
012100     05 SPAR-KDINLUPF-CDC       PIC X(4)       VALUE SPACE.               
012200     05 W-KDINLUPF-CDC          PIC X(4)       VALUE SPACE.               
012300     05 W-IDLEVNR-CDC           PIC X(5)       VALUE SPACE.               
012400     05 W-OLD-IDLOPNRM-CDC      PIC S9(9)      VALUE ZERO COMP-3.         
012500     05 W-TIGLT-MM-CDC          PIC S9(9)      VALUE ZERO COMP-3.         
012600     05 W-TOT-IDLOPNRM-CDC      PIC S9(9)      VALUE ZERO COMP-3.         
012700     05 W-TOT-KVART-CDC         PIC S9(5)      VALUE ZERO COMP-3.         
012800     05 W-TOT-KVART-DAG-CDC     PIC S9(5)      VALUE ZERO COMP-3.         
012900     05 W-TOT-KVART-KVALL-CDC   PIC S9(5)      VALUE ZERO COMP-3.         
013000     05 W-TOT-KVART-MAAL-CDC    PIC S9(5)      VALUE ZERO COMP-3.         
013100     05 W-TOT-KVART-MAAL-PRIO-CDC PIC S9(5)    VALUE ZERO COMP-3.         
013200     05 W-TOT-KVRADER-CDC       PIC S9(5)      VALUE ZERO COMP-3.         
013300     05 W-TOT-KVRADER-DAG-CDC   PIC S9(5)      VALUE ZERO COMP-3.         
013400     05 W-TOT-KVRADER-KVALL-CDC  PIC S9(5)     VALUE ZERO COMP-3.         
013500     05 W-TOT-KVRADER-PRIO-CDC   PIC S9(5)     VALUE ZERO COMP-3.         
013600     05 W-TOT-KVRADER-PRIO-DAG-CDC  PIC S9(5)  VALUE ZERO COMP-3.         
013700     05 W-TOT-KVRADER-PRIO-KVALL-CDC PIC S9(5) VALUE ZERO COMP-3.         
013800     05 W-TOT-SUBEL-CDC          PIC S9(9)V9(2) VALUE ZERO COMP-3.        
013900     05 W-TOT-VLARTNTO-CDC      PIC S9(13)V9(1) VALUE ZERO COMP-3.        
014000     05 W-TOT-SUBEL-PRIO-CDC     PIC S9(9)V9(2) VALUE ZERO COMP-3.        
014100     05 W-TOT-TIGLT-MM-CDC       PIC S9(9)      VALUE ZERO COMP-3.        
014200     05 W-TOT-TIGLT-MM-DAG-CDC   PIC S9(9)      VALUE ZERO COMP-3.        
014300     05 W-TOT-TIGLT-MM-KVALL-CDC PIC S9(9)      VALUE ZERO COMP-3.        
014400     05 W-TOT-TIGLT-PRIO-MM-CDC  PIC S9(9)      VALUE ZERO COMP-3.        
014500     05 W-TOT-TIGLT-PRIO-MM-DAG-CDC  PIC S9(9)  VALUE ZERO COMP-3.        
014600     05 W-TOT-TIGLT-PRIO-MM-KVALL-CDC PIC S9(9) VALUE ZERO COMP-3.        
014700                                                                          
014800                                                                          
014900 01  DC-SPARFAELT-SVS.                                                    
015000     05 SPAR-KDINLUPF-SVS       PIC X(4)       VALUE SPACE.               
015100     05 W-KDINLUPF-SVS          PIC X(4)       VALUE SPACE.               
015200     05 W-IDLEVNR-SVS           PIC X(5)       VALUE SPACE.               
015300     05 W-OLD-IDLOPNRM-SVS      PIC S9(9)      VALUE ZERO COMP-3.         
015400     05 W-TIGLT-MM-SVS          PIC S9(9)      VALUE ZERO COMP-3.         
015500     05 W-TOT-IDLOPNRM-SVS      PIC S9(9)      VALUE ZERO COMP-3.         
015600     05 W-TOT-KVART-SVS         PIC S9(5)      VALUE ZERO COMP-3.         
015700     05 W-TOT-KVART-DAG-SVS     PIC S9(5)      VALUE ZERO COMP-3.         
015800     05 W-TOT-KVART-KVALL-SVS   PIC S9(5)      VALUE ZERO COMP-3.         
015900     05 W-TOT-KVART-MAAL-SVS    PIC S9(5)      VALUE ZERO COMP-3.         
016000     05 W-TOT-KVART-MAAL-PRIO-SVS PIC S9(5)    VALUE ZERO COMP-3.         
016100     05 W-TOT-KVRADER-SVS       PIC S9(5)      VALUE ZERO COMP-3.         
016200     05 W-TOT-KVRADER-DAG-SVS   PIC S9(5)      VALUE ZERO COMP-3.         
016300     05 W-TOT-KVRADER-KVALL-SVS  PIC S9(5)     VALUE ZERO COMP-3.         
016400     05 W-TOT-KVRADER-PRIO-SVS   PIC S9(5)     VALUE ZERO COMP-3.         
016500     05 W-TOT-KVRADER-PRIO-DAG-SVS  PIC S9(5)  VALUE ZERO COMP-3.         
016600     05 W-TOT-KVRADER-PRIO-KVALL-SVS PIC S9(5) VALUE ZERO COMP-3.         
016700     05 W-TOT-SUBEL-SVS          PIC S9(9)V9(2) VALUE ZERO COMP-3.        
016800     05 W-TOT-VLARTNTO-SVS      PIC S9(13)V9(1) VALUE ZERO COMP-3.        
016900     05 W-TOT-SUBEL-PRIO-SVS     PIC S9(9)V9(2) VALUE ZERO COMP-3.        
017000     05 W-TOT-TIGLT-MM-SVS       PIC S9(9)      VALUE ZERO COMP-3.        
017100     05 W-TOT-TIGLT-MM-DAG-SVS   PIC S9(9)      VALUE ZERO COMP-3.        
017200     05 W-TOT-TIGLT-MM-KVALL-SVS PIC S9(9)      VALUE ZERO COMP-3.        
017300     05 W-TOT-TIGLT-PRIO-MM-SVS  PIC S9(9)      VALUE ZERO COMP-3.        
017400     05 W-TOT-TIGLT-PRIO-MM-DAG-SVS  PIC S9(9)  VALUE ZERO COMP-3.        
017500     05 W-TOT-TIGLT-PRIO-MM-KVALL-SVS PIC S9(9) VALUE ZERO COMP-3.        
017600                                                                          
017700 77  WS-TOT-SUBEL-PRIO       PIC S9(9)V9(2)  VALUE ZERO COMP-3.           
017800 77  WS-SUBEL-PRIO           PIC S9(9)V9(2)  VALUE ZERO COMP-3.           
017900 77  WS-FLEXCP               PIC X(1)        VALUE SPACE.                 
018000                                                                          
018100 77  W-TIKLOCK-OLD-MM        PIC 9(8)        VALUE ZERO.                  
018200 77  W-TIKLOCK-NEW-MM        PIC 9(8)        VALUE ZERO.                  
018300                                                                          
018400 01  W-DK-TIKLOCK            PIC 9(8)        VALUE ZERO.                  
018500 01  FILLER REDEFINES W-DK-TIKLOCK.                                       
018600   05 W-DK-HH                PIC 9(2).                                    
018700   05 W-DK-MM                PIC 9(2).                                    
018800   05 FILLER                 PIC 9(4).                                    
018900                                                                          
019000 01  W-NEW-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
019100 01  FILLER REDEFINES W-NEW-TIKLOCK.                                      
019200   05 W-NEW-HH               PIC 9(2).                                    
019300   05 W-NEW-MM               PIC 9(2).                                    
019400   05 FILLER                 PIC 9(4).                                    
019500                                                                          
019600 01  W-OLD-TIKLOCK           PIC 9(8)        VALUE ZERO.                  
019700 01  FILLER REDEFINES W-OLD-TIKLOCK.                                      
019800   05 W-OLD-HH               PIC 9(2).                                    
019900   05 W-OLD-MM               PIC 9(2).                                    
020000   05 FILLER                 PIC 9(4).                                    
020100                                                                          
020200 01  W-GLT                   PIC 9(5)        VALUE ZERO.                  
020300 01  FILLER REDEFINES W-GLT.                                              
020400   05 W-GLT-HH               PIC 9(3).                                    
020500   05 W-GLT-MM               PIC 9(2).                                    
020600                                                                          
020700 77  W-TIKLOCK-MM            PIC S9(5)      COMP-3  VALUE ZERO.           
020800 77  W-TOT-MM                PIC S9(5)      COMP-3  VALUE ZERO.           
020900 77  NEW-TIKLOCK             PIC 9(9)               VALUE ZERO.           
021000 77  OLD-TIKLOCK             PIC 9(9)               VALUE ZERO.           
021100                                                                          
021200 77  D-DISPLAY               PIC -9(8).                                   
021300 77  W-TIVV                  PIC 9(2)               VALUE ZERO.           
021400 77  W-TIAAVVD               PIC 9(5)               VALUE ZERO.           
021500 01  W-TIAAVVD-ALPHA.                                                     
021600     03 W-TI-FILLER          PIC X(4).                                    
021700     03 W-WEEKDAY            PIC 9(1).                                    
021800 77  W-TIAAVVD-OLD           PIC 9(5)               VALUE ZERO.           
021900 01  W-TIAAVVD-OLD-ALPHA.                                                 
022000     03 W-TI-FILLER-OLD      PIC X(4).                                    
022100     03 W-WEEKDAY-OLD        PIC 9(1).                                    
022200                                                                          
022300* CCID 6442199 -                                                          
022400 01  WS-TIGLT.                                                            
022500     03 WS-TIGLT-HH          PIC 9(2).                                    
022600     03 WS-TIGLT-MM          PIC 9(2).                                    
022700                                                                          
022800 77  W61140-EOF-SW               PIC X       VALUE 'N'.                   
022900     88  END-OF-W61140                       VALUE 'J'.                   
023000                                                                          
023100 77  W6115A-EOF-SW               PIC X       VALUE 'N'.                   
023200     88  END-OF-W6115A                       VALUE 'J'.                   
023300                                                                          
023400 77  OLD-SW                      PIC XX      VALUE '  '.                  
023500     88  OLD-FM                              VALUE 'FM'.                  
023600     88  OLD-EM                              VALUE 'EM'.                  
023700                                                                          
023800 77  NEW-SW                      PIC XX      VALUE '  '.                  
023900     88  NEW-FM                              VALUE 'FM'.                  
024000     88  NEW-EM                              VALUE 'EM'.                  
024100                                                                          
024200 77  FLAG-SW                     PIC XXX     VALUE '   '.                 
024300     88  SVS-FLAG                            VALUE 'SVS'.                 
024400     88  CDC-FLAG                            VALUE 'CDC'.                 
024500                                                                          
024600 77  NEW-DAG-KVALL-SW            PIC X(5)    VALUE '     '.               
024700     88  NEW-DAG                             VALUE 'DAG  '.               
024800     88  NEW-KVALL                           VALUE 'KVALL'.               
024900                                                                          
025000 77  ADD-RADER-SW                PIC X       VALUE 'N'.                   
025100     88  ADD-RADER                           VALUE 'J'.                   
025200     EJECT                                                                
025300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
025400 01  FILLER REDEFINES DAGENS-DATUM.                                       
025500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
025600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
025700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
025800     SKIP2                                                                
025900 01  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
026000     EJECT                                                                
026100*      --- VALID IDDC CODES                                               
026200*                                                                         
026300*01    -COPY WWDC99                                                       
026400*01    -COPY WWDCKONS                                                     
026500       EJECT                                                              
026600 01  DYNAMISKA-SUBPROGRAM.                                                
026700*                                                                         
026800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
026900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
027000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
027100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
027200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
027300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027500     SKIP2                                                                
027600*    --- PARAMETRAR TILL ABEND                                            
027700                                                                          
027800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
027900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
028000     SKIP2                                                                
028100 01  FELTEXT.                                                             
028200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
028300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
028400     EJECT                                                                
028500*    --- PARAMETRAR TILL DATKORT                                          
028600*                                                                         
028700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61155'.              
028800     SKIP2                                                                
028900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
029000     SKIP2                                                                
029100*01  -COPY WDATKORT                                                       
029200     EJECT                                                                
029300*    --- PARAMETRAR TILL POSTSUM                                          
029400*                                                                         
029500*01  -COPY W0005   -PRE  POSTSUM-                                         
029600     EJECT                                                                
029700 01  FILLER                      PIC X(16)  VALUE 'WORKDAY'.              
029800*01  -COPY WORKAREA                                                       
029900     EJECT                                                                
030000*01  -COPY WDATAREA                                                       
030100     EJECT                                                                
030200*    --- AREA FÖR BERÄKNING AV GLT PER PARTI                              
030300 01  DC-W-SPAR1.                                                          
030400     03  FILLER  OCCURS 27.                                               
030500*      05 AREA -COPY W6114001    -PRE W-SPAR1-                            
030600     EJECT                                                                
030700                                                                          
030800*    --- AREA FÖR BERÄKNING AV GLT PER UPPFÖLJNINGSTATUS                  
030900 01  DC-W-SPAR2.                                                          
031000     03  FILLER  OCCURS 27.                                               
031100*      05 AREA -COPY W6114001    -PRE W-SPAR2-                            
031200     EJECT                                                                
031300                                                                          
031400*    --- AREA FÖR BERÄKNING AV GLT PER UPPFÖLJNINGSTATUS                  
031500 01  DC-W-SPAR3.                                                          
031600       05 AREA -COPY W6114001    -PRE W-SPAR3-                            
031700     EJECT                                                                
031800                                                                          
031900*    --- AREA FÖR BERÄKNING AV GLT PER UPPFÖLJNINGSTATUS                  
032000 01  DC-W-SPAR4.                                                          
032100       05 AREA -COPY W6114001    -PRE W-SPAR4-                            
032200     EJECT                                                                
032300                                                                          
032400 01  IN-AREA-START          PIC X(24)   VALUE 'IN-AREA-START  '.          
032500*01  AREA -COPY W6114001     -PRE IN-                                     
032600     EJECT                                                                
032700                                                                          
032800 01  MAAL-AREA-START        PIC X(24)   VALUE 'MAAL-AREA-START'.          
032900* CCID 6442199 -                                                          
033000*01  AREA -COPY W6115A01     -PRE MIN-                                    
033100                                                                          
033200 01  MAAL-AREA.                                                           
033300     03 MAAL-AREA-DATA.                                                   
033400        05 MAAL-IDDC          PIC X(2).                                   
033500        05 MAAL-KDINLUPF      PIC X(4).                                   
033600        05 MAAL-TIGLT         PIC 9(4).                                   
033700        05 MAAL-TIGLT-PRIO    PIC 9(4).                                   
033800        05 MAAL-FLEXCP        PIC X.                                      
033900        05 MAAL-IDLEVNR       PIC X(5).                                   
034000     EJECT                                                                
034100 01  UT-AREA-START      PIC X(24)   VALUE   'UT-AREA-START  '.            
034200*01  AREA -COPY W6115501     -PRE UT-                                     
034300     EJECT                                                                
034400*    --- TABELL FÖR SUMMERINGAR PER KDINLUPF                              
034500 01  TABELL-START          PIC X(24)   VALUE                              
034600                                 'TABELL-START  '.                        
034700 01  FILLER.                                                              
034800   03 DC-W-SUM      OCCURS 27  INDEXED BY SU-DCIX.                        
034900     05 W-SUM-TAB   OCCURS 300 INDEXED BY SUM-IX.                         
035000        07  W-SUM-KDINLUPF            PIC X(4).                           
035100        07  W-SUM-KVRADER             PIC S9(5)      COMP-3.              
035200        07  W-SUM-KVRADER-DAG         PIC S9(5)      COMP-3.              
035300        07  W-SUM-KVRADER-KVALL       PIC S9(5)      COMP-3.              
035400        07  W-SUM-KVRADER-PRIO        PIC S9(5)      COMP-3.              
035500        07  W-SUM-KVRADER-PRIO-DAG    PIC S9(5)      COMP-3.              
035600        07  W-SUM-KVRADER-PRIO-KVALL  PIC S9(5)      COMP-3.              
035700        07  W-SUM-KVART               PIC S9(5)      COMP-3.              
035800        07  W-SUM-KVART-DAG           PIC S9(5)      COMP-3.              
035900        07  W-SUM-KVART-KVALL         PIC S9(5)      COMP-3.              
036000        07  W-SUM-SUBEL               PIC S9(9)V9(2) COMP-3.              
036100        07  W-SUM-SUBEL-PRIO          PIC S9(9)V9(2) COMP-3.              
036200        07  W-SUM-TIGLT-MM            PIC S9(9)      COMP-3.              
036300        07  W-SUM-TIGLT-MM-DAG        PIC S9(9)      COMP-3.              
036400        07  W-SUM-TIGLT-MM-KVALL      PIC S9(9)      COMP-3.              
036500        07  W-SUM-TIGLT-PRIO-MM       PIC S9(9)      COMP-3.              
036600        07  W-SUM-TIGLT-PRIO-MM-DAG   PIC S9(9)      COMP-3.              
036700        07  W-SUM-TIGLT-PRIO-MM-KVALL PIC S9(9)      COMP-3.              
036800        07  W-SUM-KVART-MAAL          PIC S9(5)      COMP-3.              
036900        07  W-SUM-KVART-MAAL-PRIO     PIC S9(5)      COMP-3.              
037000        07  W-SUM-VLARTNTO            PIC S9(13)V9(1) COMP-3.             
037100        07  W-SUM-IDLEVNR             PIC X(5).                           
037200        07  W-LAST-IDLOPNRM           PIC S9(9)      COMP-3.              
037300     EJECT                                                                
037400 01  FILLER                PIC X(24)   VALUE  'MAAL TABELL   '.           
037500 01  MAAL-TABELL.                                                         
037600    03 DC-W-MAAL     OCCURS 27  INDEXED BY MA-DCIX.                       
037700      04 W-MAAL-TAB  OCCURS 300 INDEXED BY MAAL-IX.                       
037800         05  W-MAAL-TOTAL.                                                
037900           06  W-MAAL-IDDC             PIC XX.                            
038000           06  W-MAAL-KDINLUPF         PIC X(4).                          
038100           06  W-MAAL-TIGLT            PIC 9(4).                          
038200           06  W-MAAL-TIGLT-PRIO       PIC 9(4).                          
038300           06  W-MAAL-FLEXCP           PIC X.                             
038400           06  W-MAAL-IDLEVNR          PIC X(5).                          
038500     EJECT                                                                
038600                                                                          
038700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
038800     SKIP3                                                                
038900 01  KEYS-FOR-DLI.                                                        
039000     03  W-IDDC-X.                                                        
039100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
039200                                                                          
039300*    --- STATUS-KOD FRÅN IMS                                              
039400 01  STATUS-WS                   PIC XX.                                  
039500     88  SEGMENT-FOUND                       VALUE '  '.                  
039600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
039700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
039800     SKIP2                                                                
039900 01  GOOD-STATUSCODES.                                                    
040000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040100     SKIP3                                                                
040200 01  SSA1                        PIC X(64).                               
040300 01  SSA2                        PIC X(64).                               
040400*    --- IMS FUNCTION CODES                                               
040500*01  -COPY W0003                                                          
040600*    ---  DLI INPUT-OUTPUT AREA                                           
040700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
040800 01  DLI-IO-WDB601.                                                       
040900*    03  -COPY WDB601                                                     
041000*                                                                         
041100 LINKAGE SECTION.                                                         
041200                                                                          
041300*01  -COPY W0008  -PRE WDB6-                                              
041400     05  FILLER                  PIC X.                                   
041500                                                                          
041600 PROCEDURE DIVISION  USING WDB6-PCB.                                      
041700     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
041800                                                                          
041900     PERFORM A-INIT                                                       
042000     PERFORM S01-LAES-W61140                                              
042100     PERFORM UNTIL END-OF-W61140                                          
042200         PERFORM B-BEHANDLA-STATUS-POST                                   
042300         PERFORM S01-LAES-W61140                                          
042400     END-PERFORM                                                          
042500                                                                          
042600     PERFORM C-TOEM-KDINLUPF-TAB                                          
042700     PERFORM D-SKRIV-TOTAL-POST                                           
042800     PERFORM D-SKRIV-TOTAL-POST-CDC                                       
042900     PERFORM D-SKRIV-TOTAL-POST-SVS                                       
043000                                                                          
043100     PERFORM Z-FINIT                                                      
043200                                                                          
043300     MOVE ZERO TO RETURN-CODE                                             
043400     GOBACK                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 A-INIT SECTION.                                                          
043800                                                                          
043900     OPEN INPUT  W61140                                                   
044000                 W6115A                                                   
044100                                                                          
044200     OPEN OUTPUT W61155                                                   
044300     SKIP2                                                                
044400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
044500     MOVE D-AAR                TO  DAGENS-DATUM-AAR                       
044600     MOVE D-MAANAD             TO  DAGENS-DATUM-MAANAD                    
044700     MOVE D-DAG                TO  DAGENS-DATUM-DAG                       
044800     MOVE K-VECKA              TO  DAGENS-VECKA                           
044900     MOVE IDPGM                TO  POSTSUM-PROGNAMN                       
045000                                                                          
045100     SET SU-DCIX          TO +1                                           
045200     SET SUM-IX           TO +1                                           
045300     PERFORM UNTIL SU-DCIX   >  +27                                       
045400       PERFORM UNTIL SUM-IX      >  MAX-SUM-IX                            
045500         MOVE SPACE  TO W-SUM-KDINLUPF            (SU-DCIX SUM-IX)        
045600         MOVE ZERO   TO W-SUM-KVRADER             (SU-DCIX SUM-IX)        
045700                        W-SUM-KVRADER-DAG         (SU-DCIX SUM-IX)        
045800                        W-SUM-KVRADER-KVALL       (SU-DCIX SUM-IX)        
045900                        W-SUM-KVRADER-PRIO        (SU-DCIX SUM-IX)        
046000                        W-SUM-KVRADER-PRIO-DAG    (SU-DCIX SUM-IX)        
046100                        W-SUM-KVRADER-PRIO-KVALL  (SU-DCIX SUM-IX)        
046200                        W-SUM-KVART               (SU-DCIX SUM-IX)        
046300                        W-SUM-KVART-DAG           (SU-DCIX SUM-IX)        
046400                        W-SUM-KVART-KVALL         (SU-DCIX SUM-IX)        
046500                        W-SUM-SUBEL               (SU-DCIX SUM-IX)        
046600                        W-SUM-SUBEL-PRIO          (SU-DCIX SUM-IX)        
046700                        W-SUM-TIGLT-MM            (SU-DCIX SUM-IX)        
046800                        W-SUM-TIGLT-MM-DAG        (SU-DCIX SUM-IX)        
046900                        W-SUM-TIGLT-MM-KVALL      (SU-DCIX SUM-IX)        
047000                        W-SUM-TIGLT-PRIO-MM       (SU-DCIX SUM-IX)        
047100                        W-SUM-TIGLT-PRIO-MM-DAG   (SU-DCIX SUM-IX)        
047200                        W-SUM-TIGLT-PRIO-MM-KVALL (SU-DCIX SUM-IX)        
047300                        W-SUM-KVART-MAAL          (SU-DCIX SUM-IX)        
047400                        W-SUM-KVART-MAAL-PRIO     (SU-DCIX SUM-IX)        
047500                        W-SUM-VLARTNTO            (SU-DCIX SUM-IX)        
047600                        W-LAST-IDLOPNRM           (SU-DCIX SUM-IX)        
047700         MOVE SPACE TO  W-SUM-IDLEVNR             (SU-DCIX SUM-IX)        
047800         SET SUM-IX UP BY +1                                              
047900       END-PERFORM                                                        
048000                                                                          
048100       MOVE 'R31 '     TO W-SUM-KDINLUPF  (SU-DCIX   1 )                  
048200       IF SU-DCIX = 1                                                     
048300         MOVE 'C2  '     TO W-SUM-KDINLUPF  (1         2 )                
048400       END-IF                                                             
048500       SET SUM-IX  TO   +1                                                
048600       SET SU-DCIX UP BY +1                                               
048700     END-PERFORM                                                          
048800                                                                          
048900     MOVE SPACE TO W-SPAR1-AREA (1)     W-SPAR1-AREA (2)                  
049000                   W-SPAR1-AREA (3)     W-SPAR1-AREA (4)                  
049100                   W-SPAR1-AREA (5)     W-SPAR1-AREA (6)                  
049200                   W-SPAR1-AREA (7)     W-SPAR1-AREA (8)                  
049300                   W-SPAR1-AREA (9)     W-SPAR1-AREA (10)                 
049400                   W-SPAR1-AREA (11)    W-SPAR1-AREA (12)                 
049500                   W-SPAR1-AREA (13)    W-SPAR1-AREA (14)                 
049600                   W-SPAR1-AREA (15)    W-SPAR1-AREA (16)                 
049700                   W-SPAR1-AREA (17)    W-SPAR1-AREA (18)                 
049800                   W-SPAR1-AREA (19)    W-SPAR1-AREA (20)                 
049900                   W-SPAR1-AREA (21)    W-SPAR1-AREA (22)                 
050000                   W-SPAR1-AREA (23)    W-SPAR1-AREA (24)                 
050010                   W-SPAR1-AREA (25)    W-SPAR1-AREA (26)                 
050010                   W-SPAR1-AREA (26)    W-SPAR1-AREA (27)                 
050100                   W-SPAR2-AREA (1)     W-SPAR2-AREA (2)                  
050200                   W-SPAR2-AREA (3)     W-SPAR2-AREA (4)                  
050300                   W-SPAR2-AREA (5)     W-SPAR2-AREA (6)                  
050400                   W-SPAR2-AREA (7)     W-SPAR2-AREA (8)                  
050500                   W-SPAR2-AREA (9)     W-SPAR2-AREA (10)                 
050600                   W-SPAR2-AREA (11)    W-SPAR2-AREA (12)                 
050700                   W-SPAR2-AREA (13)    W-SPAR2-AREA (14)                 
050800                   W-SPAR2-AREA (15)    W-SPAR2-AREA (16)                 
050900                   W-SPAR2-AREA (17)    W-SPAR2-AREA (18)                 
051000                   W-SPAR2-AREA (19)    W-SPAR2-AREA (20)                 
051100                   W-SPAR2-AREA (21)    W-SPAR2-AREA (22)                 
051110                   W-SPAR2-AREA (23)    W-SPAR2-AREA (24)                 
051200                   W-SPAR2-AREA (25)    W-SPAR1-AREA (26)                 
051200                   W-SPAR2-AREA (26)    W-SPAR1-AREA (27)                 
051300     MOVE ZERO TO  W-SPAR1-IDLOPNRM (1) W-SPAR1-IDLOPNRM (2)              
051400                   W-SPAR1-IDLOPNRM (3) W-SPAR1-IDLOPNRM (4)              
051500                   W-SPAR1-IDLOPNRM (5) W-SPAR1-IDLOPNRM (6)              
051600                   W-SPAR1-IDLOPNRM (7) W-SPAR1-IDLOPNRM (8)              
051700                   W-SPAR1-IDLOPNRM (9) W-SPAR1-IDLOPNRM (10)             
051800                   W-SPAR1-IDLOPNRM (11) W-SPAR1-IDLOPNRM (12)            
051900                   W-SPAR1-IDLOPNRM (13) W-SPAR1-IDLOPNRM (14)            
052000                   W-SPAR1-IDLOPNRM (15) W-SPAR1-IDLOPNRM (16)            
052100                   W-SPAR1-IDLOPNRM (17) W-SPAR1-IDLOPNRM (18)            
052200                   W-SPAR1-IDLOPNRM (19) W-SPAR1-IDLOPNRM (20)            
052300                   W-SPAR1-IDLOPNRM (21) W-SPAR1-IDLOPNRM (22)            
052310                   W-SPAR1-IDLOPNRM (23) W-SPAR1-IDLOPNRM (24)            
052400                   W-SPAR1-IDLOPNRM (25) W-SPAR1-IDLOPNRM (26)            
052400                   W-SPAR1-IDLOPNRM (26) W-SPAR1-IDLOPNRM (27)            
052500                   W-SPAR2-IDLOPNRM (1) W-SPAR2-IDLOPNRM (2)              
052600                   W-SPAR2-IDLOPNRM (3) W-SPAR2-IDLOPNRM (4)              
052700                   W-SPAR2-IDLOPNRM (5) W-SPAR2-IDLOPNRM (6)              
052800                   W-SPAR2-IDLOPNRM (7) W-SPAR2-IDLOPNRM (8)              
052900                   W-SPAR2-IDLOPNRM (9) W-SPAR2-IDLOPNRM (10)             
053000                   W-SPAR2-IDLOPNRM (11) W-SPAR2-IDLOPNRM (12)            
053100                   W-SPAR2-IDLOPNRM (13) W-SPAR2-IDLOPNRM (14)            
053200                   W-SPAR2-IDLOPNRM (15) W-SPAR2-IDLOPNRM (16)            
053300                   W-SPAR2-IDLOPNRM (17) W-SPAR2-IDLOPNRM (18)            
053400                   W-SPAR2-IDLOPNRM (19) W-SPAR2-IDLOPNRM (20)            
053500                   W-SPAR2-IDLOPNRM (21) W-SPAR2-IDLOPNRM (22)            
053510                   W-SPAR2-IDLOPNRM (23) W-SPAR2-IDLOPNRM (24)            
053600                   W-SPAR2-IDLOPNRM (25) W-SPAR2-IDLOPNRM (26)            
053600                   W-SPAR2-IDLOPNRM (26) W-SPAR2-IDLOPNRM (27)            
053700                   W-SPAR2-IDRADNR (1)  W-SPAR2-IDRADNR (2)               
053800                   W-SPAR2-IDRADNR (3)  W-SPAR2-IDRADNR (4)               
053900                   W-SPAR2-IDRADNR (5)  W-SPAR2-IDRADNR (6)               
054000                   W-SPAR2-IDRADNR (7)  W-SPAR2-IDRADNR (8)               
054100                   W-SPAR2-IDRADNR (9)  W-SPAR2-IDRADNR (10)              
054200                   W-SPAR2-IDRADNR (11) W-SPAR2-IDRADNR (12)              
054300                   W-SPAR2-IDRADNR (13) W-SPAR2-IDRADNR (14)              
054400                   W-SPAR2-IDRADNR (15) W-SPAR2-IDRADNR (16)              
054500                   W-SPAR2-IDRADNR (17) W-SPAR2-IDRADNR (18)              
054600                   W-SPAR2-IDRADNR (19) W-SPAR2-IDRADNR (20)              
054700                   W-SPAR2-IDRADNR (21) W-SPAR2-IDRADNR (22)              
054710                   W-SPAR2-IDRADNR (23) W-SPAR2-IDRADNR (24)              
054800                   W-SPAR2-IDRADNR (25) W-SPAR2-IDRADNR (26)              
054800                   W-SPAR2-IDRADNR (26) W-SPAR2-IDRADNR (27)              
054900     PERFORM S02-LAES-W6115A                                              
055000     PERFORM S23-CONVERT-HHMM                                             
055100                                                                          
055200     SET MAAL-IX TO +1                                                    
055300*                             --- LADDA MÅL-TABELL FÖR CDC                
055400     MOVE MAAL-IDDC    TO WS-IDDC                                         
055500     PERFORM UNTIL END-OF-W6115A                                          
055600             OR CDC-TR OR SDC OR NDC                                      
055700       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (1 MAAL-IX)                    
055800       SET MAAL-IX UP BY +1                                               
055900       PERFORM S02-LAES-W6115A                                            
056000       PERFORM S23-CONVERT-HHMM                                           
056100       MOVE MAAL-IDDC     TO WS-IDDC                                      
056200     END-PERFORM                                                          
056300*                             --- RENSA RESTEN AV CDC-TABELLEN            
056400     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
056500       MOVE SPACE TO W-MAAL-KDINLUPF   (1 MAAL-IX)                        
056600                     W-MAAL-IDDC       (1 MAAL-IX)                        
056700                     W-MAAL-IDLEVNR    (1 MAAL-IX)                        
056800       MOVE ZERO  TO W-MAAL-TIGLT      (1 MAAL-IX)                        
056900                     W-MAAL-TIGLT-PRIO (1 MAAL-IX)                        
057000       SET MAAL-IX UP BY +1                                               
057100     END-PERFORM                                                          
057200     MOVE  SPACE   TO WS-IDDC                                             
057300                                                                          
057400     SET MAAL-IX TO +1                                                    
057500*                             --- LADDA MÅL-TABELL FÖR ST                 
057600     PERFORM UNTIL END-OF-W6115A   OR SDC OR NDC                          
057700       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (2 MAAL-IX)                    
057800       SET MAAL-IX UP BY +1                                               
057900       PERFORM S02-LAES-W6115A                                            
058000       PERFORM S23-CONVERT-HHMM                                           
058100       MOVE MAAL-IDDC     TO WS-IDDC                                      
058200     END-PERFORM                                                          
058300*                             --- RENSA RESTEN AV ST-TABELLEN             
058400     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
058500       MOVE SPACE TO W-MAAL-KDINLUPF   (2 MAAL-IX)                        
058600                     W-MAAL-IDDC       (2 MAAL-IX)                        
058700                     W-MAAL-IDLEVNR    (2 MAAL-IX)                        
058800       MOVE ZERO  TO W-MAAL-TIGLT      (2 MAAL-IX)                        
058900                     W-MAAL-TIGLT-PRIO (2 MAAL-IX)                        
059000       SET MAAL-IX UP BY +1                                               
059100     END-PERFORM                                                          
059200     MOVE  SPACE   TO WS-IDDC                                             
059300                                                                          
059400     SET MAAL-IX TO +1                                                    
059500*                             --- LADDA MÅL-TABELL FÖR DC41               
059600     PERFORM UNTIL END-OF-W6115A OR NDC-US-LA                             
059700                                 OR NDC-US-SE                             
059800                                 OR NDC-US-CH                             
059900                                 OR NDC-US-JA                             
060000                                 OR NDC-US-DA                             
060100                                 OR NDC-CA                                
060010                                 OR NDC-NS                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060500       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (3 MAAL-IX)                    
060600       SET MAAL-IX UP BY +1                                               
060700       PERFORM S02-LAES-W6115A                                            
060800       PERFORM S23-CONVERT-HHMM                                           
060900       MOVE MAAL-IDDC     TO WS-IDDC                                      
061000     END-PERFORM                                                          
061100*                             --- RENSA RESTEN AV DC41-TABELLEN           
061200     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
061300       MOVE SPACE TO W-MAAL-KDINLUPF   (3 MAAL-IX)                        
061400                     W-MAAL-IDDC       (3 MAAL-IX)                        
061500                     W-MAAL-IDLEVNR    (3 MAAL-IX)                        
061600       MOVE ZERO  TO W-MAAL-TIGLT      (3 MAAL-IX)                        
061700                     W-MAAL-TIGLT-PRIO (3 MAAL-IX)                        
061800       SET MAAL-IX UP BY +1                                               
061900     END-PERFORM                                                          
062000     MOVE SPACE TO WS-IDDC                                                
062100                                                                          
062200     SET MAAL-IX TO +1                                                    
062300*                             --- LADDA MÅL-TABELL FÖR DC43               
059700     PERFORM UNTIL END-OF-W6115A OR NDC-US-SE                             
059800                                 OR NDC-US-CH                             
059900                                 OR NDC-US-JA                             
060000                                 OR NDC-US-DA                             
060100                                 OR NDC-CA                                
060010                                 OR NDC-NS                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
063200       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (4 MAAL-IX)                    
063300       SET MAAL-IX UP BY +1                                               
063400       PERFORM S02-LAES-W6115A                                            
063500       PERFORM S23-CONVERT-HHMM                                           
063600       MOVE MAAL-IDDC      TO WS-IDDC                                     
063700     END-PERFORM                                                          
063800*                             --- RENSA RESTEN AV DC43-TABELLEN           
063900     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
064000       MOVE SPACE TO W-MAAL-KDINLUPF   (4 MAAL-IX)                        
064100                     W-MAAL-IDDC       (4 MAAL-IX)                        
064200                     W-MAAL-IDLEVNR    (4 MAAL-IX)                        
064300       MOVE ZERO  TO W-MAAL-TIGLT      (4 MAAL-IX)                        
064400                     W-MAAL-TIGLT-PRIO (4 MAAL-IX)                        
064500       SET MAAL-IX UP BY +1                                               
064600     END-PERFORM                                                          
064700     MOVE SPACE TO WS-IDDC                                                
064800                                                                          
064900     SET MAAL-IX TO +1                                                    
065000*                             --- LADDA MÅL-TABELL FÖR DC44               
059800     PERFORM UNTIL END-OF-W6115A OR NDC-US-CH                             
059900                                 OR NDC-US-JA                             
060000                                 OR NDC-US-DA                             
060100                                 OR NDC-CA                                
060010                                 OR NDC-NS                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
065800       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (5 MAAL-IX)                    
065900       SET MAAL-IX UP BY +1                                               
066000       PERFORM S02-LAES-W6115A                                            
066100       PERFORM S23-CONVERT-HHMM                                           
066200       MOVE MAAL-IDDC     TO WS-IDDC                                      
066300     END-PERFORM                                                          
066400*                             --- RENSA RESTEN AV DC44-TABELLEN           
066500     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
066600       MOVE SPACE TO W-MAAL-KDINLUPF   (5 MAAL-IX)                        
066700                     W-MAAL-IDDC       (5 MAAL-IX)                        
066800                     W-MAAL-IDLEVNR    (5 MAAL-IX)                        
066900       MOVE ZERO  TO W-MAAL-TIGLT      (5 MAAL-IX)                        
067000                     W-MAAL-TIGLT-PRIO (5 MAAL-IX)                        
067100       SET MAAL-IX UP BY +1                                               
067200     END-PERFORM                                                          
067300     MOVE SPACE TO WS-IDDC                                                
067400                                                                          
067500     SET MAAL-IX TO +1                                                    
067600*                             --- LADDA MÅL-TABELL FÖR DC45               
059900     PERFORM UNTIL END-OF-W6115A OR NDC-US-JA                             
060000                                 OR NDC-US-DA                             
060100                                 OR NDC-CA                                
060010                                 OR NDC-NS                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
068300       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (6 MAAL-IX)                    
068400       SET MAAL-IX UP BY +1                                               
068500       PERFORM S02-LAES-W6115A                                            
068600       PERFORM S23-CONVERT-HHMM                                           
068700       MOVE MAAL-IDDC     TO WS-IDDC                                      
068800     END-PERFORM                                                          
068900*                             --- RENSA RESTEN AV DC45-TABELLEN           
069000     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
069100       MOVE SPACE TO W-MAAL-KDINLUPF   (6 MAAL-IX)                        
069200                     W-MAAL-IDDC       (6 MAAL-IX)                        
069300                     W-MAAL-IDLEVNR    (6 MAAL-IX)                        
069400       MOVE ZERO  TO W-MAAL-TIGLT      (6 MAAL-IX)                        
069500                     W-MAAL-TIGLT-PRIO (6 MAAL-IX)                        
069600       SET MAAL-IX UP BY +1                                               
069700     END-PERFORM                                                          
069800     MOVE SPACE TO WS-IDDC                                                
069900                                                                          
070000     SET MAAL-IX TO +1                                                    
070100*                             --- LADDA MÅL-TABELL FÖR DC46               
060000     PERFORM UNTIL END-OF-W6115A OR NDC-US-DA                             
060100                                 OR NDC-CA                                
060010                                 OR NDC-NS                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
070700       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (7 MAAL-IX)                    
070800       SET MAAL-IX UP BY +1                                               
070900       PERFORM S02-LAES-W6115A                                            
071000       PERFORM S23-CONVERT-HHMM                                           
071100       MOVE MAAL-IDDC     TO WS-IDDC                                      
071200     END-PERFORM                                                          
071300*                             --- RENSA RESTEN AV DC46-TABELLEN           
071400     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
071500       MOVE SPACE TO W-MAAL-KDINLUPF   (7 MAAL-IX)                        
071600                     W-MAAL-IDDC       (7 MAAL-IX)                        
071700                     W-MAAL-IDLEVNR    (7 MAAL-IX)                        
071800       MOVE ZERO  TO W-MAAL-TIGLT      (7 MAAL-IX)                        
071900                     W-MAAL-TIGLT-PRIO (7 MAAL-IX)                        
072000       SET MAAL-IX UP BY +1                                               
072100     END-PERFORM                                                          
072200     MOVE SPACE TO WS-IDDC                                                
072400     SET MAAL-IX TO +1                                                    
072500*                             --- LADDA MÅL-TABELL FÖR DC47               
060100     PERFORM UNTIL END-OF-W6115A OR NDC-CA                                
060010                                 OR NDC-NS                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
073000       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (8 MAAL-IX)                    
073100       SET MAAL-IX UP BY +1                                               
073200       PERFORM S02-LAES-W6115A                                            
073300       PERFORM S23-CONVERT-HHMM                                           
073400       MOVE MAAL-IDDC     TO WS-IDDC                                      
073500     END-PERFORM                                                          
068900*                             --- RENSA RESTEN AV DC47-TABELLEN           
073600     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
073700       MOVE SPACE TO W-MAAL-KDINLUPF   (8 MAAL-IX)                        
073800                     W-MAAL-IDDC       (8 MAAL-IX)                        
073900                     W-MAAL-IDLEVNR    (8 MAAL-IX)                        
074000       MOVE ZERO  TO W-MAAL-TIGLT      (8 MAAL-IX)                        
074100                     W-MAAL-TIGLT-PRIO (8 MAAL-IX)                        
074200       SET MAAL-IX UP BY +1                                               
074300     END-PERFORM                                                          
074400     MOVE SPACE TO WS-IDDC                                                
074500                                                                          
074600     SET MAAL-IX TO +1                                                    
074700*                             --- LADDA MÅL-TABELL FÖR DC51               
060010     PERFORM UNTIL END-OF-W6115A OR NDC-NS                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
075100       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (9 MAAL-IX)                    
075200       SET MAAL-IX UP BY +1                                               
075300       PERFORM S02-LAES-W6115A                                            
075400       PERFORM S23-CONVERT-HHMM                                           
075500       MOVE MAAL-IDDC     TO WS-IDDC                                      
075600     END-PERFORM                                                          
075700*                             --- RENSA RESTEN AV DC51-TABELLEN           
075800     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
075900       MOVE SPACE TO W-MAAL-KDINLUPF   (9 MAAL-IX)                        
076000                     W-MAAL-IDDC       (9 MAAL-IX)                        
076100                     W-MAAL-IDLEVNR    (9 MAAL-IX)                        
076200       MOVE ZERO  TO W-MAAL-TIGLT      (9 MAAL-IX)                        
076300                     W-MAAL-TIGLT-PRIO (9 MAAL-IX)                        
076400       SET MAAL-IX UP BY +1                                               
076500     END-PERFORM                                                          
076600     MOVE SPACE TO WS-IDDC                                                
           SET MAAL-IX TO +1                                                    
105810*                              ---- LADDA MÅL-TABELL FÖR DC52             
060010     PERFORM UNTIL END-OF-W6115A OR NDC-MX                                
060200                                 OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
105830       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (10 MAAL-IX)                   
105840       SET MAAL-IX UP BY +1                                               
105850       PERFORM S02-LAES-W6115A                                            
105860       PERFORM S23-CONVERT-HHMM                                           
105870     END-PERFORM                                                          
105880*                             --- RENSA RESTEN AV DC52-TABELLEN           
105890     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
105891       MOVE SPACE TO W-MAAL-KDINLUPF   (10 MAAL-IX)                       
105892                     W-MAAL-IDDC       (10 MAAL-IX)                       
105893                     W-MAAL-IDLEVNR    (10 MAAL-IX)                       
105894       MOVE ZERO  TO W-MAAL-TIGLT      (10 MAAL-IX)                       
105895                     W-MAAL-TIGLT-PRIO (10 MAAL-IX)                       
105896       SET MAAL-IX UP BY +1                                               
105897     END-PERFORM                                                          
           SET MAAL-IX TO +1                                                    
105898*                              ---- LADDA MÅL-TABELL FÖR DC53             
060200     PERFORM UNTIL END-OF-W6115A OR NDC-PACIFIC                           
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
105900       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (11 MAAL-IX)                   
105901       SET MAAL-IX UP BY +1                                               
105902       PERFORM S02-LAES-W6115A                                            
105903       PERFORM S23-CONVERT-HHMM                                           
105904     END-PERFORM                                                          
105905*                             --- RENSA RESTEN AV DC53-TABELLEN           
105906     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
105907       MOVE SPACE TO W-MAAL-KDINLUPF   (11 MAAL-IX)                       
105908                     W-MAAL-IDDC       (11 MAAL-IX)                       
105909                     W-MAAL-IDLEVNR    (11 MAAL-IX)                       
105910       MOVE ZERO  TO W-MAAL-TIGLT      (11 MAAL-IX)                       
105911                     W-MAAL-TIGLT-PRIO (11 MAAL-IX)                       
105912       SET MAAL-IX UP BY +1                                               
105913     END-PERFORM                                                          
076800     SET MAAL-IX TO +1                                                    
076900*                             --- LADDA MÅL-TABELL FÖR DC61               
060200     PERFORM UNTIL END-OF-W6115A OR NDC-AU                                
060200                                 OR NDC-TH-63                             
060200                                 OR NDC-TW                                
060200                                 OR NDC-KR                                
060200                                 OR NDC-MY                                
060200                                 OR NDC-IN                                
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
077300       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (12 MAAL-IX)                   
077400       SET MAAL-IX UP BY +1                                               
077500       PERFORM S02-LAES-W6115A                                            
077600       PERFORM S23-CONVERT-HHMM                                           
077700       MOVE MAAL-IDDC     TO WS-IDDC                                      
077800     END-PERFORM                                                          
077900*                             --- RENSA RESTEN AV DC61-TABELLEN           
078000     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
078100       MOVE SPACE TO W-MAAL-KDINLUPF   (12 MAAL-IX)                       
078200                     W-MAAL-IDDC       (12 MAAL-IX)                       
078300                     W-MAAL-IDLEVNR    (12 MAAL-IX)                       
078400       MOVE ZERO  TO W-MAAL-TIGLT      (12 MAAL-IX)                       
078500                     W-MAAL-TIGLT-PRIO (12 MAAL-IX)                       
078600       SET MAAL-IX UP BY +1                                               
078700     END-PERFORM                                                          
078800     MOVE SPACE TO WS-IDDC                                                
078900                                                                          
079000     SET MAAL-IX TO +1                                                    
079100*                             --- LADDA MÅL-TABELL FÖR DC62               
060200     PERFORM UNTIL END-OF-W6115A OR NDC-TH-63                             
060200                                 OR NDC-TW                                
060200                                 OR NDC-KR                                
060200                                 OR NDC-MY                                
060200                                 OR NDC-IN                                
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
079500       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (13 MAAL-IX)                   
079600       SET MAAL-IX UP BY +1                                               
079700       PERFORM S02-LAES-W6115A                                            
079800       PERFORM S23-CONVERT-HHMM                                           
079900       MOVE MAAL-IDDC     TO WS-IDDC                                      
080000     END-PERFORM                                                          
080100*                             --- RENSA RESTEN AV DC62-TABELLEN           
080200     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
080300       MOVE SPACE TO W-MAAL-KDINLUPF   (13 MAAL-IX)                       
080400                     W-MAAL-IDDC       (13 MAAL-IX)                       
080500                     W-MAAL-IDLEVNR    (13 MAAL-IX)                       
080600       MOVE ZERO  TO W-MAAL-TIGLT      (13 MAAL-IX)                       
080700                     W-MAAL-TIGLT-PRIO (13 MAAL-IX)                       
080800       SET MAAL-IX UP BY +1                                               
080900     END-PERFORM                                                          
081000     MOVE SPACE TO WS-IDDC                                                
081100*                             --- LADDA MÅL-TABELL FÖR DC63               
060200     PERFORM UNTIL END-OF-W6115A OR NDC-TW                                
060200                                 OR NDC-KR                                
060200                                 OR NDC-MY                                
060200                                 OR NDC-IN                                
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
081400                                                                          
081500       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (14 MAAL-IX)                   
081600       SET MAAL-IX UP BY +1                                               
081700       PERFORM S02-LAES-W6115A                                            
081800       PERFORM S23-CONVERT-HHMM                                           
081900       MOVE MAAL-IDDC     TO WS-IDDC                                      
082000     END-PERFORM                                                          
082100*                             --- RENSA RESTEN AV DC63-TABELLEN           
082200     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
082300       MOVE SPACE TO W-MAAL-KDINLUPF   (14 MAAL-IX)                       
082400                     W-MAAL-IDDC       (14 MAAL-IX)                       
082500                     W-MAAL-IDLEVNR    (14 MAAL-IX)                       
082600       MOVE ZERO  TO W-MAAL-TIGLT      (14 MAAL-IX)                       
082700                     W-MAAL-TIGLT-PRIO (14 MAAL-IX)                       
082800       SET MAAL-IX UP BY +1                                               
082900     END-PERFORM                                                          
083000     MOVE SPACE TO WS-IDDC                                                
083100                                                                          
083200*                             --- LADDA MÅL-TABELL FÖR DC64               
060200     PERFORM UNTIL END-OF-W6115A OR NDC-KR                                
060200                                 OR NDC-MY                                
060200                                 OR NDC-IN                                
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
083500                                                                          
083600       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (15 MAAL-IX)                   
083700       SET MAAL-IX UP BY +1                                               
083800       PERFORM S02-LAES-W6115A                                            
083900       PERFORM S23-CONVERT-HHMM                                           
084000       MOVE MAAL-IDDC     TO WS-IDDC                                      
084100     END-PERFORM                                                          
084200*                             --- RENSA RESTEN AV DC64-TABELLEN           
084300     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
084400       MOVE SPACE TO W-MAAL-KDINLUPF   (15 MAAL-IX)                       
084500                     W-MAAL-IDDC       (15 MAAL-IX)                       
084600                     W-MAAL-IDLEVNR    (15 MAAL-IX)                       
084700       MOVE ZERO  TO W-MAAL-TIGLT      (15 MAAL-IX)                       
084800                     W-MAAL-TIGLT-PRIO (15 MAAL-IX)                       
084900       SET MAAL-IX UP BY +1                                               
085000     END-PERFORM                                                          
085100     MOVE SPACE TO WS-IDDC                                                
085200                                                                          
085300     SET MAAL-IX TO +1                                                    
085400*                             --- LADDA MÅL-TABELL FÖR DC65               
060200     PERFORM UNTIL END-OF-W6115A OR NDC-MY                                
060200                                 OR NDC-IN                                
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
086000       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (16 MAAL-IX)                   
086100       SET MAAL-IX UP BY +1                                               
086200       PERFORM S02-LAES-W6115A                                            
086300       PERFORM S23-CONVERT-HHMM                                           
086400       MOVE MAAL-IDDC     TO WS-IDDC                                      
086500     END-PERFORM                                                          
086600*                             --- RENSA RESTEN AV DC65-TABELLEN           
086700     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
086800       MOVE SPACE TO W-MAAL-KDINLUPF   (16 MAAL-IX)                       
086900                     W-MAAL-IDDC       (16 MAAL-IX)                       
087000                     W-MAAL-IDLEVNR    (16 MAAL-IX)                       
087100       MOVE ZERO  TO W-MAAL-TIGLT      (16 MAAL-IX)                       
087200                     W-MAAL-TIGLT-PRIO (16 MAAL-IX)                       
087300       SET MAAL-IX UP BY +1                                               
087400     END-PERFORM                                                          
087500     MOVE SPACE TO WS-IDDC                                                
087600                                                                          
087700     SET MAAL-IX TO +1                                                    
087800*                             --- LADDA MÅL-TABELL FÖR DC66               
060200     PERFORM UNTIL END-OF-W6115A OR NDC-IN                                
060300                                 OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
088300       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (17 MAAL-IX)                   
088400       SET MAAL-IX UP BY +1                                               
088500       PERFORM S02-LAES-W6115A                                            
088600       PERFORM S23-CONVERT-HHMM                                           
088700       MOVE MAAL-IDDC     TO WS-IDDC                                      
088800     END-PERFORM                                                          
088900*                             --- RENSA RESTEN AV DC66-TABELLEN           
089000     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
089100       MOVE SPACE TO W-MAAL-KDINLUPF   (17 MAAL-IX)                       
089200                     W-MAAL-IDDC       (17 MAAL-IX)                       
089300                     W-MAAL-IDLEVNR    (17 MAAL-IX)                       
089400       MOVE ZERO  TO W-MAAL-TIGLT      (17 MAAL-IX)                       
089500                     W-MAAL-TIGLT-PRIO (17 MAAL-IX)                       
089600       SET MAAL-IX UP BY +1                                               
089700     END-PERFORM                                                          
089800     MOVE SPACE TO WS-IDDC                                                
089900                                                                          
090000     SET MAAL-IX TO +1                                                    
090100*                             --- LADDA MÅL-TABELL FÖR DC67               
060300     PERFORM UNTIL END-OF-W6115A OR NDC-CN                                
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
090500       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (18 MAAL-IX)                   
090600       SET MAAL-IX UP BY +1                                               
090700       PERFORM S02-LAES-W6115A                                            
090800       PERFORM S23-CONVERT-HHMM                                           
090900       MOVE MAAL-IDDC     TO WS-IDDC                                      
091000     END-PERFORM                                                          
091100*                             --- RENSA RESTEN AV DC67-TABELLEN           
091200     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
091300       MOVE SPACE TO W-MAAL-KDINLUPF   (18 MAAL-IX)                       
091400                     W-MAAL-IDDC       (18 MAAL-IX)                       
091500                     W-MAAL-IDLEVNR    (18 MAAL-IX)                       
091600       MOVE ZERO  TO W-MAAL-TIGLT      (18 MAAL-IX)                       
091700                     W-MAAL-TIGLT-PRIO (18 MAAL-IX)                       
091800       SET MAAL-IX UP BY +1                                               
091900     END-PERFORM                                                          
092000     MOVE SPACE TO WS-IDDC                                                
092100                                                                          
092200     SET MAAL-IX TO +1                                                    
092300*                             --- LADDA MÅL-TABELL FÖR DC71               
060300     PERFORM UNTIL END-OF-W6115A OR NDC-CN-72                             
060300                                 OR NDC-CN-73                             
060300                                 OR NDC-CN-74                             
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
092900       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (19 MAAL-IX)                   
093000       SET MAAL-IX UP BY +1                                               
093100       PERFORM S02-LAES-W6115A                                            
093200       PERFORM S23-CONVERT-HHMM                                           
093300       MOVE MAAL-IDDC     TO WS-IDDC                                      
093400     END-PERFORM                                                          
093500*                             --- RENSA RESTEN AV DC71-TABELLEN           
093600     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
093700       MOVE SPACE TO W-MAAL-KDINLUPF   (19 MAAL-IX)                       
093800                     W-MAAL-IDDC       (19 MAAL-IX)                       
093900                     W-MAAL-IDLEVNR    (19 MAAL-IX)                       
094000       MOVE ZERO  TO W-MAAL-TIGLT      (19 MAAL-IX)                       
094100                     W-MAAL-TIGLT-PRIO (19 MAAL-IX)                       
094200       SET MAAL-IX UP BY +1                                               
094300     END-PERFORM                                                          
094400     MOVE SPACE TO WS-IDDC                                                
094500                                                                          
094600     SET MAAL-IX TO +1                                                    
094700*                             --- LADDA MÅL-TABELL FÖR DC72               
060300     PERFORM UNTIL END-OF-W6115A OR NDC-CN-73                             
060300                                 OR NDC-CN-74                             
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
095200       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (20 MAAL-IX)                   
095300       SET MAAL-IX UP BY +1                                               
095400       PERFORM S02-LAES-W6115A                                            
095500       PERFORM S23-CONVERT-HHMM                                           
095600     END-PERFORM                                                          
095700*                             --- RENSA RESTEN AV DC72-TABELLEN           
095800     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
095900       MOVE SPACE TO W-MAAL-KDINLUPF   (20 MAAL-IX)                       
096000                     W-MAAL-IDDC       (20 MAAL-IX)                       
096100                     W-MAAL-IDLEVNR    (20 MAAL-IX)                       
096200       MOVE ZERO  TO W-MAAL-TIGLT      (20 MAAL-IX)                       
096300                     W-MAAL-TIGLT-PRIO (20 MAAL-IX)                       
096400       SET MAAL-IX UP BY +1                                               
096500     END-PERFORM                                                          
096600     MOVE SPACE TO WS-IDDC                                                
096700                                                                          
096800     SET MAAL-IX TO +1                                                    
096900*                             --- LADDA MÅL-TABELL FÖR DC73               
060300     PERFORM UNTIL END-OF-W6115A OR NDC-CN-74                             
060300                                 OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
097300       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (21 MAAL-IX)                   
097400       SET MAAL-IX UP BY +1                                               
097500       PERFORM S02-LAES-W6115A                                            
097600       PERFORM S23-CONVERT-HHMM                                           
097700     END-PERFORM                                                          
097800*                             --- RENSA RESTEN AV DC73-TABELLEN           
097900     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
098000       MOVE SPACE TO W-MAAL-KDINLUPF   (21 MAAL-IX)                       
098100                     W-MAAL-IDDC       (21 MAAL-IX)                       
098200                     W-MAAL-IDLEVNR    (21 MAAL-IX)                       
098300       MOVE ZERO  TO W-MAAL-TIGLT      (21 MAAL-IX)                       
098400                     W-MAAL-TIGLT-PRIO (21 MAAL-IX)                       
098500       SET MAAL-IX UP BY +1                                               
098600     END-PERFORM                                                          
098700                                                                          
098800     SET MAAL-IX TO +1                                                    
098900*                             --- LADDA MÅL-TABELL FÖR DC74               
060300     PERFORM UNTIL END-OF-W6115A OR NDC-NX                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
099200       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (22 MAAL-IX)                   
099300       SET MAAL-IX UP BY +1                                               
099400       PERFORM S02-LAES-W6115A                                            
099500       PERFORM S23-CONVERT-HHMM                                           
099600     END-PERFORM                                                          
099700*                             --- RENSA RESTEN AV DC74-TABELLEN           
099800     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
099900       MOVE SPACE TO W-MAAL-KDINLUPF   (22 MAAL-IX)                       
100000                     W-MAAL-IDDC       (22 MAAL-IX)                       
100100                     W-MAAL-IDLEVNR    (22 MAAL-IX)                       
100200       MOVE ZERO  TO W-MAAL-TIGLT      (22 MAAL-IX)                       
100300                     W-MAAL-TIGLT-PRIO (22 MAAL-IX)                       
100400       SET MAAL-IX UP BY +1                                               
100500     END-PERFORM                                                          
100600                                                                          
100700     SET MAAL-IX TO +1                                                    
100800*                             --- LADDA MÅL-TABELL FÖR DC85               
060300     PERFORM UNTIL END-OF-W6115A OR NDC-TR                                
060300                                 OR NDC-AE                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
101100       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (23 MAAL-IX)                   
101200       SET MAAL-IX UP BY +1                                               
101300       PERFORM S02-LAES-W6115A                                            
101400       PERFORM S23-CONVERT-HHMM                                           
101500     END-PERFORM                                                          
101600*                             --- RENSA RESTEN AV DC85-TABELLEN           
101700     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
101800       MOVE SPACE TO W-MAAL-KDINLUPF   (23 MAAL-IX)                       
101900                     W-MAAL-IDDC       (23 MAAL-IX)                       
102000                     W-MAAL-IDLEVNR    (23 MAAL-IX)                       
102100       MOVE ZERO  TO W-MAAL-TIGLT      (23 MAAL-IX)                       
102200                     W-MAAL-TIGLT-PRIO (23 MAAL-IX)                       
102300       SET MAAL-IX UP BY +1                                               
102400     END-PERFORM                                                          
102500                                                                          
100800*                             --- LADDA MÅL-TABELL FÖR DC86               
060300     PERFORM UNTIL END-OF-W6115A OR NDC-AE                                
060400                                 OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
101100       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (24 MAAL-IX)                   
101200       SET MAAL-IX UP BY +1                                               
101300       PERFORM S02-LAES-W6115A                                            
101400       PERFORM S23-CONVERT-HHMM                                           
101500     END-PERFORM                                                          
101600*                             --- RENSA RESTEN AV DC86-TABELLEN           
101700     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
101800       MOVE SPACE TO W-MAAL-KDINLUPF   (24 MAAL-IX)                       
101900                     W-MAAL-IDDC       (24 MAAL-IX)                       
102000                     W-MAAL-IDLEVNR    (24 MAAL-IX)                       
102100       MOVE ZERO  TO W-MAAL-TIGLT      (24 MAAL-IX)                       
102200                     W-MAAL-TIGLT-PRIO (24 MAAL-IX)                       
102300       SET MAAL-IX UP BY +1                                               
102400     END-PERFORM                                                          
102500                                                                          
102600*                             --- LADDA MÅL-TABELL FÖR DC87               
060400     PERFORM UNTIL END-OF-W6115A OR NDC-US-BAT                            
060400                                 OR NDC-TH-93                             
102800       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (25 MAAL-IX)                   
102900       SET MAAL-IX UP BY +1                                               
103000       PERFORM S02-LAES-W6115A                                            
103100       PERFORM S23-CONVERT-HHMM                                           
103200     END-PERFORM                                                          
103300*                             --- RENSA RESTEN AV DC87-TABELLEN           
103400     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
103500       MOVE SPACE TO W-MAAL-KDINLUPF   (25 MAAL-IX)                       
103600                     W-MAAL-IDDC       (25 MAAL-IX)                       
103700                     W-MAAL-IDLEVNR    (25 MAAL-IX)                       
103800       MOVE ZERO  TO W-MAAL-TIGLT      (25 MAAL-IX)                       
103900                     W-MAAL-TIGLT-PRIO (25 MAAL-IX)                       
104000       SET MAAL-IX UP BY +1                                               
104100     END-PERFORM                                                          
104200                                                                          
104300*                              ---- LADDA MÅL-TABELL FÖR DC92             
060400     PERFORM UNTIL END-OF-W6115A OR NDC-TH-93                             
104500       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (26 MAAL-IX)                   
104600       SET MAAL-IX UP BY +1                                               
104700       PERFORM S02-LAES-W6115A                                            
104800       PERFORM S23-CONVERT-HHMM                                           
104900     END-PERFORM                                                          
105000*                             --- RENSA RESTEN AV DC92-TABELLEN           
105100     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
105200       MOVE SPACE TO W-MAAL-KDINLUPF   (26 MAAL-IX)                       
105300                     W-MAAL-IDDC       (26 MAAL-IX)                       
105400                     W-MAAL-IDLEVNR    (26 MAAL-IX)                       
105500       MOVE ZERO  TO W-MAAL-TIGLT      (26 MAAL-IX)                       
105600                     W-MAAL-TIGLT-PRIO (26 MAAL-IX)                       
105700       SET MAAL-IX UP BY +1                                               
105800     END-PERFORM                                                          
104300*                              ---- LADDA MÅL-TABELL FÖR DC93             
104400     PERFORM UNTIL END-OF-W6115A                                          
104500       MOVE MAAL-AREA-DATA TO W-MAAL-TOTAL (27 MAAL-IX)                   
104600       SET MAAL-IX UP BY +1                                               
104700       PERFORM S02-LAES-W6115A                                            
104800       PERFORM S23-CONVERT-HHMM                                           
104900     END-PERFORM                                                          
105000*                             --- RENSA RESTEN AV DC93-TABELLEN           
105100     PERFORM UNTIL MAAL-IX > MAX-MAAL-IX                                  
105200       MOVE SPACE TO W-MAAL-KDINLUPF   (27 MAAL-IX)                       
105300                     W-MAAL-IDDC       (27 MAAL-IX)                       
105400                     W-MAAL-IDLEVNR    (27 MAAL-IX)                       
105500       MOVE ZERO  TO W-MAAL-TIGLT      (27 MAAL-IX)                       
105600                     W-MAAL-TIGLT-PRIO (27 MAAL-IX)                       
105700       SET MAAL-IX UP BY +1                                               
105800     END-PERFORM                                                          
105920     .                                                                    
106000     EJECT                                                                
106100 B-BEHANDLA-STATUS-POST     SECTION.                                      
106200                                                                          
106300     MOVE IN-IDDC        TO WS-IDDC                                       
106400     IF CDC-SE                                                            
106500       MOVE +1           TO DCIX                                          
106600     END-IF                                                               
106700     IF CDC-TR                                                            
106800       MOVE +2           TO DCIX                                          
106900     END-IF                                                               
107000     IF NDC-US-RU                                                         
107100       MOVE +3           TO DCIX                                          
107200     END-IF                                                               
107300     IF NDC-US-LA                                                         
107400       MOVE +4           TO DCIX                                          
107500     END-IF                                                               
107600     IF NDC-US-SE                                                         
107700       MOVE +5           TO DCIX                                          
107800     END-IF                                                               
107900     IF NDC-US-CH                                                         
108000       MOVE +6           TO DCIX                                          
108100     END-IF                                                               
108200     IF NDC-US-JA                                                         
108300       MOVE +7           TO DCIX                                          
108400     END-IF                                                               
108500     IF NDC-US-DA                                                         
108600       MOVE +8           TO DCIX                                          
108700     END-IF                                                               
108800     IF NDC-CA                                                            
108900       MOVE +9           TO DCIX                                          
109000     END-IF                                                               
113210     IF NDC-BR                                                            
113220       MOVE +10          TO DCIX                                          
113230     END-IF                                                               
113240     IF NDC-MX                                                            
113250       MOVE +11          TO DCIX                                          
113260     END-IF                                                               
109100     IF NDC-JP-61                                                         
109200       MOVE +12          TO DCIX                                          
109300     END-IF                                                               
109400     IF NDC-AU                                                            
109500       MOVE +13          TO DCIX                                          
109600     END-IF                                                               
109700     IF NDC-TH                                                            
109800       MOVE +14          TO DCIX                                          
109900     END-IF                                                               
110000     IF NDC-TW                                                            
110100       MOVE +15          TO DCIX                                          
110200     END-IF                                                               
110300     IF NDC-KR                                                            
110400       MOVE +16          TO DCIX                                          
110500     END-IF                                                               
110600     IF NDC-MY                                                            
110700       MOVE +17          TO DCIX                                          
110800     END-IF                                                               
110900     IF NDC-IN                                                            
111000       MOVE +18          TO DCIX                                          
111100     END-IF                                                               
111200     IF NDC-CN-71                                                         
111300       MOVE +19          TO DCIX                                          
111400     END-IF                                                               
111500     IF NDC-CN-72                                                         
111600       MOVE +20          TO DCIX                                          
111700     END-IF                                                               
111800     IF NDC-CN-73                                                         
111900       MOVE +21          TO DCIX                                          
112000     END-IF                                                               
112100     IF NDC-CN-74                                                         
112200       MOVE +22          TO DCIX                                          
112300     END-IF                                                               
112100     IF NDC-ZA                                                            
112200       MOVE +23          TO DCIX                                          
112300     END-IF                                                               
112400     IF NDC-TR                                                            
112500       MOVE +24          TO DCIX                                          
112600     END-IF                                                               
112700     IF NDC-AE                                                            
112800       MOVE +25          TO DCIX                                          
112900     END-IF                                                               
113000     IF NDC-US-BAT                                                        
113100       MOVE +26          TO DCIX                                          
113200     END-IF                                                               
113000     IF NDC-TH-93                                                         
113100       MOVE +27          TO DCIX                                          
113200     END-IF                                                               
113300     SET  SU-DCIX        TO  DCIX                                         
113400     SET  MA-DCIX        TO  DCIX                                         
113500                                                                          
113600     MOVE NEJ TO TIGLT-ADDED                                              
113700* CCID 6442199 -                                                          
113800     MOVE IN-TIKLOCK     TO W-DK-TIKLOCK                                  
113900     MOVE 'DAG' TO NEW-DAG-KVALL-SW                                       
114000     IF W-DK-HH <= 15                                                     
114100       IF W-DK-HH = 15 AND W-DK-MM > 30                                   
114200         MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                 
114300       END-IF                                                             
114400     ELSE                                                                 
114500       MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                   
114600     END-IF                                                               
114700                                                                          
114800     IF IN-IDLOPNRM NOT = W-SPAR1-IDLOPNRM (DCIX)                         
114900       MOVE ZERO TO W-SPAR3-TIREGDAT                                      
115000                    W-SPAR3-TIKLOCK                                       
115100                    W-SPAR4-TIREGDAT                                      
115200                    W-SPAR4-TIKLOCK                                       
115300     END-IF                                                               
115400                                                                          
115500     IF IN-IDRADNR = 1 AND DCIX = 1                                       
115600       IF IN-KVINLART = 0                                                 
115700         MOVE IN-AREA TO W-SPAR4-AREA                                     
115800       ELSE                                                               
115900         MOVE IN-AREA TO W-SPAR3-AREA                                     
116000       END-IF                                                             
116100     END-IF                                                               
116200                                                                          
116300     PERFORM BA-KOLLA-OM-NY-KDINLUPF                                      
116400     IF IN-IDLOPNRM    = W-SPAR1-IDLOPNRM (DCIX)                          
116500       IF IN-IDRADNR = 2                                                  
116600         IF ((IN-TIREGDAT = W-SPAR1-TIREGDAT(DCIX)) AND                   
116700             (IN-TIKLOCK = W-SPAR1-TIKLOCK(DCIX)))  OR                    
116800             (IN-TIREGDAT < W-SPAR1-TIREGDAT(DCIX))                       
116900           MOVE IN-AREA TO W-SPAR1-AREA (DCIX)                            
117000         END-IF                                                           
117100       END-IF                                                             
117200     ELSE                                                                 
117300       MOVE IN-AREA TO W-SPAR1-AREA (DCIX)                                
117400     END-IF                                                               
117500                                                                          
117600     PERFORM S21-KONVERTERA-DATUM                                         
117700                                                                          
117800     IF IN-IDLOPNRM        = W-SPAR2-IDLOPNRM (DCIX) AND                  
117900        IN-IDRADNR         = W-SPAR2-IDRADNR  (DCIX) AND                  
118000        W-TIVV             = DAGENS-VECKA                                 
118100       PERFORM BB-TA-FRAM-KDINLUPF-INDEX                                  
118200                                                                          
118300       IF IN-KDINLUPF-NXT = SPACE                                         
118400         IF IN-KDINLUPF   = W-SPAR2-KDINLUPF (DCIX) AND                   
118500            IN-KDINLSTA   = W-SPAR2-KDINLSTA (DCIX) AND                   
118600            IN-KVINLART   = W-SPAR2-KVINLART (DCIX)                       
118700           CONTINUE                                                       
118800         ELSE                                                             
118900           PERFORM BC-BEHANDLA-PRODUCERAT-IDAG                            
119000           PERFORM BD-BEHANDLA-GENOMLOPPSTID                              
119100         END-IF                                                           
119200       ELSE                                                               
119300         IF IN-KDINLUPF-NXT = W-SPAR2-KDINLUPF (DCIX) AND                 
119400            IN-KDINLSTA     = W-SPAR2-KDINLSTA (DCIX) AND                 
119500            IN-KVINLART     = W-SPAR2-KVINLART (DCIX)                     
119600           CONTINUE                                                       
119700         ELSE                                                             
119800           PERFORM BC-BEHANDLA-PRODUCERAT-IDAG                            
119900           PERFORM BD-BEHANDLA-GENOMLOPPSTID                              
120000         END-IF                                                           
120100       END-IF                                                             
120200     END-IF                                                               
120300                                                                          
120400     IF (IN-KDINLSTA = 'INL' OR 'VOR' OR 'FRD') AND                       
120500         W-TIVV      = DAGENS-VECKA                                       
120600       IF IN-IDRADNR NOT = W-SPAR2-IDRADNR (DCIX)                         
120700                                                                          
120800         PERFORM BA-KOLLA-OM-NY-KDINLUPF                                  
120900         MOVE IN-TIKLOCK TO NEW-TIKLOCK                                   
121000         MOVE W-SPAR4-TIKLOCK TO OLD-TIKLOCK                              
121100         IF TIGLT-ADDED = JA OR (W-SPAR4-TIREGDAT > 0 AND                 
121200             OLD-TIKLOCK(1:4) = NEW-TIKLOCK(1:4))                         
121300                                                                          
121400           ADD  +1  TO W-SUM-KVRADER       (SU-DCIX SUM-IX)               
121500* CCID 6442199 -                                                          
121600           IF NEW-DAG                                                     
121700             ADD +1 TO W-SUM-KVRADER-DAG   (SU-DCIX SUM-IX)               
121800           ELSE                                                           
121900             ADD +1 TO W-SUM-KVRADER-KVALL (SU-DCIX SUM-IX)               
122000           END-IF                                                         
122100                                                                          
122200           IF IN-KDINLPRIO      <  +31                                    
122300             ADD +1    TO W-SUM-KVRADER-PRIO (SU-DCIX SUM-IX)             
122400* CCID 6442199 -                                                          
122500             IF NEW-DAG                                                   
122600               ADD +1  TO W-SUM-KVRADER-PRIO-DAG (SU-DCIX SUM-IX)         
122700             ELSE                                                         
122800               ADD +1  TO W-SUM-KVRADER-PRIO-KVALL(SU-DCIX SUM-IX)        
122900             END-IF                                                       
123000                                                                          
123100             COMPUTE WS-SUBEL-PRIO  =  IN-PRARTSTD * IN-KVINLART          
123200             ADD     WS-SUBEL-PRIO                                        
123300                     TO W-SUM-SUBEL-PRIO (SU-DCIX SUM-IX)                 
123400           END-IF                                                         
123500                                                                          
123600           COMPUTE W-SUM-SUBEL (SU-DCIX SUM-IX) =                         
123700                   W-SUM-SUBEL (SU-DCIX SUM-IX) +                         
123800                  (IN-KVINLART * IN-PRARTSTD)                             
123900                                                                          
124000           COMPUTE W-SUM-VLARTNTO (SU-DCIX SUM-IX) =                      
124100                   W-SUM-VLARTNTO (SU-DCIX SUM-IX) +                      
124200                  (IN-VLARTNTO * IN-KVINLART)                             
124300                                                                          
124400           IF TIGLT-ADDED = NEJ                                           
124500             IF W-SPAR3-TIREGDAT > 0 AND W-SPAR3-KDINLUPF =               
124600               IN-KDINLUPF AND W-SPAR3-KVINLART = IN-KVINLART             
124700               MOVE W-SPAR3-TIREGDAT TO W-SPAR2-TIREGDAT (DCIX)           
124800               MOVE W-SPAR3-TIKLOCK  TO W-SPAR2-TIKLOCK (DCIX)            
124900               PERFORM BD-BEHANDLA-GENOMLOPPSTID                          
125000             END-IF                                                       
125100           END-IF                                                         
125200         END-IF                                                           
125300                                                                          
125400         IF IN-IDLOPNRM NOT = W-LAST-IDLOPNRM (SU-DCIX SUM-IX)            
125500           ADD +1           TO W-SUM-KVART (SU-DCIX SUM-IX)               
125600           MOVE IN-IDLOPNRM TO W-LAST-IDLOPNRM (SU-DCIX SUM-IX)           
125700* CCID 6442199 -                                                          
125800           IF NEW-DAG                                                     
125900             ADD +1         TO W-SUM-KVART-DAG (SU-DCIX SUM-IX)           
126000           ELSE                                                           
126100             ADD +1         TO W-SUM-KVART-KVALL(SU-DCIX SUM-IX)          
126200           END-IF                                                         
126300         END-IF                                                           
126400       END-IF                                                             
126500     END-IF                                                               
126600                                                                          
126700     MOVE IN-AREA      TO W-SPAR2-AREA (DCIX)                             
126800                                                                          
126900     IF W-TIVV          = DAGENS-VECKA             AND                    
127000       (IN-KDINLSTA      = 'INL' OR 'VOR' OR 'FRD') AND                   
127100        IN-IDRADNR   NOT = +1                                             
127200       PERFORM BE-BEHANDLA-TOTALER                                        
127300***    RESET THE FLAG, DEFAULT IT TO CDC                                  
127400       MOVE 'CDC' TO FLAG-SW                                              
127500     END-IF                                                               
127600     .                                                                    
127700     EJECT                                                                
127800 BA-KOLLA-OM-NY-KDINLUPF     SECTION.                                     
127900                                                                          
128000     MOVE IN-KDINLUPF      TO W-KDINLUPF (DCIX)                           
128100                                                                          
128200     IF  W-KDINLUPF (DCIX)   = SPACE                                      
128300     AND IN-KDINLSTA     = SPACE                                          
128400        MOVE 'R31 '           TO W-KDINLUPF (DCIX)                        
128500     END-IF                                                               
128600                                                                          
128700     IF  W-KDINLUPF (DCIX)   = SPACE                                      
128800     AND IN-KDINLSTA     = 'AVI'                                          
128900        MOVE 'C2  '           TO W-KDINLUPF (DCIX)                        
129000     END-IF                                                               
129100                                                                          
129200*    HÄMTA LEVERANTÖRSID FRÅN MÅL TABELL                                  
129300     PERFORM S11-KOLLA-MAAL-TABELL                                        
129400     IF MAAL-IX NOT > MAX-MAAL-IX                                         
129500       MOVE W-MAAL-IDLEVNR (MA-DCIX MAAL-IX)                              
129600                              TO W-IDLEVNR (DCIX)                         
129700       MOVE W-MAAL-FLEXCP (MA-DCIX MAAL-IX)                               
129800                              TO WS-FLEXCP                                
129900     ELSE                                                                 
130000      MOVE SPACE              TO W-IDLEVNR (DCIX)                         
130100      MOVE SPACE              TO WS-FLEXCP                                
130200     END-IF                                                               
130300                                                                          
130400     IF WS-FLEXCP = 'Y' OR 'J'                                            
130500        MOVE 'SVS'    TO FLAG-SW                                          
130600     END-IF                                                               
130700                                                                          
130800     SET SUM-IX    TO +1                                                  
130900     PERFORM UNTIL SUM-IX  >  MAX-SUM-IX                                  
131000             OR W-SUM-KDINLUPF (SU-DCIX SUM-IX) = SPACE                   
131100             OR W-SUM-KDINLUPF (SU-DCIX SUM-IX) = W-KDINLUPF(DCIX)        
131200                 SET SUM-IX UP BY +1                                      
131300     END-PERFORM                                                          
131400                                                                          
131500     IF SUM-IX   > MAX-SUM-IX                                             
131600         STRING 'SUM TAB FULL FÖR IDDC=' IN-IDDC                          
131700         DELIMITED BY SIZE   INTO  FELTEXT-STR                            
131800         DISPLAY FELTEXT                                                  
131900         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
132000     ELSE                                                                 
132100       IF W-SUM-KDINLUPF (SU-DCIX SUM-IX) = SPACE                         
132200         MOVE W-KDINLUPF (DCIX)                                           
132300                   TO  W-SUM-KDINLUPF            (SU-DCIX SUM-IX)         
132400         MOVE W-IDLEVNR  (DCIX)                                           
132500                   TO  W-SUM-IDLEVNR             (SU-DCIX SUM-IX)         
132600         MOVE ZERO TO  W-SUM-KVRADER             (SU-DCIX SUM-IX)         
132700                       W-SUM-KVRADER-DAG         (SU-DCIX SUM-IX)         
132800                       W-SUM-KVRADER-KVALL       (SU-DCIX SUM-IX)         
132900                       W-SUM-KVRADER-PRIO        (SU-DCIX SUM-IX)         
133000                       W-SUM-KVRADER-PRIO-DAG    (SU-DCIX SUM-IX)         
133100                       W-SUM-KVRADER-PRIO-KVALL  (SU-DCIX SUM-IX)         
133200                       W-SUM-KVART               (SU-DCIX SUM-IX)         
133300                       W-SUM-KVART-DAG           (SU-DCIX SUM-IX)         
133400                       W-SUM-KVART-KVALL         (SU-DCIX SUM-IX)         
133500                       W-SUM-SUBEL               (SU-DCIX SUM-IX)         
133600                       W-SUM-SUBEL-PRIO          (SU-DCIX SUM-IX)         
133700                       W-SUM-TIGLT-MM            (SU-DCIX SUM-IX)         
133800                       W-SUM-TIGLT-MM-DAG        (SU-DCIX SUM-IX)         
133900                       W-SUM-TIGLT-MM-KVALL      (SU-DCIX SUM-IX)         
134000                       W-SUM-TIGLT-PRIO-MM       (SU-DCIX SUM-IX)         
134100                       W-SUM-TIGLT-PRIO-MM-DAG   (SU-DCIX SUM-IX)         
134200                       W-SUM-TIGLT-PRIO-MM-KVALL (SU-DCIX SUM-IX)         
134300                       W-SUM-KVART-MAAL          (SU-DCIX SUM-IX)         
134400                       W-SUM-KVART-MAAL-PRIO     (SU-DCIX SUM-IX)         
134500                       W-SUM-VLARTNTO            (SU-DCIX SUM-IX)         
134600                       W-LAST-IDLOPNRM           (SU-DCIX SUM-IX)         
134700       END-IF                                                             
134800     END-IF                                                               
134900     .                                                                    
135000     EJECT                                                                
135100 BB-TA-FRAM-KDINLUPF-INDEX  SECTION.                                      
135200                                                                          
135300** GÅ IGENOM KDINLUPF TAB FÖR ATT FÅ FRAM RÄTT SUM-IX EFTERSOM            
135400** GENOMLOPPSTIDEN M M SKALL BERÄKNAS PÅ FÖREGÅENDE POST                  
135500** FÖR DETTA IDDC.                                                        
135600                                                                          
135700     MOVE W-SPAR2-KDINLUPF (DCIX)     TO W-KDINLUPF (DCIX)                
135800                                                                          
135900     IF  W-KDINLUPF (DCIX)          = SPACE                               
136000     AND W-SPAR2-KDINLSTA (DCIX)    = SPACE                               
136100         MOVE 'R31 '           TO W-KDINLUPF (DCIX)                       
136200     END-IF                                                               
136300                                                                          
136400     IF  W-KDINLUPF (DCIX)          = SPACE                               
136500     AND W-SPAR2-KDINLSTA (DCIX)    = 'AVI'                               
136600         MOVE 'C2  '           TO W-KDINLUPF (DCIX)                       
136700     END-IF                                                               
136800                                                                          
136900     SET SUM-IX      TO +1                                                
137000     PERFORM UNTIL SUM-IX  >  MAX-SUM-IX                                  
137100             OR W-SUM-KDINLUPF (SU-DCIX SUM-IX) = W-KDINLUPF(DCIX)        
137200                  SET SUM-IX UP BY +1                                     
137300     END-PERFORM                                                          
137400                                                                          
137500     IF SUM-IX                 > MAX-SUM-IX                               
137600         STRING  'KDINLUPF SAKNAS I SUM TAB FÖR IDDC=' IN-IDDC            
137700         DELIMITED BY SIZE   INTO  FELTEXT-STR                            
137800         DISPLAY FELTEXT                                                  
137900         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
138000     END-IF                                                               
138100     .                                                                    
138200     EJECT                                                                
138300 BC-BEHANDLA-PRODUCERAT-IDAG SECTION.                                     
138400                                                                          
138500     IF IN-KDINLSTA = W-SPAR2-KDINLSTA (DCIX) AND                         
138600        IN-KDINLUPF = W-SPAR2-KDINLUPF (DCIX) AND                         
138700       (IN-KVINLART NOT = W-SPAR2-KVINLART (DCIX))                        
138800       MOVE NEJ TO ADD-RADER-SW                                           
138900     ELSE                                                                 
139000       MOVE JA  TO ADD-RADER-SW                                           
139100       ADD +1   TO W-SUM-KVRADER       (SU-DCIX SUM-IX)                   
139200* CCID 6442199 -                                                          
139300       IF NEW-DAG                                                         
139400         ADD +1 TO W-SUM-KVRADER-DAG   (SU-DCIX SUM-IX)                   
139500       ELSE                                                               
139600         ADD +1 TO W-SUM-KVRADER-KVALL (SU-DCIX SUM-IX)                   
139700       END-IF                                                             
139800       IF IN-KDINLPRIO      <  +31                                        
139900         ADD +1    TO W-SUM-KVRADER-PRIO (SU-DCIX  SUM-IX)                
140000* CCID 6442199 -                                                          
140100         IF NEW-DAG                                                       
140200           ADD +1  TO W-SUM-KVRADER-PRIO-DAG (SU-DCIX SUM-IX)             
140300         ELSE                                                             
140400           ADD +1  TO W-SUM-KVRADER-PRIO-KVALL (SU-DCIX SUM-IX)           
140500         END-IF                                                           
140600                                                                          
140700         COMPUTE WS-SUBEL-PRIO = IN-PRARTSTD * IN-KVINLART                
140800         ADD     WS-SUBEL-PRIO                                            
140900                   TO W-SUM-SUBEL-PRIO (SU-DCIX SUM-IX)                   
141000       END-IF                                                             
141100                                                                          
141200       COMPUTE W-SUM-SUBEL (SU-DCIX SUM-IX) =                             
141300               W-SUM-SUBEL (SU-DCIX SUM-IX) +                             
141400              (IN-KVINLART * IN-PRARTSTD)                                 
141500                                                                          
141600       COMPUTE W-SUM-VLARTNTO (SU-DCIX SUM-IX) =                          
141700               W-SUM-VLARTNTO (SU-DCIX SUM-IX) +                          
141800              (IN-VLARTNTO * IN-KVINLART)                                 
141900                                                                          
142000       IF IN-IDLOPNRM NOT = W-LAST-IDLOPNRM   (SU-DCIX SUM-IX)            
142100         ADD +1            TO W-SUM-KVART     (SU-DCIX SUM-IX)            
142200         MOVE IN-IDLOPNRM  TO W-LAST-IDLOPNRM (SU-DCIX SUM-IX)            
142300* CCID 6442199 -                                                          
142400         IF NEW-DAG                                                       
142500           ADD +1 TO W-SUM-KVART-DAG          (SU-DCIX SUM-IX)            
142600         ELSE                                                             
142700           ADD +1 TO W-SUM-KVART-KVALL        (SU-DCIX SUM-IX)            
142800         END-IF                                                           
142900       END-IF                                                             
143000     END-IF                                                               
143100                                                                          
143200     .                                                                    
143300     EJECT                                                                
143400 BD-BEHANDLA-GENOMLOPPSTID   SECTION.                                     
143500                                                                          
143600* CCID 6628002 -                                                          
143700     IF IN-KDINLSTA = W-SPAR2-KDINLSTA (DCIX) AND                         
143800        IN-KDINLUPF = W-SPAR2-KDINLUPF (DCIX) AND                         
143900       (IN-KVINLART NOT = W-SPAR2-KVINLART (DCIX))                        
144000       CONTINUE                                                           
144100     ELSE                                                                 
144200       MOVE JA TO TIGLT-ADDED                                             
144300       PERFORM BDA-BERAEKNA-ARBETSDAGAR                                   
144400       MOVE IN-TIKLOCK              TO W-NEW-TIKLOCK                      
144500       MOVE W-SPAR2-TIKLOCK (DCIX)  TO W-OLD-TIKLOCK                      
144600                                                                          
144700* CCID 6442199 -                                                          
144800       MOVE W-SPAR2-TIREGDAT(DCIX) TO DAT-I-TIDATUM                       
144900       PERFORM S22-CALC-WEEKDAY                                           
145000                                                                          
145100       PERFORM S20-BERAEKNA-TIGLT                                         
145200                                                                          
145300       IF IN-KVKOLLI = +0                                                 
145400         ADD W-TOT-MM   TO W-SUM-TIGLT-MM       (SU-DCIX SUM-IX)          
145500* CCID 6442199 -                                                          
145600         IF NEW-DAG                                                       
145700           ADD W-TOT-MM TO W-SUM-TIGLT-MM-DAG   (SU-DCIX SUM-IX)          
145800         ELSE                                                             
145900           ADD W-TOT-MM TO W-SUM-TIGLT-MM-KVALL (SU-DCIX SUM-IX)          
146000         END-IF                                                           
146100       ELSE                                                               
146200         COMPUTE W-SUM-TIGLT-MM (SU-DCIX SUM-IX) =                        
146300                 W-SUM-TIGLT-MM (SU-DCIX SUM-IX) +                        
146400                 IN-KVKOLLI * W-TOT-MM                                    
146500* CCID 6442199 -                                                          
146600         IF NEW-DAG                                                       
146700           COMPUTE W-SUM-TIGLT-MM-DAG (SU-DCIX SUM-IX) =                  
146800                   W-SUM-TIGLT-MM-DAG (SU-DCIX SUM-IX) +                  
146900                   IN-KVKOLLI * W-TOT-MM                                  
147000         ELSE                                                             
147100           COMPUTE W-SUM-TIGLT-MM-KVALL (SU-DCIX SUM-IX) =                
147200                   W-SUM-TIGLT-MM-KVALL (SU-DCIX SUM-IX) +                
147300                   IN-KVKOLLI * W-TOT-MM                                  
147400         END-IF                                                           
147500       END-IF                                                             
147600                                                                          
147700       PERFORM S11-KOLLA-MAAL-TABELL                                      
147800                                                                          
147900       IF IN-KDINLPRIO      <  +31                                        
148000         ADD W-TOT-MM   TO W-SUM-TIGLT-PRIO-MM (SU-DCIX SUM-IX)           
148100* CCID 6442199 -                                                          
148200         IF NEW-DAG                                                       
148300           ADD W-TOT-MM TO                                                
148400               W-SUM-TIGLT-PRIO-MM-DAG (SU-DCIX SUM-IX)                   
148500         ELSE                                                             
148600           ADD W-TOT-MM TO                                                
148700               W-SUM-TIGLT-PRIO-MM-KVALL(SU-DCIX SUM-IX)                  
148800         END-IF                                                           
148900                                                                          
149000         IF ADD-RADER OR IN-FLINLI = JA                                   
149100           IF MAAL-IX NOT > MAX-MAAL-IX                                   
149200             IF W-TOT-MM < W-MAAL-TIGLT-PRIO   (MA-DCIX MAAL-IX)          
149300                                                                          
149400               ADD +1 TO W-SUM-KVART-MAAL-PRIO (SU-DCIX SUM-IX)           
149500               ADD +1 TO W-SUM-KVART-MAAL (SU-DCIX SUM-IX)                
149600             END-IF                                                       
149700           END-IF                                                         
149800         END-IF                                                           
149900       ELSE                                                               
150000                                                                          
150100         IF ADD-RADER OR IN-FLINLI = JA                                   
150200           IF MAAL-IX NOT > MAX-MAAL-IX                                   
150300             IF W-TOT-MM < W-MAAL-TIGLT   (MA-DCIX MAAL-IX)               
150400               ADD +1 TO W-SUM-KVART-MAAL (SU-DCIX SUM-IX)                
150500             END-IF                                                       
150600           END-IF                                                         
150700         END-IF                                                           
150800       END-IF                                                             
150900     END-IF                                                               
151000     .                                                                    
151100     EJECT                                                                
151200 BDA-BERAEKNA-ARBETSDAGAR    SECTION.                                     
151300                                                                          
151400     MOVE +001                 TO WORK-KDCALL                             
151500                                                                          
151600     IF WS-IDDC = '12'                                                    
151700       MOVE '21'               TO WORK-IDDC                               
151800     ELSE                                                                 
151900       MOVE WS-IDDC            TO WORK-IDDC                               
152000     END-IF                                                               
152100                                                                          
152200     MOVE W-SPAR2-TIREGDAT (DCIX) TO WORK-TIAAMMDD-FOM                    
152300     MOVE IN-TIREGDAT             TO WORK-TIAAMMDD-TOM                    
152400*FIX                                                                      
152500     IF W-SPAR2-TIREGDAT(DCIX) < 900000 AND > 001231                      
152600        MOVE W-SPAR2-TIREGDAT(DCIX) TO WORK-TIAAMMDD-FOM                  
152700     ELSE                                                                 
152800*       IF W-SPAR2-TIREGDAT(DCIX) < 980000                                
152900           MOVE W-SPAR2-TIREGDAT(DCIX) TO WS-TIREGDATFIX                  
153000           MOVE 01 TO WS-TIREGDATFIX-AA                                   
153100           MOVE WS-TIREGDATFIX TO WORK-TIAAMMDD-FOM                       
153200*       ELSE                                                              
153300*          MOVE W-SPAR2-TIREGDAT(DCIX) TO WORK-TIAAMMDD-FOM               
153400*       END-IF                                                            
153500     END-IF                                                               
153600                                                                          
153700     IF IN-TIREGDAT < 900000 AND > 001231                                 
153800        MOVE IN-TIREGDAT TO WORK-TIAAMMDD-TOM                             
153900     ELSE                                                                 
154000*       IF IN-TIREGDAT < 980000                                           
154100           MOVE IN-TIREGDAT TO WS-TIREGDATFIX                             
154200           MOVE 01 TO WS-TIREGDATFIX-AA                                   
154300           MOVE WS-TIREGDATFIX TO WORK-TIAAMMDD-TOM                       
154400*       ELSE                                                              
154500*          MOVE IN-TIREGDAT TO WORK-TIAAMMDD-TOM                          
154600*       END-IF                                                            
154700     END-IF                                                               
154800                                                                          
154900*SLUTFIX                                                                  
155000     MOVE WORK-TIAAMMDD-FOM   TO TMP1-YYMMDD                              
155100     MOVE WORK-TIAAMMDD-TOM   TO TMP2-YYMMDD                              
155200     PERFORM WY2000P1                                                     
155300     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
155400       MOVE ZERO TO WORK-KVWORKD                                          
155500       DISPLAY 'NY TIDRÄKNING???? '                                       
155600               ' FOM = ' WORK-TIAAMMDD-FOM                                
155700               ' TOM = ' WORK-TIAAMMDD-TOM                                
155800     ELSE                                                                 
155900       IF WORK-TIAAMMDD-FOM  =  WORK-TIAAMMDD-TOM                         
156000         MOVE ZERO             TO  WORK-KVWORKD                           
156100       ELSE                                                               
156200         CALL WORKDAY USING WORK-KDCALL                                   
156300                   WORK-DATE-AREA WORK-KDSVAR                             
156400                                                                          
156500         IF WORK-KDSVAR-OK                                                
156600** STARTDAG OCH SLUTDAG FÖRUTSÄTTS VARA ICKE-HELA ARBETSDAGAR             
156700             IF WORK-KVWORKD > 2                                          
156800               COMPUTE  WORK-KVWORKD = WORK-KVWORKD - 2                   
156900             ELSE                                                         
157000               MOVE ZERO TO WORK-KVWORKD                                  
157100             END-IF                                                       
157200         ELSE                                                             
157300             MOVE 'FEL UR WORKDAY' TO FELTEXT-STR                         
157400             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
157500         END-IF                                                           
157600       END-IF                                                             
157700     END-IF                                                               
157800     .                                                                    
157900     EJECT                                                                
158000 BE-BEHANDLA-TOTALER         SECTION.                                     
158100                                                                          
158200     MOVE IN-TIKLOCK     TO W-DK-TIKLOCK                                  
158300     MOVE 'DAG' TO NEW-DAG-KVALL-SW                                       
158400     IF W-DK-HH <= 15                                                     
158500       IF W-DK-HH = 15 AND W-DK-MM > 30                                   
158600         MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                 
158700       END-IF                                                             
158800     ELSE                                                                 
158900       MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                   
159000     END-IF                                                               
159100                                                                          
159200     ADD +1                   TO W-TOT-KVRADER (DCIX)                     
159300     IF DCIX = 1                                                          
159400        IF SVS-FLAG                                                       
159500           ADD +1                TO W-TOT-KVRADER-SVS                     
159600        ELSE                                                              
159700           ADD +1                TO W-TOT-KVRADER-CDC                     
159800        END-IF                                                            
159900     END-IF                                                               
160000                                                                          
160100* CCID 6442199 -                                                          
160200     IF NEW-DAG                                                           
160300       ADD +1 TO W-TOT-KVRADER-DAG (DCIX)                                 
160400       IF DCIX = 1                                                        
160500          IF SVS-FLAG                                                     
160600             ADD +1              TO W-TOT-KVRADER-DAG-SVS                 
160700          ELSE                                                            
160800             ADD +1              TO W-TOT-KVRADER-DAG-CDC                 
160900          END-IF                                                          
161000       END-IF                                                             
161100     ELSE                                                                 
161200       ADD +1 TO W-TOT-KVRADER-KVALL (DCIX)                               
161300       IF DCIX = 1                                                        
161400          IF SVS-FLAG                                                     
161500             ADD +1              TO W-TOT-KVRADER-KVALL-SVS               
161600          ELSE                                                            
161700             ADD +1              TO W-TOT-KVRADER-KVALL-CDC               
161800          END-IF                                                          
161900       END-IF                                                             
162000     END-IF                                                               
162100                                                                          
162200     COMPUTE W-TOT-SUBEL (DCIX) = W-TOT-SUBEL (DCIX)                      
162300                                + (IN-KVINLART * IN-PRARTSTD)             
162400                                                                          
162500     IF DCIX = 1                                                          
162600        IF SVS-FLAG                                                       
162700           COMPUTE W-TOT-SUBEL-SVS = W-TOT-SUBEL-SVS                      
162800                                   + (IN-KVINLART * IN-PRARTSTD)          
162900        ELSE                                                              
163000           COMPUTE W-TOT-SUBEL-CDC = W-TOT-SUBEL-CDC                      
163100                                   + (IN-KVINLART * IN-PRARTSTD)          
163200        END-IF                                                            
163300     END-IF                                                               
163400                                                                          
163500     COMPUTE W-TOT-VLARTNTO (DCIX) =                                      
163600             W-TOT-VLARTNTO (DCIX) +                                      
163700            (IN-VLARTNTO * IN-KVINLART)                                   
163800                                                                          
163900     IF DCIX = 1                                                          
164000        IF SVS-FLAG                                                       
164100           COMPUTE W-TOT-VLARTNTO-SVS  =                                  
164200                   W-TOT-VLARTNTO-SVS  +                                  
164300                  (IN-VLARTNTO * IN-KVINLART)                             
164400        ELSE                                                              
164500           COMPUTE W-TOT-VLARTNTO-CDC =                                   
164600                   W-TOT-VLARTNTO-CDC +                                   
164700                  (IN-VLARTNTO * IN-KVINLART)                             
164800        END-IF                                                            
164900     END-IF                                                               
165000                                                                          
165100     IF IN-IDLOPNRM NOT = W-TOT-IDLOPNRM (DCIX)                           
165200       ADD +1 TO W-TOT-KVART (DCIX)                                       
165300       MOVE IN-IDLOPNRM TO W-TOT-IDLOPNRM (DCIX)                          
165400* CCID 6442199 -                                                          
165500       IF DCIX = 1                                                        
165600          IF SVS-FLAG                                                     
165700             ADD +1 TO W-TOT-KVART-SVS                                    
165800             MOVE IN-IDLOPNRM TO W-TOT-IDLOPNRM-SVS                       
165900          ELSE                                                            
166000             ADD +1 TO W-TOT-KVART-CDC                                    
166100             MOVE IN-IDLOPNRM TO W-TOT-IDLOPNRM-CDC                       
166200          END-IF                                                          
166300       END-IF                                                             
166400       IF NEW-DAG                                                         
166500         ADD +1 TO W-TOT-KVART-DAG (DCIX)                                 
166600         IF DCIX = 1                                                      
166700            IF SVS-FLAG                                                   
166800               ADD +1 TO W-TOT-KVART-DAG-SVS                              
166900            ELSE                                                          
167000               ADD +1 TO W-TOT-KVART-DAG-CDC                              
167100            END-IF                                                        
167200         END-IF                                                           
167300       ELSE                                                               
167400         ADD +1 TO W-TOT-KVART-KVALL (DCIX)                               
167500         IF DCIX = 1                                                      
167600            IF SVS-FLAG                                                   
167700               ADD +1 TO W-TOT-KVART-KVALL-SVS                            
167800            ELSE                                                          
167900               ADD +1 TO W-TOT-KVART-KVALL-CDC                            
168000            END-IF                                                        
168100         END-IF                                                           
168200       END-IF                                                             
168300     END-IF                                                               
168400                                                                          
168500     PERFORM BEA-BEHANDLA-GENOMLOPPSTID-TOT                               
168600     .                                                                    
168700     EJECT                                                                
168800 BEA-BEHANDLA-GENOMLOPPSTID-TOT   SECTION.                                
168900                                                                          
169000* CCID 6442199 -                                                          
169100     MOVE IN-TIKLOCK     TO W-DK-TIKLOCK                                  
169200     MOVE 'DAG' TO NEW-DAG-KVALL-SW                                       
169300     IF W-DK-HH <= 15                                                     
169400       IF W-DK-HH = 15 AND W-DK-MM > 30                                   
169500         MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                 
169600       END-IF                                                             
169700     ELSE                                                                 
169800       MOVE 'KVALL' TO NEW-DAG-KVALL-SW                                   
169900     END-IF                                                               
170000                                                                          
170100     PERFORM BEAA-BERAEKNA-ARBETSDAGAR-TOT                                
170200     MOVE IN-TIKLOCK              TO W-NEW-TIKLOCK                        
170300     MOVE W-SPAR1-TIKLOCK (DCIX)  TO W-OLD-TIKLOCK                        
170400                                                                          
170500* CCID 6442199 -                                                          
170600     MOVE W-SPAR1-TIREGDAT(DCIX) TO DAT-I-TIDATUM                         
170700     PERFORM S22-CALC-WEEKDAY                                             
170800                                                                          
170900     PERFORM S20-TOT-BERAEKNA-TIGLT                                       
171000                                                                          
171100     ADD W-TOT-MM              TO W-TOT-TIGLT-MM (DCIX)                   
171200     IF DCIX = 1                                                          
171300        IF SVS-FLAG                                                       
171400           ADD W-TOT-MM           TO W-TOT-TIGLT-MM-SVS                   
171500        ELSE                                                              
171600           ADD W-TOT-MM           TO W-TOT-TIGLT-MM-CDC                   
171700        END-IF                                                            
171800     END-IF                                                               
171900* CCID 6442199 -                                                          
172000     IF NEW-DAG                                                           
172100       ADD W-TOT-MM            TO W-TOT-TIGLT-MM-DAG (DCIX)               
172200       IF DCIX = 1                                                        
172300          IF SVS-FLAG                                                     
172400             ADD W-TOT-MM         TO W-TOT-TIGLT-MM-DAG-SVS               
172500          ELSE                                                            
172600             ADD W-TOT-MM         TO W-TOT-TIGLT-MM-DAG-CDC               
172700          END-IF                                                          
172800       END-IF                                                             
172900     ELSE                                                                 
173000       ADD W-TOT-MM            TO W-TOT-TIGLT-MM-KVALL (DCIX)             
173100       IF DCIX = 1                                                        
173200          IF SVS-FLAG                                                     
173300             ADD W-TOT-MM         TO W-TOT-TIGLT-MM-KVALL-SVS             
173400          ELSE                                                            
173500             ADD W-TOT-MM         TO W-TOT-TIGLT-MM-KVALL-CDC             
173600          END-IF                                                          
173700       END-IF                                                             
173800     END-IF                                                               
173900     MOVE W-KDINLUPF (DCIX)    TO SPAR-KDINLUPF (DCIX)                    
174000     MOVE 'TOT '               TO W-KDINLUPF    (DCIX)                    
174100     MOVE 'CDC '               TO W-KDINLUPF-CDC                          
174200     MOVE 'SVS '               TO W-KDINLUPF-SVS                          
174300     PERFORM S11-KOLLA-MAAL-TABELL                                        
174400     MOVE SPAR-KDINLUPF (DCIX) TO W-KDINLUPF    (DCIX)                    
174500                                                                          
174600     IF IN-KDINLPRIO           <  +31                                     
174700       ADD W-TOT-MM          TO W-TOT-TIGLT-PRIO-MM (DCIX)                
174800       IF DCIX = 1                                                        
174900          IF SVS-FLAG                                                     
175000             ADD W-TOT-MM       TO W-TOT-TIGLT-PRIO-MM-SVS                
175100          ELSE                                                            
175200             ADD W-TOT-MM       TO W-TOT-TIGLT-PRIO-MM-CDC                
175300          END-IF                                                          
175400       END-IF                                                             
175500* CCID 6442199 -                                                          
175600       IF NEW-DAG                                                         
175700         ADD W-TOT-MM        TO W-TOT-TIGLT-PRIO-MM-DAG (DCIX)            
175800         IF DCIX = 1                                                      
175900            IF SVS-FLAG                                                   
176000               ADD W-TOT-MM     TO                                        
176100                              W-TOT-TIGLT-PRIO-MM-DAG-SVS                 
176200            ELSE                                                          
176300               ADD W-TOT-MM     TO                                        
176400                              W-TOT-TIGLT-PRIO-MM-DAG-CDC                 
176500            END-IF                                                        
176600         END-IF                                                           
176700       ELSE                                                               
176800         ADD W-TOT-MM        TO W-TOT-TIGLT-PRIO-MM-KVALL (DCIX)          
176900         IF DCIX = 1                                                      
177000            IF SVS-FLAG                                                   
177100               ADD W-TOT-MM     TO                                        
177200                              W-TOT-TIGLT-PRIO-MM-KVALL-SVS               
177300            ELSE                                                          
177400               ADD W-TOT-MM     TO                                        
177500                              W-TOT-TIGLT-PRIO-MM-KVALL-CDC               
177600            END-IF                                                        
177700         END-IF                                                           
177800       END-IF                                                             
177900       ADD +1                TO W-TOT-KVRADER-PRIO  (DCIX)                
178000* CCID 6442199 -                                                          
178100       IF DCIX = 1                                                        
178200          IF SVS-FLAG                                                     
178300             ADD +1             TO W-TOT-KVRADER-PRIO-SVS                 
178400          ELSE                                                            
178500             ADD +1             TO W-TOT-KVRADER-PRIO-CDC                 
178600          END-IF                                                          
178700       END-IF                                                             
178800       IF NEW-DAG                                                         
178900         ADD +1  TO W-TOT-KVRADER-PRIO-DAG (DCIX)                         
179000         IF DCIX = 1                                                      
179100            IF SVS-FLAG                                                   
179200               ADD +1  TO W-TOT-KVRADER-PRIO-DAG-SVS                      
179300            ELSE                                                          
179400               ADD +1  TO W-TOT-KVRADER-PRIO-DAG-CDC                      
179500            END-IF                                                        
179600         END-IF                                                           
179700       ELSE                                                               
179800         ADD +1  TO W-TOT-KVRADER-PRIO-KVALL (DCIX)                       
179900         IF DCIX = 1                                                      
180000            IF SVS-FLAG                                                   
180100               ADD +1  TO W-TOT-KVRADER-PRIO-KVALL-SVS                    
180200            ELSE                                                          
180300               ADD +1  TO W-TOT-KVRADER-PRIO-KVALL-CDC                    
180400            END-IF                                                        
180500         END-IF                                                           
180600       END-IF                                                             
180700                                                                          
180800       COMPUTE WS-TOT-SUBEL-PRIO  = IN-PRARTSTD * IN-KVINLART             
180900       ADD     WS-TOT-SUBEL-PRIO  TO W-TOT-SUBEL-PRIO (DCIX)              
181000       IF DCIX = 1                                                        
181100          IF SVS-FLAG                                                     
181200             ADD  WS-TOT-SUBEL-PRIO  TO W-TOT-SUBEL-PRIO-SVS              
181300          ELSE                                                            
181400             ADD  WS-TOT-SUBEL-PRIO  TO W-TOT-SUBEL-PRIO-CDC              
181500          END-IF                                                          
181600       END-IF                                                             
181700       IF MAAL-IX NOT > MAX-MAAL-IX                                       
181800         IF W-TOT-MM  < W-MAAL-TIGLT-PRIO  (MA-DCIX MAAL-IX)              
181900           ADD +1 TO W-TOT-KVART-MAAL-PRIO (DCIX)                         
182000           ADD +1 TO W-TOT-KVART-MAAL (DCIX)                              
182100           IF DCIX = 1                                                    
182200              IF SVS-FLAG                                                 
182300                 ADD +1 TO W-TOT-KVART-MAAL-PRIO-SVS                      
182400                 ADD +1 TO W-TOT-KVART-MAAL-SVS                           
182500              ELSE                                                        
182600                 ADD +1 TO W-TOT-KVART-MAAL-PRIO-CDC                      
182700                 ADD +1 TO W-TOT-KVART-MAAL-CDC                           
182800              END-IF                                                      
182900           END-IF                                                         
183000         END-IF                                                           
183100       END-IF                                                             
183200     ELSE                                                                 
183300       IF MAAL-IX NOT > MAX-MAAL-IX                                       
183400         IF W-TOT-MM < W-MAAL-TIGLT (MA-DCIX MAAL-IX)                     
183500           ADD +1 TO W-TOT-KVART-MAAL (DCIX)                              
183600           IF DCIX = 1                                                    
183700              IF SVS-FLAG                                                 
183800                 ADD +1 TO W-TOT-KVART-MAAL-SVS                           
183900              ELSE                                                        
184000                 ADD +1 TO W-TOT-KVART-MAAL-CDC                           
184100              END-IF                                                      
184200           END-IF                                                         
184300         END-IF                                                           
184400       END-IF                                                             
184500     END-IF                                                               
184600     .                                                                    
184700     EJECT                                                                
184800 BEAA-BERAEKNA-ARBETSDAGAR-TOT  SECTION.                                  
184900                                                                          
185000     MOVE +001                 TO WORK-KDCALL                             
185100                                                                          
185200     IF WS-IDDC = '12'                                                    
185300       MOVE '21'               TO WORK-IDDC                               
185400     ELSE                                                                 
185500       MOVE WS-IDDC            TO WORK-IDDC                               
185600     END-IF                                                               
185700                                                                          
185800     MOVE W-SPAR1-TIREGDAT (DCIX) TO WORK-TIAAMMDD-FOM                    
185900     MOVE IN-TIREGDAT             TO WORK-TIAAMMDD-TOM                    
186000                                                                          
186100     MOVE WORK-TIAAMMDD-FOM       TO TMP1-YYMMDD                          
186200     MOVE WORK-TIAAMMDD-TOM       TO TMP2-YYMMDD                          
186300     PERFORM WY2000P1                                                     
186400     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
186500       MOVE ZERO TO WORK-KVWORKD                                          
186600       DISPLAY 'NY TIDRÄKNING???? '                                       
186700               ' FOM = ' WORK-TIAAMMDD-FOM                                
186800               ' TOM = ' WORK-TIAAMMDD-TOM                                
186900     ELSE                                                                 
187000       IF WORK-TIAAMMDD-FOM =  WORK-TIAAMMDD-TOM                          
187100         MOVE ZERO             TO WORK-KVWORKD                            
187200       ELSE                                                               
187300         CALL WORKDAY USING WORK-KDCALL                                   
187400                   WORK-DATE-AREA WORK-KDSVAR                             
187500                                                                          
187600         IF WORK-KDSVAR-OK                                                
187700** STARTDAG OCH SLUTDAG FÖRUTSÄTTS VARA ICKE-HELA ARBETSDAGAR             
187800             IF WORK-KVWORKD > 2                                          
187900               COMPUTE  WORK-KVWORKD = WORK-KVWORKD - 2                   
188000             ELSE                                                         
188100               MOVE ZERO TO WORK-KVWORKD                                  
188200             END-IF                                                       
188300         ELSE                                                             
188400             MOVE 'FEL UR WORKDAY' TO FELTEXT-STR                         
188500             CALL ABEND USING RKOD-ABEND-MED-DUMP                         
188600         END-IF                                                           
188700       END-IF                                                             
188800     END-IF                                                               
188900     .                                                                    
189000     EJECT                                                                
189100 C-TOEM-KDINLUPF-TAB        SECTION.                                      
189200                                                                          
189300     MOVE   'RAD'    TO   UT-IDPTYP                                       
189400     MOVE    +1      TO   DCIX                                            
189500     SET   SU-DCIX   TO   DCIX                                            
189600     PERFORM UNTIL SU-DCIX  >  +27                                        
189700                                                                          
189800       IF DCIX = +1                                                       
189900         MOVE WC-CDC-SE      TO  UT-IDDC                                  
190000       END-IF                                                             
190100       IF DCIX = +2                                                       
190200         MOVE WC-CDC-TR      TO  UT-IDDC                                  
190300       END-IF                                                             
190400       IF DCIX = +3                                                       
190500         MOVE WC-NDC-US-RU   TO  UT-IDDC                                  
190600       END-IF                                                             
190700       IF DCIX = +4                                                       
190800         MOVE WC-NDC-US-LA   TO  UT-IDDC                                  
190900       END-IF                                                             
191000       IF DCIX = +5                                                       
191100         MOVE WC-NDC-US-SE   TO  UT-IDDC                                  
191200       END-IF                                                             
191300       IF DCIX = +6                                                       
191400         MOVE WC-NDC-US-CH   TO  UT-IDDC                                  
191500       END-IF                                                             
191600       IF DCIX = +7                                                       
191700         MOVE WC-NDC-US-JA   TO  UT-IDDC                                  
191800       END-IF                                                             
191900       IF DCIX = +8                                                       
192000         MOVE WC-NDC-US-DA   TO  UT-IDDC                                  
192100       END-IF                                                             
192200       IF DCIX = +9                                                       
192300         MOVE WC-NDC-CA      TO  UT-IDDC                                  
192400       END-IF                                                             
196610       IF DCIX = +10                                                      
196620         MOVE WC-NDC-BR      TO  UT-IDDC                                  
196630       END-IF                                                             
196640       IF DCIX = +11                                                      
196650         MOVE WC-NDC-MX      TO  UT-IDDC                                  
196660       END-IF                                                             
192500       IF DCIX = +12                                                      
192600         MOVE WC-NDC-JP-61   TO  UT-IDDC                                  
192700       END-IF                                                             
192800       IF DCIX = +13                                                      
192900         MOVE WC-NDC-AU      TO  UT-IDDC                                  
193000       END-IF                                                             
193100       IF DCIX = +14                                                      
193200         MOVE WC-NDC-TH      TO  UT-IDDC                                  
193300       END-IF                                                             
193400       IF DCIX = +15                                                      
193500         MOVE WC-NDC-TW      TO  UT-IDDC                                  
193600       END-IF                                                             
193700       IF DCIX = +16                                                      
193800         MOVE WC-NDC-KR      TO  UT-IDDC                                  
193900       END-IF                                                             
194000       IF DCIX = +17                                                      
194100         MOVE WC-NDC-MY      TO  UT-IDDC                                  
194200       END-IF                                                             
194300       IF DCIX = +18                                                      
194400         MOVE WC-NDC-IN      TO  UT-IDDC                                  
194500       END-IF                                                             
194600       IF DCIX = +19                                                      
194700         MOVE WC-NDC-CN-71   TO  UT-IDDC                                  
194800       END-IF                                                             
194900       IF DCIX = +20                                                      
195000         MOVE WC-NDC-CN-72   TO  UT-IDDC                                  
195100       END-IF                                                             
195200       IF DCIX = +21                                                      
195300         MOVE WC-NDC-CN-73   TO  UT-IDDC                                  
195400       END-IF                                                             
195500       IF DCIX = +22                                                      
195600         MOVE WC-NDC-CN-74   TO  UT-IDDC                                  
195700       END-IF                                                             
195500       IF DCIX = +23                                                      
195600         MOVE WC-NDC-ZA      TO  UT-IDDC                                  
195700       END-IF                                                             
195800       IF DCIX = +24                                                      
195900         MOVE WC-NDC-TR      TO  UT-IDDC                                  
196000       END-IF                                                             
196100       IF DCIX = +25                                                      
196200         MOVE WC-NDC-AE      TO  UT-IDDC                                  
196300       END-IF                                                             
196400       IF DCIX = +26                                                      
196500         MOVE WC-NDC-US-BAT  TO  UT-IDDC                                  
196600       END-IF                                                             
196640       IF DCIX = +27                                                      
196650         MOVE WC-NDC-TH-93   TO  UT-IDDC                                  
196660       END-IF                                                             
196700       MOVE UT-IDDC          TO  W-IDDC                                   
196800       MOVE SPACES           TO   UT-ADCITY                               
196900       PERFORM IMS-GU-WDB601                                              
197000       IF SEGMENT-FOUND                                                   
197100         MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                             
197200                                  TO UT-ADCITY                            
197300       END-IF                                                             
197400                                                                          
197500       SET SUM-IX                         TO +1                           
197600       PERFORM UNTIL SUM-IX > MAX-SUM-IX OR                               
197700                     W-SUM-KDINLUPF (SU-DCIX SUM-IX) = SPACE              
197800         IF W-SUM-KVRADER (SU-DCIX SUM-IX) > 0                            
197900         MOVE W-SUM-KDINLUPF     (SU-DCIX SUM-IX) TO UT-KDINLUPF          
198000         MOVE W-SUM-KVRADER      (SU-DCIX SUM-IX) TO UT-KVRADER           
198100         MOVE W-SUM-KVRADER-DAG  (SU-DCIX SUM-IX) TO                      
198200                                  UT-KVRADER-DAG                          
198300         MOVE W-SUM-KVRADER-KVALL (SU-DCIX SUM-IX) TO                     
198400                                  UT-KVRADER-KVALL                        
198500         MOVE W-SUM-KVRADER-PRIO (SU-DCIX SUM-IX)                         
198600                                               TO UT-KVRADER-PRIO         
198700         MOVE W-SUM-KVART        (SU-DCIX SUM-IX) TO UT-KVART             
198800         MOVE W-SUM-KVART-DAG    (SU-DCIX SUM-IX) TO                      
198900                                  UT-KVART-DAG                            
199000         MOVE W-SUM-KVART-KVALL  (SU-DCIX SUM-IX) TO                      
199100                                  UT-KVART-KVALL                          
199200         MOVE W-SUM-SUBEL        (SU-DCIX SUM-IX) TO UT-SUBEL             
199300         MOVE W-SUM-SUBEL-PRIO   (SU-DCIX SUM-IX) TO UT-SUBEL-PRIO        
199400         MOVE W-SUM-KVART-MAAL   (SU-DCIX SUM-IX) TO UT-KVART-MAAL        
199500         MOVE W-SUM-KVART-MAAL-PRIO (SU-DCIX SUM-IX)                      
199600                                             TO UT-KVART-MAAL-PRIO        
199700* CCID 6442199 -                                                          
199800         COMPUTE UT-VLARTNTO =                                            
199900                W-SUM-VLARTNTO(SU-DCIX SUM-IX) / 1000000                  
200000         MOVE W-SUM-IDLEVNR      (SU-DCIX SUM-IX) TO UT-IDLEVNR           
200100                                                                          
200200         MOVE ZERO                       TO UT-TIGLT                      
200300                                            UT-TIGLT-DAG                  
200400                                            UT-TIGLT-KVALL                
200500                                            UT-TIGLT-PRIO                 
200600                                            UT-TIGLT-PRIO-DAG             
200700                                            UT-TIGLT-PRIO-KVALL           
200800                                                                          
200900         IF W-SUM-KVRADER (SU-DCIX SUM-IX) > ZERO                         
201000           COMPUTE W-TIGLT-MM (DCIX)  =                                   
201100                   W-SUM-TIGLT-MM (SU-DCIX SUM-IX)                        
201200                 / W-SUM-KVRADER  (SU-DCIX SUM-IX)                        
201300           DIVIDE  W-TIGLT-MM (DCIX) BY 60 GIVING                         
201400                   W-GLT-HH REMAINDER W-GLT-MM                            
201500           COMPUTE UT-TIGLT     =  W-GLT   / 100                          
201600                                                                          
201700           IF W-SUM-TIGLT-PRIO-MM (SU-DCIX SUM-IX) = ZERO OR              
201800              W-SUM-KVRADER-PRIO  (SU-DCIX SUM-IX) = ZERO                 
201900             MOVE ZERO                TO UT-TIGLT-PRIO                    
202000           ELSE                                                           
202100             COMPUTE W-TIGLT-MM (DCIX) =                                  
202200                     W-SUM-TIGLT-PRIO-MM (SU-DCIX SUM-IX) /               
202300                     W-SUM-KVRADER-PRIO  (SU-DCIX SUM-IX)                 
202400             DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                        
202500                    W-GLT-HH REMAINDER W-GLT-MM                           
202600             COMPUTE UT-TIGLT-PRIO = W-GLT / 100                          
202700           END-IF                                                         
202800         END-IF                                                           
202900                                                                          
203000         IF W-SUM-TIGLT-MM-DAG (SU-DCIX SUM-IX) > ZERO AND                
203100            W-SUM-KVRADER-DAG  (SU-DCIX SUM-IX) > ZERO                    
203200           COMPUTE W-TIGLT-MM (DCIX)  =                                   
203300                   W-SUM-TIGLT-MM-DAG (SU-DCIX SUM-IX)                    
203400                 / W-SUM-KVRADER-DAG  (SU-DCIX SUM-IX)                    
203500           DIVIDE  W-TIGLT-MM (DCIX) BY 60 GIVING                         
203600                   W-GLT-HH REMAINDER W-GLT-MM                            
203700           COMPUTE UT-TIGLT-DAG    = W-GLT / 100                          
203800                                                                          
203900           IF W-SUM-TIGLT-PRIO-MM-DAG (SU-DCIX SUM-IX) = ZERO OR          
204000              W-SUM-KVRADER-PRIO-DAG  (SU-DCIX SUM-IX) = ZERO             
204100             MOVE ZERO                TO UT-TIGLT-PRIO-DAG                
204200           ELSE                                                           
204300             COMPUTE W-TIGLT-MM (DCIX) =                                  
204400                     W-SUM-TIGLT-PRIO-MM-DAG (SU-DCIX SUM-IX) /           
204500                     W-SUM-KVRADER-PRIO-DAG  (SU-DCIX SUM-IX)             
204600             DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                        
204700                    W-GLT-HH REMAINDER W-GLT-MM                           
204800             COMPUTE UT-TIGLT-PRIO-DAG = W-GLT / 100                      
204900           END-IF                                                         
205000         END-IF                                                           
205100                                                                          
205200         IF W-SUM-TIGLT-MM-KVALL (SU-DCIX SUM-IX) > ZERO AND              
205300            W-SUM-KVRADER-KVALL  (SU-DCIX SUM-IX) > ZERO                  
205400           COMPUTE W-TIGLT-MM (DCIX)  =                                   
205500                   W-SUM-TIGLT-MM-KVALL (SU-DCIX SUM-IX)                  
205600                 / W-SUM-KVRADER-KVALL  (SU-DCIX SUM-IX)                  
205700           DIVIDE  W-TIGLT-MM (DCIX) BY 60 GIVING                         
205800                   W-GLT-HH REMAINDER W-GLT-MM                            
205900           COMPUTE UT-TIGLT-KVALL      = W-GLT / 100                      
206000                                                                          
206100           IF W-SUM-TIGLT-PRIO-MM-KVALL (SU-DCIX SUM-IX) = ZERO OR        
206200              W-SUM-KVRADER-PRIO-KVALL  (SU-DCIX SUM-IX) = ZERO           
206300             MOVE ZERO                TO UT-TIGLT-PRIO-KVALL              
206400           ELSE                                                           
206500             COMPUTE W-TIGLT-MM (DCIX) =                                  
206600                     W-SUM-TIGLT-PRIO-MM-KVALL (SU-DCIX SUM-IX) /         
206700                     W-SUM-KVRADER-PRIO-KVALL  (SU-DCIX SUM-IX)           
206800             DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                        
206900                    W-GLT-HH REMAINDER W-GLT-MM                           
207000             COMPUTE UT-TIGLT-PRIO-KVALL = W-GLT / 100                    
207100           END-IF                                                         
207200         END-IF                                                           
207300                                                                          
207400         PERFORM S03-SKRIV-W61155                                         
207500         END-IF                                                           
207600         SET SUM-IX UP BY +1                                              
207700       END-PERFORM                                                        
207800                                                                          
207900       SET SU-DCIX UP BY +1                                               
208000       ADD   +1    TO    DCIX                                             
208100     END-PERFORM                                                          
208200     .                                                                    
208300     EJECT                                                                
208400 D-SKRIV-TOTAL-POST         SECTION.                                      
208500                                                                          
208600     MOVE      +1     TO   DCIX                                           
208700     MOVE    'TOT'    TO   UT-IDPTYP                                      
208800     MOVE    SPACE    TO   UT-KDINLUPF                                    
208900     MOVE    SPACE    TO   UT-IDLEVNR                                     
209000     PERFORM UNTIL DCIX > +27                                             
209100                                                                          
209200       IF DCIX = +1                                                       
209300         MOVE WC-CDC-SE      TO  UT-IDDC                                  
209400       END-IF                                                             
209500       IF DCIX = +2                                                       
209600         MOVE WC-CDC-TR      TO  UT-IDDC                                  
209700       END-IF                                                             
209800       IF DCIX = +3                                                       
209900         MOVE WC-NDC-US-RU   TO  UT-IDDC                                  
210000       END-IF                                                             
210100       IF DCIX = +4                                                       
210200         MOVE WC-NDC-US-LA   TO  UT-IDDC                                  
210300       END-IF                                                             
210400       IF DCIX = +5                                                       
210500         MOVE WC-NDC-US-SE   TO  UT-IDDC                                  
210600       END-IF                                                             
210700       IF DCIX = +6                                                       
210800         MOVE WC-NDC-US-CH   TO  UT-IDDC                                  
210900       END-IF                                                             
211000       IF DCIX = +7                                                       
211100         MOVE WC-NDC-US-JA   TO  UT-IDDC                                  
211200       END-IF                                                             
211300       IF DCIX = +8                                                       
211400         MOVE WC-NDC-US-DA   TO  UT-IDDC                                  
211500       END-IF                                                             
211600       IF DCIX = +9                                                       
211700         MOVE WC-NDC-CA      TO  UT-IDDC                                  
211800       END-IF                                                             
216010       IF DCIX = +10                                                      
216020         MOVE WC-NDC-BR      TO  UT-IDDC                                  
216030       END-IF                                                             
216040       IF DCIX = +11                                                      
216050         MOVE WC-NDC-MX      TO  UT-IDDC                                  
216060       END-IF                                                             
211900       IF DCIX = +12                                                      
212000         MOVE WC-NDC-JP-61   TO  UT-IDDC                                  
212100       END-IF                                                             
212200       IF DCIX = +13                                                      
212300         MOVE WC-NDC-AU      TO  UT-IDDC                                  
212400       END-IF                                                             
212500       IF DCIX = +14                                                      
212600         MOVE WC-NDC-TH      TO  UT-IDDC                                  
212700       END-IF                                                             
212800       IF DCIX = +15                                                      
212900         MOVE WC-NDC-TW      TO  UT-IDDC                                  
213000       END-IF                                                             
213100       IF DCIX = +16                                                      
213200         MOVE WC-NDC-KR      TO  UT-IDDC                                  
213300       END-IF                                                             
213400       IF DCIX = +17                                                      
213500         MOVE WC-NDC-MY      TO  UT-IDDC                                  
213600       END-IF                                                             
213700       IF DCIX = +18                                                      
213800         MOVE WC-NDC-IN      TO  UT-IDDC                                  
213900       END-IF                                                             
214000       IF DCIX = +19                                                      
214100         MOVE WC-NDC-CN-71   TO  UT-IDDC                                  
214200       END-IF                                                             
214300       IF DCIX = +20                                                      
214400         MOVE WC-NDC-CN-72   TO  UT-IDDC                                  
214500       END-IF                                                             
214600       IF DCIX = +21                                                      
214700         MOVE WC-NDC-CN-73   TO  UT-IDDC                                  
214800       END-IF                                                             
214900       IF DCIX = +22                                                      
215000         MOVE WC-NDC-CN-74   TO  UT-IDDC                                  
215100       END-IF                                                             
214900       IF DCIX = +23                                                      
215000         MOVE WC-NDC-ZA      TO  UT-IDDC                                  
215100       END-IF                                                             
215200       IF DCIX = +24                                                      
215300         MOVE WC-NDC-TR      TO  UT-IDDC                                  
215400       END-IF                                                             
215500       IF DCIX = +25                                                      
215600         MOVE WC-NDC-AE      TO  UT-IDDC                                  
215700       END-IF                                                             
215800       IF DCIX = +26                                                      
215900         MOVE WC-NDC-US-BAT  TO  UT-IDDC                                  
216000       END-IF                                                             
216040       IF DCIX = +27                                                      
216050         MOVE WC-NDC-TH-93   TO  UT-IDDC                                  
216060       END-IF                                                             
216100       MOVE UT-IDDC          TO  W-IDDC                                   
216200       PERFORM IMS-GU-WDB601                                              
216300       IF SEGMENT-FOUND                                                   
216400         MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                             
216500                                  TO UT-ADCITY                            
216600       END-IF                                                             
216700                                                                          
216800       MOVE W-TOT-KVRADER       (DCIX) TO UT-KVRADER                      
216900       MOVE W-TOT-KVRADER-DAG   (DCIX) TO UT-KVRADER-DAG                  
217000       MOVE W-TOT-KVRADER-KVALL (DCIX) TO UT-KVRADER-KVALL                
217100       MOVE W-TOT-KVRADER-PRIO  (DCIX) TO UT-KVRADER-PRIO                 
217200       MOVE W-TOT-KVART         (DCIX) TO UT-KVART                        
217300       MOVE W-TOT-KVART-DAG     (DCIX) TO UT-KVART-DAG                    
217400       MOVE W-TOT-KVART-KVALL   (DCIX) TO UT-KVART-KVALL                  
217500       MOVE W-TOT-SUBEL         (DCIX) TO UT-SUBEL                        
217600       MOVE W-TOT-SUBEL-PRIO    (DCIX) TO UT-SUBEL-PRIO                   
217700         COMPUTE UT-VLARTNTO =                                            
217800                         W-TOT-VLARTNTO(DCIX) / 1000000                   
217900       MOVE W-TOT-KVART-MAAL    (DCIX) TO UT-KVART-MAAL                   
218000       MOVE W-TOT-KVART-MAAL-PRIO (DCIX) TO UT-KVART-MAAL-PRIO            
218100                                                                          
218200       MOVE ZERO                       TO UT-TIGLT                        
218300                                          UT-TIGLT-DAG                    
218400                                          UT-TIGLT-KVALL                  
218500                                          UT-TIGLT-PRIO-DAG               
218600                                          UT-TIGLT-PRIO-KVALL             
218700                                                                          
218800       IF W-TOT-KVRADER (DCIX) =  ZERO                                    
218900           MOVE ZERO          TO UT-TIGLT                                 
219000       ELSE                                                               
219100           COMPUTE   W-TIGLT-MM     (DCIX)  =                             
219200                     W-TOT-TIGLT-MM (DCIX)                                
219300                   / W-TOT-KVRADER  (DCIX)                                
219400                                                                          
219500           DIVIDE W-TIGLT-MM (DCIX)  BY 60 GIVING                         
219600                  W-GLT-HH REMAINDER W-GLT-MM                             
219700                                                                          
219800           COMPUTE UT-TIGLT      =  W-GLT / 100                           
219900       END-IF                                                             
220000                                                                          
220100       IF W-TOT-KVRADER-PRIO (DCIX) =  ZERO                               
220200           MOVE ZERO             TO UT-TIGLT-PRIO                         
220300       ELSE                                                               
220400           COMPUTE    W-TIGLT-MM          (DCIX) =                        
220500                      W-TOT-TIGLT-PRIO-MM (DCIX)                          
220600                    / W-TOT-KVRADER-PRIO  (DCIX)                          
220700                                                                          
220800           DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                          
220900                  W-GLT-HH REMAINDER W-GLT-MM                             
221000                                                                          
221100           COMPUTE  UT-TIGLT-PRIO   =   W-GLT  / 100                      
221200       END-IF                                                             
221300                                                                          
221400       IF W-TOT-KVRADER-DAG (DCIX) =  ZERO                                
221500           MOVE ZERO          TO UT-TIGLT-DAG                             
221600       ELSE                                                               
221700           COMPUTE   W-TIGLT-MM         (DCIX)  =                         
221800                     W-TOT-TIGLT-MM-DAG (DCIX)                            
221900                   / W-TOT-KVRADER-DAG  (DCIX)                            
222000                                                                          
222100           DIVIDE W-TIGLT-MM (DCIX)  BY 60 GIVING                         
222200                  W-GLT-HH REMAINDER W-GLT-MM                             
222300                                                                          
222400           COMPUTE UT-TIGLT-DAG  =  W-GLT / 100                           
222500       END-IF                                                             
222600                                                                          
222700       IF W-TOT-KVRADER-PRIO-DAG (DCIX) =  ZERO                           
222800           MOVE ZERO             TO UT-TIGLT-PRIO-DAG                     
222900       ELSE                                                               
223000           COMPUTE    W-TIGLT-MM              (DCIX) =                    
223100                      W-TOT-TIGLT-PRIO-MM-DAG (DCIX)                      
223200                    / W-TOT-KVRADER-PRIO-DAG  (DCIX)                      
223300                                                                          
223400           DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                          
223500                  W-GLT-HH REMAINDER W-GLT-MM                             
223600                                                                          
223700           COMPUTE  UT-TIGLT-PRIO-DAG  =   W-GLT  / 100                   
223800       END-IF                                                             
223900                                                                          
224000       IF W-TOT-KVRADER-KVALL (DCIX) =  ZERO                              
224100           MOVE ZERO          TO UT-TIGLT-KVALL                           
224200       ELSE                                                               
224300           COMPUTE   W-TIGLT-MM           (DCIX)  =                       
224400                     W-TOT-TIGLT-MM-KVALL (DCIX)                          
224500                   / W-TOT-KVRADER-KVALL  (DCIX)                          
224600                                                                          
224700           DIVIDE W-TIGLT-MM (DCIX)  BY 60 GIVING                         
224800                  W-GLT-HH REMAINDER W-GLT-MM                             
224900                                                                          
225000           COMPUTE UT-TIGLT-KVALL = W-GLT / 100                           
225100       END-IF                                                             
225200                                                                          
225300       IF W-TOT-KVRADER-PRIO-KVALL (DCIX) =  ZERO                         
225400           MOVE ZERO             TO UT-TIGLT-PRIO-KVALL                   
225500       ELSE                                                               
225600           COMPUTE    W-TIGLT-MM                (DCIX) =                  
225700                      W-TOT-TIGLT-PRIO-MM-KVALL (DCIX)                    
225800                    / W-TOT-KVRADER-PRIO-KVALL  (DCIX)                    
225900                                                                          
226000           DIVIDE W-TIGLT-MM (DCIX) BY 60 GIVING                          
226100                  W-GLT-HH REMAINDER W-GLT-MM                             
226200                                                                          
226300           COMPUTE  UT-TIGLT-PRIO-KVALL  =   W-GLT  / 100                 
226400       END-IF                                                             
226500                                                                          
226600       PERFORM S03-SKRIV-W61155                                           
226700                                                                          
226800       ADD  +1   TO   DCIX                                                
226900     END-PERFORM                                                          
227000     .                                                                    
227100     EJECT                                                                
227200 D-SKRIV-TOTAL-POST-CDC     SECTION.                                      
227300                                                                          
227400     MOVE    'CDC'                   TO UT-IDPTYP                         
227500     MOVE    SPACE                   TO UT-KDINLUPF                       
227600     MOVE    SPACE                   TO UT-IDLEVNR                        
227700     MOVE WC-CDC-SE                  TO UT-IDDC                           
227800                                                                          
227900     MOVE W-TOT-KVRADER-CDC          TO UT-KVRADER                        
228000     MOVE W-TOT-KVRADER-DAG-CDC      TO UT-KVRADER-DAG                    
228100     MOVE W-TOT-KVRADER-KVALL-CDC    TO UT-KVRADER-KVALL                  
228200     MOVE W-TOT-KVRADER-PRIO-CDC     TO UT-KVRADER-PRIO                   
228300     MOVE W-TOT-KVART-CDC            TO UT-KVART                          
228400     MOVE W-TOT-KVART-DAG-CDC        TO UT-KVART-DAG                      
228500     MOVE W-TOT-KVART-KVALL-CDC      TO UT-KVART-KVALL                    
228600     MOVE W-TOT-SUBEL-CDC            TO UT-SUBEL                          
228700     MOVE W-TOT-SUBEL-PRIO-CDC       TO UT-SUBEL-PRIO                     
228800       COMPUTE UT-VLARTNTO =                                              
228900                       W-TOT-VLARTNTO-CDC / 1000000                       
229000     MOVE W-TOT-KVART-MAAL-CDC       TO UT-KVART-MAAL                     
229100     MOVE W-TOT-KVART-MAAL-PRIO-CDC  TO UT-KVART-MAAL-PRIO                
229200                                                                          
229300     MOVE ZERO                       TO UT-TIGLT                          
229400                                        UT-TIGLT-DAG                      
229500                                        UT-TIGLT-KVALL                    
229600                                        UT-TIGLT-PRIO-DAG                 
229700                                        UT-TIGLT-PRIO-KVALL               
229800                                                                          
229900     IF W-TOT-KVRADER-CDC =  ZERO                                         
230000         MOVE ZERO          TO UT-TIGLT                                   
230100     ELSE                                                                 
230200         COMPUTE   W-TIGLT-MM-CDC     =                                   
230300                   W-TOT-TIGLT-MM-CDC                                     
230400                 / W-TOT-KVRADER-CDC                                      
230500                                                                          
230600         DIVIDE W-TIGLT-MM-CDC  BY 60 GIVING                              
230700                W-GLT-HH REMAINDER W-GLT-MM                               
230800                                                                          
230900         COMPUTE UT-TIGLT      =  W-GLT / 100                             
231000     END-IF                                                               
231100                                                                          
231200     IF W-TOT-KVRADER-PRIO-CDC =  ZERO                                    
231300         MOVE ZERO             TO UT-TIGLT-PRIO                           
231400     ELSE                                                                 
231500         COMPUTE    W-TIGLT-MM-CDC           =                            
231600                    W-TOT-TIGLT-PRIO-MM-CDC                               
231700                  / W-TOT-KVRADER-PRIO-CDC                                
231800                                                                          
231900         DIVIDE W-TIGLT-MM-CDC BY 60 GIVING                               
232000                W-GLT-HH REMAINDER W-GLT-MM                               
232100                                                                          
232200         COMPUTE  UT-TIGLT-PRIO   =   W-GLT  / 100                        
232300     END-IF                                                               
232400                                                                          
232500     IF W-TOT-KVRADER-DAG-CDC =  ZERO                                     
232600         MOVE ZERO          TO UT-TIGLT-DAG                               
232700     ELSE                                                                 
232800         COMPUTE   W-TIGLT-MM-CDC           =                             
232900                   W-TOT-TIGLT-MM-DAG-CDC                                 
233000                 / W-TOT-KVRADER-DAG-CDC                                  
233100                                                                          
233200         DIVIDE W-TIGLT-MM-CDC BY 60 GIVING                               
233300                W-GLT-HH REMAINDER W-GLT-MM                               
233400                                                                          
233500         COMPUTE UT-TIGLT-DAG  =  W-GLT / 100                             
233600     END-IF                                                               
233700                                                                          
233800     IF W-TOT-KVRADER-PRIO-DAG-CDC =  ZERO                                
233900         MOVE ZERO             TO UT-TIGLT-PRIO-DAG                       
234000     ELSE                                                                 
234100         COMPUTE    W-TIGLT-MM-CDC               =                        
234200                    W-TOT-TIGLT-PRIO-MM-DAG-CDC                           
234300                  / W-TOT-KVRADER-PRIO-DAG-CDC                            
234400                                                                          
234500         DIVIDE W-TIGLT-MM-CDC BY 60 GIVING                               
234600                W-GLT-HH REMAINDER W-GLT-MM                               
234700                                                                          
234800         COMPUTE  UT-TIGLT-PRIO-DAG  =   W-GLT  / 100                     
234900     END-IF                                                               
235000                                                                          
235100     IF W-TOT-KVRADER-KVALL-CDC =  ZERO                                   
235200         MOVE ZERO          TO UT-TIGLT-KVALL                             
235300     ELSE                                                                 
235400         COMPUTE   W-TIGLT-MM-CDC             =                           
235500                   W-TOT-TIGLT-MM-KVALL-CDC                               
235600                 / W-TOT-KVRADER-KVALL-CDC                                
235700                                                                          
235800         DIVIDE W-TIGLT-MM-CDC BY 60 GIVING                               
235900                W-GLT-HH REMAINDER W-GLT-MM                               
236000                                                                          
236100         COMPUTE UT-TIGLT-KVALL = W-GLT / 100                             
236200     END-IF                                                               
236300                                                                          
236400     IF W-TOT-KVRADER-PRIO-KVALL-CDC =  ZERO                              
236500         MOVE ZERO             TO UT-TIGLT-PRIO-KVALL                     
236600     ELSE                                                                 
236700         COMPUTE    W-TIGLT-MM-CDC                 =                      
236800                    W-TOT-TIGLT-PRIO-MM-KVALL-CDC                         
236900                  / W-TOT-KVRADER-PRIO-KVALL-CDC                          
237000                                                                          
237100         DIVIDE W-TIGLT-MM-CDC BY 60 GIVING                               
237200                W-GLT-HH REMAINDER W-GLT-MM                               
237300                                                                          
237400         COMPUTE  UT-TIGLT-PRIO-KVALL  =   W-GLT  / 100                   
237500     END-IF                                                               
237600                                                                          
237700     PERFORM S03-SKRIV-W61155                                             
237800     .                                                                    
237900     EJECT                                                                
238000 D-SKRIV-TOTAL-POST-SVS     SECTION.                                      
238100                                                                          
238200     MOVE    'SVS'                    TO UT-IDPTYP                        
238300     MOVE    SPACE                    TO UT-KDINLUPF                      
238400     MOVE    SPACE                    TO UT-IDLEVNR                       
238500     MOVE WC-CDC-SE                   TO UT-IDDC                          
238600                                                                          
238700     MOVE W-TOT-KVRADER-SVS           TO UT-KVRADER                       
238800     MOVE W-TOT-KVRADER-DAG-SVS       TO UT-KVRADER-DAG                   
238900     MOVE W-TOT-KVRADER-KVALL-SVS     TO UT-KVRADER-KVALL                 
239000     MOVE W-TOT-KVRADER-PRIO-SVS      TO UT-KVRADER-PRIO                  
239100     MOVE W-TOT-KVART-SVS             TO UT-KVART                         
239200     MOVE W-TOT-KVART-DAG-SVS         TO UT-KVART-DAG                     
239300     MOVE W-TOT-KVART-KVALL-SVS       TO UT-KVART-KVALL                   
239400     MOVE W-TOT-SUBEL-SVS             TO UT-SUBEL                         
239500     MOVE W-TOT-SUBEL-PRIO-SVS        TO UT-SUBEL-PRIO                    
239600       COMPUTE UT-VLARTNTO =                                              
239700                       W-TOT-VLARTNTO-SVS / 1000000                       
239800     MOVE W-TOT-KVART-MAAL-SVS       TO UT-KVART-MAAL                     
239900     MOVE W-TOT-KVART-MAAL-PRIO-SVS  TO UT-KVART-MAAL-PRIO                
240000                                                                          
240100     MOVE ZERO                       TO UT-TIGLT                          
240200                                        UT-TIGLT-DAG                      
240300                                        UT-TIGLT-KVALL                    
240400                                        UT-TIGLT-PRIO-DAG                 
240500                                        UT-TIGLT-PRIO-KVALL               
240600                                                                          
240700     IF W-TOT-KVRADER-SVS =  ZERO                                         
240800         MOVE ZERO          TO UT-TIGLT                                   
240900     ELSE                                                                 
241000         COMPUTE   W-TIGLT-MM-SVS       =                                 
241100                   W-TOT-TIGLT-MM-SVS                                     
241200                 / W-TOT-KVRADER-SVS                                      
241300                                                                          
241400         DIVIDE W-TIGLT-MM-SVS BY 60 GIVING                               
241500                W-GLT-HH REMAINDER W-GLT-MM                               
241600                                                                          
241700         COMPUTE UT-TIGLT      =  W-GLT / 100                             
241800     END-IF                                                               
241900                                                                          
242000     IF W-TOT-KVRADER-PRIO-SVS =  ZERO                                    
242100         MOVE ZERO             TO UT-TIGLT-PRIO                           
242200     ELSE                                                                 
242300         COMPUTE    W-TIGLT-MM-SVS           =                            
242400                    W-TOT-TIGLT-PRIO-MM-SVS                               
242500                  / W-TOT-KVRADER-PRIO-SVS                                
242600                                                                          
242700         DIVIDE W-TIGLT-MM-SVS BY 60 GIVING                               
242800                W-GLT-HH REMAINDER W-GLT-MM                               
242900                                                                          
243000         COMPUTE  UT-TIGLT-PRIO   =   W-GLT  / 100                        
243100     END-IF                                                               
243200                                                                          
243300     IF W-TOT-KVRADER-DAG-SVS =  ZERO                                     
243400         MOVE ZERO          TO UT-TIGLT-DAG                               
243500     ELSE                                                                 
243600         COMPUTE   W-TIGLT-MM-SVS           =                             
243700                   W-TOT-TIGLT-MM-DAG-SVS                                 
243800                 / W-TOT-KVRADER-DAG-SVS                                  
243900                                                                          
244000         DIVIDE W-TIGLT-MM-SVS BY 60 GIVING                               
244100                W-GLT-HH REMAINDER W-GLT-MM                               
244200                                                                          
244300         COMPUTE UT-TIGLT-DAG  =  W-GLT / 100                             
244400     END-IF                                                               
244500                                                                          
244600     IF W-TOT-KVRADER-PRIO-DAG-SVS =  ZERO                                
244700         MOVE ZERO             TO UT-TIGLT-PRIO-DAG                       
244800     ELSE                                                                 
244900         COMPUTE    W-TIGLT-MM-SVS               =                        
245000                    W-TOT-TIGLT-PRIO-MM-DAG-SVS                           
245100                  / W-TOT-KVRADER-PRIO-DAG-SVS                            
245200                                                                          
245300         DIVIDE W-TIGLT-MM-SVS BY 60 GIVING                               
245400                W-GLT-HH REMAINDER W-GLT-MM                               
245500                                                                          
245600         COMPUTE  UT-TIGLT-PRIO-DAG  =   W-GLT  / 100                     
245700     END-IF                                                               
245800                                                                          
245900     IF W-TOT-KVRADER-KVALL-SVS =  ZERO                                   
246000         MOVE ZERO          TO UT-TIGLT-KVALL                             
246100     ELSE                                                                 
246200         COMPUTE   W-TIGLT-MM-SVS             =                           
246300                   W-TOT-TIGLT-MM-KVALL-SVS                               
246400                 / W-TOT-KVRADER-KVALL-SVS                                
246500                                                                          
246600         DIVIDE W-TIGLT-MM-SVS BY 60 GIVING                               
246700                W-GLT-HH REMAINDER W-GLT-MM                               
246800                                                                          
246900         COMPUTE UT-TIGLT-KVALL = W-GLT / 100                             
247000     END-IF                                                               
247100                                                                          
247200     IF W-TOT-KVRADER-PRIO-KVALL-SVS =  ZERO                              
247300         MOVE ZERO             TO UT-TIGLT-PRIO-KVALL                     
247400     ELSE                                                                 
247500         COMPUTE    W-TIGLT-MM-SVS                 =                      
247600                    W-TOT-TIGLT-PRIO-MM-KVALL-SVS                         
247700                  / W-TOT-KVRADER-PRIO-KVALL-SVS                          
247800                                                                          
247900         DIVIDE W-TIGLT-MM-SVS BY 60 GIVING                               
248000                W-GLT-HH REMAINDER W-GLT-MM                               
248100                                                                          
248200         COMPUTE  UT-TIGLT-PRIO-KVALL  =   W-GLT  / 100                   
248300     END-IF                                                               
248400                                                                          
248500     PERFORM S03-SKRIV-W61155                                             
248600     .                                                                    
248700     EJECT                                                                
248800 Z-FINIT SECTION.                                                         
248900     CLOSE W61140                                                         
249000           W6115A                                                         
249100           W61155                                                         
249200     SKIP2                                                                
249300     MOVE 'S' TO POSTSUM-OPKOD                                            
249400     CALL POSTSUM USING POSTSUM-PARM                                      
249500     .                                                                    
249600     EJECT                                                                
249700 S01-LAES-W61140  SECTION.                                                
249800     SKIP2                                                                
249900     READ W61140 INTO IN-AREA                                             
250000     AT END                                                               
250100        SET END-OF-W61140 TO TRUE                                         
250200                                                                          
250300     NOT AT END                                                           
250400        MOVE 'W61140'          TO POSTSUM-FDNAMN                          
250500        MOVE 'W61155D1'        TO POSTSUM-DDNAMN2                         
250600        CALL POSTSUM USING POSTSUM-PARM                                   
250700     END-READ                                                             
250800     .                                                                    
250900     EJECT                                                                
251000* CCID 6442199 -                                                          
251100 S02-LAES-W6115A   SECTION.                                               
251200     SKIP2                                                                
251300     READ W6115A   INTO MIN-W6115A01                                      
251400     AT END                                                               
251500        SET END-OF-W6115A      TO TRUE                                    
251600                                                                          
251700     NOT AT END                                                           
251800        MOVE 'W6115A  '        TO POSTSUM-FDNAMN                          
251900        MOVE 'W61155D2'        TO POSTSUM-DDNAMN2                         
252000        CALL POSTSUM USING POSTSUM-PARM                                   
252100     END-READ                                                             
252200                                                                          
252300     MOVE MIN-W6115A01         TO MAAL-AREA                               
252400     .                                                                    
252500     EJECT                                                                
252600 S03-SKRIV-W61155 SECTION.                                                
252700     SKIP2                                                                
252800     WRITE UT-POST FROM UT-AREA                                           
252900                                                                          
253000     MOVE UT-IDPTYP            TO POSTSUM-TRANSTYP                        
253100     MOVE 'W61155'             TO POSTSUM-FDNAMN                          
253200     MOVE 'W61155D3'           TO POSTSUM-DDNAMN2                         
253300     CALL POSTSUM USING POSTSUM-PARM                                      
253400     .                                                                    
253500     EJECT                                                                
253600 S11-KOLLA-MAAL-TABELL SECTION.                                           
253700     SET MAAL-IX               TO +1                                      
253800     PERFORM UNTIL MAAL-IX  > MAX-MAAL-IX                                 
253900             OR    W-MAAL-KDINLUPF  (MA-DCIX MAAL-IX)                     
254000                 = W-KDINLUPF       (DCIX)                                
254100       SET MAAL-IX      UP BY       +1                                    
254200     END-PERFORM                                                          
254300     .                                                                    
254400     EJECT                                                                
254500 S20-BERAEKNA-TIGLT   SECTION.                                            
254600                                                                          
254700     PERFORM S20A-JUSTERA-TIDERNA                                         
254800                                                                          
254900     PERFORM S20B-BESTAEM-FM-EM                                           
255000                                                                          
255100     PERFORM S20C-BERAEKNA-OLD                                            
255200                                                                          
255300     PERFORM S20D-BERAEKNA-NEW                                            
255400                                                                          
255500* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
255600     IF W-WEEKDAY = 5                                                     
255700       IF W-SPAR2-TIREGDAT (DCIX) = IN-TIREGDAT                           
255800         COMPUTE W-TIKLOCK-MM =                                           
255900       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 738)                      
256000       ELSE                                                               
256100        COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM        
256200       END-IF                                                             
256300     END-IF                                                               
256400                                                                          
256500     IF W-WEEKDAY = 1 OR 2 OR 3 OR 4                                      
256600       IF W-SPAR2-TIREGDAT (DCIX) = IN-TIREGDAT                           
256700         COMPUTE W-TIKLOCK-MM =                                           
256800       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 972)                      
256900       ELSE                                                               
257000        COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM        
257100       END-IF                                                             
257200                                                                          
257300     END-IF                                                               
257400                                                                          
257500*** 16,2 = 972 MIN                                                        
257600     IF WORK-KVWORKD > 0                                                  
257700       COMPUTE W-TOT-MM    = (WORK-KVWORKD * 972) +                       
257800                                    W-TIKLOCK-MM                          
257900       IF W-WEEKDAY-OLD > W-WEEKDAY                                       
258000** HAR LEGAT EN FREDAG SOM HAR 234 FÄRRE ARBETSMINUTER                    
258100         COMPUTE W-TOT-MM  = W-TOT-MM - 234                               
258200       END-IF                                                             
258300     ELSE                                                                 
258400       MOVE W-TIKLOCK-MM TO W-TOT-MM                                      
258500     END-IF                                                               
258600     .                                                                    
258700     EJECT                                                                
258800 S20-TOT-BERAEKNA-TIGLT   SECTION.                                        
258900                                                                          
259000     PERFORM S20A-JUSTERA-TIDERNA                                         
259100                                                                          
259200     PERFORM S20B-BESTAEM-FM-EM                                           
259300                                                                          
259400     PERFORM S20C-BERAEKNA-OLD                                            
259500                                                                          
259600     PERFORM S20D-BERAEKNA-NEW                                            
259700                                                                          
259800* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
259900     IF W-WEEKDAY = 5                                                     
260000       IF W-SPAR1-TIREGDAT (DCIX) = IN-TIREGDAT                           
260100         COMPUTE W-TIKLOCK-MM =                                           
260200       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 738)                      
260300       ELSE                                                               
260400        COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM        
260500       END-IF                                                             
260600     END-IF                                                               
260700                                                                          
260800     IF W-WEEKDAY = 1 OR 2 OR 3 OR 4                                      
260900       IF W-SPAR1-TIREGDAT (DCIX) = IN-TIREGDAT                           
261000         COMPUTE W-TIKLOCK-MM =                                           
261100       ((W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM) - 972)                      
261200       ELSE                                                               
261300        COMPUTE W-TIKLOCK-MM = W-TIKLOCK-OLD-MM + W-TIKLOCK-NEW-MM        
261400       END-IF                                                             
261500                                                                          
261600     END-IF                                                               
261700                                                                          
261800*** 16,2 = 972 MIN                                                        
261900     IF WORK-KVWORKD > 0                                                  
262000       COMPUTE W-TOT-MM    = (WORK-KVWORKD * 972) +                       
262100                                    W-TIKLOCK-MM                          
262200       IF W-WEEKDAY-OLD > W-WEEKDAY                                       
262300** HAR LEGAT EN FREDAG SOM HAR 234 FÄRRE ARBETSMINUTER                    
262400         COMPUTE W-TOT-MM  = W-TOT-MM - 234                               
262500       END-IF                                                             
262600     ELSE                                                                 
262700       MOVE W-TIKLOCK-MM TO W-TOT-MM                                      
262800     END-IF                                                               
262900     .                                                                    
263000     EJECT                                                                
263100 S20A-JUSTERA-TIDERNA  SECTION.                                           
263200                                                                          
263300* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
263400                                                                          
263500** TIDER UTANFÖR 06.30 - 11.00 ; 11.42 - 18.00 ; 18.30 - 23.54            
263600** JUSTERAS TILL NÄRMSTA INTERVALL                                        
263700                                                                          
263800** SATURDAY/SUNDAY AND MONDAY BEFORE 6.30 WILL BE SET TO                  
263900** FRIDAY 20.00                                                           
264000     IF W-WEEKDAY = 6 OR 7 OR                                             
264100       (W-WEEKDAY = 1 AND                                                 
264200      ((W-OLD-HH < 6) OR (W-OLD-HH = 6 AND W-OLD-MM < 30)))               
264300                                                                          
264400       MOVE 20     TO W-OLD-HH                                            
264500       MOVE 00     TO W-OLD-MM                                            
264600     ELSE                                                                 
264700                                                                          
264800** MONDAY-FRIDAY BETWEEN 11.00 - 11.21 WILL BE SET TO 11.00               
264900** MONDAY-FRIDAY BETWEEN 11.22 - 11.42 WILL BE SET TO 11.42               
265000     IF W-OLD-HH = 11                                                     
265100       IF W-OLD-MM < 42                                                   
265200         IF W-OLD-MM < 21                                                 
265300           MOVE 00 TO W-OLD-MM                                            
265400         ELSE                                                             
265500           MOVE 42 TO W-OLD-MM                                            
265600         END-IF                                                           
265700       END-IF                                                             
265800     ELSE                                                                 
265900                                                                          
266000** MONDAY-FRIDAY BETWEEN 18.00 - 18.14 WILL BE SET TO 18.00               
266100** MONDAY-FRIDAY BETWEEN 18.15 - 18.30 WILL BE SET TO 18.30               
266200     IF W-OLD-HH = 18                                                     
266300       IF W-OLD-MM < 30                                                   
266400         IF W-OLD-MM < 15                                                 
266500           MOVE 00 TO W-OLD-MM                                            
266600         ELSE                                                             
266700           MOVE 30 TO W-OLD-MM                                            
266800         END-IF                                                           
266900       END-IF                                                             
267000     ELSE                                                                 
267100                                                                          
267200** FRIDAY AFTER 20.00 WILL BE SET TO 20.00                                
267300     IF W-WEEKDAY = 5                                                     
267400       IF (W-OLD-HH > 20) OR (W-OLD-HH = 20 AND W-OLD-MM > 00)            
267500         MOVE 20   TO W-OLD-HH                                            
267600         MOVE 00   TO W-OLD-MM                                            
267700       END-IF                                                             
267800     ELSE                                                                 
267900                                                                          
268000** MONDAY-FRIDAY BEFORE 6.30 WILL BE SET TO 6.30                          
268100     IF (W-OLD-HH < 6) OR (W-OLD-HH = 6 AND W-OLD-MM < 30)                
268200       MOVE  6     TO W-OLD-HH                                            
268300       MOVE 30     TO W-OLD-MM                                            
268400     ELSE                                                                 
268500                                                                          
268600** MONDAY-FRIDAY AFTER 23.54 WILL BE SET TO 23.54                         
268700     IF (W-OLD-HH > 23) OR (W-OLD-HH = 23 AND W-OLD-MM > 54)              
268800       MOVE 23     TO W-OLD-HH                                            
268900       MOVE 54     TO W-OLD-MM                                            
269000     END-IF                                                               
269100     END-IF                                                               
269200     END-IF                                                               
269300     END-IF                                                               
269400     END-IF                                                               
269500     END-IF                                                               
269600                                                                          
269700                                                                          
269800** SATURDAY/SUNDAY AND MONDAY BEFORE 6.30 WILL BE SET TO                  
269900** FRIDAY 20.00                                                           
270000     IF (W-WEEKDAY = 6 OR 7) OR                                           
270100        (W-WEEKDAY = 1 AND                                                
270200        (W-NEW-HH < 6) OR (W-NEW-HH = 6 AND W-NEW-MM < 30))               
270300       MOVE 20     TO W-NEW-HH                                            
270400       MOVE 00     TO W-NEW-MM                                            
270500     ELSE                                                                 
270600                                                                          
270700** MONDAY-FRIDAY BETWEEN 11.00 - 11.21 WILL BE SET TO 11.00               
270800** MONDAY-FRIDAY BETWEEN 11.22 - 11.42 WILL BE SET TO 11.42               
270900     IF W-NEW-HH = 11                                                     
271000       IF W-NEW-MM < 42                                                   
271100         IF W-NEW-MM < 21                                                 
271200           MOVE 00 TO W-NEW-MM                                            
271300         ELSE                                                             
271400           MOVE 42 TO W-NEW-MM                                            
271500         END-IF                                                           
271600       END-IF                                                             
271700     ELSE                                                                 
271800                                                                          
271900** FRIDAY AFTER 20.00 WILL BE SET TO 20.00                                
272000     IF W-WEEKDAY = 5                                                     
272100       IF (W-NEW-HH > 20) OR (W-NEW-HH = 20 AND W-NEW-MM > 00)            
272200         MOVE 20   TO W-NEW-HH                                            
272300         MOVE 00   TO W-NEW-MM                                            
272400       END-IF                                                             
272500     ELSE                                                                 
272600                                                                          
272700** MONDAY-FRIDAY BEFORE 6.30 WILL BE SET TO 6.30                          
272800     IF (W-NEW-HH < 6) OR (W-NEW-HH = 6 AND W-NEW-MM < 30)                
272900       MOVE  6     TO W-NEW-HH                                            
273000       MOVE 30     TO W-NEW-MM                                            
273100     ELSE                                                                 
273200                                                                          
273300** MONDAY-FRIDAY AFTER 23.54 WILL BE SET TO 23.54                         
273400     IF (W-NEW-HH > 23) OR (W-NEW-HH = 23 AND W-NEW-MM > 54)              
273500       MOVE 23     TO W-NEW-HH                                            
273600       MOVE 54     TO W-NEW-MM                                            
273700     END-IF                                                               
273800     END-IF                                                               
273900     END-IF                                                               
274000     END-IF                                                               
274100     END-IF                                                               
274200     .                                                                    
274300     EJECT                                                                
274400 S20B-BESTAEM-FM-EM  SECTION.                                             
274500                                                                          
274600     MOVE 'FM' TO OLD-SW                                                  
274700     IF W-OLD-HH <= 11                                                    
274800       IF W-OLD-HH = 11 AND W-OLD-MM > 41                                 
274900         MOVE 'EM' TO OLD-SW                                              
275000       END-IF                                                             
275100     ELSE                                                                 
275200       MOVE 'EM' TO OLD-SW                                                
275300     END-IF                                                               
275400                                                                          
275500     MOVE 'FM' TO NEW-SW                                                  
275600     IF W-NEW-HH <= 11                                                    
275700       IF W-NEW-HH = 11 AND W-NEW-MM > 41                                 
275800         MOVE 'EM' TO NEW-SW                                              
275900       END-IF                                                             
276000     ELSE                                                                 
276100       MOVE 'EM' TO NEW-SW                                                
276200     END-IF                                                               
276300                                                                          
276400     .                                                                    
276500     EJECT                                                                
276600 S20C-BERAEKNA-OLD SECTION.                                               
276700                                                                          
276800* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
276900                                                                          
277000     IF OLD-FM                                                            
277100       IF W-WEEKDAY = 5                                                   
277200         COMPUTE W-TIKLOCK-OLD-MM =                                       
277300         (((11 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM)) + 468           
277400** 7.8 TIM  = 468 MIN                                                     
277500       ELSE                                                               
277600         IF W-WEEKDAY = 1 OR 2 OR 3 OR 4                                  
277700           COMPUTE W-TIKLOCK-OLD-MM =                                     
277800           (((11 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM)) + 702         
277900** 11.7 TIM = 702 MIN                                                     
278000         END-IF                                                           
278100       END-IF                                                             
278200     ELSE                                                                 
278300       IF W-WEEKDAY = 5                                                   
278400         IF W-OLD-HH > 18 OR (W-OLD-HH = 18 AND W-OLD-MM > 15)            
278500           COMPUTE W-TIKLOCK-OLD-MM =                                     
278600           (((20 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM))               
278700         ELSE                                                             
278800           COMPUTE W-TIKLOCK-OLD-MM =                                     
278900           (((20 * 60) - (W-OLD-HH * 60)) + (0 - W-OLD-MM - 30))          
279000         END-IF                                                           
279100       ELSE                                                               
279200       IF W-WEEKDAY = 1 OR 2 OR 3 OR 4                                    
279300         IF W-OLD-HH > 18 OR (W-OLD-HH = 18 AND W-OLD-MM > 15)            
279400           COMPUTE W-TIKLOCK-OLD-MM =                                     
279500           (((23 * 60) - (W-OLD-HH * 60)) + (54 - W-OLD-MM))              
279600         ELSE                                                             
279700           COMPUTE W-TIKLOCK-OLD-MM =                                     
279800           (((23 * 60) - (W-OLD-HH * 60)) + (54 - W-OLD-MM - 30))         
279900         END-IF                                                           
280000       END-IF                                                             
280100       END-IF                                                             
280200     END-IF                                                               
280300     .                                                                    
280400     EJECT                                                                
280500 S20D-BERAEKNA-NEW SECTION.                                               
280600                                                                          
280700* CCID 6442199 - NEW RULES FOR TIME CALCULATION IN THIS SECTION           
280800                                                                          
280900     IF NEW-FM                                                            
281000       COMPUTE W-TIKLOCK-NEW-MM =                                         
281100       (((W-NEW-HH * 60) - (6.5 * 60)) + W-NEW-MM)                        
281200     ELSE                                                                 
281300       COMPUTE W-TIKLOCK-NEW-MM =                                         
281400       (((270 + (W-NEW-HH * 60)) - ((11 * 60) + 42))) + W-NEW-MM          
281500** 4.5 TIM = 270                                                          
281600     END-IF                                                               
281700     .                                                                    
281800     EJECT                                                                
281900 S21-KONVERTERA-DATUM SECTION.                                            
282000                                                                          
282100     MOVE IN-TIREGDAT TO DAT-I-TIDATUM                                    
282200     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
282300                                                                          
282400     CALL WDATKONV USING DAT-KDDATFORM                                    
282500                         DAT-I-TIDATUM                                    
282600                         DAT-O-TIDATUM                                    
282700                         DAT-KDSVAR                                       
282800                                                                          
282900     IF DAT-KDSVAR = SPACE                                                
283000       MOVE DAT-TIVV       TO W-TIVV                                      
283100     ELSE                                                                 
283200       MOVE 'FEL FRÅN WDATKONV' TO FELTEXT-STR                            
283300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
283400     END-IF                                                               
283500     .                                                                    
283600     EJECT                                                                
283700 S22-CALC-WEEKDAY SECTION.                                                
283800                                                                          
283900* CCID 6442199 - NEW SECTION                                              
284000                                                                          
284100     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
284200                                                                          
284300     CALL WDATKONV USING DAT-KDDATFORM                                    
284400                         DAT-I-TIDATUM                                    
284500                         DAT-O-TIDATUM                                    
284600                         DAT-KDSVAR                                       
284700                                                                          
284800     IF DAT-KDSVAR = SPACE                                                
284900       MOVE DAT-TIAAVVD    TO W-TIAAVVD                                   
285000       MOVE W-TIAAVVD      TO W-TIAAVVD-OLD-ALPHA                         
285100     ELSE                                                                 
285200       MOVE 'FEL FRÅN WDATKONV' TO FELTEXT-STR                            
285300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
285400     END-IF                                                               
285500                                                                          
285600     MOVE IN-TIREGDAT TO DAT-I-TIDATUM                                    
285700     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
285800                                                                          
285900     CALL WDATKONV USING DAT-KDDATFORM                                    
286000                         DAT-I-TIDATUM                                    
286100                         DAT-O-TIDATUM                                    
286200                         DAT-KDSVAR                                       
286300                                                                          
286400     IF DAT-KDSVAR = SPACE                                                
286500       MOVE DAT-TIAAVVD    TO W-TIAAVVD                                   
286600       MOVE W-TIAAVVD      TO W-TIAAVVD-ALPHA                             
286700     ELSE                                                                 
286800       MOVE 'FEL FRÅN WDATKONV' TO FELTEXT-STR                            
286900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
287000     END-IF                                                               
287100     .                                                                    
287200     EJECT                                                                
287300 S23-CONVERT-HHMM SECTION.                                                
287400                                                                          
287500* CCID 6442199 - NEW SECTION                                              
287600                                                                          
287700                                                                          
287800     MOVE MAAL-TIGLT      TO WS-TIGLT                                     
287900     COMPUTE MAAL-TIGLT = (WS-TIGLT-HH * 60) + WS-TIGLT-MM                
288000     END-COMPUTE                                                          
288100                                                                          
288200     MOVE MAAL-TIGLT-PRIO TO WS-TIGLT                                     
288300     COMPUTE MAAL-TIGLT-PRIO = (WS-TIGLT-HH * 60) +                       
288400                                WS-TIGLT-MM                               
288500     END-COMPUTE                                                          
288600     .                                                                    
288700     EJECT                                                                
288800*    -COPY WY2000P1                                                       
288900 IMS-GU-WDB601    SECTION.                                                
289000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
289100          DELIMITED BY SIZE INTO SSA1                                     
289200     MOVE '  GE' TO GOOD-STATUSCODES                                      
289300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
289400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
289500     PERFORM IMS-STATUSKONTROLL                                           
289600     .                                                                    
289700     EJECT                                                                
289800                                                                          
289900 IMS-STATUSKONTROLL SECTION.                                              
290000     SET STATUS-IX TO 1                                                   
290100     SEARCH GOOD-STATUS                                                   
290200       AT END                                                             
290300         CALL FELLOG                                                      
290400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
290500         CONTINUE                                                         
290600     END-SEARCH                                                           
290700     .                                                                    
290800     EJECT                                                                
