000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4068200.                                                
000400 AUTHOR.         CAMELIA  & GAVIN                                         
000500 DATE-WRITTEN.   97/01/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKRIVER 'BILL OF LADING' FÖR NDC I  AUSTRALIEN.                  
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300* PROGRAMMET LÄSER             WLORQI (WDQ2)  NAMN OCH ADRESS             
001400*                              WL4463 (WDR4)  BL BASEN                    
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T682                                              
001800*        MID:         W4I68201                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O68201                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W4068200'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                         PIC X          VALUE 'J'.                 
003600 77  NEJ                        PIC X          VALUE 'N'.                 
003700 77  FG-INDX                    PIC S9(9)     VALUE +0  COMP SYNC.        
003800 77  FG-MAX-INDX                PIC S9(9)     VALUE +50 COMP SYNC.        
003900 77  WS-SUM-WEIGHT-SNDR         PIC S9(6)V9(1) VALUE ZERO COMP-3.         
004000 77  WS-SUM-WEIGHT-TOT          PIC S9(6)V9(1) VALUE ZERO COMP-3.         
004100 77  WS-SUM-ITEMS-SNDR          PIC S9(4)      VALUE ZERO COMP-3.         
004200 77  WS-SUM-ITEMS-TOT           PIC S9(4)      VALUE ZERO COMP-3.         
004300 77  SPAR-IDDISTR               PIC S9(5)      VALUE ZERO COMP-3.         
004400 77  SPAR-IDKUNDNR              PIC S9(7)      VALUE ZERO COMP-3.         
004500 77  SPAR-IDKUNDRF-7            PIC 9(7)       VALUE ZERO.                
004600                                                                          
004700 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +33 COMP SYNC.          
004800                                                                          
004900 01  RADSKIP-COUNTER            PIC S9(3) COMP-3 VALUE +1.                
005000                                                                          
005100     EJECT                                                                
005200*** KONSTANTER                                                            
005300 01  FILLER                     PIC X(16)  VALUE 'KONSTANTER'.            
005400                                                                          
005500 01  ACCOUNT-100                PIC X(12)  VALUE 'V980515,01  '.          
005600 01  ACCOUNT-130                PIC X(12)  VALUE 'V980530,0150'.          
005700 01  ACCOUNT-300                PIC X(12)  VALUE '8269259     '.          
005800 01  ACCOUNT-RESTEN             PIC X(12)  VALUE '            '.          
005900*                                                                         
006000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006100                                                                          
006200                                                                          
006300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006400     88  NYCKLAR-OK                          VALUE 'J'.                   
006500     88  NYCKLAR-FEL                         VALUE 'N'.                   
006600                                                                          
006700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006800     88  EGEN-MID                            VALUE '4682'.                
006900     88  GODK-MID                            VALUE '4681' '4682'          
007000                                                   '4683' '4684'          
007100                                                   '4685' '4686'          
007200                                                   '4687' '4688'          
007300                                                   '4689'.                
007400     88  HELP-MID                            VALUE '0551'.                
007500     EJECT                                                                
007600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007700 01  GENERELLA-SUBPROGRAM.                                                
007800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008300     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
008400     EJECT                                                                
008500 01    FILLER                    PIC X(16)   VALUE 'FG-TABELL'.           
008600                                                                          
008700 01    RAD-TABELL.                                                        
008800   03    TAB-POST OCCURS 50 INDEXED BY RADINDX.                           
008900     05  RAD-SNDREF              PIC 9(8).                                
009000     05  RAD-ITEMS               PIC 9(4).                                
009100     05  RAD-WEIGHT              PIC 9(6)V9(1).                           
009200                                                                          
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009500*01 -COPY WMEDAREA                                                        
009600     SKIP3                                                                
009700 01  MESSAGE-CODES.                                                       
009800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009900     EJECT                                                                
010000*01  -COPY W006PRAR                                                       
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010300                                                                          
010400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010500     SKIP3                                                                
010600*01 -COPY WMSGINIT                                                        
010700     SKIP3                                                                
010800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010900                                                                          
011000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011100     SKIP3                                                                
011200*01  MID -COPY W4I68801                                                   
011300     EJECT                                                                
011400 01  FILLER                    PIC X(16)   VALUE 'MSG/MOD-AREA'.          
011500     SKIP3                                                                
011600*01  -COPY WMSGAREA                                                       
011700     EJECT                                                                
011800 01  FILLER                    PIC X(16)   VALUE 'MFS-AREA'.              
011900     SKIP3                                                                
012000*01  -COPY WMFSAREA                                                       
012100     EJECT                                                                
012200 01  FILLER                    PIC X(16)   VALUE 'PRT-AREA  '.            
012300 01  WS-PRT-AREA.                                                         
012400                                                                          
012500     03  WS-PRT-DUMMY          PIC X.                                     
012620     03  WS-PRT-IDPRTLST       PIC X(8)    VALUE '4BL62   '  .            
012700     03  WS-LIST-RAD           PIC X(132).                                
012800                                                                          
012900 01  FILLER                    PIC X(16)   VALUE 'LIST-RADER'.            
013000 01  LIST-RADER.                                                          
013100                                                                          
013200     03 RAD-1.                                                            
013300       05 FILLER               PIC X(60)   VALUE SPACE.                   
013400       05 FILLER               PIC X(5)    VALUE 'VAPL'.                  
013500       05 RAD1-ORDNO           PIC Z(8)    VALUE ZERO.                    
013600     03 RAD-2.                                                            
013700       05 FILLER               PIC X(9)    VALUE SPACE.                   
013800       05 RAD2-NAME            PIC X(35)   VALUE SPACE.                   
013900       05 FILLER               PIC X(16)   VALUE SPACE.                   
014000       05 RAD2-ACCOUNT         PIC X(12)   VALUE SPACE.                   
014100     03 RAD-3.                                                            
014200       05 FILLER               PIC X(9)    VALUE SPACE.                   
014300       05 RAD3-ADDRESS1        PIC X(35)   VALUE SPACE.                   
014400     03 RAD-4.                                                            
014500       05 FILLER               PIC X(9)    VALUE SPACE.                   
014600       05 RAD4-ADDRESS2        PIC X(35)   VALUE SPACE.                   
014700     03 RAD-5.                                                            
014800       05 FILLER               PIC X(9)    VALUE SPACE.                   
014900       05 RAD5-STATE           PIC X(35)   VALUE SPACE.                   
015000       05 FILLER               PIC X(1)    VALUE SPACE.                   
015100       05 RAD5-CUSTNO          PIC Z(7)    VALUE ZERO.                    
015200     03 RAD-6.                                                            
015300       05 FILLER               PIC X(9)    VALUE SPACE.                   
015400       05 RAD6-FREIGHTCO       PIC X(12)   VALUE 'TNT         '.          
015500     03 RAD-7.                                                            
015600       05 FILLER               PIC X(9)    VALUE SPACE.                   
015700       05 RAD7-DESCR           PIC X(11)   VALUE SPACE.                   
015800       05 FILLER               PIC X(7)    VALUE SPACE.                   
015900       05 RAD7-SNDREF          PIC Z(8)    VALUE ZERO.                    
016000       05 FILLER               PIC X(2)    VALUE SPACE.                   
016100       05 RAD7-ITEMS           PIC Z(4)    VALUE SPACE.                   
016200       05 FILLER               PIC X(4)    VALUE SPACE.                   
016300       05 RAD7-WEIGHT          PIC Z(5)9.9 VALUE SPACE.                   
016400     03 RAD-8.                                                            
016500       05 FILLER               PIC X(27)   VALUE SPACE.                   
016600       05 RAD8-ITEMS-TOT       PIC Z(4)    VALUE ZERO.                    
016700       05 FILLER               PIC X(14)   VALUE SPACE.                   
016800       05 RAD8-WEIGHT-TOT      PIC Z(5)9.9 VALUE ZERO.                    
016900     03 RAD-9.                                                            
017000       05 RAD9-DD             PIC Z9       VALUE ZERO.                    
017100       05 FILLER               PIC X(1)    VALUE '/'.                     
017200       05 RAD9-MM             PIC 99       VALUE ZERO.                    
017300       05 FILLER               PIC X(1)    VALUE '/'.                     
017400       05 RAD9-YY             PIC 99       VALUE ZERO.                    
017500                                                                          
017600                                                                          
017700     EJECT                                                                
017800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017900*                                                                         
018000     SKIP2                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018200     SKIP3                                                                
018300 01  NYCKLAR-TILL-DLI.                                                    
018400                                                                          
018500     03  W-IDGMTREF-X.                                                    
018600         05  W-IDDISTR-WDQ2      PIC S9(5)   VALUE ZERO COMP-3.           
018700         05  W-IDKUNDNR-WDQ2     PIC S9(7)   VALUE ZERO COMP-3.           
018800         05  W-IDKUNDRF-WDQ2     PIC X(10)   VALUE SPACE.                 
018900                                                                          
019000     03  W-WDGXKEY-X.                                                     
019100         05  FILLER              PIC X(4)    VALUE '4463'.                
019200         05  FILLER              PIC X(2)    VALUE '62'.                  
019300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
019400                                                                          
019500     03  W-DASKEPPNING-X.                                                 
019600         05  W-DASKEPPN          PIC  9(8)   VALUE ZERO.                  
019700                                                                          
019800     03  W-KY4466-X.                                                      
019900         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
020000         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
020100                                                                          
020200     03  W-KY4468-X.                                                      
020300         05  W-IDDISTR-4468       PIC S9(5)   VALUE ZERO  COMP-3.         
020400         05  W-IDKUNDNR-4468      PIC S9(7)   VALUE ZERO  COMP-3.         
020500         05  W-IDKUNDRF           PIC X(10).                              
020600         05  W-IDKUNDRF-IDORDNR-FILLER REDEFINES W-IDKUNDRF.              
020700           07  W-IDORDNR7         PIC  9(7).                              
020800           07  FILLER             PIC  X(3).                              
020900         05  W-IDPRODNR           PIC S9(7)   VALUE ZERO  COMP-3.         
021000         05  W-IDKOLLI            PIC S9(5)   VALUE ZERO  COMP-3.         
021100     SKIP2                                                                
021200*    --- STATUS-KOD FRÅN IMS                                              
021300 01  STATUS-WS                    PIC XX.                                 
021400     88  SEGMENT-FINNS                        VALUE '  '.                 
021500     88  SEGMENT-FINNS-REDAN                  VALUE 'II'.                 
021600     88  SEGMENT-SAKNAS                       VALUE 'GE'.                 
021700     88  BASEN-SLUT                           VALUE 'GB'.                 
021800     SKIP2                                                                
021900 01  GODK-STATUSKODER.                                                    
022000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022100     SKIP3                                                                
022200 01  SSA1                         PIC X(128).                             
022300 01  SSA2                         PIC X(64).                              
022400 01  SSA3                         PIC X(64).                              
022500     EJECT                                                                
022600*    --- IMS FUNKTIONSKODER                                               
022700*01  -COPY W0003                                                          
022800     EJECT                                                                
022900*    ---  DLI INPUT-OUTPUT AREA                                           
023000     EJECT                                                                
023100 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4463'.        
023200     SKIP3                                                                
023300 01  DLI-IO-AREA-4463.                                                    
023400     03  WL446301.                                                        
023500*        05  -COPY WDGX4463                                               
023600     EJECT                                                                
023700 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4464'.        
023800     SKIP3                                                                
023900 01  DLI-IO-AREA-4464.                                                    
024000     03  WL446311.                                                        
024100*        05  -COPY WDGX4464                                               
024200     EJECT                                                                
024300 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4466'.        
024400     SKIP3                                                                
024500 01  DLI-IO-AREA-4466.                                                    
024600     03  WL446321.                                                        
024700*        05  -COPY WDGX4466                                               
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)  VALUE 'DLI-AREA-4468'.        
025000     SKIP3                                                                
025100 01  DLI-IO-AREA-4468.                                                    
025200     03  WL446331.                                                        
025300*        05  -COPY WDGX4468                                               
025400     EJECT                                                                
025500 01  DLI-IO-AREA-OHUV.                                                    
025600     03  WLORQI01.                                                        
025700*        05  -COPY WDQ201                                                 
025800     EJECT                                                                
025900 LINKAGE SECTION.                                                         
026000                                                                          
026100*01  -COPY W0009   -PRE MSG-                                              
026200                                                                          
026300*01  -COPY W0009   -PRE ALT-                                              
026400                                                                          
026500*01  -COPY W0008   -PRE 4463-                                             
026600     05  FILLER                  PIC X.                                   
026700     EJECT                                                                
026800*01  -COPY W0008  -PRE ORQI-                                              
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  4463-PCB                     
027200                           ORQI-PCB         .                             
027300     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  4463-PCB                     
027400                           ORQI-PCB         .                             
027500                                                                          
027600     PERFORM IMS-GET-MSG                                                  
027700     IF SEGMENT-FINNS                                                     
027800       PERFORM A-INIT                                                     
027900       IF NYCKLAR-OK                                                      
028000                                                                          
028100         IF MID-KDSVAR = 'P' OR 'p' OR 'X' or 'x'                         
028200           PERFORM IMS-GU-WL446321-TRANSP                                 
028300                                                                          
028400           IF SEGMENT-FINNS                                               
028500             PERFORM C-LAES-SKRIV-BILL-OF-LADING                          
028600           END-IF                                                         
028700         END-IF                                                           
028800                                                                          
028900         PERFORM Z-STAENG-PRINTER                                         
029000       END-IF                                                             
029100     END-IF                                                               
029200                                                                          
029300     MOVE ZERO TO RETURN-CODE                                             
029400     GOBACK                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 A-INIT SECTION.                                                          
029800                                                                          
029900     MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W4I68801                  
030000     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
030100                                                                          
030200     MOVE MID-TIDATUM     TO W-DASKEPPN                                   
030300     IF MID-TIDATUM NOT = ZERO                                            
030400       IF MID-TIDATUM < 500000                                            
030500         MOVE 20          TO W-DASKEPPN (1:2)                             
030600       ELSE                                                               
030700         IF MID-TIDATUM < 999999                                          
030800           MOVE 19        TO W-DASKEPPN (1:2)                             
030900         ELSE                                                             
031000           MOVE 99999999  TO W-DASKEPPN                                   
031100         END-IF                                                           
031200       END-IF                                                             
031300     END-IF                                                               
031400                                                                          
031500     SET RADINDX TO +1                                                    
031600     PERFORM UNTIL RADINDX > FG-MAX-INDX                                  
031700       MOVE ZERO          TO RAD-SNDREF(RADINDX)                          
031800                             RAD-ITEMS (RADINDX)                          
031900                             RAD-WEIGHT(RADINDX)                          
032000       SET RADINDX UP BY 1                                                
032100     END-PERFORM                                                          
032110     SET RADINDX TO +1                                                    
032200                                                                          
032300                                                                          
032400     MOVE MID-IDTRPTNR    TO W-IDTRPTNR                                   
032500     MOVE MID-IDLBBET     TO W-IDLBBET                                    
032600                                                                          
032700                                                                          
032800     CALL W006PRS1  USING PRT-FORMS-OVR                                   
032900                          PRT-OPEN                                        
033000                          WS-PRT-IDPRTLST                                 
033100                          ALT-PCB                                         
033200                          WS-PRT-DUMMY                                    
033300                          WS-PRT-DUMMY                                    
033400                                                                          
033500     .                                                                    
033600     EJECT                                                                
033700 C-LAES-SKRIV-BILL-OF-LADING SECTION.                                     
033800                                                                          
033900     PERFORM IMS-GNP-WL446331-KOLLI                                       
034000                                                                          
034100     PERFORM UNTIL SEGMENT-SAKNAS                                         
034200                                                                          
034210       PERFORM CA-SKAPA-HUVUDET                                           
034220                                                                          
034230       PERFORM UNTIL  SEGMENT-SAKNAS    OR                                
034240                     (4468-IDDISTR  NOT = SPAR-IDDISTR)  OR               
034250                     (4468-IDKUNDNR NOT = SPAR-IDKUNDNR)                  
034260                                                                          
034290         MOVE 4468-IDKUNDRF (1:7) TO SPAR-IDKUNDRF-7                      
034291                                                                          
034400         PERFORM UNTIL SEGMENT-SAKNAS    OR                               
034410                     (4468-IDDISTR  NOT = SPAR-IDDISTR)  OR               
034420                     (4468-IDKUNDNR NOT = SPAR-IDKUNDNR) OR               
034500                     (4468-IDKUNDRF (1:7) NOT = SPAR-IDKUNDRF-7)          
034600           ADD 4468-VKORDBTO-KOLLI TO WS-SUM-WEIGHT-SNDR                  
034700                                      WS-SUM-WEIGHT-TOT                   
034800           ADD     1               TO WS-SUM-ITEMS-SNDR                   
034900                                      WS-SUM-ITEMS-TOT                    
035000                                                                          
035100           PERFORM IMS-GNP-WL446331-KOLLI                                 
035200         END-PERFORM                                                      
035300                                                                          
035400         MOVE SPAR-IDKUNDRF-7      TO RAD-SNDREF (RADINDX)                
035500         MOVE WS-SUM-ITEMS-SNDR    TO RAD-ITEMS  (RADINDX)                
035600         MOVE WS-SUM-WEIGHT-SNDR   TO RAD-WEIGHT (RADINDX)                
035610         MOVE ZERO                 TO WS-SUM-WEIGHT-SNDR                  
035620                                      WS-SUM-ITEMS-SNDR                   
035700         SET RADINDX UP BY 1                                              
035800                                                                          
035900       END-PERFORM                                                        
040800                                                                          
040900       MOVE WS-SUM-ITEMS-TOT       TO RAD8-ITEMS-TOT                      
041000       MOVE WS-SUM-WEIGHT-TOT      TO RAD8-WEIGHT-TOT                     
041010       MOVE ZERO                   TO WS-SUM-WEIGHT-TOT                   
041020                                      WS-SUM-ITEMS-TOT                    
041100                                                                          
041200       PERFORM CC-SKRIV                                                   
041300                                                                          
041310       SET RADINDX TO +1                                                  
041320       PERFORM UNTIL RADINDX > FG-MAX-INDX                                
041330         MOVE ZERO                 TO RAD-SNDREF(RADINDX)                 
041340                                      RAD-ITEMS (RADINDX)                 
041350                                      RAD-WEIGHT(RADINDX)                 
041360         SET RADINDX UP BY 1                                              
041370       END-PERFORM                                                        
041371       SET RADINDX TO +1                                                  
041380                                                                          
041400     END-PERFORM                                                          
041500     .                                                                    
041600     EJECT                                                                
041601 CA-SKAPA-HUVUDET SECTION.                                                
041602                                                                          
041603       IF 4466-IDTRPTNR = 100                                             
041604         MOVE ACCOUNT-100          TO RAD2-ACCOUNT                        
041605       ELSE                                                               
041606         IF 4466-IDTRPTNR = 130                                           
041607           MOVE ACCOUNT-130        TO RAD2-ACCOUNT                        
041608         ELSE                                                             
041609           IF 4466-IDTRPTNR = 300                                         
041610             MOVE ACCOUNT-300      TO RAD2-ACCOUNT                        
041611           ELSE                                                           
041612             MOVE ACCOUNT-RESTEN   TO RAD2-ACCOUNT                        
041613           END-IF                                                         
041614         END-IF                                                           
041615       END-IF                                                             
041616                                                                          
041617       MOVE 4468-IDDISTR           TO SPAR-IDDISTR                        
041618                                      W-IDDISTR-WDQ2                      
041620       MOVE 4468-IDKUNDNR          TO SPAR-IDKUNDNR                       
041621                                      W-IDKUNDNR-WDQ2                     
041622       MOVE 4468-IDKUNDRF          TO W-IDKUNDRF-WDQ2                     
041623                                                                          
041691                                                                          
041692       PERFORM IMS-GU-ORQI-WDQ201                                         
041693       IF SEGMENT-FINNS                                                   
041694         MOVE OHUV-BEGMT-RAD1      TO RAD2-NAME                           
041695         MOVE OHUV-ADGMT-GATA      TO RAD3-ADDRESS1                       
041696         MOVE OHUV-ADGMT-PADR      TO RAD4-ADDRESS2                       
041697         MOVE OHUV-ADGMT-LAND      TO RAD5-STATE                          
041698       ELSE                                                               
041699         MOVE 'ORDERNUMMER ÄR BLANKT' TO FELTEXT                          
041700         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
041701       END-IF                                                             
041702                                                                          
041703       MOVE  4468-IDKUNDRF (1:7)   TO RAD1-ORDNO                          
041704       MOVE  4468-IDKUNDNR         TO RAD5-CUSTNO                         
041705       MOVE  W-DASKEPPN (3:2)      TO RAD9-YY                             
041706       MOVE  W-DASKEPPN (5:2)      TO RAD9-MM                             
041707       MOVE  W-DASKEPPN (7:2)      TO RAD9-DD                             
041708                                                                          
041709     .                                                                    
041710     EJECT                                                                
041720 CC-SKRIV SECTION.                                                        
041800                                                                          
041900     MOVE SPACE           TO WS-LIST-RAD                                  
042000     MOVE PRT-NYSIDA-RAD1 TO PRT-RADSKIP                                  
042100     PERFORM S03-SKRIV                                                    
042200                                                                          
042300     MOVE       RAD-1     TO WS-LIST-RAD                                  
042400     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
042500     PERFORM S03-SKRIV                                                    
042600                                                                          
042700     MOVE       RAD-2     TO WS-LIST-RAD                                  
042800     MOVE PRT-AFTER-8     TO PRT-RADSKIP                                  
042900     PERFORM S03-SKRIV                                                    
043000                                                                          
043100     MOVE       RAD-3     TO WS-LIST-RAD                                  
043200     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
043300     PERFORM S03-SKRIV                                                    
043400                                                                          
043500     MOVE       RAD-4     TO WS-LIST-RAD                                  
043600     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
043700     PERFORM S03-SKRIV                                                    
043800                                                                          
043900     MOVE       RAD-5     TO WS-LIST-RAD                                  
044000     MOVE PRT-AFTER-1     TO PRT-RADSKIP                                  
044100     PERFORM S03-SKRIV                                                    
044200                                                                          
044300     MOVE       RAD-6     TO WS-LIST-RAD                                  
044400     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
044500     PERFORM S03-SKRIV                                                    
044600                                                                          
044610     MOVE +1                          TO RADSKIP-COUNTER                  
044700     PERFORM VARYING RADINDX FROM 1 BY 1 UNTIL RADINDX > 10               
044800                                                                          
044900       IF RAD-SNDREF (RADINDX) = ZERO                                     
045100         ADD +1 TO RADSKIP-COUNTER                                        
045200       ELSE                                                               
045300         IF RADINDX = 1                                                   
045400           MOVE PRT-AFTER-2           TO PRT-RADSKIP                      
045500           MOVE 'MOTOR PARTS'         TO RAD7-DESCR                       
045600         ELSE                                                             
045700           MOVE PRT-AFTER-1           TO PRT-RADSKIP                      
045800           MOVE SPACE                 TO RAD7-DESCR                       
045900         END-IF                                                           
046000         MOVE RAD-SNDREF (RADINDX)    TO RAD7-SNDREF                      
046100         MOVE RAD-ITEMS  (RADINDX)    TO RAD7-ITEMS                       
046200         MOVE RAD-WEIGHT (RADINDX)    TO RAD7-WEIGHT                      
046300         MOVE RAD-7                   TO WS-LIST-RAD                      
046400         PERFORM S03-SKRIV                                                
046500       END-IF                                                             
046600     END-PERFORM                                                          
046700                                                                          
046800     MOVE RAD-8                       TO WS-LIST-RAD                      
046900     MOVE RADSKIP-COUNTER             TO PRT-RADSKIP                      
047000     PERFORM S03-SKRIV                                                    
047100                                                                          
047200     MOVE SPACE                       TO WS-LIST-RAD                      
047300     MOVE PRT-AFTER-2                 TO PRT-RADSKIP                      
047400     PERFORM S03-SKRIV                                                    
047410                                                                          
047500     MOVE       RAD-9                 TO WS-LIST-RAD                      
047600     MOVE PRT-AFTER-9                 TO PRT-RADSKIP                      
047700     PERFORM S03-SKRIV                                                    
047800     .                                                                    
047900     EJECT                                                                
048000 Z-STAENG-PRINTER SECTION.                                                
048100                                                                          
048200     CALL W006PRS1  USING PRT-FORMS-OVR                                   
048300                          PRT-CLOSE                                       
048400                          WS-PRT-IDPRTLST                                 
048500                          ALT-PCB                                         
048600                          WS-PRT-DUMMY                                    
048700                          WS-PRT-DUMMY                                    
048800                                                                          
048900     .                                                                    
049000     EJECT                                                                
049100 S03-SKRIV SECTION.                                                       
049200                                                                          
049300     CALL W006PRS1 USING PRT-FORMS-OVR                                    
049400                         PRT-WRITE                                        
049500                         WS-PRT-IDPRTLST                                  
049600                         ALT-PCB                                          
049700                         PRT-RADSKIP                                      
049800                         WS-LIST-RAD                                      
049900     .                                                                    
050000     EJECT                                                                
050100                                                                          
050200* --- IMS SEKTIONER ---                                                   
050300     SKIP3                                                                
050400 IMS-GET-MSG SECTION.                                                     
050500                                                                          
050600     MOVE '  QC' TO GODK-STATUSKODER                                      
050700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
050800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050900     PERFORM IMS-STATUSKONTROLL                                           
051000     .                                                                    
051100     SKIP3                                                                
051200 IMS-GU-WL446321-TRANSP SECTION.                                          
051300                                                                          
051400     STRING 'WL446301(WDGXKEY  =' W-WDGXKEY-X ')'                         
051500          DELIMITED BY SIZE INTO SSA1                                     
051600     STRING 'WL446311(DASKEPPN =' W-DASKEPPNING-X ')'                     
051700          DELIMITED BY SIZE INTO SSA2                                     
051800     STRING 'WL446321(KY4466   =' W-KY4466-X ')'                          
051900          DELIMITED BY SIZE INTO SSA3                                     
052000     MOVE '  GE' TO GODK-STATUSKODER                                      
052100     CALL CBLTDLI USING GU 4463-PCB DLI-IO-AREA-4466                      
052200                        SSA1 SSA2 SSA3                                    
052300     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
052400     PERFORM IMS-STATUSKONTROLL                                           
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-GNP-WL446331-KOLLI SECTION.                                          
052800                                                                          
052900     MOVE 'WL446331 ' TO SSA1                                             
053000     MOVE '  GE' TO GODK-STATUSKODER                                      
053100     CALL CBLTDLI USING GNP 4463-PCB DLI-IO-AREA-4468 SSA1                
053200     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
053300     PERFORM IMS-STATUSKONTROLL                                           
053400     .                                                                    
053500     SKIP3                                                                
053600                                                                          
053700 IMS-GU-ORQI-WDQ201 SECTION.                                              
053800                                                                          
053900     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
054000          DELIMITED BY SIZE INTO SSA1                                     
054100     MOVE '  GE'               TO GODK-STATUSKODER                        
054200     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-OHUV SSA1                 
054300     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     .                                                                    
054600     EJECT                                                                
054700                                                                          
054800 IMS-STATUSKONTROLL SECTION.                                              
054900                                                                          
055000     SET STATUS-IX TO 1                                                   
055100     SEARCH GODK-STATUS                                                   
055200       AT END                                                             
055300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055400         DELIMITED BY SIZE INTO FELTEXT                                   
055500         CALL FELLOG                                                      
055600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055700         CONTINUE                                                         
055800     END-SEARCH                                                           
055900     .                                                                    
