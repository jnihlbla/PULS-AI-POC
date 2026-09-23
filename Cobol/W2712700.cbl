000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2712700.                                                
000400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500*DATE-WRITTEN.   DEC 2002.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        HÄMTAR KLASS MHA W271REFL                                        
001100*                                                                         
001200*        SKAPAR EN FIL FÖR SOM LIGGER TILL GRUND                          
001300*        FÖR BESTÄLLNINGAR FRÅN 2348                                      
001400*                                                                         
001500*                                                                         
001600*    ABENDCODES:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          ---                                                            
002900     SELECT W27126                     ASSIGN TO W27127D1.                
003000     SKIP2                                                                
003100*          --- FIL MED SAMTLIGA ARTIKLAR FÖR UPPFÖLJNING                  
003200     SELECT W27127                     ASSIGN TO W27127D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W27126                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W27126      -L.                                                
004300     SKIP3                                                                
004400 FD  W27127                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W27126 -PRE UT-     -L.                                   
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100     SKIP2                                                                
005200*    -COPY WY2000W1                                                       
005300     SKIP3                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700     EJECT                                                                
005800                                                                          
005900 77  IDPGM                       PIC X(8)    VALUE 'W2712700'.            
006000 77  YES                         PIC X       VALUE 'Y'.                   
006100 77  NOO                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 01  ARBETSFALT.                                                          
006400     03 IX                       PIC 9(2)    VALUE ZERO.                  
006500     03 MAX-IX                   PIC 9(2)    VALUE 45.                    
006600     03 WS-PRIS                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006700     03 WS-DAPUBL                PIC 9(5)    VALUE ZERO.                  
006800                                                                          
006900 01  ERRTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200                                                                          
007300 77  W27126-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W27126                       VALUE 'Y'.                   
007500 01  TABELL-BEMODELL.                                                     
007600     03  W-BEMODELL-RAD          OCCURS 45.                               
007700         05 W-BEMODELL           PIC X(15) VALUE SPACE.                   
007800 01  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
007900     EJECT                                                                
008000*                                                                         
008100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008200 01  FILLER REDEFINES DAGENS-DATUM.                                       
008300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008600     EJECT                                                                
008700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008800*      --- VALID IDDC CODES                                               
008900*                                                                         
009000*01    -COPY WWDC99                                                       
009100       EJECT                                                              
009200*01    -COPY WWLNDKON                                                     
009300       EJECT                                                              
009400*01    -COPY WWDCKONS                                                     
009500       EJECT                                                              
009600 01  GENERAL-SUBPROGRAM.                                                  
009700*                                                                         
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010200     03  W271REFL                PIC X(8)    VALUE 'W271REFL'.            
010300     03  W271UTUP                PIC X(8)    VALUE 'W271UTUP'.            
010400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010500     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL POSTSUM                                          
010800*                                                                         
010900*01  -COPY W0005   -PRE  POSTSUM-                                         
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL DATKORT                                          
011200*                                                                         
011300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27113'.              
011400     SKIP2                                                                
011500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
011600     SKIP2                                                                
011700*01  -COPY WDATKORT                                                       
011800     EJECT                                                                
011900*01 -COPY WDATAREA                                                        
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL W271REFL                                         
012200*                                                                         
012300 01  FILLER                      PIC X(8)    VALUE 'W271REFL'.            
012400*01  -COPY W271REFL                                                       
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL W271UTUP                                         
012700*                                                                         
012800 01  FILLER                      PIC X(8)    VALUE 'W271UTUP'.            
012900*01  -COPY W271UTUP                                                       
013000     EJECT                                                                
013100 01  IN-AREA-START               PIC X(24)   VALUE                        
013200                                             'IN-AREA-START'.             
013300     SKIP2                                                                
013400                                                                          
013500*01  AREA -COPY W27126     -PRE IN-                                       
013600*                                                                         
013700     EJECT                                                                
013800 01  UT-AREA-START               PIC X(24)   VALUE                        
013900                                             'UT-AREA-START'.             
014000     SKIP2                                                                
014100                                                                          
014200*01  AREA -COPY W27126     -PRE UT-                                       
014300*                                                                         
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014600     SKIP3                                                                
014700 01  KEYS-TILL-DLI.                                                       
014800     03  W-IDARTNR-X.                                                     
014900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015000     03  W-IDLAND-X.                                                      
015100         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
015200     03  W-IDDC-X.                                                        
015300         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
015400     03  W-IDLEVNR-X.                                                     
015500         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
015600     03  W-KDSEGKEY-X.                                                    
015700         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
015800     03  W-WDN611KY-MIN-X.                                                
015900         05  W-IDFORDON-MIN        PIC S9(3) COMP-3 VALUE ZERO.           
016000         05  W-TIOMBRYT-9KOMPL-MIN PIC S9(7) COMP-3 VALUE ZERO.           
016100     03  W-IDMODELL-MIN-X.                                                
016200         05  W-IDMODELL-MIN      PIC X(3)    VALUE LOW-VALUE.             
016300     03  W-WDN611KY-MAX-X.                                                
016400         05  W-IDFORDON-MAX        PIC S9(3)                              
016500                                       VALUE +999 COMP-3.                 
016600         05  W-TIOMBRYT-9KOMPL-MAX PIC S9(7)                              
016700                                       VALUE +9999999 COMP-3.             
016800     03  W-IDMODELL-MAX-X.                                                
016900         05  W-IDMODELL-MAX      PIC X(3)    VALUE HIGH-VALUE.            
017000     SKIP2                                                                
017100*    --- STATUS-KOD FRÅN IMS                                              
017200 01  STATUS-WS                   PIC XX.                                  
017300     88  SEGMENT-FOUND                       VALUE '  '.                  
017400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017600     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
017700     88  IMS-NOT-OK                          VALUE 'XD'.                  
017800     SKIP2                                                                
017900 01  GOOD-STATUSCODES.                                                    
018000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100     SKIP3                                                                
018200 01  SSA1                        PIC X(64).                               
018300 01  SSA2                        PIC X(64).                               
018400 01  SSA3                        PIC X(64).                               
018500     EJECT                                                                
018600*    --- IMS FUNCTION CODES                                               
018700*01  -COPY W0003                                                          
018800     EJECT                                                                
018900*    ---  DLI INPUT-OUTPUT AREA                                           
019000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDF116'.                      
019100 01  DLI-IO-WDF116.                                                       
019200*    03  -COPY WDF116                                                     
019300     EJECT                                                                
019400                                                                          
019500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDN601'.                      
019600 01  DLI-IO-WDN601.                                                       
019700*    03  -COPY WDN601                                                     
019800     EJECT                                                                
019900                                                                          
020000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDN611'.                      
020100 01  DLI-IO-WDN611.                                                       
020200*    03  -COPY WDN611                                                     
020300     EJECT                                                                
020400                                                                          
020500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDN621'.                      
020600 01  DLI-IO-WDN621.                                                       
020700*    03  -COPY WDN621                                                     
020800     EJECT                                                                
020900                                                                          
021000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
021100 01  DLI-IO-WDK712.                                                       
021200*    03  -COPY WDK712                                                     
021300     EJECT                                                                
021400                                                                          
021500 LINKAGE SECTION.                                                         
021600                                                                          
021700*01  -COPY W0008  -PRE WDF1-                                              
021800     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000*01  -COPY W0008  -PRE REFL-2501-                                         
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008  -PRE WDB6-                                              
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008  -PRE WDN6-                                              
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE WDK7-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE REFL-WDK7-                                         
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500 01  REFL-UTIL-WDK6-PCB          PIC X.                                   
023600 01  REFL-UTIL-WDK7-PCB          PIC X.                                   
023700 01  REFL-UTIL-WDB6-PCB          PIC X.                                   
023800     EJECT                                                                
023900*****W271UTUP**********                                                   
024000 01  UTUP1-WDK7-PCB              PIC X.                                   
024100 01  UTUP1-WDB6-PCB              PIC X.                                   
024200 01  UTUP1-UTIL-WDK6-PCB         PIC X.                                   
024300 01  UTUP1-UTIL-WDK7-PCB         PIC X.                                   
024400 01  UTUP1-UTIL-WDB6-PCB         PIC X.                                   
024500     EJECT                                                                
024600 PROCEDURE DIVISION  USING WDF1-PCB REFL-2501-PCB WDB6-PCB                
024700                           WDN6-PCB WDK7-PCB REFL-WDK7-PCB                
024800                           REFL-UTIL-WDK6-PCB                             
024900                           REFL-UTIL-WDK7-PCB                             
025000                           REFL-UTIL-WDB6-PCB                             
025100                           UTUP1-WDK7-PCB                                 
025200                           UTUP1-WDB6-PCB                                 
025300                           UTUP1-UTIL-WDK6-PCB                            
025400                           UTUP1-UTIL-WDK7-PCB                            
025500                           UTUP1-UTIL-WDB6-PCB.                           
025600                                                                          
025700     ENTRY 'DLITCBL' USING WDF1-PCB REFL-2501-PCB WDB6-PCB                
025800                           WDN6-PCB WDK7-PCB REFL-WDK7-PCB                
025900                           REFL-UTIL-WDK6-PCB                             
026000                           REFL-UTIL-WDK7-PCB                             
026100                           REFL-UTIL-WDB6-PCB                             
026200                           UTUP1-WDK7-PCB                                 
026300                           UTUP1-WDB6-PCB                                 
026400                           UTUP1-UTIL-WDK6-PCB                            
026500                           UTUP1-UTIL-WDK7-PCB                            
026600                           UTUP1-UTIL-WDB6-PCB.                           
026700                                                                          
026800     PERFORM A-INIT                                                       
026900     PERFORM S01-READ-W27126                                              
027000                                                                          
027100     PERFORM UNTIL END-OF-W27126                                          
027200                                                                          
027300       MOVE IN-IDDC      TO WS-IDDC                                       
027400       IF (NDC-CN OR NDC-US)  AND                                         
027500          IN-IDDC-REF = SPACES                                            
027600         CONTINUE                                                         
027700       ELSE                                                               
027800         MOVE SPACE        TO REFL-W271REFL                               
027900         MOVE ZERO         TO REFL-NDC-KVDAGAR-TBT-DC                     
028000                              REFL-KVREFPKT                               
028100                              REFL-KVREFBER                               
028200                              REFL-KVREFOVL                               
028300                                                                          
028400         MOVE IN-IDDC      TO REFL-IDDC                                   
028500                              W-IDDC                                      
028600         MOVE IN-IDARTNR   TO REFL-IDARTNR                                
028700                              W-IDARTNR                                   
028800         MOVE IN-IDDC-REF  TO REFL-IDDC-REF                               
028900         MOVE IN-IDREFTAB  TO REFL-IDREFTAB                               
029000         MOVE IN-FLWILSON  TO REFL-FLWILSON                               
029100         MOVE IN-IDLEVNR-DC                                               
029200                           TO REFL-IN-IDLEVNR-DC                          
029300         MOVE IN-FLFLYG    TO REFL-FLFLYG                                 
029400                                                                          
029500         IF NDC-CN OR NDC-NA                                              
029600           IF NDC-CN                                                      
029700             MOVE WC-LAND-CN TO W-IDLAND                                  
029800           END-IF                                                         
029900           IF NDC-US                                                      
030000             MOVE WC-LAND-US TO W-IDLAND                                  
030100           END-IF                                                         
030200           IF NDC-CA                                                      
030300             MOVE WC-LAND-CA TO W-IDLAND                                  
030400           END-IF                                                         
030500           PERFORM IMS-GU-WDK712                                          
030600           IF SEGMENT-FOUND                                               
030700             MOVE LART-PRMATRL TO REFL-PRARTBES                           
030800                                  WS-PRIS                                 
030900             IF LART-DAPUBL > ZERO                                        
031000               MOVE 'AAMMDD'           TO DAT-KDDATFORM                   
031100               MOVE LART-DAPUBL        TO DAT-I-TIDATUM                   
031200               CALL WDATKONV USING DAT-KDDATFORM                          
031300                                   DAT-I-TIDATUM                          
031400                                   DAT-O-TIDATUM                          
031500                                   DAT-KDSVAR                             
031600               IF DAT-KDSVAR-OK                                           
031700                  MOVE DAT-TIAAVVD       TO WS-DAPUBL                     
031800               ELSE                                                       
031900                  MOVE 'FEL FRÅN WDATKONV' TO                             
032000                            FELTEXT-STR                                   
032100                  DISPLAY FELTEXT                                         
032200                  PERFORM S99-ABEND                                       
032300               END-IF                                                     
032400             END-IF                                                       
032500           ELSE                                                           
032600             MOVE ZERO    TO REFL-PRARTBES                                
032700                             WS-PRIS                                      
032800                             WS-DAPUBL                                    
032900           END-IF                                                         
033000         ELSE                                                             
033100           MOVE IN-PRARTBES  TO REFL-PRARTBES                             
033200         END-IF                                                           
033300         MOVE 1 TO IX                                                     
033400         PERFORM UNTIL IX > 12                                            
033500            MOVE IN-RESEASON(IX)                                          
033600                             TO REFL-RESEASON(IX)                         
033700            ADD 1 TO IX                                                   
033800         END-PERFORM                                                      
033900                                                                          
034000         IF NDC                                                           
034100*           IF IN-IDDC-REF = WC-CDC-SE OR WC-NDC-CN-71 OR                 
034200*                            WC-NDC-CN-72 OR WC-NDC-CN-73                 
034300*              CONTINUE                                                   
034400*           ELSE                                                          
034500            IF IN-IDDC-REF = SPACE                                        
034600                                                                          
034700               IF IN-KVDAGAR-MANLT > ZERO                                 
034800                 MOVE IN-KVDAGAR-MANLT                                    
034900                             TO REFL-NDC-KVDAGAR-TBT-DC                   
035000               ELSE                                                       
035100                 MOVE IN-IDLEVNR-DC                                       
035200                             TO W-IDLEVNR                                 
035300                 MOVE IN-IDDC                                             
035400                             TO W-IDDC                                    
035500                 PERFORM IMS-GU-WDF116                                    
035600                 IF SEGMENT-FOUND                                         
035700                    MOVE NDC-KVDAGAR-TBT TO                               
035800                              REFL-NDC-KVDAGAR-TBT-DC                     
035900                 ELSE                                                     
036000                    MOVE 1 TO REFL-NDC-KVDAGAR-TBT-DC                     
036100                 END-IF                                                   
036200               END-IF                                                     
036300            END-IF                                                        
036400         END-IF                                                           
036500         MOVE IN-TIREFPAF  TO TMP1-YYMMDD                                 
036600         MOVE DAGENS-DATUM TO TMP2-YYMMDD                                 
036700         PERFORM WY2000P1                                                 
036800         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
036900*                                                                         
037000*-----     MANUELL PÅFYLLNADSKVANT ÄR SATT                                
037100*                                                                         
037200           MOVE IN-KVREFBER TO REFL-IN-KVREFBER                           
037300         ELSE                                                             
037400           MOVE ZERO        TO REFL-IN-KVREFBER                           
037500         END-IF                                                           
037600                                                                          
037700         MOVE IN-TIREFPKT  TO TMP1-YYMMDD                                 
037800         MOVE DAGENS-DATUM TO TMP2-YYMMDD                                 
037900         PERFORM WY2000P1                                                 
038000         IF TMP1-YYMMDD >= TMP2-YYMMDD                                    
038100*                                                                         
038200*-----     MANUELL PÅFYLLNADSPUNKT ÄR SATT                                
038300*                                                                         
038400           MOVE IN-KVREFPKT TO REFL-IN-KVREFPKT                           
038500         ELSE                                                             
038600           MOVE ZERO        TO REFL-IN-KVREFPKT                           
038700         END-IF                                                           
038800                                                                          
038900***      CALL W271UTUP TO GET LEADTIME ADJUSTED DEMAND                    
039000*                                                                         
039100         INITIALIZE UTUP-W271UTUP                                         
039200         MOVE 004               TO UTUP-KDCALL                            
039300         MOVE W-IDARTNR         TO UTUP-IDARTNR                           
039400         MOVE W-IDDC            TO UTUP-IDDC                              
039500         MOVE IN-IDDC-REF       TO UTUP-IDDC-REF                          
039600         MOVE REFL-NDC-KVDAGAR-TBT-DC                                     
039700                                TO UTUP-LEADTIME                          
039800                                                                          
039900         CALL W271UTUP USING UTUP-W271UTUP                                
040000                             UTUP1-WDK7-PCB                               
040100                             UTUP1-WDB6-PCB                               
040200                             UTUP1-UTIL-WDK6-PCB                          
040300                             UTUP1-UTIL-WDK7-PCB                          
040400                             UTUP1-UTIL-WDB6-PCB                          
040500         IF UTUP-KDSVAR-OK                                                
040600            MOVE UTUP-LEADTID-BEHOV  TO REFL-IN-LEADTID-BEHOV             
040700         ELSE                                                             
040800            DISPLAY 'W271UTUP-ERROR :' UTUP-TEXT                          
040900            CALL FELLOG                                                   
041000         END-IF                                                           
041100                                                                          
041200         CALL W271REFL USING REFL-W271REFL REFL-2501-PCB                  
041300                             WDB6-PCB REFL-WDK7-PCB                       
041400                             REFL-UTIL-WDK6-PCB                           
041500                             REFL-UTIL-WDK7-PCB                           
041600                             REFL-UTIL-WDB6-PCB                           
041700                                                                          
041800         PERFORM C-BEHANDLA-UTPOST                                        
041900       END-IF                                                             
042000       PERFORM S01-READ-W27126                                            
042100                                                                          
042200     END-PERFORM                                                          
042300                                                                          
042400     PERFORM Z-FINIT                                                      
042500                                                                          
042600     MOVE ZERO TO RETURN-CODE                                             
042700     GOBACK                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 A-INIT SECTION.                                                          
043100                                                                          
043200     OPEN INPUT W27126                                                    
043300                                                                          
043400     OPEN OUTPUT W27127                                                   
043500                                                                          
043600     ACCEPT DAGENS-DATUM FROM DATE                                        
043700     .                                                                    
043800     EJECT                                                                
043900 C-BEHANDLA-UTPOST SECTION.                                               
044000                                                                          
044100     MOVE IN-AREA     TO UT-AREA                                          
044200     MOVE REFL-KLASS  TO UT-KLASS                                         
044300     MOVE IN-IDARTNR    TO W-IDARTNR                                      
044400     IF NDC-CN OR NDC-NA                                                  
044500       MOVE WS-PRIS   TO UT-PRARTBES                                      
044600                         UT-PRARTSTD                                      
044700       IF WS-DAPUBL > ZERO                                                
044800         MOVE WS-DAPUBL   TO UT-TIFINLV                                   
044900       END-IF                                                             
045000     END-IF                                                               
045100     COMPUTE UT-KVPB-REF = IN-KVPB-REF + IN-KVPBREOI                      
045200     IF IN-IDARTNR = WS-IDARTNR                                           
045300       MOVE +1  TO IX                                                     
045400       PERFORM UNTIL IX > MAX-IX                                          
045500         MOVE W-BEMODELL(IX)   TO UT-BEMODELL(IX)                         
045600         ADD +1 TO IX                                                     
045700       END-PERFORM                                                        
045800     ELSE                                                                 
045900       MOVE +1  TO IX                                                     
046000       PERFORM UNTIL IX > MAX-IX                                          
046100         MOVE SPACE  TO UT-BEMODELL(IX)                                   
046200                        W-BEMODELL(IX)                                    
046300         ADD +1 TO IX                                                     
046400       END-PERFORM                                                        
046500       PERFORM IMS-GU-WDN601                                              
046600       IF SEGMENT-FOUND                                                   
046700         PERFORM IMS-GNP-WDN621                                           
046800         MOVE +1 TO IX                                                    
046900         PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                  
047000         OR IX > MAX-IX                                                   
047100           MOVE MDL-BEMODELL TO UT-BEMODELL(IX)                           
047200                                  W-BEMODELL(IX)                          
047300           PERFORM IMS-GNP-WDN621                                         
047400           ADD +1 TO IX                                                   
047500         END-PERFORM                                                      
047600       END-IF                                                             
047700     END-IF                                                               
047800     MOVE IN-IDARTNR    TO WS-IDARTNR                                     
047900                                                                          
048000     PERFORM S11-SKRIV-W27127                                             
048100     .                                                                    
048200     EJECT                                                                
048300 Z-FINIT SECTION.                                                         
048400                                                                          
048500     CLOSE W27126                                                         
048600           W27127                                                         
048700     SKIP2                                                                
048800     MOVE 'S' TO POSTSUM-OPKOD                                            
048900     CALL POSTSUM USING POSTSUM-PARM                                      
049000     .                                                                    
049100     EJECT                                                                
049200 S01-READ-W27126  SECTION.                                                
049300     SKIP2                                                                
049400     READ W27126 INTO IN-AREA                                             
049500     AT END                                                               
049600        SET END-OF-W27126 TO TRUE                                         
049700                                                                          
049800     NOT AT END                                                           
049900        MOVE 'W27126' TO POSTSUM-FDNAMN                                   
050000        MOVE 'W27127D1' TO POSTSUM-DDNAMN2                                
050100        CALL POSTSUM USING POSTSUM-PARM                                   
050200     END-READ                                                             
050300     .                                                                    
050400     EJECT                                                                
050500 S11-SKRIV-W27127 SECTION.                                                
050600     SKIP2                                                                
050700     WRITE UT-POST FROM UT-AREA                                           
050800                                                                          
050900     MOVE UT-IDARTNR TO POSTSUM-TRANSTYP                                  
051000     MOVE 'W27127 '  TO POSTSUM-FDNAMN                                    
051100     MOVE 'W27127D2' TO POSTSUM-DDNAMN2                                   
051200     CALL POSTSUM    USING POSTSUM-PARM                                   
051300     .                                                                    
051400     EJECT                                                                
051500* --- IMS SECTIONS  ---                                                   
051600     EJECT                                                                
051700 IMS-GU-WDF116 SECTION.                                                   
051800                                                                          
051900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
052000          DELIMITED BY SIZE INTO SSA1                                     
052100     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
052200          DELIMITED BY SIZE INTO SSA2                                     
052300     MOVE '  GE' TO GOOD-STATUSCODES                                      
052400     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
052500     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
052600     PERFORM IMS-STATUSCHECK                                              
052700     .                                                                    
052800     EJECT                                                                
052900 IMS-GU-WDN601 SECTION.                                                   
053000                                                                          
053100     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
053200          DELIMITED BY SIZE INTO SSA1                                     
053300     MOVE '  GE' TO GOOD-STATUSCODES                                      
053400     CALL CBLTDLI USING GU WDN6-PCB DLI-IO-WDN601 SSA1                    
053500     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
053600     PERFORM IMS-STATUSCHECK                                              
053700     .                                                                    
053800     EJECT                                                                
053900 IMS-GNP-WDN621 SECTION.                                                  
054000                                                                          
054100*    STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
054200*         DELIMITED BY SIZE INTO SSA1                                     
054300     STRING 'WDN611  (WDN611KY=>' W-WDN611KY-MIN-X                        
054400                    '&WDN611KY=<' W-WDN611KY-MAX-X ')'                    
054500          DELIMITED BY SIZE INTO SSA1                                     
054600     STRING 'WDN621  (IDMODELL>=' W-IDMODELL-MIN-X                        
054700                    '&IDMODELL<=' W-IDMODELL-MAX-X ')'                    
054800          DELIMITED BY SIZE INTO SSA2                                     
054900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
055000     CALL CBLTDLI USING GNP WDN6-PCB DLI-IO-WDN621 SSA1 SSA2              
055100     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
055200     PERFORM IMS-STATUSCHECK                                              
055300     .                                                                    
055400     EJECT                                                                
055500 IMS-GU-WDK712   SECTION.                                                 
055600                                                                          
055700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
055800          DELIMITED BY SIZE INTO SSA1                                     
055900     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
056000          DELIMITED BY SIZE INTO SSA2                                     
056100     MOVE '  GE' TO GOOD-STATUSCODES                                      
056200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712                         
056300          SSA1 SSA2                                                       
056400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
056500     PERFORM IMS-STATUSCHECK                                              
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 IMS-STATUSCHECK SECTION.                                                 
057000     SKIP2                                                                
057100     SET STATUS-IX TO 1                                                   
057200     SEARCH GOOD-STATUS                                                   
057300       AT END                                                             
057400         MOVE 'WRONG CODE' TO ERRTEXT-STR                                 
057500         DISPLAY ERRTEXT                                                  
057600         CALL FELLOG                                                      
057700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
057800         CONTINUE                                                         
057900     END-SEARCH                                                           
058000     .                                                                    
058100     EJECT                                                                
058200 S99-ABEND SECTION.                                                       
058300                                                                          
058400     SKIP2                                                                
058500     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
058600     .                                                                    
058700     EJECT                                                                
058800*    -COPY WY2000P1                                                       
