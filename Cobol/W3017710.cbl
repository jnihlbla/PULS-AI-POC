000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3017710.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   01/05/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISA HISTORIK FÖR BYTES RETURER.                                 
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDM6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W3T177                                              
001400*        MID:         W3I177n1                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W3O177n1                                            
001800*                                                                         
001900*    CHANGE LOG:                                                          
002000*                                                                         
002100*    DIGAMBAR/20021003                                                    
002200*    WDM6E INDEX IS CHANGED TO REFER THE IDBYTRAP-9KOMPL INSTEAD          
002300*    OF THE IDBYTRAP. THIS IS TO SHOW THE DETAILS IN DESCENDING           
002400*    ORDER OF THE IDBYTRAP.IDBYTRAP-9KOMPL FIELD IS ADDED IN              
002500*    WDM611                                                               
002600                                                                          
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W3017710'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 01  ALL-SPACE.                                                           
004000     03 FILLER                   PIC X(50)   VALUE SPACE.                 
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  W-9KOMPL                    PIC 9(07)  VALUE 9999999.                
004800 77  W-MSGI-IDBYTRAP             PIC 9(07)  VALUE ZERO.                   
004900 77  W-IDBYTRAP-MIN              PIC S9(07) VALUE ZERO.                   
005000 77  W-IDBYTRAP-MAX              PIC S9(07) VALUE 9999999.                
005100 77  WS-IDARTNR                  PIC S9(9)        COMP-3.                 
005200 77  WS-IDDISTR                  PIC S9(5)        COMP-3.                 
005300 77  WS-IDKUNDNR                 PIC S9(7)        COMP-3.                 
005400 77  WS-IDBYTRAP-9KOMPL          PIC S9(7)        COMP-3.                 
005500 77  WS-KDBYTSTA                 PIC X.                                   
005600 77  WS-IDDC                     PIC X(2).                                
005700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005800                                                                          
005900                                                                          
006000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006100     88  NYCKLAR-OK                          VALUE 'J'.                   
006200     88  NYCKLAR-FEL                         VALUE 'N'.                   
006300                                                                          
006400 77  VALID-INTERVAL              PIC X       VALUE 'N'.                   
006500     88  VAL-INT                             VALUE 'J'.                   
006600     88  FEL-INT                             VALUE 'N'.                   
006700                                                                          
006800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006900 01  GENERELLA-SUBPROGRAM.                                                
007000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
007700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008300     SKIP3                                                                
008400*01  -COPY WMFSAREA                                                       
008500     EJECT                                                                
008600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
009200     03  W-WDM6E1KY-MIN.                                                  
009300         05  W-IDARTNR-MIN   PIC S9(9)   COMP-3.                          
009400         05  W-IDDISTR-MIN   PIC S9(5)   COMP-3 VALUE ZERO.               
009500         05  W-IDBYTRAP-9KOMPL-MIN                                        
009600                             PIC S9(7)   COMP-3 VALUE ZERO.               
009700         05  W-IDBYTRAD-MIN  PIC S9(5)   COMP-3 VALUE ZERO.               
009800     03  W-WDM6E1KY-MAX.                                                  
009900         05  W-IDARTNR-MAX   PIC S9(9)   COMP-3.                          
010000         05  W-IDDISTR-MAX   PIC S9(5)   COMP-3 VALUE 99999.              
010100         05  W-IDBYTRAP-9KOMPL-MAX                                        
010200                             PIC S9(7)   COMP-3 VALUE 9999999.            
010300         05  W-IDBYTRAD-MAX  PIC S9(5)   COMP-3 VALUE 99999.              
010400                                                                          
010500     03  W-WDM601KY.                                                      
010600         05  W-IDDISTR-601   PIC S9(5)   COMP-3.                          
010700         05  W-IDBYTRAP-601  PIC S9(7)   COMP-3.                          
010800     03  W-IDBYTRAD.                                                      
010900         05  W-IDBYTRAD-611  PIC S9(5)        COMP-3.                     
011000     03  W-INTERNA-NYCKLAR.                                               
011100         05  W-IDDC-MIN      PIC XX   VALUE '00'.                         
011200         05  W-IDDC-MAX      PIC XX   VALUE '99'.                         
011300         05  W-KDBYTSTA-MIN  PIC X    VALUE '0'.                          
011400         05  W-KDBYTSTA-MAX  PIC X    VALUE '9'.                          
011500         05  W-IDKUNDNR-MIN  PIC S9(7)   COMP-3 VALUE ZERO.               
011600         05  W-IDKUNDNR-MAX  PIC S9(7)   COMP-3 VALUE 9999999.            
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400                                                                          
013500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM6E1'.                      
013600 01  DLI-IO-WDM6E1.                                                       
013700*    03  -COPY WDM6E1                                                     
013800     EJECT                                                                
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM601'.                      
014000 01  DLI-IO-WDM601.                                                       
014100*    03  -COPY WDM601                                                     
014200     EJECT                                                                
014300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM611'.                      
014400 01  DLI-IO-WDM611.                                                       
014500*    03  -COPY WDM611                                                     
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800                                                                          
014900 01  REQU-AREA.                                                           
015000*    03 -COPY WZ01REQU                                                    
015100*    03 -COPY W30177I1                                                    
015200     EJECT                                                                
015300 01  RESP-AREA.                                                           
015400*    03 -COPY WZ01RESP                                                    
015500*    03 -COPY W30177O1                                                    
015600     EJECT                                                                
015700 01  MAX-KVRADER                 PIC S9(4) COMP.                          
015800                                                                          
015900*01  -COPY W0008  -PRE WDM6E-                                             
016000     05  FILLER                  PIC X.                                   
016100*01  -COPY W0008  -PRE WDM6-                                              
016200     05  FILLER                  PIC X.                                   
016300     EJECT                                                                
016400 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
016500                                          WDM6E-PCB WDM6-PCB.             
016600 MAIN SECTION.                                                            
016700     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER                
016800                                          WDM6E-PCB WDM6-PCB.             
016900     PERFORM A-INIT                                                       
017000     PERFORM B-KOLLA-NYCKLAR                                              
017100     IF NYCKLAR-OK                                                        
017200       IF REQU-FIRST                                                      
017300         PERFORM C-FOERSTA-SIDA                                           
017400       ELSE                                                               
017500         IF REQU-NEXT                                                     
017600           PERFORM D-NAESTA-SIDA                                          
017700         ELSE                                                             
017800           PERFORM E-SAMMA-SIDA                                           
017900         END-IF                                                           
018000       END-IF                                                             
018100       PERFORM F-LAES-VISA-INFO                                           
018200     END-IF                                                               
018300                                                                          
018400     MOVE ZERO                   TO RETURN-CODE                           
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900                                                                          
019000     MOVE ALL '+'                TO RESP-W30177O1                         
019100                                                                          
019200     MOVE 001                    TO RESP-IDMSGVER                         
019300     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
019400                                    RESP-IDMSG-INFO                       
019500                                    RESP-IDELMT-ERROR                     
019600                                                                          
019700     MOVE REQU-KVRADER           TO RESP-KVRADER                          
019800     .                                                                    
019900     EJECT                                                                
020000 B-KOLLA-NYCKLAR SECTION.                                                 
020100                                                                          
020200     MOVE JA                     TO NYCKLAR-SW                            
020300                                                                          
020400     IF REQU-IDARTNR-KEY NUMERIC                                          
020500       IF REQU-IDARTNR-KEY > '000000000'                                  
020600         MOVE REQU-IDARTNR-KEY   TO WS-IDARTNR                            
020700       ELSE                                                               
020800         MOVE NEJ                TO NYCKLAR-SW                            
020900         MOVE 'IDARTNR-OBJ'      TO RESP-IDELMT-ERROR                     
021000       END-IF                                                             
021100     ELSE                                                                 
021200       MOVE NEJ                  TO NYCKLAR-SW                            
021300       MOVE 'IDARTNR-OBJ'        TO RESP-IDELMT-ERROR                     
021400     END-IF                                                               
021500                                                                          
021600     IF REQU-IDDISTR-KEY NUMERIC                                          
021700       IF REQU-IDDISTR-KEY > '0000'                                       
021800         MOVE REQU-IDDISTR-KEY   TO WS-IDDISTR                            
021900       ELSE                                                               
022000         MOVE ZERO               TO WS-IDDISTR                            
022100       END-IF                                                             
022200     ELSE                                                                 
022300       MOVE ZERO                 TO WS-IDDISTR                            
022400     END-IF                                                               
022500                                                                          
022600     IF REQU-IDKUNDNR-KEY NUMERIC                                         
022700       IF REQU-IDKUNDNR-KEY > '000000'                                    
022800         MOVE REQU-IDKUNDNR-KEY  TO WS-IDKUNDNR                           
022900       ELSE                                                               
023000         MOVE ZERO               TO WS-IDKUNDNR                           
023100       END-IF                                                             
023200     ELSE                                                                 
023300       MOVE ZERO                 TO WS-IDKUNDNR                           
023400     END-IF                                                               
023500                                                                          
023600     IF REQU-IDBYTRAP-KEY NUMERIC                                         
023700       IF REQU-IDBYTRAP-KEY > '0000000'                                   
023800         MOVE REQU-IDBYTRAP-KEY  TO W-MSGI-IDBYTRAP                       
023900         COMPUTE WS-IDBYTRAP-9KOMPL = W-9KOMPL - W-MSGI-IDBYTRAP          
024000       ELSE                                                               
024100         MOVE ZERO               TO WS-IDBYTRAP-9KOMPL                    
024200       END-IF                                                             
024300     ELSE                                                                 
024400       MOVE ZERO                 TO WS-IDBYTRAP-9KOMPL                    
024500     END-IF                                                               
024600                                                                          
024700     IF REQU-KDBYTSTA-KEY NUMERIC                                         
024800       IF REQU-KDBYTSTA-KEY > '0'                                         
024900          MOVE REQU-KDBYTSTA-KEY TO WS-KDBYTSTA                           
025000       ELSE                                                               
025100          MOVE SPACE             TO WS-KDBYTSTA                           
025200       END-IF                                                             
025300     ELSE                                                                 
025400       MOVE SPACE                TO WS-KDBYTSTA                           
025500     END-IF                                                               
025600                                                                          
025700     IF REQU-IDDC-KEY NUMERIC                                             
025800       IF REQU-IDDC-KEY > '00'                                            
025900         MOVE REQU-IDDC-KEY      TO WS-IDDC                               
026000       ELSE                                                               
026100         MOVE SPACE              TO WS-IDDC                               
026200       END-IF                                                             
026300     ELSE                                                                 
026400       MOVE SPACE                TO WS-IDDC                               
026500     END-IF                                                               
026600                                                                          
026700     IF NYCKLAR-FEL                                                       
026800       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
026900       MOVE ZERO                 TO RESP-KVRADER                          
027000       PERFORM MFS-RENSA-FAELT-UT                                         
027100     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 C-FOERSTA-SIDA SECTION.                                                  
027500                                                                          
027600     MOVE WS-IDARTNR             TO W-IDARTNR-MIN                         
027700                                    W-IDARTNR-MAX                         
027800                                                                          
027900*                                                                         
028000     IF WS-IDDISTR > ZERO                                                 
028100       MOVE WS-IDDISTR           TO W-IDDISTR-MIN                         
028200                                    W-IDDISTR-MAX                         
028300     END-IF                                                               
028400*                                                                         
028500     IF WS-IDKUNDNR > ZERO                                                
028600       MOVE WS-IDKUNDNR          TO W-IDKUNDNR-MIN                        
028700                                    W-IDKUNDNR-MAX                        
028800     END-IF                                                               
028900*                                                                         
029000     IF WS-IDBYTRAP-9KOMPL > ZERO                                         
029100       MOVE WS-IDBYTRAP-9KOMPL   TO W-IDBYTRAP-9KOMPL-MIN                 
029200                                    W-IDBYTRAP-9KOMPL-MAX                 
029300     END-IF                                                               
029400*                                                                         
029500     IF WS-KDBYTSTA > '0'                                                 
029600       MOVE WS-KDBYTSTA          TO W-KDBYTSTA-MIN                        
029700                                    W-KDBYTSTA-MAX                        
029800     END-IF                                                               
029900*                                                                         
030000     IF WS-IDDC > '00'                                                    
030100       MOVE WS-IDDC              TO W-IDDC-MIN                            
030200                                    W-IDDC-MAX                            
030300     END-IF                                                               
030400*                                                                         
030500     MOVE INF-FIRST-PAGE         TO RESP-IDMSG-INFO                       
030600                                                                          
030700     .                                                                    
030800     EJECT                                                                
030900 D-NAESTA-SIDA SECTION.                                                   
031000                                                                          
031100     MOVE REQU-IDARTNR-START     TO W-IDARTNR-MIN                         
031200                                    W-IDARTNR-MAX                         
031300     IF WS-IDDISTR > ZERO                                                 
031400       MOVE WS-IDDISTR           TO W-IDDISTR-MIN                         
031500                                    W-IDDISTR-MAX                         
031600     ELSE                                                                 
031700       MOVE REQU-IDDISTR-START   TO W-IDDISTR-MIN                         
031800     END-IF                                                               
031900                                                                          
032000     IF WS-IDKUNDNR > ZERO                                                
032100       MOVE WS-IDKUNDNR          TO W-IDKUNDNR-MIN                        
032200                                    W-IDKUNDNR-MAX                        
032300     ELSE                                                                 
032400       MOVE REQU-IDKUNDNR-START  TO W-IDKUNDNR-MIN                        
032500     END-IF                                                               
032600                                                                          
032700     IF WS-IDBYTRAP-9KOMPL > ZERO                                         
032800       MOVE WS-IDBYTRAP-9KOMPL   TO W-IDBYTRAP-9KOMPL-MIN                 
032900                                    W-IDBYTRAP-9KOMPL-MAX                 
033000     ELSE                                                                 
033100       COMPUTE W-IDBYTRAP-9KOMPL-MIN                                      
033200                                  = W-9KOMPL - REQU-IDBYTRAP-START        
033300     END-IF                                                               
033400                                                                          
033500     IF WS-KDBYTSTA > '0'                                                 
033600       MOVE WS-KDBYTSTA          TO W-KDBYTSTA-MIN                        
033700                                    W-KDBYTSTA-MAX                        
033800     ELSE                                                                 
033900       MOVE REQU-KDBYTSTA-START  TO W-KDBYTSTA-MIN                        
034000     END-IF                                                               
034100                                                                          
034200     IF WS-IDDC > '00'                                                    
034300       MOVE WS-IDDC              TO W-IDDC-MIN                            
034400                                    W-IDDC-MAX                            
034500     ELSE                                                                 
034600       MOVE REQU-IDDC-START      TO W-IDDC-MIN                            
034700     END-IF                                                               
034800                                                                          
034900     MOVE REQU-IDBYTRAD-START    TO W-IDBYTRAD-MIN                        
035000     .                                                                    
035100     EJECT                                                                
035200 E-SAMMA-SIDA SECTION.                                                    
035300                                                                          
035400     MOVE REQU-IDARTNR-START     TO W-IDARTNR-MIN                         
035500                                    W-IDARTNR-MAX                         
035600     IF WS-IDDISTR > ZERO                                                 
035700       MOVE WS-IDDISTR           TO W-IDDISTR-MIN                         
035800                                    W-IDDISTR-MAX                         
035900     ELSE                                                                 
036000       MOVE REQU-IDDISTR-START   TO W-IDDISTR-MIN                         
036100     END-IF                                                               
036200                                                                          
036300     IF WS-IDKUNDNR > ZERO                                                
036400       MOVE WS-IDKUNDNR          TO W-IDKUNDNR-MIN                        
036500                                    W-IDKUNDNR-MAX                        
036600     ELSE                                                                 
036700       MOVE REQU-IDKUNDNR-START  TO W-IDKUNDNR-MIN                        
036800     END-IF                                                               
036900                                                                          
037000     IF WS-IDBYTRAP-9KOMPL > ZERO                                         
037100       MOVE WS-IDBYTRAP-9KOMPL   TO W-IDBYTRAP-9KOMPL-MIN                 
037200                                    W-IDBYTRAP-9KOMPL-MAX                 
037300     ELSE                                                                 
037400       COMPUTE W-IDBYTRAP-9KOMPL-MIN                                      
037500                                  = W-9KOMPL - REQU-IDBYTRAP-START        
037600     END-IF                                                               
037700                                                                          
037800     IF WS-KDBYTSTA > '0'                                                 
037900       MOVE WS-KDBYTSTA          TO W-KDBYTSTA-MIN                        
038000                                    W-KDBYTSTA-MAX                        
038100     ELSE                                                                 
038200       MOVE REQU-KDBYTSTA-START  TO W-KDBYTSTA-MIN                        
038300     END-IF                                                               
038400                                                                          
038500     IF WS-IDDC > '00'                                                    
038600       MOVE WS-IDDC              TO W-IDDC-MIN                            
038700                                    W-IDDC-MAX                            
038800     ELSE                                                                 
038900       MOVE REQU-IDDC-START      TO W-IDDC-MIN                            
039000     END-IF                                                               
039100                                                                          
039200     MOVE REQU-IDBYTRAD-START    TO W-IDBYTRAD-MIN                        
039300     .                                                                    
039400     EJECT                                                                
039500 F-LAES-VISA-INFO SECTION.                                                
039600                                                                          
039700     MOVE ZERO                   TO RESP-KVRADER                          
039800                                                                          
039900*    STARTA GENOM ATT LÄSA FÖRSTA 6E POST**                               
040000     PERFORM IMS-GET-WDM6E1                                               
040100     MOVE +1                     TO INDX                                  
040200     PERFORM                                                              
040300       UNTIL INDX > MAX-KVRADER                                           
040400       PERFORM                                                            
040500         UNTIL SEGMENT-SAKNAS OR                                          
040600               INDX > MAX-KVRADER                                         
040700                                                                          
040800*         MOVE FIELDS FROM WDM6E TO 601 & 611                             
040900         MOVE SEQE-IDDISTR       TO W-IDDISTR-601                         
041000         COMPUTE W-IDBYTRAP-601 = W-9KOMPL - SEQE-IDBYTRAP-9KOMPL         
041100         MOVE SEQE-IDBYTRAD      TO W-IDBYTRAD-611                        
041200                                                                          
041300*        READ 601 SGMENT                                                  
041400         PERFORM IMS-GET-WDM601                                           
041500                                                                          
041600*        CHECK IF VALID INTERVAL                                          
041700**************fix av nyckel efter första läsning*****************         
041800         IF W-IDKUNDNR-MIN = W-IDKUNDNR-MAX                               
041900           CONTINUE                                                       
042000         ELSE                                                             
042100           MOVE ZERO             TO W-IDKUNDNR-MIN                        
042200         END-IF                                                           
042300*****************************************************************         
042400**************FIX AV NYCKEL EFTER FÖRSTA LÄSNING*****************         
042500         IF W-KDBYTSTA-MIN = W-KDBYTSTA-MAX                               
042600           CONTINUE                                                       
042700         ELSE                                                             
042800           MOVE '0'              TO W-KDBYTSTA-MIN                        
042900         END-IF                                                           
043000*****************************************************************         
043100**************fix av nyckel efter första läsning*****************         
043200         IF W-IDDISTR-MIN = W-IDDISTR-MAX                                 
043300           CONTINUE                                                       
043400         ELSE                                                             
043500           MOVE ZERO             TO W-IDDISTR-MIN                         
043600         END-IF                                                           
043700*****************************************************************         
043800**************fix av nyckel efter första läsning*****************         
043900         IF W-IDBYTRAP-9KOMPL-MIN = W-IDBYTRAP-9KOMPL-MAX                 
044000           CONTINUE                                                       
044100         ELSE                                                             
044200           MOVE ZERO             TO W-IDBYTRAP-9KOMPL-MIN                 
044300         END-IF                                                           
044400*****************************************************************         
044500**************fix av nyckel efter första läsning*****************         
044600         IF W-IDDC-MIN = W-IDDC-MAX                                       
044700           CONTINUE                                                       
044800         ELSE                                                             
044900           MOVE '00'             TO W-IDDC-MIN                            
045000         END-IF                                                           
045100*****************************************************************         
045200**************fix av nyckel efter första läsning*****************         
045300         MOVE ZERO               TO W-IDBYTRAD-MIN                        
045400*****************************************************************         
045500         IF W-IDBYTRAP-9KOMPL-MIN > ZERO                                  
045600           COMPUTE W-IDBYTRAP-MIN = W-9KOMPL -                            
045700                                           W-IDBYTRAP-9KOMPL-MIN          
045800         ELSE                                                             
045900           MOVE ZERO             TO W-IDBYTRAP-MIN                        
046000         END-IF                                                           
046100                                                                          
046200         IF W-IDBYTRAP-9KOMPL-MAX > ZERO  AND                             
046300            W-IDBYTRAP-9KOMPL-MAX < 9999999                               
046400           COMPUTE W-IDBYTRAP-MAX = W-9KOMPL -                            
046500                                          W-IDBYTRAP-9KOMPL-MAX           
046600         END-IF                                                           
046700                                                                          
046800         IF (RAPP-IDKUNDNR >= W-IDKUNDNR-MIN) AND                         
046900            (RAPP-IDKUNDNR <= W-IDKUNDNR-MAX) AND                         
047000            (RAPP-KDBYTSTA-RAPP >= W-KDBYTSTA-MIN) AND                    
047100            (RAPP-KDBYTSTA-RAPP <= W-KDBYTSTA-MAX) AND                    
047200            (RAPP-IDBYTRAP <= W-IDBYTRAP-MAX) AND                         
047300            (RAPP-IDBYTRAP >= W-IDBYTRAP-MIN) AND                         
047400            (RAPP-IDDC     >= W-IDDC-MIN) AND                             
047500            (RAPP-IDDC     <= W-IDDC-MAX)                                 
047600*          IF VALID INTERVAL, READ 611 SEGMENT                            
047700           PERFORM IMS-GET-WDM611                                         
047800           MOVE 'J'              TO VALID-INTERVAL                        
047900           ADD 1                 TO RESP-KVRADER                          
048000*          AND MOVE DATA TO MOD- FIELDS                                   
048100*          AND SAVE DATA IN SPAR AREAS(PAR AREAS)                         
048200           MOVE RAPP-IDDISTR     TO RESP-IDDISTR-LINE (INDX)              
048300           MOVE RAPP-IDKUNDNR    TO RESP-IDKUNDNR-LINE (INDX)             
048400           MOVE RAPP-IDBYTRAP    TO RESP-IDBYTRAP-LINE (INDX)             
048500           MOVE RAPP-DAREGDAT(3:6)                                        
048600                                 TO RESP-TIREGDAT-LINE (INDX)             
048700           MOVE RAPP-DAANKDAG(3:6)                                        
048800                                 TO RESP-TIANKDAG-LINE (INDX)             
048900           MOVE RAPP-DAREGDAT-GODK(3:6)                                   
049000                                 TO RESP-TIREGDAT-GODK-LINE (INDX)        
049100           MOVE RAPP-KDBYTSTA-RAPP                                        
049200                                 TO RESP-KDBYTSTA-LINE (INDX)             
049300           IF RAPP-FLBYTGAR = 'J'                                         
049400             MOVE 'Y'            TO RAPP-FLBYTGAR                         
049500           END-IF                                                         
049600           MOVE RAPP-FLBYTGAR    TO RESP-FLBYTGAR-LINE (INDX)             
049700           MOVE OBJ-KVRETUR-URSP TO RESP-KVRETUR-URSP-LINE (INDX)         
049800           MOVE OBJ-KVRETUR-GODK TO RESP-KVRETUR-GODK-LINE (INDX)         
049900           MOVE OBJ-KDBYTREF     TO RESP-ANMARKNINGKOD-LINE (INDX)        
050000*          AND SAVE DATA IN SPAR AREAS(SPAR AREAS)                        
050100           IF INDX = 1                                                    
050200             MOVE W-IDARTNR-MIN  TO RESP-IDARTNR-START                    
050300             MOVE RAPP-IDDISTR   TO RESP-IDDISTR-START                    
050400             MOVE RAPP-IDKUNDNR  TO RESP-IDKUNDNR-START                   
050500             MOVE RAPP-IDBYTRAP  TO RESP-IDBYTRAP-START                   
050600             MOVE OBJ-IDBYTRAD   TO RESP-IDBYTRAD-START                   
050700             MOVE RAPP-KDBYTSTA-RAPP                                      
050800                                 TO RESP-KDBYTSTA-START                   
050900             MOVE RAPP-IDDC      TO RESP-IDDC-START                       
051000           END-IF                                                         
051100         END-IF                                                           
051200*        IF NOT VALID INTERVAL, SUBTRACT 1 FROM INDEX                     
051300         IF FEL-INT                                                       
051400           SUBTRACT 1          FROM INDX                                  
051500         END-IF                                                           
051600*        READ NEXT WDM6E POST                                             
051700         PERFORM IMS-GET-WDM6E1                                           
051800         MOVE 'N'                TO VALID-INTERVAL                        
051900         ADD 1                   TO INDX                                  
052000       END-PERFORM                                                        
052100*      IF INDX > MAX-KVRADER AND SEGMENT-FINNS,                           
052200*      WRITE MSG 'MORE LINES                                              
052300       IF INDX > MAX-KVRADER AND SEGMENT-FINNS                            
052400         MOVE INF-MORE-INFO-EXISTS                                        
052500                                 TO RESP-IDMSG-INFO                       
052600*        SAVE SPAR AREA IN SPAR-NEXT                                      
052700         MOVE RAPP-IDDISTR       TO RESP-IDDISTR-NEXT                     
052800         MOVE RAPP-IDKUNDNR      TO RESP-IDKUNDNR-NEXT                    
052900         MOVE RAPP-IDBYTRAP      TO RESP-IDBYTRAP-NEXT                    
053000         MOVE OBJ-IDBYTRAD       TO RESP-IDBYTRAD-NEXT                    
053100         MOVE RAPP-KDBYTSTA-RAPP TO RESP-KDBYTSTA-NEXT                    
053200         MOVE RAPP-IDDC          TO RESP-IDDC-NEXT                        
053300       END-IF                                                             
053400       IF INDX = 1  AND SEGMENT-SAKNAS                                    
053500         MOVE W-IDARTNR-MIN      TO RESP-IDARTNR-START                    
053600         MOVE W-IDDISTR-MIN      TO RESP-IDDISTR-START                    
053700         MOVE W-IDKUNDNR-MIN     TO RESP-IDKUNDNR-START                   
053800         COMPUTE RESP-IDBYTRAP-START =                                    
053900                                  W-9KOMPL - W-IDBYTRAP-9KOMPL-MIN        
054000         MOVE W-IDBYTRAD-MIN     TO RESP-IDBYTRAD-START                   
054100         MOVE W-KDBYTSTA-MIN     TO RESP-KDBYTSTA-START                   
054200         MOVE W-IDDC-MIN         TO RESP-IDDC-START                       
054300       END-IF                                                             
054400       IF INDX = MAX-KVRADER AND SEGMENT-SAKNAS                           
054500         MOVE ZERO               TO RESP-IDDISTR-NEXT                     
054600                                    RESP-IDKUNDNR-NEXT                    
054700                                    RESP-IDBYTRAP-NEXT                    
054800                                    RESP-IDBYTRAD-NEXT                    
054900         MOVE SPACE              TO RESP-KDBYTSTA-NEXT                    
055000                                    RESP-IDDC-NEXT                        
055100       END-IF                                                             
055200*      IF INDX <= MAX-KVRADER AND SEGMENT-SAKNAS,                         
055300*      THEN HIDE INPUT LINE                                               
055400       IF INDX <= MAX-KVRADER AND SEGMENT-SAKNAS                          
055500         MOVE ALL-SPACE          TO                                       
055600                                    RESP-IDDISTR-LINE (INDX)              
055700                                    RESP-IDKUNDNR-LINE (INDX)             
055800                                    RESP-IDBYTRAP-LINE (INDX)             
055900                                    RESP-TIANKDAG-LINE (INDX)             
056000                                    RESP-TIREGDAT-GODK-LINE (INDX)        
056100                                    RESP-TIREGDAT-LINE (INDX)             
056200                                    RESP-KDBYTSTA-LINE (INDX)             
056300                                    RESP-FLBYTGAR-LINE (INDX)             
056400                                    RESP-KVRETUR-URSP-LINE (INDX)         
056500                                    RESP-KVRETUR-GODK-LINE (INDX)         
056600                                    RESP-ANMARKNINGKOD-LINE (INDX)        
056700         ADD +1                  TO INDX                                  
056800       END-IF                                                             
056900     END-PERFORM                                                          
057000     .                                                                    
057100     EJECT                                                                
057200 MFS-RENSA-FAELT-UT SECTION.                                              
057300                                                                          
057400      MOVE +1 TO INDX                                                     
057500      PERFORM UNTIL INDX > MAX-KVRADER                                    
057600        MOVE ALL-SPACE       TO RESP-IDDISTR-LINE (INDX)                  
057700                                RESP-IDKUNDNR-LINE (INDX)                 
057800                                RESP-IDBYTRAP-LINE (INDX)                 
057900                                RESP-TIREGDAT-LINE (INDX)                 
058000                                RESP-TIANKDAG-LINE (INDX)                 
058100                                RESP-TIREGDAT-GODK-LINE (INDX)            
058200                                RESP-KDBYTSTA-LINE (INDX)                 
058300                                RESP-FLBYTGAR-LINE (INDX)                 
058400                                RESP-KVRETUR-URSP-LINE (INDX)             
058500                                RESP-KVRETUR-GODK-LINE (INDX)             
058600                                RESP-ANMARKNINGKOD-LINE (INDX)            
058700        ADD +1 TO INDX                                                    
058800      END-PERFORM                                                         
058900     .                                                                    
059000     SKIP3                                                                
059100* --- IMS SEKTIONER ---                                                   
059200     SKIP3                                                                
059300 IMS-GET-WDM6E1 SECTION.                                                  
059400                                                                          
059500     STRING 'WDM6E1  (WDM6E1KY=>' W-WDM6E1KY-MIN                          
059600                    '&WDM6E1KY<=' W-WDM6E1KY-MAX ')'                      
059700          DELIMITED BY SIZE INTO SSA1                                     
059800     MOVE '  GE' TO GODK-STATUSKODER                                      
059900     CALL CBLTDLI USING GN WDM6E-PCB DLI-IO-WDM6E1 SSA1                   
060000     MOVE WDM6E-STATUS-CODE TO STATUS-WS                                  
060100     PERFORM IMS-STATUSKONTROLL                                           
060200     .                                                                    
060300     EJECT                                                                
060400 IMS-GET-WDM601 SECTION.                                                  
060500                                                                          
060600     STRING 'WDM601  (WDM601KY =' W-WDM601KY ')'                          
060700          DELIMITED BY SIZE INTO SSA1                                     
060800     MOVE '  GE' TO GODK-STATUSKODER                                      
060900     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
061000     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
061100     PERFORM IMS-STATUSKONTROLL                                           
061200     .                                                                    
061300     EJECT                                                                
061400 IMS-GET-WDM611 SECTION.                                                  
061500                                                                          
061600     STRING 'WDM601  (WDM601KY =' W-WDM601KY ')'                          
061700          DELIMITED BY SIZE INTO SSA1                                     
061800     STRING 'WDM611  (IDBYTRAD =' W-IDBYTRAD ')'                          
061900          DELIMITED BY SIZE INTO SSA2                                     
062000     MOVE '  GE' TO GODK-STATUSKODER                                      
062100     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM611 SSA1 SSA2               
062200     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062500     EJECT                                                                
062600 IMS-STATUSKONTROLL SECTION.                                              
062700                                                                          
062800     SET STATUS-IX TO 1                                                   
062900     SEARCH GODK-STATUS                                                   
063000       AT END                                                             
063100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
063200         DELIMITED BY SIZE INTO FELTEXT                                   
063300         CALL FELLOG                                                      
063400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
063500         CONTINUE                                                         
063600     END-SEARCH                                                           
063700     .                                                                    
