000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3016900.                                                
000400 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000500 DATE-WRITTEN.   00/03/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800**   FUNKTION:                                                            
000900*        FRÅGEPGM SOM LÄSER DB2-TABELL BYLRAD.                            
001000*        UTIFRÅN NYCKLEFÄLTEN VÄLJS RADER UT OCH SUMMERING GÖRS           
001100*        UTAV KVPOINT VECKOVIS PER DISTRIKT SOM VISAS PÅ BILDEN.          
001200*        MAXINTERVALL FÖR DIREKTSÖKNING ÄR 8 VECKOR/DISTRIKT.             
001300*                                                                         
001400*        PROGRAMMET LÄSER      DB2-TABELL BYLRAD                          
001500*                                                                         
001600*        NYCKELFÄLT: IDDISTR-FOM       BYLRAD                             
001700*                    IDDISTR-TOM       BYLRAD                             
001800*                    TIAAVV-FOM        BYLRAD                             
001900*                    TIAAVV-TOM        BYLRAD                             
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W3T169                                              
002300*                     W3T169U                                             
002400*        MID:         W3I16901                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W3O16901                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W3016900'.            
003800                                                                          
003900*    ---INDEX FÖR BLÄDDRINGSRADER                                         
004000 77  RAD-INDX                    PIC S9(4)   VALUE +0  COMP SYNC.         
004100 77  MAX-RAD-INDX                PIC S9(4)   VALUE +15 COMP SYNC.         
004200 77  KOL-INDX                    PIC S9(4)   VALUE +0  COMP SYNC.         
004300 77  MAX-KOL-INDX                PIC S9(4)   VALUE +8  COMP SYNC.         
004400 77  WS-VECKA-MIN                PIC 9(2)    VALUE ZERO.                  
004500 77  WS-VECKA-MAX                PIC 9(2)    VALUE ZERO.                  
004600                                                                          
004700 77  WS-KVANTAL                PIC S9(7)      COMP-3   VALUE ZERO.        
004800 77  WS-KVANT-SUM              PIC S9(7)V9(3) COMP-3   VALUE ZERO.        
004900 77  WS-KVANT-TOT              PIC S9(4).                                 
005000 77  WS-TEST-IDDISTR           PIC S9(5)      COMP-3   VALUE ZERO.        
005100 77  WS-TEST-DAAAVV            PIC X(6).                                  
005200 77  WS-VECKA-INTERVALL        PIC S9(4)      COMP-3   VALUE ZERO.        
005300                                                                          
005400                                                                          
005500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005600     88  ALLT-OK                             VALUE 'J'.                   
005700                                                                          
005800 77  INTERVALL-SW                PIC X       VALUE 'J'.                   
005900     88  INTERVALL-OK                        VALUE 'J'.                   
006000     88  INTERVALL-FEL                       VALUE 'N'.                   
006100                                                                          
006200 01  WS-DATUM-X.                                                          
006300     03  WS-DAR                  PIC X(2)    VALUE '20'.                  
006400     03  WS-AAVV                 PIC X(4).                                
006500 01  DATUM-WS.                                                            
006600     03 AA-WS                    PIC 9(2).                                
006700     03 VV-WS                    PIC 9(2).                                
006800                                                                          
006900 01  W-FOM-IN.                                                            
007000     03  WS-DAR-FOM             PIC X(2)    VALUE '20'.                   
007100     03  WS-AAVV-FOM            PIC X(4).                                 
007200 01  WS-FOM-IN                  PIC X(6).                                 
007300 01  WS-FOM-NUM.                                                          
007400     03  WS-DAR-FOM             PIC 9(2).                                 
007500     03  WS-AAR-FOM             PIC 9(2).                                 
007600     03  WS-VECKA-FOM           PIC 9(2).                                 
007700                                                                          
007800 01  W-TOM-IN.                                                            
007900     03  WS-DAR-TOM              PIC X(2)    VALUE '20'.                  
008000     03  WS-AAVV-TOM             PIC X(4).                                
008100 01  WS-TOM-IN                   PIC X(6).                                
008200 01  WS-TOM-NUM.                                                          
008300     03  WS-DAR-TOM              PIC 9(2).                                
008400     03  WS-AAR-TOM              PIC 9(2).                                
008500     03  WS-VECKA-TOM            PIC 9(2).                                
008600                                                                          
008700 01  W-IDDISTR-FOM              PIC S9(5)    COMP-3.                      
008800 01  W-IDDISTR-TOM              PIC S9(5)    COMP-3.                      
008900                                                                          
009000     SKIP2                                                                
009100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
009200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
009300                                                                          
009400 77  JA                          PIC X       VALUE 'J'.                   
009500 77  NEJ                         PIC X       VALUE 'N'.                   
009600                                                                          
009700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009800                                                                          
009900                                                                          
010000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010100     88  NYCKLAR-OK                          VALUE 'J'.                   
010200     88  NYCKLAR-FEL                         VALUE 'N'.                   
010300                                                                          
010400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010500     88  EGEN-MID                            VALUE '3169'.                
010600     88  GODK-MID                            VALUE '3169'.                
010700     88  HELP-MID                            VALUE '0551'.                
010800     EJECT                                                                
010900                                                                          
011000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011100 01  GENERELLA-SUBPROGRAM.                                                
011200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011900*01 -COPY WMEDAREA                                                        
012000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012100*01 -COPY WDATAREA                                                        
012200     SKIP3                                                                
012300 01  MESSAGE-CODES.                                                       
012400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012600     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
012700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012800     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
012900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
013100     03  INFORMATION-MISSING     PIC X(3)    VALUE '413'.                 
013200     03  MED-1.                                                           
013300       05  FILLER                PIC X(40)                                
013400         VALUE 'MAXIMUM 8 WEEKS'.                                         
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013900     SKIP3                                                                
014000*01 -COPY WMSGINIT                                                        
014100     EJECT                                                                
014200*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
014500 01  SPAR-AREA.                                                           
014600     03  SPAR-IDTRANS            PIC X(4)    VALUE '3169'.                
014700     03  SPAR-TIAAVV-ENTER       PIC 9(6).                                
014800     03  SPAR-TIAAVV-NEXT        PIC 9(6).                                
014900     03  SPAR-IDDISTR-ENTER      PIC 9(5)  COMP-3.                        
015000     03  SPAR-IDDISTR-NEXT       PIC 9(5)  COMP-3.                        
015100     03  SPAR-TIAAVV-FOM         PIC 9(6).                                
015200     03  SPAR-TIAAVV-TOM         PIC 9(6).                                
015300     03  SPAR-IDDISTR-FOM        PIC 9(5)  COMP-3.                        
015400     03  SPAR-IDDISTR-TOM        PIC 9(5)  COMP-3.                        
015500                                                                          
015600                                                                          
015700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016000     SKIP3                                                                
016100*01  MID -COPY W3I16901                                                   
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
016400     SKIP3                                                                
016500*01  -COPY WMSGAREA                                                       
016600     EJECT                                                                
016700     03  MOD REDEFINES MSG-AREA.                                          
016800*      05  -COPY W3O16901                                                 
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017100     SKIP3                                                                
017200*01  -COPY WMFSAREA                                                       
017300     EJECT                                                                
017400                                                                          
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600*                                                                         
017700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017800     SKIP3                                                                
017900*    --- STATUS-KOD FRÅN IMS                                              
018000 01  STATUS-WS                   PIC XX.                                  
018100     88  SEGMENT-FINNS                        VALUE '  '.                 
018200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018300     SKIP2                                                                
018400 01  GODK-STATUSKODER.                                                    
018500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018600     SKIP3                                                                
018700*    ___ IMS FUNKTIONSKODER                                               
018800*01  -COPY W0003                                                          
018900     EJECT                                                                
019000 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
019100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
019200                                                                          
019300 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
019400 01  DB2-WS.                                                              
019500     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
019600         88  CURSOR-OK                       VALUE 000.                   
019700         88  RADER-FINNS                     VALUE 000.                   
019800         88  RADER-SAKNAS                    VALUE 100.                   
019900         88  ATKOMST-FEL                     VALUE 904.                   
020000     03  GODK-SQLCODEKODER.                                               
020100         05  GODK-SQLCODE OCCURS 5                                        
020200             INDEXED BY SQLCODE-IX PIC 9(3).                              
020300     EJECT                                                                
020400*    ---  DLI INPUT-OUTPUT AREA                                           
020500     EJECT                                                                
020600 01  FILLER         PIC X(16) VALUE 'BYLRAD-AREA     '.                   
020700*01  -COPY BYLRAD -PRE BYLRAD-                                            
020800     EJECT                                                                
020900 01  FILLER                      PIC X(16)   VALUE 'BYLRAD-AREA'.         
021000       EXEC SQL INCLUDE BYLRAD  END-EXEC.                                 
021100     EJECT                                                                
021200 LINKAGE SECTION.                                                         
021300*01  -COPY W0009   -PRE MSG-                                              
021400*01  -COPY W0008   -PRE WDP7-                                             
021500     05  FILLER                  PIC X.                                   
021600                                                                          
021700     EJECT                                                                
021800 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB.                              
021900 MAIN SECTION.                                                            
022000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB.                              
022100                                                                          
022200     PERFORM IMS-GET-MSG                                                  
022300                                                                          
022400     IF SEGMENT-FINNS                                                     
022500       PERFORM A-INIT                                                     
022600       PERFORM B-KOLLA-NYCKLAR                                            
022700       IF NYCKLAR-OK                                                      
022800         IF MFS-FIRST                                                     
022900           PERFORM C-FOERSTA-SIDA                                         
023000         ELSE                                                             
023100           IF MFS-NEXT                                                    
023200             PERFORM D-NAESTA-SIDA                                        
023300           ELSE                                                           
023400             PERFORM E-SAMMA-SIDA                                         
023500           END-IF                                                         
023600         END-IF                                                           
023700         PERFORM F-LAES-VISA-INFO                                         
023800       END-IF                                                             
023900       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O16901 + 4                      
024000       PERFORM IMS-INSERT-MSG                                             
024100     END-IF                                                               
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 A-INIT SECTION.                                                          
024800                                                                          
024900     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
025000     ACCEPT DAT-I-TIDATUM FROM DATE                                       
025100     CALL WDATKONV          USING DAT-KDDATFORM                           
025200                                  DAT-I-TIDATUM                           
025300                                  DAT-O-TIDATUM                           
025400                                  DAT-KDSVAR                              
025500     IF DAT-KDSVAR-OK                                                     
025600        MOVE DAT-TIAA-VECKA TO    AA-WS                                   
025700        MOVE DAT-TIVV       TO    VV-WS                                   
025800        SUBTRACT 1          FROM  VV-WS                                   
025900        IF VV-WS = ZERO                                                   
026000          SUBTRACT 1        FROM  AA-WS                                   
026100          MOVE 52           TO    VV-WS                                   
026200        END-IF                                                            
026300        MOVE DATUM-WS       TO    WS-AAVV                                 
026400     ELSE                                                                 
026500        MOVE ZERO           TO AA-WS                                      
026600                               VV-WS                                      
026700        MOVE DATUM-WS       TO WS-AAVV                                    
026800     END-IF                                                               
026900                                                                          
027000     IF MSG-DUBBLA-TRANSKODER                                             
027100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I16901                 
027200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027400     ELSE                                                                 
027500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I16901                  
027600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027800     END-IF                                                               
027900                                                                          
028000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028300                                                                          
028400     MOVE LOW-VALUE TO MSG-AREA                                           
028500     MOVE 'W3O169N1' TO MFS-IDMOD                                         
028600     MOVE '3169' TO MOD-IDTRANS                                           
028700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028800                                                                          
028900                                                                          
029000     IF EGEN-MID OR HELP-MID                                              
029100       CONTINUE                                                           
029200     ELSE                                                                 
029300       MOVE SPACE TO MFS-KDTRTYP                                          
029400       MOVE '7' TO MFS-IDPFK                                              
029500     END-IF                                                               
029600                                                                          
029700     MOVE 'GB'                            TO MED-IDSKYLT                  
029800                                                                          
029900     INITIALIZE GODK-SQLCODEKODER                                         
030000     .                                                                    
030100     EJECT                                                                
030200 B-KOLLA-NYCKLAR SECTION.                                                 
030300                                                                          
030400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030500     MOVE '001'             TO MSGI-KDCALL                                
030600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030800     MOVE '3169'            TO MSGI-IDTRANS                               
030900                                                                          
031000                                                                          
031100     IF GODK-MID                                                          
031200       MOVE MID-IDDISTR-FOM-IN      TO MSGI-IDDISTR-FOM                   
031300       IF MID-IDDISTR-FOM-IN NOT = ALL '+' AND                            
031400          MID-IDDISTR-TOM-IN     = ALL '+' AND                            
031500          MID-IDDISTR-FOM-UT     = ALL SPACE                              
031600         IF MID-IDDISTR-TOM-IN   = ALL '+'                                
031700           MOVE MID-IDDISTR-FOM-IN  TO MSGI-IDDISTR-TOM                   
031800           MOVE MID-IDDISTR-FOM-IN  TO MID-IDDISTR-TOM-IN                 
031900         END-IF                                                           
032000       END-IF                                                             
032100       IF MID-TIAAVV-FOM-IN NOT  = ALL '+' AND                            
032200          MID-TIAAVV-TOM-IN      = ALL '+' AND                            
032300          MID-TIAAVV-FOM-UT      = ALL SPACE                              
032400         IF MID-TIAAVV-TOM-IN    = ALL '+'                                
032500           MOVE MID-TIAAVV-FOM-IN   TO MID-TIAAVV-TOM-IN                  
032600         END-IF                                                           
032700       END-IF                                                             
032800       IF MID-IDDISTR-TOM-IN NOT = ALL '+'                                
032900         MOVE MID-IDDISTR-TOM-IN    TO MSGI-IDDISTR-TOM                   
033000       END-IF                                                             
033100       MOVE MID-TIAAVV-FOM-IN       TO MSGI-TIAAVV-FOM                    
033200       MOVE MID-TIAAVV-TOM-IN       TO MSGI-TIAAVV-TOM                    
033300     END-IF                                                               
033400                                                                          
033500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
033600     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
033700                                                                          
033800     MOVE NEJ TO ALLT-SW                                                  
033900                                                                          
034000     IF GODK-MID OR EGEN-MID                                              
034100       MOVE JA TO NYCKLAR-SW                                              
034200     ELSE                                                                 
034300       MOVE NEJ TO NYCKLAR-SW                                             
034400     END-IF                                                               
034500                                                                          
034600*    -- KONTROLL AV NYCKLAR                                               
034700                                                                          
034800     IF GODK-MID                                                          
034900       IF MID-IDDISTR-FOM-IN = ALL '+' AND                                
035000          MID-IDDISTR-TOM-IN = ALL '+' AND                                
035100          MID-IDDISTR-FOM-UT = ALL SPACE                                  
035200          MOVE NEJ TO NYCKLAR-SW                                          
035300       END-IF                                                             
035400     END-IF                                                               
035500                                                                          
035600     IF NYCKLAR-OK                                                        
035700       IF GODK-MID                                                        
035800         IF  MID-IDDISTR-FOM-IN = ALL '+' AND                             
035900           MID-IDDISTR-TOM-IN = ALL '+'                                   
036000           CONTINUE                                                       
036100         ELSE                                                             
036200           IF NOT MFS-NEXT                                                
036300             MOVE '7'   TO MFS-IDPFK                                      
036400           END-IF                                                         
036500         END-IF                                                           
036600                                                                          
036700         IF MID-TIAAVV-FOM-IN = ALL '+' AND                               
036800           MID-TIAAVV-TOM-IN = ALL '+'                                    
036900           CONTINUE                                                       
037000         ELSE                                                             
037100           IF NOT MFS-NEXT                                                
037200             MOVE '7' TO MFS-IDPFK                                        
037300           END-IF                                                         
037400         END-IF                                                           
037500                                                                          
037600         PERFORM BA-PREPARERA-DISTR-FAELT                                 
037700         PERFORM BC-PREPARERA-VECKA-FAELT                                 
037800       END-IF                                                             
037900     ELSE                                                                 
038000       MOVE WS-AAVV           TO WS-AAVV-FOM                              
038100                                 MSGI-TIAAVV-FOM                          
038200                                 MSGI-TIAAVV-TOM                          
038300                                 WS-AAVV-TOM                              
038400                                 MOD-TIAAVV-FOM-UT                        
038500                                 MOD-TIAAVV-TOM-UT                        
038600       MOVE WS-DATUM-X        TO WS-FOM-IN                                
038700                                 WS-TOM-IN                                
038800                                 SPAR-TIAAVV-FOM                          
038900                                 SPAR-TIAAVV-TOM                          
039000     END-IF                                                               
039100     MOVE MFS-RENSA-FAELT TO MOD-TIAAVV-FOM-IN                            
039200                             MOD-TIAAVV-TOM-IN                            
039300                             MOD-IDDISTR-FOM-IN                           
039400                             MOD-IDDISTR-TOM-IN                           
039500                                                                          
039600     IF GODK-MID AND NYCKLAR-OK                                           
039700       MOVE MID-IDDISTR-FOM-IN TO MOD-IDDISTR-FOM-UT                      
039800       INSPECT MOD-IDDISTR-FOM-UT REPLACING LEADING ZERO                  
039900                                            BY SPACE                      
040000       MOVE MID-IDDISTR-TOM-IN TO MOD-IDDISTR-TOM-UT                      
040100       INSPECT MOD-IDDISTR-TOM-UT REPLACING LEADING ZERO                  
040200                                            BY SPACE                      
040300       MOVE WS-AAVV-FOM      TO MOD-TIAAVV-FOM-UT                         
040400       INSPECT MOD-TIAAVV-FOM-UT REPLACING LEADING SPACE                  
040500       BY ZERO                                                            
040600       MOVE WS-AAVV-TOM       TO MOD-TIAAVV-TOM-UT                        
040700       INSPECT MOD-TIAAVV-TOM-UT REPLACING LEADING SPACE                  
040800       BY ZERO                                                            
040900     END-IF                                                               
041000                                                                          
041100                                                                          
041200     IF NYCKLAR-FEL AND GODK-MID                                          
041300       IF MID-IDDISTR-FOM-IN NOT = ALL '+' AND                            
041400         MID-IDDISTR-TOM-IN NOT = ALL '+'                                 
041500          MOVE MID-IDDISTR-FOM-IN TO MOD-IDDISTR-FOM-UT                   
041600          INSPECT MOD-IDDISTR-FOM-UT REPLACING LEADING ZERO               
041700                                               BY SPACE                   
041800          MOVE MID-IDDISTR-TOM-IN TO MOD-IDDISTR-TOM-UT                   
041900          INSPECT MOD-IDDISTR-TOM-UT REPLACING LEADING ZERO               
042000                                               BY SPACE                   
042100       END-IF                                                             
042200       IF MID-TIAAVV-FOM-IN NOT = ALL '+' AND                             
042300         MID-TIAAVV-TOM-IN NOT = ALL '+'                                  
042400          MOVE MID-TIAAVV-FOM-IN TO MOD-TIAAVV-FOM-UT                     
042500          INSPECT MOD-TIAAVV-FOM-UT REPLACING LEADING ZERO                
042600                                               BY SPACE                   
042700          MOVE MID-TIAAVV-TOM-IN TO MOD-TIAAVV-TOM-UT                     
042800          INSPECT MOD-TIAAVV-TOM-UT REPLACING LEADING ZERO                
042900                                               BY SPACE                   
043000       END-IF                                                             
043100*      IF WS-AAVV-FOM NOT = ALL '+' AND                                   
043200*        WS-AAVV-TOM NOT = ALL '+'                                        
043300*         MOVE WS-AAVV-FOM      TO MOD-TIAAVV-FOM-UT                      
043400*         INSPECT MOD-TIAAVV-FOM-UT REPLACING LEADING ZERO                
043500*                                              BY SPACE                   
043600*         MOVE WS-AAVV-TOM      TO MOD-TIAAVV-TOM-UT                      
043700*         INSPECT MOD-TIAAVV-TOM-UT REPLACING LEADING ZERO                
043800*                                              BY SPACE                   
043900*      END-IF                                                             
044000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
044100       CALL WMEDKONV USING MED-WMEDAREA                                   
044200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 BA-PREPARERA-DISTR-FAELT SECTION.                                        
044700                                                                          
044800     IF MID-IDDISTR-FOM-IN = ALL '+' AND                                  
044900       MID-IDDISTR-FOM-UT NOT = ALL SPACE                                 
045000       MOVE MID-IDDISTR-FOM-UT TO MID-IDDISTR-FOM-IN                      
045100     END-IF                                                               
045200     IF MID-IDDISTR-TOM-IN = ALL '+' AND                                  
045300       MID-IDDISTR-TOM-UT NOT = ALL SPACE                                 
045400       MOVE MID-IDDISTR-TOM-UT TO MID-IDDISTR-TOM-IN                      
045500     END-IF                                                               
045600     INSPECT MID-IDDISTR-FOM-IN REPLACING LEADING SPACE BY ZERO           
045700     IF MID-IDDISTR-FOM-IN NOT = ALL '+'                                  
045800       IF MID-IDDISTR-FOM-IN  NUMERIC AND                                 
045900         MID-IDDISTR-FOM-IN > 0                                           
046000         MOVE MID-IDDISTR-FOM-IN TO W-IDDISTR-FOM                         
046100                                    SPAR-IDDISTR-FOM                      
046200       ELSE                                                               
046300         MOVE NEJ TO NYCKLAR-SW                                           
046400       END-IF                                                             
046500     END-IF                                                               
046600                                                                          
046700     INSPECT MID-IDDISTR-TOM-IN REPLACING LEADING SPACE BY ZERO           
046800     IF MID-IDDISTR-TOM-IN NOT = ALL '+'                                  
046900       IF MID-IDDISTR-TOM-IN NUMERIC AND                                  
047000         MID-IDDISTR-TOM-IN > 0                                           
047100         MOVE MID-IDDISTR-TOM-IN TO W-IDDISTR-TOM                         
047200                                    SPAR-IDDISTR-TOM                      
047300       ELSE                                                               
047400         MOVE NEJ TO NYCKLAR-SW                                           
047500       END-IF                                                             
047600     ELSE                                                                 
047700*** OM INTE TOM IFYLLT SKALL FOM FLYTTAS TILL TOM                         
047800       MOVE MID-IDDISTR-FOM-IN TO MOD-IDDISTR-TOM-UT                      
047900                                  W-IDDISTR-TOM                           
048000                                  SPAR-IDDISTR-TOM                        
048100     END-IF                                                               
048200                                                                          
048300     IF MID-IDDISTR-FOM-IN > MID-IDDISTR-TOM-IN                           
048400       MOVE NEJ TO NYCKLAR-SW                                             
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 BC-PREPARERA-VECKA-FAELT SECTION.                                        
048900                                                                          
049000     IF MID-TIAAVV-FOM-IN = ALL '+' AND                                   
049100       MID-TIAAVV-TOM-IN = ALL '+'                                        
049200                                                                          
049300       MOVE MID-TIAAVV-FOM-UT  TO WS-AAVV-FOM                             
049400       MOVE W-FOM-IN           TO WS-FOM-IN                               
049500                                  SPAR-TIAAVV-FOM                         
049600       MOVE MID-TIAAVV-TOM-UT  TO WS-AAVV-TOM                             
049700       MOVE W-TOM-IN           TO WS-TOM-IN                               
049800                                  SPAR-TIAAVV-TOM                         
049900     ELSE                                                                 
050000       IF MID-TIAAVV-FOM-IN NOT = ALL '+'                                 
050100         INSPECT MID-TIAAVV-FOM-IN REPLACING LEADING SPACE                
050200                                             BY ZERO                      
050300         IF MID-TIAAVV-FOM-IN NUMERIC                                     
050400           IF MID-TIAAVV-FOM-IN NOT = '0000'                              
050500             MOVE MID-TIAAVV-FOM-IN TO WS-AAVV-FOM                        
050600             MOVE W-FOM-IN          TO WS-FOM-NUM                         
050700                                       WS-FOM-IN                          
050800                                       SPAR-TIAAVV-FOM                    
050900           END-IF                                                         
051000         ELSE                                                             
051100           MOVE NEJ               TO NYCKLAR-SW                           
051200         END-IF                                                           
051300       ELSE                                                               
051400         MOVE MID-TIAAVV-FOM-UT TO WS-AAVV-FOM                            
051500         MOVE W-FOM-IN          TO WS-FOM-NUM                             
051600                                   WS-FOM-IN                              
051700                                   SPAR-TIAAVV-FOM                        
051800       END-IF                                                             
051900       IF MID-TIAAVV-TOM-IN NOT = ALL '+'                                 
052000         INSPECT MID-TIAAVV-TOM-IN REPLACING LEADING SPACE                
052100                                             BY ZERO                      
052200         IF MID-TIAAVV-TOM-IN NUMERIC                                     
052300           IF MID-TIAAVV-TOM-IN NOT = '0000'                              
052400             MOVE MID-TIAAVV-TOM-IN  TO WS-AAVV-TOM                       
052500             MOVE W-TOM-IN           TO WS-TOM-NUM                        
052600                                        WS-TOM-IN                         
052700                                        SPAR-TIAAVV-TOM                   
052800           END-IF                                                         
052900         ELSE                                                             
053000           MOVE NEJ                TO NYCKLAR-SW                          
053100         END-IF                                                           
053200       ELSE                                                               
053300         MOVE MID-TIAAVV-TOM-UT TO WS-AAVV-TOM                            
053400         MOVE W-TOM-IN          TO WS-TOM-NUM                             
053500                                   WS-TOM-IN                              
053600                                   SPAR-TIAAVV-TOM                        
053700       END-IF                                                             
053800                                                                          
053900       IF NYCKLAR-OK                                                      
054000         IF (WS-FOM-NUM NUMERIC) AND                                      
054100            (WS-VECKA-FOM > 0 AND                                         
054200             WS-VECKA-FOM  < 53)   AND                                    
054300             WS-FOM-NUM <= WS-TOM-NUM                                     
054400           CONTINUE                                                       
054500         ELSE                                                             
054600           MOVE NEJ TO NYCKLAR-SW                                         
054700         END-IF                                                           
054800                                                                          
054900         IF (WS-TOM-NUM NUMERIC) AND                                      
055000            (WS-VECKA-TOM > 0 AND                                         
055100             WS-VECKA-TOM  < 53)   AND                                    
055200             WS-TOM-NUM >= WS-FOM-NUM                                     
055300             CONTINUE                                                     
055400         ELSE                                                             
055500           MOVE NEJ TO NYCKLAR-SW                                         
055600         END-IF                                                           
055700       END-IF                                                             
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 C-FOERSTA-SIDA SECTION.                                                  
056200                                                                          
056300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
056400     CALL WMEDKONV USING MED-WMEDAREA                                     
056500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
056600                                                                          
056700     MOVE SPAR-TIAAVV-FOM    TO WS-FOM-IN                                 
056800                                 W-FOM-IN                                 
056900     MOVE SPAR-TIAAVV-TOM    TO WS-TOM-IN                                 
057000                                 W-TOM-IN                                 
057100     MOVE SPAR-IDDISTR-FOM   TO W-IDDISTR-FOM                             
057200     MOVE SPAR-IDDISTR-TOM   TO W-IDDISTR-TOM                             
057300     .                                                                    
057400     EJECT                                                                
057500 D-NAESTA-SIDA SECTION.                                                   
057600                                                                          
057700     IF SPAR-IDTRANS = '3169'                                             
057800       MOVE SPAR-TIAAVV-FOM      TO WS-FOM-IN                             
057900                                    W-FOM-IN                              
058000       MOVE SPAR-TIAAVV-TOM      TO WS-TOM-IN                             
058100                                    W-TOM-IN                              
058200       MOVE SPAR-IDDISTR-NEXT    TO W-IDDISTR-FOM                         
058300       MOVE SPAR-IDDISTR-TOM     TO W-IDDISTR-TOM                         
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 E-SAMMA-SIDA SECTION.                                                    
058800                                                                          
058900     IF SPAR-IDTRANS = '3169' OR '0551'                                   
059000*      MOVE SPAR-TIAAVV-ENTER   TO WS-FOM-IN                              
059100*      MOVE SPAR-TIAAVV-NEXT    TO WS-FOM-IN                              
059200       MOVE SPAR-TIAAVV-FOM     TO WS-FOM-IN                              
059300                                   W-FOM-IN                               
059400       MOVE SPAR-TIAAVV-TOM     TO WS-TOM-IN                              
059500                                   W-TOM-IN                               
059600       MOVE SPAR-IDDISTR-ENTER  TO W-IDDISTR-FOM                          
059700       MOVE SPAR-IDDISTR-TOM    TO W-IDDISTR-TOM                          
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 F-LAES-VISA-INFO SECTION.                                                
060200                                                                          
060300     MOVE SPAR-TIAAVV-FOM(5:2) TO WS-VECKA-MIN                            
060400     MOVE WS-AAVV-TOM(3:2)     TO WS-VECKA-MAX                            
060500                                                                          
060600     COMPUTE WS-VECKA-INTERVALL = WS-VECKA-MAX -                          
060700                                  WS-VECKA-MIN +                          
060800                                  1                                       
060900     END-COMPUTE                                                          
061000     IF WS-VECKA-INTERVALL > MAX-KOL-INDX                                 
061100       MOVE NEJ                TO INTERVALL-SW                            
061200     END-IF                                                               
061300                                                                          
061400     IF INTERVALL-OK                                                      
061500       PERFORM DB2-DCL-OPN-CRS-BYLRAD                                     
061600       PERFORM DB2-FETCH-BYLRAD                                           
061700                                                                          
061800       IF RADER-FINNS                                                     
061900***FLYTTAR DET FÖRSTA DISTR SOM LÄSES TILL SPAR-AREA                      
062000         MOVE BYLRAD-IDDISTR   TO SPAR-IDDISTR-ENTER                      
062100                                  WS-TEST-IDDISTR                         
062200                                  SPAR-IDDISTR-FOM                        
062300         MOVE BYLRAD-DAAAVV    TO SPAR-TIAAVV-ENTER                       
062400                                  WS-TEST-DAAAVV                          
062500                                  SPAR-TIAAVV-FOM                         
062600         MOVE JA               TO ALLT-SW                                 
062700       ELSE                                                               
062800         MOVE NEJ              TO ALLT-SW                                 
062900       END-IF                                                             
063000                                                                          
063100       MOVE +1 TO RAD-INDX                                                
063200       MOVE +1 TO KOL-INDX                                                
063300       PERFORM UNTIL RAD-INDX > MAX-RAD-INDX OR                           
063400                     RADER-SAKNAS                                         
063500                                                                          
063600         MOVE WS-TEST-IDDISTR   TO MOD-IDDISTR(RAD-INDX)                  
063700         PERFORM UNTIL RADER-SAKNAS                         OR            
063800                       RAD-INDX > MAX-RAD-INDX              OR            
063900                       BYLRAD-IDDISTR NOT = WS-TEST-IDDISTR               
064000                                                                          
064100           PERFORM UNTIL RADER-SAKNAS                       OR            
064200                         BYLRAD-DAAAVV NOT = WS-TEST-DAAAVV               
064300                                                                          
064400             IF BYLRAD-IDPTYP = 'RET' OR 'KRE' OR 'ADJ'                   
064500               COMPUTE WS-KVANTAL   = WS-KVANTAL     +                    
064600                                     (BYLRAD-KVANTAL *                    
064700                                      BYLRAD-KVPOINT)                     
064800               END-COMPUTE                                                
064900             ELSE                                                         
065000               IF BYLRAD-IDPTYP = 'FAK'                                   
065100                 COMPUTE WS-KVANTAL   = WS-KVANTAL     +                  
065200                                       (BYLRAD-KVANTAL *                  
065300                                        BYLRAD-KVPOINT *                  
065400                                        -1)                               
065500                 END-COMPUTE                                              
065600               END-IF                                                     
065700             END-IF                                                       
065800                                                                          
065900             PERFORM DB2-FETCH-BYLRAD                                     
066000           END-PERFORM                                                    
066100                                                                          
066200           COMPUTE WS-KVANT-SUM = WS-KVANTAL / 1000                       
066300           COMPUTE WS-KVANT-TOT ROUNDED = WS-KVANT-SUM                    
066400                                                                          
066500           PERFORM FB-FLYTTA-TILL-MOD                                     
066600           PERFORM FC-KONTROLLERA-DISTR-VECKA                             
066700         END-PERFORM                                                      
066800       END-PERFORM                                                        
066900                                                                          
067000*** OM RAD-INDX = 15 OCH RADER-FINNS                                      
067100*** FLYTTA BYLRAD-IDDISTR OCH BYLRAD-DAAAVV TILL SPAR-NEXT                
067200                                                                          
067300       IF ALLT-OK                                                         
067400         IF RADER-FINNS                                                   
067500           MOVE WS-TEST-IDDISTR        TO SPAR-IDDISTR-NEXT               
067600*          MOVE WS-TEST-TIAAVV         TO SPAR-TIAAVV-NEXT                
067700           MOVE SPAR-TIAAVV-FOM(5:2)   TO SPAR-TIAAVV-NEXT                
067800           MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                    
067900           CALL WMEDKONV USING MED-WMEDAREA                               
068000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
068100                                                                          
068200         ELSE                                                             
068300           MOVE WS-TEST-IDDISTR        TO SPAR-IDDISTR-NEXT               
068400*          MOVE WS-TEST-TIAAVV         TO SPAR-TIAAVV-NEXT                
068500           MOVE SPAR-TIAAVV-FOM(5:2)   TO SPAR-TIAAVV-NEXT                
068600                                          SPAR-TIAAVV-ENTER               
068700           MOVE INF-LAST-PAGE TO MED-IDMFSINF                             
068800           CALL WMEDKONV USING MED-WMEDAREA                               
068900           MOVE MED-MFSINF TO MOD-TEMFSINF                                
069000                                                                          
069100           PERFORM UNTIL RAD-INDX > MAX-RAD-INDX                          
069200             MOVE MFS-RENSA-FAELT TO MOD-IDDISTR(RAD-INDX)                
069300             PERFORM UNTIL KOL-INDX > MAX-KOL-INDX                        
069400              MOVE MFS-RENSA-FAELT TO MOD-TIVV(RAD-INDX, KOL-INDX)        
069500               MOVE MFS-RENSA-FAELT                                       
069600                      TO MOD-KVPOINT(RAD-INDX, KOL-INDX)                  
069700               ADD +1 TO KOL-INDX                                         
069800             END-PERFORM                                                  
069900             ADD +1 TO RAD-INDX                                           
070000           END-PERFORM                                                    
070100         END-IF                                                           
070200         MOVE '002'  TO MSGI-KDCALL                                       
070300         MOVE '3169' TO SPAR-IDTRANS                                      
070400         MOVE SPAR-AREA TO MSGI-SPAR-AREA                                 
070500         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
070600       ELSE                                                               
070700         MOVE INFORMATION-MISSING TO MED-IDMFSFEL                         
070800         CALL WMEDKONV USING MED-WMEDAREA                                 
070900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
071000*        PERFORM MFS-RENSA-FAELT-UT                                       
071100       END-IF                                                             
071200       PERFORM DB2-CLOSE-BYLRAD-CRS                                       
071300     ELSE                                                                 
071400       MOVE MED-1 TO MOD-TEMFSINF                                         
071500       MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                               
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 FB-FLYTTA-TILL-MOD SECTION.                                              
072000     IF WS-VECKA-MIN < WS-TEST-DAAAVV(5:2)                                
072100       PERFORM UNTIL WS-VECKA-MIN = WS-TEST-DAAAVV(5:2)                   
072200         MOVE WS-VECKA-MIN TO MOD-TIVV(RAD-INDX, KOL-INDX)                
072300         MOVE ZERO         TO MOD-KVPOINT(RAD-INDX, KOL-INDX)             
072400         ADD +1            TO KOL-INDX                                    
072500         ADD +1            TO WS-VECKA-MIN                                
072600       END-PERFORM                                                        
072700     ELSE                                                                 
072800       IF WS-VECKA-MIN = WS-TEST-DAAAVV(5:2)                              
072900         MOVE WS-TEST-DAAAVV(5:2) TO MOD-TIVV(RAD-INDX, KOL-INDX)         
073000         MOVE WS-KVANT-TOT     TO MOD-KVPOINT(RAD-INDX, KOL-INDX)         
073100         ADD +1 TO KOL-INDX                                               
073200         ADD +1 TO WS-VECKA-MIN                                           
073300       END-IF                                                             
073400     END-IF                                                               
073500     .                                                                    
073600     EJECT                                                                
073700 FC-KONTROLLERA-DISTR-VECKA SECTION.                                      
073800                                                                          
073900     IF NOT RADER-SAKNAS                                                  
074000       IF BYLRAD-IDDISTR = WS-TEST-IDDISTR                                
074100         MOVE BYLRAD-DAAAVV TO WS-TEST-DAAAVV                             
074200       ELSE                                                               
074300         PERFORM FCA-PREPARERA-FAELT                                      
074400                                                                          
074500         MOVE BYLRAD-IDDISTR TO WS-TEST-IDDISTR                           
074600         MOVE BYLRAD-DAAAVV  TO WS-TEST-DAAAVV                            
074700         MOVE WS-AAVV-FOM(3:2) TO WS-VECKA-MIN                            
074800         ADD +1              TO RAD-INDX                                  
074900         IF RAD-INDX <= MAX-RAD-INDX                                      
075000           MOVE WS-TEST-IDDISTR TO MOD-IDDISTR(RAD-INDX)                  
075100           MOVE +1             TO KOL-INDX                                
075200         END-IF                                                           
075300       END-IF                                                             
075400     ELSE                                                                 
075500       PERFORM FCA-PREPARERA-FAELT                                        
075600                                                                          
075700       ADD +1               TO RAD-INDX                                   
075800       MOVE WS-AAVV-FOM(3:2) TO WS-VECKA-MIN                              
075900     END-IF                                                               
076000     .                                                                    
076100     EJECT                                                                
076200 FCA-PREPARERA-FAELT SECTION.                                             
076300     PERFORM UNTIL WS-VECKA-MIN > WS-VECKA-MAX                            
076400       MOVE WS-VECKA-MIN TO MOD-TIVV(RAD-INDX, KOL-INDX)                  
076500       MOVE ZERO         TO MOD-KVPOINT(RAD-INDX, KOL-INDX)               
076600       ADD +1 TO KOL-INDX                                                 
076700       ADD +1 TO WS-VECKA-MIN                                             
076800     END-PERFORM                                                          
076900     PERFORM UNTIL KOL-INDX > MAX-KOL-INDX                                
077000       MOVE MFS-RENSA-FAELT TO MOD-TIVV(RAD-INDX, KOL-INDX)               
077100       MOVE MFS-RENSA-FAELT TO MOD-KVPOINT(RAD-INDX, KOL-INDX)            
077200       ADD +1 TO KOL-INDX                                                 
077300     END-PERFORM                                                          
077400     .                                                                    
077500     EJECT                                                                
077600                                                                          
077700 MFS-RENSA-FAELT-UT SECTION.                                              
077800                                                                          
077900*    --- ALLA UTDATA-FÄLT                                                 
078000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                 
078100                                MOD-IDDISTR-FOM-UT                        
078200                                MOD-IDDISTR-TOM-UT                        
078300                                MOD-TIAAVV-FOM-UT                         
078400                                MOD-TIAAVV-TOM-UT                         
078500     MOVE +1 TO RAD-INDX                                                  
078600     MOVE +1 TO KOL-INDX                                                  
078700     PERFORM UNTIL RAD-INDX > MAX-RAD-INDX                                
078800       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR(RAD-INDX)                      
078900       PERFORM UNTIL KOL-INDX > MAX-KOL-INDX                              
079000         MOVE MFS-RENSA-FAELT TO MOD-TIVV(RAD-INDX, KOL-INDX)             
079100       MOVE MFS-RENSA-FAELT TO MOD-KVPOINT(RAD-INDX, KOL-INDX)            
079200         ADD +1 TO KOL-INDX                                               
079300       END-PERFORM                                                        
079400       ADD +1 TO RAD-INDX                                                 
079500     END-PERFORM                                                          
079600     .                                                                    
079700     SKIP3                                                                
079800 IMS-GET-MSG SECTION.                                                     
079900                                                                          
080000     MOVE '  QC' TO GODK-STATUSKODER                                      
080100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
080200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080300     PERFORM IMS-STATUSKONTROLL                                           
080400     .                                                                    
080500     SKIP3                                                                
080600 IMS-INSERT-MSG SECTION.                                                  
080700                                                                          
080800     IF MSGI-IDLAND-SPR = 'GB'                                            
080900       MOVE 'N' TO MFS-KDHUVOMR                                           
081000     END-IF                                                               
081100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
081200     MOVE SPACE TO GODK-STATUSKODER                                       
081300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
081400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081500     PERFORM IMS-STATUSKONTROLL                                           
081600     .                                                                    
081700     EJECT                                                                
081800 IMS-STATUSKONTROLL SECTION.                                              
081900                                                                          
082000     SET STATUS-IX TO 1                                                   
082100     SEARCH GODK-STATUS                                                   
082200       AT END                                                             
082300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
082400         DELIMITED BY SIZE INTO FELTEXT                                   
082500         CALL FELLOG                                                      
082600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
082700         CONTINUE                                                         
082800     END-SEARCH                                                           
082900     .                                                                    
083000     EJECT                                                                
083100 DB2-DCL-OPN-CRS-BYLRAD SECTION.                                          
083200     EXEC SQL DECLARE BYLRAD-CRS CURSOR FOR                               
083300     SELECT DAAAVV,                                                       
083400            IDPTYP,                                                       
083500            IDDISTR,                                                      
083600            KVANTAL,                                                      
083700            KVPOINT                                                       
083800                                                                          
083900     FROM BYLRAD                                                          
084000                                                                          
084100     WHERE (DAAAVV  >= :WS-FOM-IN                                         
084200     AND    DAAAVV  <= :WS-TOM-IN)                                        
084300     AND   (IDDISTR >= :W-IDDISTR-FOM                                     
084400     AND    IDDISTR <= :W-IDDISTR-TOM)                                    
084500                                                                          
084600     ORDER BY IDDISTR,                                                    
084700              DAAAVV                                                      
084800                                                                          
084900     OPTIMIZE FOR 100 ROWS                                                
085000     END-EXEC                                                             
085100                                                                          
085200     MOVE 000            TO GODK-SQLCODEKODER                             
085300     EXEC SQL OPEN BYLRAD-CRS END-EXEC                                    
085400     MOVE SQLCODE        TO SQLCODE-WS                                    
085500     PERFORM DB2-STATUSKONTROLL                                           
085600     .                                                                    
085700     EJECT                                                                
085800 DB2-FETCH-BYLRAD SECTION.                                                
085900     MOVE 000100         TO GODK-SQLCODEKODER                             
086000     EXEC SQL FETCH BYLRAD-CRS INTO                                       
086100            :BYLRAD-DAAAVV,                                               
086200            :BYLRAD-IDPTYP,                                               
086300            :BYLRAD-IDDISTR,                                              
086400            :BYLRAD-KVANTAL,                                              
086500            :BYLRAD-KVPOINT                                               
086600     END-EXEC                                                             
086700     MOVE SQLCODE        TO SQLCODE-WS                                    
086800     PERFORM DB2-STATUSKONTROLL                                           
086900     .                                                                    
087000     EJECT                                                                
087100 DB2-CLOSE-BYLRAD-CRS SECTION.                                            
087200     SKIP2                                                                
087300     EXEC SQL CLOSE BYLRAD-CRS END-EXEC                                   
087400     .                                                                    
087500     EJECT                                                                
087600 DB2-STATUSKONTROLL  SECTION.                                             
087700                                                                          
087800     SET SQLCODE-IX TO 1                                                  
087900     SEARCH GODK-SQLCODE                                                  
088000       AT END CALL FELLOG                                                 
088100       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
088200     END-SEARCH                                                           
088300     .                                                                    
