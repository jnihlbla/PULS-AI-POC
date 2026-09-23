000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2010700.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   99/11/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FÖRSÄLJNINGSSTATISTIK                                            
000900*        KOPIERAT STOMMEN FRÅN W2035600                                   
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLUSEA (WDP7)                              
001200*        PROGRAMMET LÄSER      WDL8                                       
001300*                              WDD3                                       
001400*                              WDK6                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T107                                              
001900*        MID:         W2I10701                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O10701                                            
002300*                                                                         
002400*   ÄNDRINGAR:                                                            
002500*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002600*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002700*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
002800*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W2010700'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  IX-AR                       PIC 9(9)    VALUE ZERO  COMP-3.          
004600 77  IX-PER                      PIC 9(9)    VALUE ZERO  COMP-3.          
004700 77  IX-FRAN-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
004800 77  IX-TILL-AR                  PIC 9(9)    VALUE ZERO  COMP-3.          
004900 77  IX-RULL                     PIC 9(9)    VALUE ZERO  COMP-3.          
005000 77  SPRAK-IX                    PIC 9(9)    VALUE ZERO  COMP-3.          
005100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 77  DAGENS-PER                  PIC  9(4)   VALUE ZERO.                  
005300 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
005400 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
005600 01  FILLER                      PIC X(04)   VALUE 'WS'.                  
005700 01  WS.                                                                  
005800  05 WS-DAGENS-AAAAMMDD          PIC 9(8)    VALUE ZERO.                  
005900  05 FILLER                      PIC X(08)   VALUE 'WS-AAPP'.             
006000  05 WS-AAPP                     PIC 9(4)    VALUE ZERO.                  
006100  05 FILLER REDEFINES            WS-AAPP.                                 
006200   10 WS-AA                      PIC 9(2).                                
006300   10 WS-PP                      PIC 9(2).                                
006400  05 WS-VAL                      PIC X(1)    VALUE SPACE.                 
006500  05 WS-AARTAL                   PIC 9(4)    VALUE ZERO.                  
006600  05 WS-PER                      PIC 9(2)    VALUE ZERO.                  
006700  05 WS-KVOI-RED                 PIC 9(7)    VALUE ZERO.                  
006800  05 WS-TOTAL-KVOI               PIC 9(7)    VALUE ZERO.                  
007000  05 WS-KVOI-PER         OCCURS 6 TIMES.                                  
007100   10 WS-KVOI-TOT-TAB            PIC 9(7)        VALUE ZERO.              
007200   10 WS-KVOI-TAB        OCCURS 12 TIMES                                  
007300                                 PIC 9(7)        VALUE ZERO.              
007500  05 WS-VV                       PIC  9(2)   VALUE ZERO.                  
007600  05 FILLER                      PIC  X(16)  VALUE 'WS-TABELL'.           
007700  05  WS-TABELL    OCCURS 12.                                             
007800   10 WS-FORSTA-V                PIC  9(2)   VALUE ZERO.                  
007900   10 WS-SISTA-V                 PIC  9(2)   VALUE ZERO.                  
008000   10 WS-KVVIPER                 PIC 9       VALUE ZERO.                  
008100   10 WS-KVOI                    PIC S9(7)   VALUE ZERO COMP-3.           
008200  05 WS-TESTFAELT.                                                        
008300   10 WS-FORSTA-TF               PIC  9(2)   VALUE ZERO.                  
008400   10 FILLER                     PIC  X      VALUE SPACE.                 
008500   10 WS-SISTA-TF                PIC  9(2)   VALUE ZERO.                  
008600   10 FILLER                     PIC  X      VALUE SPACE.                 
008700   10 WS-KVOI-TF                 PIC  9(7)   VALUE ZERO.                  
008800                                                                          
008900                                                                          
009000                                                                          
009100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009200                                                                          
009300 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
009400                                                                          
009500                                                                          
009600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009700     88  INDATA-OK                           VALUE 'J'.                   
009800     88  INDATA-FEL                          VALUE 'N'.                   
009900                                                                          
010000 77  SECURITY-SW                 PIC X       VALUE 'N'.                   
010100     88  PASSED-SECURITY-CHECK               VALUE 'J'.                   
010200     88  BLOCKED-SECURITY-CHECK              VALUE 'N'.                   
010300                                                                          
010400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
010500     88  NYCKLAR-OK                          VALUE 'J'.                   
010600     88  NYCKLAR-FEL                         VALUE 'N'.                   
010700                                                                          
010800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010900     88  EGEN-MID                            VALUE '2107'.                
011000     88  GODK-MID                            VALUE '2351' '2352'          
011100                                                   '2353' '2354'          
011200                                                   '2355' '2356'          
011300                                                   '2357' '2358'          
011400                                                   '2359'.                
011500     88  HELP-MID                            VALUE '0551'.                
011600     EJECT                                                                
011700*                                                                         
011800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011900 01  GENERELLA-SUBPROGRAM.                                                
012000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012700*01  -COPY WDATAREA                                                       
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013000*01 -COPY WMEDAREA                                                        
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'DISTRIKT-TAB'.        
013300     SKIP3                                                                
013400 01  MESSAGE-CODES.                                                       
013500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013800     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
013900     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
014000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014100     03  ARTIKEL-ERSATT          PIC X(3)    VALUE '220'.                 
014200     03  PRIS-SAKNAS             PIC X(3)    VALUE '301'.                 
014300     03  DIREKTLEV               PIC X(3)    VALUE '306'.                 
014400     03  EJ-GODK-REFILL          PIC X(3)    VALUE '307'.                 
014500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
014700                                                                          
014800 01  MEDDELANDE.                                                          
014900     03  MED-1                  PIC X(30)                                 
014600         VALUE 'TYPE : T,P,D,S,N,E,R OR L     '.                          
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015500     SKIP3                                                                
015600*01 -COPY WMSGINIT                                                        
015700     SKIP3                                                                
015800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016100     SKIP3                                                                
016200*01  MID -COPY W2I10701                                                   
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016500     SKIP3                                                                
016600*01  -COPY WMSGAREA                                                       
016700     EJECT                                                                
016800     03  MOD REDEFINES MSG-AREA.                                          
016900*      05  -COPY W2O10701                                                 
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017200     SKIP3                                                                
017300*01  -COPY WMFSAREA                                                       
017400     EJECT                                                                
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600*                                                                         
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017900     SKIP3                                                                
018000 01  NYCKLAR-TILL-DLI.                                                    
018100     03  W-IDARTNR-X.                                                     
018200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018300     03  W-IDUSER-X.                                                      
018400         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
018500     03  W-IDSKYLT-X.                                                     
018600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
018700     03  W-TIAAAA-X.                                                      
018400         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
019100     SKIP2                                                                
019200*    --- STATUS-KOD FRÅN IMS                                              
019300 01  STATUS-WS                   PIC XX.                                  
019400     88  SEGMENT-FINNS                       VALUE '  '.                  
019500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019600     88  SEGMENT-SAKNAS                      VALUE 'GE'                   
019700                                                   'GB'.                  
019800     SKIP2                                                                
019900 01  GODK-STATUSKODER.                                                    
020000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020100     SKIP3                                                                
020200 01  SSA1                        PIC X(64).                               
020300 01  SSA2                        PIC X(64).                               
020400     EJECT                                                                
020500*    --- IMS FUNKTIONSKODER                                               
020600*01  -COPY W0003                                                          
020700     EJECT                                                                
020800*    ---  DLI INPUT-OUTPUT AREA                                           
020900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL801'.             
021000     SKIP3                                                                
021100 01  DLI-IO-AREA-WDL801.                                                  
021200*        05  -COPY WDL801                                                 
021300     EJECT                                                                
021400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
021500     SKIP3                                                                
021600 01  DLI-IO-AREA-WDL811.                                                  
021700*        05  -COPY WDL811                                                 
021800     EJECT                                                                
023100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD301'.             
023200     SKIP3                                                                
023300 01  DLI-IO-AREA-WDD301.                                                  
023400*        05  -COPY WDD301  -PRE BENA-                                     
023500     EJECT                                                                
023600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD311'.             
023700     SKIP3                                                                
023800 01  DLI-IO-AREA-WDD311.                                                  
023900*        05  -COPY WDD311  -PRE BENA-                                     
024000     EJECT                                                                
024100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
024200     SKIP3                                                                
024300 01  DLI-IO-WDK601.                                                       
024400*        05  -COPY WDK601 .                                               
022700     EJECT                                                                
025200 LINKAGE SECTION.                                                         
025300                                                                          
025400*01  -COPY W0009   -PRE MSG-                                              
025500     EJECT                                                                
025600*01  -COPY W0008  -PRE  USEA-                                             
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900*01  -COPY W0008  -PRE  WDL8-                                             
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200*01  -COPY W0008  -PRE  WDD3-                                             
026300     05  FILLER                  PIC X.                                   
026400     EJECT                                                                
026500*01  -COPY W0008  -PRE  WDK6-                                             
026600     05  FILLER                  PIC X.                                   
026700     EJECT                                                                
027500 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDL8-PCB                      
024500     WDD3-PCB WDK6-PCB.                                                   
027700 MAIN SECTION.                                                            
027800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDL8-PCB                      
024800     WDD3-PCB WDK6-PCB.                                                   
028000                                                                          
028100     PERFORM IMS-GET-MSG                                                  
028200     IF SEGMENT-FINNS                                                     
028300       PERFORM A-INIT                                                     
028400       PERFORM B-KOLLA-NYCKLAR                                            
028500                                                                          
028600       PERFORM SEC-URITY                                                  
028700       IF MED-IDMFSFEL = ARTIKEL-SAKNAS                                   
028800         CONTINUE                                                         
028900       ELSE                                                               
029000         IF PASSED-SECURITY-CHECK                                         
029100           IF NYCKLAR-OK                                                  
029200             PERFORM F-LAES-VISA-INFO                                     
029700           END-IF                                                         
029800         END-IF                                                           
029900       END-IF                                                             
030000                                                                          
030100       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O10701 + 4                      
030200       PERFORM IMS-INSERT-MSG                                             
030300     END-IF                                                               
030400                                                                          
030500     MOVE ZERO TO RETURN-CODE                                             
030600     GOBACK                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 A-INIT SECTION.                                                          
031000                                                                          
031100     IF MSG-DUBBLA-TRANSKODER                                             
031200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I10701                 
031300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031500     ELSE                                                                 
031600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I10701                  
031700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031900     END-IF                                                               
032000                                                                          
032100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032400                                                                          
032500     MOVE LOW-VALUE TO MSG-AREA                                           
032600     MOVE 'W2O107N1' TO MFS-IDMOD                                         
032700     MOVE '2107' TO MOD-IDTRANS                                           
032800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
032900                                                                          
033000     IF EGEN-MID OR HELP-MID                                              
033100       CONTINUE                                                           
033200     ELSE                                                                 
033300       MOVE SPACE TO MFS-KDTRTYP                                          
033400       MOVE '7' TO MFS-IDPFK                                              
033500     END-IF                                                               
033600                                                                          
033700     IF ENGLISH-TEXT                                                      
033800       MOVE +2    TO SPRAK-IX                                             
033900       MOVE 'GB ' TO MED-IDSKYLT                                          
034000     ELSE                                                                 
034100       MOVE +1    TO SPRAK-IX                                             
034200       MOVE 'S  ' TO MED-IDSKYLT                                          
034300     END-IF                                                               
034400                                                                          
034500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
034600     ACCEPT DAGENS-DATUM FROM DATE                                        
034700                                                                          
034800     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
034900     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
035000                                                                          
035100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
035200                     DAT-O-TIDATUM DAT-KDSVAR                             
035300                                                                          
035400     IF DAT-KDSVAR-OK                                                     
035500****             HÄMTA SEKELSIFFROR                                       
035600                                                                          
035700       MOVE DAT-TISEKEL      TO DAGENS-AAR(1:2)                           
035800       MOVE DAT-TIAARP       TO DAGENS-PER                                
035900       MOVE DAGENS-PER(3:2)  TO WS-PER                                    
036000                                                                          
036100     ELSE                                                                 
036200         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
036300         DELIMITED BY SIZE INTO FELTEXT                                   
036400         CALL FELLOG                                                      
036500     END-IF                                                               
036600                                                                          
036700     MOVE DAGENS-DATUM(1:2)   TO DAGENS-AAR(3:2)                          
036800                                                                          
036900     IF EGEN-MID                                                          
037000       CONTINUE                                                           
037100     ELSE                                                                 
037200       MOVE 'T'              TO MID-VAL-IN                                
037300       MOVE SPACE TO MFS-KDTRTYP                                          
037400       MOVE '7' TO MFS-IDPFK                                              
037500     END-IF                                                               
037600                                                                          
037700     .                                                                    
037800     EJECT                                                                
037900 B-KOLLA-NYCKLAR SECTION.                                                 
038000                                                                          
038100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
038200     MOVE '001'             TO MSGI-KDCALL                                
038300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
038400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
038500     MOVE '2107'            TO MSGI-IDTRANS                               
038600     IF EGEN-MID                                                          
038700       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
038800     ELSE                                                                 
038900       IF  MID-IDARTNR-IN NUMERIC                                         
039000       AND MID-IDARTNR-IN > ZERO                                          
039100         MOVE MID-IDARTNR-IN                                              
039200                            TO MSGI-IDARTNR                               
039300       END-IF                                                             
039400     END-IF                                                               
039500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
039600                                                                          
039700     IF MSGI-IDLAND-SPR = 'SE'                                            
039800        MOVE '0'             TO MFS-KDHUVOMR                              
039900        MOVE 'S  '          TO W-IDSKYLT                                  
040000     ELSE                                                                 
040100        MOVE 'GB '          TO W-IDSKYLT                                  
040200     END-IF                                                               
040300                                                                          
040400     MOVE JA TO NYCKLAR-SW                                                
040500                                                                          
040600*    -- KONTROLL AV IDARTNR                                               
040700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
040800                                                                          
040900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
041000       MOVE '7'         TO MFS-IDPFK                                      
041100       MOVE SPACE       TO MFS-KDTRTYP                                    
041200     END-IF                                                               
041300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
041400     IF MSGI-IDARTNR NUMERIC                                              
041500       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
041600     ELSE                                                                 
041700       MOVE NEJ TO NYCKLAR-SW                                             
041800     END-IF                                                               
041900                                                                          
042000     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
042100                                                                          
042200*    -- KONTROLL AV VAL                                                   
042300     MOVE MFS-RENSA-FAELT    TO MOD-VAL-IN                                
042400                                                                          
042500     IF MID-VAL-IN = ALL '+'                                              
042600       IF MID-VAL-UT = 'TOTALT'                                           
042700         MOVE 'T'            TO WS-VAL                                    
042800       END-IF                                                             
042900       IF MID-VAL-UT = 'PROGNOS'                                          
043000         MOVE 'P'            TO WS-VAL                                    
043100       END-IF                                                             
043200       IF MID-VAL-UT = 'DIVERSE'                                          
043300         MOVE 'D'            TO WS-VAL                                    
043400       END-IF                                                             
043500       IF MID-VAL-UT = 'NDC    '                                          
043600         MOVE 'N'            TO WS-VAL                                    
043700       END-IF                                                             
043800       IF MID-VAL-UT = 'SDC    '                                          
043900         MOVE 'E'            TO WS-VAL                                    
044000       END-IF                                                             
044100       IF MID-VAL-UT = 'SATS   '                                          
044200         MOVE 'S'            TO WS-VAL                                    
044300       END-IF                                                             
044400       IF MID-VAL-UT = 'LEDTID '                                          
044500         MOVE 'L'            TO WS-VAL                                    
044600       END-IF                                                             
044700       IF MID-VAL-UT = 'REFILL '                                          
044800         MOVE 'R'            TO WS-VAL                                    
044900       END-IF                                                             
045300     ELSE                                                                 
045400       MOVE MID-VAL-IN TO WS-VAL                                          
045500       MOVE '7'              TO MFS-IDPFK                                 
045600       MOVE SPACE            TO MFS-KDTRTYP                               
045700     END-IF                                                               
045800                                                                          
045900     IF WS-VAL = 'T'                                                      
046000     OR WS-VAL = 'P'                                                      
046100     OR WS-VAL = 'D'                                                      
046200     OR WS-VAL = 'N'                                                      
046300     OR WS-VAL = 'E'                                                      
046400     OR WS-VAL = 'S'                                                      
046500     OR WS-VAL = 'L'                                                      
046600     OR WS-VAL = 'R'                                                      
046800       IF WS-VAL = 'T'                                                    
046900          MOVE 'TOTALT'      TO MOD-VAL-UT                                
047000       END-IF                                                             
047100       IF WS-VAL = 'P'                                                    
047200          MOVE 'PROGNOS'     TO MOD-VAL-UT                                
047300       END-IF                                                             
047400       IF WS-VAL = 'D'                                                    
047500          MOVE 'DIVERSE'     TO MOD-VAL-UT                                
047600       END-IF                                                             
047700       IF WS-VAL = 'N'                                                    
047800          MOVE 'NDC    '     TO MOD-VAL-UT                                
047900       END-IF                                                             
048000       IF WS-VAL = 'E'                                                    
048100          MOVE 'SDC    '     TO MOD-VAL-UT                                
048200       END-IF                                                             
048300       IF WS-VAL = 'S'                                                    
048400          MOVE 'SATS   '     TO MOD-VAL-UT                                
048500       END-IF                                                             
048600       IF WS-VAL = 'L'                                                    
048700          MOVE 'LEDTID '     TO MOD-VAL-UT                                
048800       END-IF                                                             
048900       IF WS-VAL = 'R'                                                    
049000          MOVE 'REFILL '     TO MOD-VAL-UT                                
049400       END-IF                                                             
049500     ELSE                                                                 
049600       MOVE MED-1            TO MOD-TEMFSINF                              
049700       MOVE NEJ TO NYCKLAR-SW                                             
049800     END-IF                                                               
049900                                                                          
050000     MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                              
050100                              W-IDARTNR                                   
050200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
050300                                                                          
050400     IF NYCKLAR-FEL                                                       
050500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
050600       CALL WMEDKONV USING MED-WMEDAREA                                   
050700       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
050800       PERFORM MFS-RENSA-FAELT-UT                                         
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 F-LAES-VISA-INFO SECTION.                                                
051300                                                                          
051400     PERFORM FA-HAEMTA-BENAEMNING                                         
051500                                                                          
051600     PERFORM FB-HAMTA-VV-I-PER                                            
051700                                                                          
051800     MOVE DAGENS-AAR         TO MOD-AARTAL (6)                            
051900                                WS-AARTAL                                 
052000     SUBTRACT 1 FROM WS-AARTAL                                            
052100     MOVE WS-AARTAL          TO MOD-AARTAL (5)                            
052200     SUBTRACT 1 FROM WS-AARTAL                                            
052300     MOVE WS-AARTAL          TO MOD-AARTAL (4)                            
052400     SUBTRACT 1 FROM WS-AARTAL                                            
052500     MOVE WS-AARTAL          TO MOD-AARTAL (3)                            
052600     SUBTRACT 1 FROM WS-AARTAL                                            
052700     MOVE WS-AARTAL          TO MOD-AARTAL (2)                            
052800     SUBTRACT 1 FROM WS-AARTAL                                            
052900     MOVE WS-AARTAL          TO MOD-AARTAL (1)                            
053000                                                                          
048900     PERFORM FC-HAEMTA-HISTORIK                                           
053200                                                                          
053300     MOVE +1                 TO IX-AR                                     
053400                                IX-PER                                    
053500     PERFORM UNTIL IX-AR > +6                                             
053600       PERFORM UNTIL IX-PER > +12                                         
053700         MOVE WS-KVOI-TAB (IX-AR, IX-PER)                                 
053800                             TO MOD-KVOI (IX-AR, IX-PER)                  
053900         ADD +1              TO IX-PER                                    
054000       END-PERFORM                                                        
054100                                                                          
054200       MOVE WS-KVOI-TOT-TAB (IX-AR)                                       
054300                             TO MOD-KVOI-TOT (IX-AR)                      
054400       ADD +1                TO IX-AR                                     
054500       MOVE +1               TO IX-PER                                    
054600     END-PERFORM                                                          
054700     .                                                                    
054800     EJECT                                                                
054900 FA-HAEMTA-BENAEMNING SECTION.                                            
055000                                                                          
055100     PERFORM IMS-GU-D301                                                  
055200     IF SEGMENT-FINNS                                                     
055300       PERFORM IMS-GNP-D311                                               
055400       IF SEGMENT-FINNS                                                   
055500         MOVE BENA-TEXT-BEART  TO MOD-BEART-ENG                           
055600       ELSE                                                               
055700         MOVE MFS-RENSA-FAELT  TO MOD-BEART-ENG                           
055800       END-IF                                                             
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 FB-HAMTA-VV-I-PER SECTION.                                               
056300                                                                          
056400*    --- FYLL I VECKONR FÖR PERIODERNA                                    
056500     MOVE DAGENS-DATUM(1:2)  TO WS-AAPP(1:2)                              
056600     MOVE 01                 TO WS-AAPP(3:2)                              
056700     MOVE WS-AAPP            TO DAT-I-TIDATUM                             
056800     MOVE 'AARP'             TO DAT-KDDATFORM                             
056900     MOVE 1                  TO WS-PP                                     
057000                                                                          
057100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
057200                     DAT-O-TIDATUM DAT-KDSVAR                             
057300                                                                          
057400     IF DAT-KDSVAR-OK                                                     
057500                                                                          
057600       MOVE DAT-KVVIPER      TO WS-KVVIPER(WS-PP)                         
057700       MOVE 1                TO WS-FORSTA-V(WS-PP)                        
057800                                                                          
057900     ELSE                                                                 
058000         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
058100         DELIMITED BY SIZE INTO FELTEXT                                   
058200         CALL FELLOG                                                      
058300     END-IF                                                               
058400                                                                          
058500     PERFORM UNTIL WS-PP      >  12                                       
058600       ADD 1                  TO WS-PP                                    
058700       IF WS-PP = 13                                                      
058800         MOVE 53             TO WS-SISTA-V(12)                            
058900       ELSE                                                               
059000                                                                          
059100         MOVE WS-AAPP         TO DAT-I-TIDATUM                            
059200         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
059300                             DAT-O-TIDATUM DAT-KDSVAR                     
059400         IF DAT-KDSVAR-OK                                                 
059500             COMPUTE WS-SISTA-V(WS-PP - 1) = DAT-TIVV - 1                 
059600             IF WS-PP > 1                                                 
059700               MOVE DAT-TIVV  TO WS-FORSTA-V(WS-PP)                       
059800               MOVE DAT-KVVIPER TO WS-KVVIPER(WS-PP)                      
059900             END-IF                                                       
060000         ELSE                                                             
060100           MOVE 'FELAKTIGT DATUM - DATKONV3' TO FELTEXT                   
060200           CALL FELLOG                                                    
060300         END-IF                                                           
060400       END-IF                                                             
060500     END-PERFORM                                                          
060600     .                                                                    
060700     EJECT                                                                
056600 FC-HAEMTA-HISTORIK SECTION.                                              
060900                                                                          
061000     MOVE WS-DAGENS-AAAAMMDD (1:4)                                        
061100                               TO W-TIAAAA                                
061200     PERFORM IMS-GU-L811                                                  
061300                                                                          
061400     IF SEGMENT-FINNS                                                     
061500                                                                          
061600       MOVE +6                 TO IX-AR                                   
061700       MOVE +1                 TO IX-PER                                  
061800       MOVE ZERO               TO WS-TOTAL-KVOI                           
061900                                                                          
062000       PERFORM UNTIL IX-PER    =  WS-PER                                  
062100         MOVE ZERO             TO WS-KVOI (IX-PER)                        
062200         MOVE WS-FORSTA-V(IX-PER)                                         
062300                               TO WS-VV                                   
062400         PERFORM UNTIL WS-VV   >  WS-SISTA-V(IX-PER)                      
062500           IF WS-VAL = 'T'                                                
062600             ADD AAR-KVOI-PROG(WS-VV)                                     
062700                               TO WS-KVOI(IX-PER)                         
062800             ADD AAR-KVOI-NDC(WS-VV)                                      
062900                               TO WS-KVOI(IX-PER)                         
063000             ADD AAR-KVOI-SDC(WS-VV)                                      
063100                               TO WS-KVOI(IX-PER)                         
063200             ADD AAR-KVOI-SATS(WS-VV)                                     
063300                               TO WS-KVOI(IX-PER)                         
063400             ADD AAR-KVOI-DIV(WS-VV)                                      
063500                               TO WS-KVOI(IX-PER)                         
063600           END-IF                                                         
063700           IF WS-VAL = 'P'                                                
063800             ADD AAR-KVOI-PROG(WS-VV)                                     
063900                               TO WS-KVOI(IX-PER)                         
064000           END-IF                                                         
064100           IF WS-VAL = 'D'                                                
064200             ADD AAR-KVOI-DIV(WS-VV)                                      
064300                               TO WS-KVOI(IX-PER)                         
064400           END-IF                                                         
064500           IF WS-VAL = 'N'                                                
064600             ADD AAR-KVOI-NDC(WS-VV)                                      
064700                               TO WS-KVOI(IX-PER)                         
064800           END-IF                                                         
064900           IF WS-VAL = 'E'                                                
065000             ADD AAR-KVOI-SDC(WS-VV)                                      
065100                               TO WS-KVOI(IX-PER)                         
065200           END-IF                                                         
065300           IF WS-VAL = 'R'                                                
065400             ADD AAR-KVOI-REFILL(WS-VV)                                   
065500                               TO WS-KVOI(IX-PER)                         
065600           END-IF                                                         
065700           IF WS-VAL = 'S'                                                
065800             ADD AAR-KVOI-SATS(WS-VV)                                     
065900                               TO WS-KVOI(IX-PER)                         
066000           END-IF                                                         
066100           IF WS-VAL = 'L'                                                
066200             ADD AAR-KVOI-LEDTID(WS-VV)                                   
066300                               TO WS-KVOI(IX-PER)                         
066400           END-IF                                                         
067800           ADD +1              TO WS-VV                                   
067900         END-PERFORM                                                      
068114         COMPUTE WS-KVOI-RED       ROUNDED =                              
062600                 WS-KVOI (IX-PER)                                         
062700                            / WS-KVVIPER (IX-PER) * +4.33                 
062800                                                                          
068400         ADD WS-KVOI-RED       TO WS-KVOI-TAB (IX-AR, IX-PER)             
068500                                  WS-TOTAL-KVOI                           
068600         ADD +1                TO IX-PER                                  
068700       END-PERFORM                                                        
068800                                                                          
068900       ADD WS-TOTAL-KVOI       TO WS-KVOI-TOT-TAB (IX-AR)                 
069000     END-IF                                                               
069100                                                                          
069200     MOVE +5                   TO IX-AR                                   
069300     PERFORM UNTIL IX-AR < +1                                             
069400                                                                          
069500*  LÄGG UT HISTORIK FÖR FÖREGÅENDE ÅR OCH TIDIGARE                        
069600                                                                          
069700       SUBTRACT 1              FROM W-TIAAAA                              
069800       PERFORM IMS-GU-L811                                                
069900                                                                          
070000       IF SEGMENT-FINNS                                                   
070100         MOVE 1                TO IX-PER                                  
070200         MOVE ZERO             TO WS-TOTAL-KVOI                           
070300         PERFORM UNTIL IX-PER > 12                                        
070400                                                                          
070500           MOVE ZERO           TO WS-KVOI (IX-PER)                        
070600           MOVE WS-FORSTA-V (IX-PER)                                      
070700                               TO WS-VV                                   
070800           PERFORM UNTIL WS-VV > WS-SISTA-V (IX-PER)                      
070900             IF WS-VAL = 'T'                                              
071000               ADD AAR-KVOI-PROG(WS-VV)                                   
071100                                 TO WS-KVOI(IX-PER)                       
071200               ADD AAR-KVOI-NDC(WS-VV)                                    
071300                                 TO WS-KVOI(IX-PER)                       
071400               ADD AAR-KVOI-SDC(WS-VV)                                    
071500                                 TO WS-KVOI(IX-PER)                       
071600               ADD AAR-KVOI-SATS(WS-VV)                                   
071700                                 TO WS-KVOI(IX-PER)                       
071800               ADD AAR-KVOI-DIV(WS-VV)                                    
071900                                 TO WS-KVOI(IX-PER)                       
072000             END-IF                                                       
072100             IF WS-VAL = 'P'                                              
072200               ADD AAR-KVOI-PROG(WS-VV)                                   
072300                                 TO WS-KVOI(IX-PER)                       
072400             END-IF                                                       
072500             IF WS-VAL = 'D'                                              
072600               ADD AAR-KVOI-DIV(WS-VV)                                    
072700                                 TO WS-KVOI(IX-PER)                       
072800             END-IF                                                       
072900             IF WS-VAL = 'N'                                              
073000               ADD AAR-KVOI-NDC(WS-VV)                                    
073100                                 TO WS-KVOI(IX-PER)                       
073200             END-IF                                                       
073300             IF WS-VAL = 'E'                                              
073400               ADD AAR-KVOI-SDC(WS-VV)                                    
073500                                 TO WS-KVOI(IX-PER)                       
073600             END-IF                                                       
073700             IF WS-VAL = 'R'                                              
073800               ADD AAR-KVOI-REFILL(WS-VV)                                 
073900                                 TO WS-KVOI(IX-PER)                       
074000             END-IF                                                       
074100             IF WS-VAL = 'S'                                              
074200               ADD AAR-KVOI-SATS(WS-VV)                                   
074300                                 TO WS-KVOI(IX-PER)                       
074400             END-IF                                                       
074500             IF WS-VAL = 'L'                                              
074600               ADD AAR-KVOI-LEDTID(WS-VV)                                 
074700                                 TO WS-KVOI(IX-PER)                       
074800             END-IF                                                       
076200             ADD 1             TO WS-VV                                   
076300           END-PERFORM                                                    
076400                                                                          
076634           COMPUTE WS-KVOI-RED ROUNDED =                                  
069800              WS-KVOI (IX-PER) * +4.33                                    
069900                 / WS-KVVIPER (IX-PER)                                    
076800                                                                          
076910           ADD WS-KVOI-RED     TO WS-KVOI-TAB (IX-AR, IX-PER)             
077000                                 WS-TOTAL-KVOI                            
077100           ADD +1              TO IX-PER                                  
077200         END-PERFORM                                                      
077300                                                                          
077400         ADD WS-TOTAL-KVOI     TO WS-KVOI-TOT-TAB (IX-AR)                 
077500       END-IF                                                             
077600                                                                          
077700       SUBTRACT +1             FROM IX-AR                                 
077800     END-PERFORM                                                          
077900     .                                                                    
078000     EJECT                                                                
105300                                                                          
105400 SEC-URITY SECTION.                                                       
105500     SKIP2                                                                
105600*    --- CHECK IF USER IS GRANTED TO SEE PART-INFO                        
105700     PERFORM IMS-GU-K601                                                  
105800     IF  SEGMENT-FINNS                                                    
105900        MOVE ART-IDLEVNR         TO WS-IDLEVNR-8                          
106000        IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                         
106100        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
106200*         --- BEHÖRIG USER                                                
106300          SET PASSED-SECURITY-CHECK TO TRUE                               
106400        ELSE                                                              
106500*         --- OBEHÖRIG USER / USER NOT AUTHORIZED                         
106600            MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                       
106700            CALL WMEDKONV USING MED-WMEDAREA                              
106800            MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                             
106900        END-IF                                                            
107000     ELSE                                                                 
107100       MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                                
107200       CALL WMEDKONV USING MED-WMEDAREA                                   
107300       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
107400       PERFORM MFS-RENSA-FAELT-UT                                         
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800                                                                          
107900 MFS-RENSA-FAELT-UT SECTION.                                              
108000                                                                          
108100*    --- ALLA UTDATA-FÄLT                                                 
108200     MOVE MFS-RENSA-FAELT TO MOD-BEART-ENG                                
108300                                                                          
108400     MOVE +1                 TO IX-AR                                     
108500                                IX-PER                                    
108600     PERFORM UNTIL IX-AR > +6                                             
108700       MOVE MFS-RENSA-FAELT  TO MOD-AARTAL (IX-AR)                        
108800                                MOD-KVOI-TOT (IX-AR)                      
108900       PERFORM UNTIL IX-PER > +12                                         
109000         MOVE MFS-RENSA-FAELT                                             
109100                             TO MOD-KVOI (IX-AR, IX-PER)                  
109200         ADD +1              TO IX-PER                                    
109300       END-PERFORM                                                        
109400       ADD +1                TO IX-AR                                     
109500       MOVE +1               TO IX-PER                                    
109600     END-PERFORM                                                          
109700     .                                                                    
109800     EJECT                                                                
109900* --- IMS SEKTIONER ---                                                   
110000     SKIP3                                                                
110100 IMS-GET-MSG SECTION.                                                     
110200                                                                          
110300     MOVE '  QC' TO GODK-STATUSKODER                                      
110400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
110500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110600     PERFORM IMS-STATUSKONTROLL                                           
110700     .                                                                    
110800     SKIP3                                                                
110900 IMS-INSERT-MSG SECTION.                                                  
111000                                                                          
111100     IF ENGLISH-TEXT                                                      
111200       MOVE 'N' TO MFS-KDHUVOMR                                           
111300     END-IF                                                               
111400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
111500     MOVE SPACE TO GODK-STATUSKODER                                       
111600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
111700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111800     PERFORM IMS-STATUSKONTROLL                                           
111900     .                                                                    
112000     EJECT                                                                
112100 IMS-GU-L811  SECTION.                                                    
112200                                                                          
112300     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
112400          DELIMITED BY SIZE INTO SSA1                                     
112500     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
112600          DELIMITED BY SIZE INTO SSA2                                     
112700     MOVE '  GE' TO GODK-STATUSKODER                                      
112800     CALL CBLTDLI USING GU WDL8-PCB                                       
112900                                 DLI-IO-AREA-WDL811 SSA1 SSA2             
113000     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
113100     PERFORM IMS-STATUSKONTROLL                                           
113200     .                                                                    
113300     EJECT                                                                
116100 IMS-GU-D301 SECTION.                                                     
116200                                                                          
116300     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
116400          DELIMITED BY SIZE INTO SSA1                                     
116500     MOVE '  GE' TO GODK-STATUSKODER                                      
116600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA-WDD301 SSA1               
116700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
116800     PERFORM IMS-STATUSKONTROLL                                           
116900     .                                                                    
117000     SKIP3                                                                
117100 IMS-GNP-D311 SECTION.                                                    
117200                                                                          
117300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
117400          DELIMITED BY SIZE INTO SSA1                                     
117500     MOVE '  GE' TO GODK-STATUSKODER                                      
117600     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-AREA-WDD311 SSA1              
117700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
117800     PERFORM IMS-STATUSKONTROLL                                           
117900     .                                                                    
118000     EJECT                                                                
118100 IMS-GU-K601 SECTION.                                                     
118200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
118300            DELIMITED BY SIZE INTO SSA1                                   
118400     MOVE '  GE' TO GODK-STATUSKODER                                      
118500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
118600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
118700     PERFORM IMS-STATUSKONTROLL                                           
118800     .                                                                    
118900     EJECT                                                                
123600 IMS-STATUSKONTROLL SECTION.                                              
123700                                                                          
123800     SET STATUS-IX TO 1                                                   
123900     SEARCH GODK-STATUS                                                   
124000       AT END                                                             
124100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
124200         DELIMITED BY SIZE INTO FELTEXT                                   
124300         CALL FELLOG                                                      
124400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
124500         CONTINUE                                                         
124600     END-SEARCH                                                           
124700     .                                                                    
