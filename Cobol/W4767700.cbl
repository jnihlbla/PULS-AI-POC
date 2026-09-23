000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4767700.                                                
000300*AUTHOR.         STINA MOGREN                                             
000400*DATE-WRITTEN.   2004-09-28                                               
000500*    KOPIA AV W4752B00                                                    
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        ACTIONS FOR NDC RETURNS TO THE CDC:                              
001000*        - UPDATING OF AK-PAV BALANCE     (WDK6)                          
001100*        - UPDATING OF SUPPLIER FOLLOW-UP (WDL2)                          
001200*        - UPDATING OF NEW SERIAL NO                                      
001300*          RECEIVING REPORT               (W6G1)                          
001400*        - UPDATING OF BALANCE CHANGE     (WDL9)                          
001500*                                                                         
001600*    ABENDKODER:                                                          
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
002800*          --- INVOICE INFO                                               
002900     SELECT W47677                     ASSIGN TO W47677D1.                
003000     SELECT W476UT                     ASSIGN TO W47677D2.                
003100     SELECT W4762B                     ASSIGN TO W47677D3.                
003200     SELECT W4762C                     ASSIGN TO W47677D4.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W47677                                                               
003900     RECORDING       V                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  IN-FAKT-RAD.                                                         
004300*03  -COPY W418REFB     -L.                                               
004400     EJECT                                                                
004500 FD  W476UT                                                               
004600     RECORDING       V                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  UT-FAKT-RAD.                                                         
005000*03  -COPY W418REFB     -L.                                               
005100     EJECT                                                                
005200 FD  W4762B                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600 01  UT-FAKT-RAD2.                                                        
005700*03  -COPY W418REF      -L.                                               
005800     EJECT                                                                
005900 FD  W4762C                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  SAPPOST -COPY W51060 -PRE  UT-  -L.                                  
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600     SKIP2                                                                
006700*    -- CHECKED BY WY2000                                                 
006800     SKIP3                                                                
006900 77  IDPGM                      PIC X(8)    VALUE 'W4767700'.             
007000 77  JA                         PIC X       VALUE 'J'.                    
007100 77  NEJ                        PIC X       VALUE 'N'.                    
007200     SKIP2                                                                
007300 77  MAX-TAB-IX                 PIC S9(4)   VALUE +10 COMP SYNC.          
007400 01  POST-ANT                   PIC S9(7)   VALUE ZERO COMP-3.            
007500 01  MAX-POST                   PIC S9(7)   VALUE 750  COMP-3.            
008100 01  TEST-IDDISTR           PIC  9(5) COMP-3 VALUE ZERO.                  
008200     SKIP2                                                                
008300*01  FILLER  -COPY WWDIST18    -RED TEST-IDDISTR.                         
008400     EJECT                                                                
008500*01  FILLER  -COPY WWDIST79    -RED TEST-IDDISTR.                         
008600     EJECT                                                                
008700*      --- VALID IDDC CODES                                               
008800*                                                                         
008900*01    -COPY WWDCKONS                                                     
009000       EJECT                                                              
009400 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
009500 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
009600                                                                          
009700 01  FILLER.                                                              
009800     03  W-IDLEVNR-PIC9          PIC 9(5).                                
009900                                                                          
010000 01  W-IDDCTEXT-MSGI.                                                     
010100     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
010200     03  W-IDDC-MSGI         PIC X(2).                                    
010300     SKIP2                                                                
010400 01  FILLER          PIC X(16)   VALUE 'DC-ZON-TAB-START'.                
010500     SKIP2                                                                
010600 01  TABELL-DC-ZON.                                                       
010700   03  DC-ZON-TAB.                                                        
010800       05  FILLER         PIC X(100)                                      
010810           VALUE '11XX00000021XX00000024XX00000025XX00000026XX0000        
010820-                '0041XX00000043XX00000051XX00000061XX00000062XX00        
010830-                '0000'.                                                  
011200   03  RE-DC-ZON   REDEFINES DC-ZON-TAB.                                  
011300       05  DC-ZON OCCURS 10                                               
011400             ASCENDING KEY TAB-IDDC                                       
011500             INDEXED BY TAB-IX.                                           
011600           07  TAB-IDDC             PIC X(2).                             
011700           07  TAB-IDTIDZON         PIC X(2).                             
011800           07  TAB-TILOKDAT         PIC X(6).                             
011900     EJECT                                                                
012000 01  WS-ARBETSAREA.                                                       
012100*    DATE + TIME  FÖR SKAPANDE AV INLEVERANSNUMMER                        
012200     03  WS-TIAAAAMMDDTTMMSSTH   PIC 9(16)  VALUE ZERO.                   
012300     03  FILLER REDEFINES WS-TIAAAAMMDDTTMMSSTH.                          
012400         05  WS-TISEKEL          PIC 9(2).                                
012500         05  WS-TIAAMMDD-DATE    PIC 9(6).                                
012600         05  WS-TTMMSSTH-TIME    PIC 9(8).                                
012700     03  WS-DAINLEV              PIC 9(16) VALUE ZERO.                    
012800                                                                          
012900*     -- FÖR REDIGERING                                                   
013000 01      WS-IDKONTO-L            PIC 9(10)   VALUE ZERO.                  
013100 01      FILLER                  REDEFINES WS-IDKONTO-L.                  
013200   03    WS-IDKONTO-L-1-4        PIC 9(4).                                
013300   03    FILLER                  PIC X(2).                                
013400   03    WS-IDKONTO-L-7          PIC 9(1).                                
013500   03    FILLER                  PIC X(1).                                
013600   03    WS-IDKONTO-L-9-10       PIC 9(2).                                
013700                                                                          
013800*     -- FÖR UPPDELNING AV FÄLTET                                         
013900 01      W-IDLKTO                PIC S9(7) COMP-3  VALUE ZERO.            
014000 01      WS-IDLKTO               PIC 9(6)    VALUE ZERO.                  
014100 01      FILLER                  REDEFINES WS-IDLKTO.                     
014200   03    WS-IDLKTO-1-4           PIC 9(4).                                
014300   03    WS-IDLKTO-5-6           PIC 9(2).                                
014400                                                                          
014500 01  W-IDLOPNRM                  PIC 9(9)    VALUE ZERO.                  
014600                                                                          
014700 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM.                                   
014800  03 FILLER                      PIC 9(1).                                
014900  03 W-VVD                       PIC 9(3).                                
015000  03 W-LLLL                      PIC 9(4).                                
015100  03 W-K                         PIC 9(1).                                
015200                                                                          
015300 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
015400     03  FLT-LGD                 PIC S9  COMP SYNC   VALUE +7.            
015500     03  VAEGNINGSTAL            PIC 9(7)        VALUE 2121212.           
015600     03  VAEGNTAL-LGD            PIC S9  COMP SYNC   VALUE +7.            
015700     03  MODUL-10-11             PIC 9(2)            VALUE 10.            
015800     03  ALT-A-B                 PIC X(1)            VALUE 'B'.           
015900     EJECT                                                                
016000 01  FELTEXT.                                                             
016100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
016200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
016300                                                                          
016400 77  W47677-EOF                  PIC X       VALUE 'N'.                   
016500     88  END-OF-W47677                       VALUE 'J'.                   
016600     EJECT                                                                
016700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
016800 01  FILLER REDEFINES DAGENS-DATUM.                                       
016900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
017000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
017100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
017200     EJECT                                                                
017300 01  WS-TID                      PIC 9(9).                                
017400 01  WS-DAGENS-DATUM             PIC 9(8).                                
017500 01  WS-TIFAKT                   PIC 9(6).                                
017600     EJECT                                                                
017700 01  DYNAMISKA-SUBPROGRAM.                                                
017800*                                                                         
017900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
018200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
018300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018400     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
018500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018600     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
018800     EJECT                                                                
018900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
019000*01 -COPY WMSGINIT                                                        
019100     EJECT                                                                
019200*    --- PARAMETRAR TILL W009CIA                                          
019300*01  -COPY W009CIA                                                        
019400     EJECT                                                                
019500*    --- PARAMETRAR TILL POSTSUM                                          
019600*01  -COPY W0005   -PRE  POSTSUM-                                         
019700     EJECT                                                                
019800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
019900*01 -COPY WDATAREA                                                        
020000     EJECT                                                                
020400 01  IN-AREA-START               PIC X(24)   VALUE                        
020500                                             'IN-AREA-START'.             
020600     SKIP2                                                                
020700 01  IN-AREA.                                                             
020800     03  FILLER -COPY W418REFB   -PRE IN-                                 
020900     EJECT                                                                
021000*                                                                         
021100 01  UT-AREA-START               PIC X(24)   VALUE                        
021200                                             'UT-AREA-START'.             
021300     SKIP2                                                                
021400 01  UT-AREA.                                                             
021500     03  FILLER -COPY W418REF  -PRE UT-                                   
021600     EJECT                                                                
021700 01  UT-AREA-SAP-START           PIC X(24)   VALUE                        
021800                                 'UT-AREA-SAP-START'.                     
021900                                                                          
022000 01  UT-SAP-AREA.                                                         
022100     03  FILLER -COPY W51060   -PRE UT-                                   
022200     EJECT                                                                
022300*                                                                         
022400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022500     SKIP3                                                                
022600 01  NYCKLAR-TILL-DLI.                                                    
022700                                                                          
022800     03  W-IDARTNR-X.                                                     
022900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023000                                                                          
023100     03  W-DAINLEV-X.                                                     
023200         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
023300                                                                          
023400     03  W-W6GXKEY-6017-X.                                                
023500         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
023600         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
023700*                                                                         
023800     03  W-W6GXKEY-6018-X.                                                
023900         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
026300     SKIP2                                                                
026400*    --- STATUS-KOD FRÅN IMS                                              
026500 01  STATUS-WS                   PIC XX.                                  
026600     88  SEGMENT-FINNS                       VALUE '  '.                  
026700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
027000     88  IMS-EJ-OK                           VALUE 'XD'.                  
027100     SKIP2                                                                
027200 01  GODK-STATUSKODER.                                                    
027300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027400     SKIP3                                                                
027500 01  SSA1                        PIC X(64).                               
027600 01  SSA2                        PIC X(64).                               
027700     EJECT                                                                
027800*    --- IMS FUNKTIONSKODER                                               
027900*01  -COPY W0003                                                          
028000     EJECT                                                                
028100*    ---  DLI INPUT-OUTPUT AREA                                           
028200 01  FILLER              PIC X(16)   VALUE 'WDL901'.                      
028300*01  WDL901-AREA  -COPY WDL901                                            
028400     EJECT                                                                
028500 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDK601'.               
028600 01  DLI-IO-WDK601.                                                       
028700*    03  -COPY WDK601  -PRE WDK6-                                         
028800     EJECT                                                                
028900 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDK611'.               
029000 01  DLI-IO-WDK611.                                                       
029100*    03  -COPY WDK611  -PRE WDK6-                                         
029200     EJECT                                                                
029300 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDL201'.               
029400 01  DLI-IO-WDL201.                                                       
029500*    03  -COPY WDL201     -PRE INLE-                                      
029600     EJECT                                                                
029700 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDL211'.               
029800 01  DLI-IO-WDL211.                                                       
029900*    03  -COPY WDL211     -PRE INLE-                                      
030000     EJECT                                                                
030100 01  FILLER              PIC X(16)   VALUE 'DLI-IO-WDL221'.               
030200 01  DLI-IO-WDL221.                                                       
030300*    03  -COPY WDL221     -PRE INLE-                                      
030400     EJECT                                                                
030500 01  FILLER              PIC X(16)   VALUE 'DLI-IO-LOPA11'.               
030600 01  DLI-IO-LOPA11.                                                       
030700*    03  -COPY W6GX6018   -PRE LOPA-                                      
030800     EJECT                                                                
031100 LINKAGE SECTION.                                                         
031200                                                                          
031300*01  -COPY W0009  -PRE MSG-                                               
031400     EJECT                                                                
031500*01  -COPY W0009  -PRE WDP7-                                              
031600     EJECT                                                                
031700*01  -COPY W0008  -PRE WDL9-                                              
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01  -COPY W0008  -PRE WDK6-                                              
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300*01  -COPY W0008  -PRE WDL2-                                              
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600*01  -COPY W0008  -PRE LOPA-                                              
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
033200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB                      
033300                           WDL2-PCB LOPA-PCB WDL9-PCB.                    
033500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB                      
033600                           WDL2-PCB LOPA-PCB WDL9-PCB.                    
033800                                                                          
033900     SKIP2                                                                
034000     PERFORM A-INIT                                                       
034100                                                                          
034200     PERFORM IMS-GHU-LOPA11                                               
034300     MOVE LOPA-6018-IDLOPNRM   TO W-IDLOPNRM                              
034400                                                                          
034500     PERFORM S01-LAES-INFIL                                               
034600     IF NOT END-OF-W47677                                                 
034700                                                                          
034800       PERFORM UNTIL END-OF-W47677                                        
034900                                                                          
035000         PERFORM B-BEHANDLA-INFIL                                         
035100         PERFORM S01-LAES-INFIL                                           
035200                                                                          
035300       END-PERFORM                                                        
035400                                                                          
035500     END-IF                                                               
035600                                                                          
035700     MOVE W-IDLOPNRM           TO LOPA-6018-IDLOPNRM                      
035800     PERFORM IMS-REPL-LOPA11                                              
035900                                                                          
036000     PERFORM Z-FINIT                                                      
036100                                                                          
036200     MOVE ZERO TO RETURN-CODE                                             
036300     GOBACK                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 A-INIT SECTION.                                                          
036700     SKIP2                                                                
036800                                                                          
036900     OPEN INPUT  W47677                                                   
037000          OUTPUT W476UT                                                   
037100          OUTPUT W4762B                                                   
037200          OUTPUT W4762C                                                   
037300     MOVE +0   TO POST-ANT                                                
037400                                                                          
037500     SET TAB-IX TO 1                                                      
037600     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
037700       MOVE ALL '+'           TO MSGI-WMSGINIT                            
037800       MOVE '001'             TO MSGI-KDCALL                              
037900       MOVE TAB-IDDC (TAB-IX) TO W-IDDC-MSGI                              
038000       MOVE W-IDDCTEXT-MSGI   TO MSGI-IDUSER                              
038100       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
038200       IF MSGI-KDSVAR = SPACE                                             
038300         MOVE MSGI-IDTIDZON   TO TAB-IDTIDZON (TAB-IX)                    
038400         MOVE MSGI-TILOKDAT   TO TAB-TILOKDAT (TAB-IX)                    
038500       ELSE                                                               
038600         MOVE 'FEL FRÅN SUBPGM W005INIT' TO FELTEXT                       
038700         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
038800       END-IF                                                             
038900                                                                          
039000       SET TAB-IX UP BY 1                                                 
039100     END-PERFORM                                                          
039200                                                                          
039300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
039400                                                                          
039500     ACCEPT DAGENS-DATUM FROM DATE                                        
039600                                                                          
039700     MOVE 'IDAG'               TO DAT-KDDATFORM                           
039800     CALL WDATKONV USING          DAT-KDDATFORM                           
039900                                  DAT-I-TIDATUM                           
040000                                  DAT-O-TIDATUM                           
040100                                  DAT-KDSVAR                              
040200     .                                                                    
040300     EJECT                                                                
040400                                                                          
040500 B-BEHANDLA-INFIL SECTION.                                                
040600     SKIP2                                                                
040700     ADD +1 TO POST-ANT                                                   
040800                                                                          
040900     IF POST-ANT < MAX-POST                                               
041000       MOVE IN-IDDISTR     TO TEST-IDDISTR                                
041100       IF DIST18-SCRAP-NDC-QUAL                                           
041200         MOVE IN-AREA      TO UT-AREA                                     
041300         MOVE WC-CDC-SE    TO UT-IDDC                                     
041400       ELSE                                                               
041500         MOVE IN-AREA      TO UT-AREA                                     
041600         MOVE IN-IDARTNR   TO W-IDARTNR                                   
041700         PERFORM BA-UPPDATERA-AK-PAV                                      
041800         PERFORM BB-LAGG-UPP-INLEVHIST                                    
041900         MOVE W-IDLOPNRM   TO UT-IDLOPNRM                                 
042000         PERFORM BC-LAGG-UPP-SAPINFO                                      
042100         PERFORM S06-SKRIV-W4762C                                         
042200       END-IF                                                             
042300       PERFORM S03-SKRIV-W4762B                                           
042400     ELSE                                                                 
042500       MOVE IN-AREA      TO UT-AREA                                       
042600       PERFORM S02-SKRIV-W476UT                                           
042700     END-IF                                                               
042800                                                                          
042900     .                                                                    
043000     EJECT                                                                
043100 BA-UPPDATERA-AK-PAV SECTION.                                             
043200     SKIP2                                                                
043300     MOVE IN-IDARTNR   TO W-IDARTNR                                       
043400     PERFORM IMS-GU-WDK601                                                
043500     PERFORM IMS-GHNP-WDK611                                              
043600                                                                          
043700     ADD IN-KVLEVANM       TO WDK6-CLAG-KVAKS-PAV                         
043800     PERFORM IMS-REPL-WDK611                                              
043900     PERFORM S04-FLYTTA-SALDOLOGG-DATA                                    
044000     .                                                                    
044100     EJECT                                                                
044200 BB-LAGG-UPP-INLEVHIST SECTION.                                           
044300                                                                          
044400*    -- SKAPA IDINLEV                                                     
044500     ACCEPT WS-TIAAMMDD-DATE FROM DATE                                    
044600     ACCEPT WS-TTMMSSTH-TIME FROM TIME                                    
044700     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
044800     COMPUTE WS-DAINLEV          = 9999999999999999                       
044900                                 - WS-TIAAAAMMDDTTMMSSTH                  
045000     END-COMPUTE                                                          
045100     PERFORM IMS-GU-WDL201                                                
045200                                                                          
045300     IF  SEGMENT-SAKNAS                                                   
045400       MOVE IN-IDARTNR         TO INLE-ART-IDARTNR                        
045500       PERFORM IMS-ISRT-WDL201                                            
045600     END-IF                                                               
045700                                                                          
045800     MOVE WS-DAINLEV             TO INLE-INL-DAINLEV                      
045900     PERFORM IMS-ISRT-WDL211                                              
046000                                                                          
046100     IF SEGMENT-FINNS-REDAN                                               
046200       PERFORM UNTIL SEGMENT-FINNS                                        
046300         ADD 1                   TO WS-TIAAAAMMDDTTMMSSTH                 
046400         COMPUTE WS-DAINLEV          = 9999999999999999                   
046500                                     - WS-TIAAAAMMDDTTMMSSTH              
046600         MOVE WS-DAINLEV         TO INLE-INL-DAINLEV                      
046700         PERFORM IMS-ISRT-WDL211                                          
046800       END-PERFORM                                                        
046900     END-IF                                                               
047000                                                                          
047100     MOVE '310'                  TO INLE-MOT-IDPTYP                       
047200     PERFORM BBA-TA-UT-IDLOPNR                                            
047300     MOVE W-IDLOPNRM             TO INLE-MOT-IDLOPNRM                     
047400***  MOVE IN-IDDISTR             TO INLE-MOT-IDLEVNR                      
047500     MOVE IN-IDDISTR             TO W-IDLEVNR-PIC9                        
047600     MOVE ZERO TO TALLY                                                   
047700     INSPECT W-IDLEVNR-PIC9 TALLYING TALLY FOR LEADING ZEROES             
047800     IF TALLY = 5                                                         
047900         MOVE SPACE TO INLE-MOT-IDLEVNR                                   
048000     ELSE                                                                 
048100        MOVE W-IDLEVNR-PIC9(TALLY + 1:) TO INLE-MOT-IDLEVNR               
048200     END-IF                                                               
048300     MOVE IN-IDRAPPNR            TO INLE-MOT-IDAVINR                      
048400     MOVE ZERO                   TO INLE-MOT-IDKONTO                      
048500     MOVE WDK6-CLAG-ADLAGOMR     TO INLE-MOT-ADLAGOMR                     
048600     MOVE WDK6-CLAG-ADGANG       TO INLE-MOT-ADGANG                       
048700     MOVE WDK6-CLAG-ADPLATS      TO INLE-MOT-ADPLATS                      
048800     MOVE WC-CDC-SE              TO INLE-MOT-IDDC                         
048900     MOVE 7                      TO INLE-MOT-KDRT                         
049000     MOVE SPACE                  TO INLE-MOT-IDFS                         
049100     MOVE ZERO                   TO INLE-MOT-KDAVVANT                     
049200     MOVE ZERO                   TO INLE-MOT-KDAVVKV                      
049300     MOVE ZERO                   TO INLE-MOT-KVANTMOT                     
049400     MOVE IN-KVLEVANM            TO INLE-MOT-KVAVIS                       
049500     MOVE ZERO                   TO INLE-MOT-KVFORDEL                     
049600     MOVE ZERO                   TO INLE-MOT-KVRETUR                      
049700     MOVE ZERO                   TO INLE-MOT-KVFORV                       
049800                                                                          
049900     SET TAB-IX TO 1                                                      
050000     SEARCH DC-ZON AT END                                                 
050100          MOVE ' FEL I TABELLSÖKNING    ' TO FELTEXT                      
050200          CALL ABEND USING RKOD-ABEND-MED-DUMP                            
050300      WHEN TAB-IDDC   (TAB-IX) = IN-IDDC                                  
050400        MOVE TAB-TILOKDAT (TAB-IX) TO INLE-MOT-TIAVIDAT                   
050500     END-SEARCH                                                           
050600                                                                          
050700     MOVE ZERO                   TO INLE-MOT-TIUPPDAT                     
050800     MOVE ZERO                   TO INLE-MOT-IDSHIPM                      
050900                                                                          
051000     PERFORM IMS-ISRT-WDL221                                              
051100     .                                                                    
051200     EJECT                                                                
051300 BBA-TA-UT-IDLOPNR       SECTION.                                         
051400                                                                          
051500     IF DAT-TIAAVVD-GRP (3:3)  =  W-VVD                                   
051600         ADD +1                TO W-LLLL                                  
051700      ELSE                                                                
051800         MOVE DAT-TIAAVVD-GRP (3:3) TO W-VVD                              
051900         MOVE +1                    TO W-LLLL                             
052000     END-IF                                                               
052100     CALL CHECK USING W-IDLOPNRM (2:7) FLT-LGD                            
052200          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
052300     .                                                                    
052400     EJECT                                                                
052500 BC-LAGG-UPP-SAPINFO SECTION.                                             
052600                                                                          
052700     MOVE 'W4767700'           TO UT-EKHT-IDPGM                           
052800     MOVE FUNCTION CURRENT-DATE (1:8) TO UT-EKHT-DAREGDAT                 
052900     MOVE FUNCTION CURRENT-DATE (9:8) TO UT-EKHT-TIKLOCK                  
053000     MOVE 1                    TO UT-EKHT-IDSEKVNR                        
053100     MOVE 'W510EKHA'           TO UT-EKHT-IDCPYTXT                        
053200                                                                          
053300     MOVE IN-TIFAKT            TO WS-TIFAKT                               
053400     IF WS-TIFAKT (1:2) < 50                                              
053500       MOVE 20                 TO UT-EKHT-DAVERDAT (1:2)                  
053600     ELSE                                                                 
053700       MOVE 19                 TO UT-EKHT-DAVERDAT (1:2)                  
053800     END-IF                                                               
053900     MOVE WS-TIFAKT            TO UT-EKHT-DAVERDAT (3:6)                  
054000                                                                          
054100     MOVE IN-IDARTNR           TO UT-EKHT-IDARTNR                         
054200     MOVE IN-IDDC              TO UT-EKHT-IDDC-SEND                       
054300     MOVE TAB-IDDC(1)          TO UT-EKHT-IDDC-REC                        
054400     MOVE IN-IDDISTR           TO UT-EKHT-IDDISTR                         
054500     MOVE IN-IDKUNDNR          TO UT-EKHT-IDKUNDNR                        
054600                                                                          
054700     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
054800     MOVE IN-IDRAPPNR          TO CIA-IDARTBET-IN                         
054900     CALL W009CIA USING           CIA-W009CIA                             
055000     MOVE CIA-IDARTBET-UT      TO UT-EKHT-IDVERGL                         
055100                                                                          
055200     MOVE IN-KDANMORS          TO UT-EKHT-KDANMORS                        
055300     MOVE '302'                TO UT-EKHT-KDEKHHT                         
055400     MOVE '303'                TO UT-EKHT-KDEKSHT                         
055500     MOVE 'DET'                TO UT-EKHT-KDEKNIVA                        
055600     MOVE WDK6-ART-KDPRODSL    TO UT-EKHT-KDPRODSL                        
055800     MOVE IN-KDVALISO          TO UT-EKHT-KDVALISO                        
055900     MOVE IN-KVLEVANM          TO UT-EKHT-KVANTAL                         
056000     MOVE WDK6-CLAG-PRARTSTD   TO UT-EKHT-PRARTSTD                        
056100     MOVE 1                    TO UT-EKHT-PRKURS                          
057900     MOVE WDK6-ART-KDSORT      TO UT-EKHT-KDSORT                          
058000                                                                          
058100     MOVE ZERO                 TO UT-EKHT-BEVAT                           
058200                                  UT-EKHT-IDKONTO                         
058400                                  UT-EKHT-KDFRAKT                         
058500                                  UT-EKHT-KDPSLLOC                        
058600                                  UT-EKHT-PRARTNTO                        
058700                                  UT-EKHT-PRARTSJK                        
058800                                  UT-EKHT-PRHEMTAG                        
058900                                  UT-EKHT-PRDIRLON                        
059000                                  UT-EKHT-PRDMTRL                         
059100                                  UT-EKHT-PRINK                           
059200                                  UT-EKHT-PRLANDCO                        
059300                                  UT-EKHT-PROVRPAL                        
059400                                  UT-EKHT-SUBEL                           
059500                                  UT-EKHT-SUVAT                           
059600                                  UT-EKHT-DAAVIDAT                        
059700                                  UT-EKHT-IDAVINR                         
059800                                  UT-EKHT-KDAVVTYP                        
059900                                  UT-EKHT-KDRT                            
060000                                  UT-EKHT-KVANTMOT                        
060100                                  UT-EKHT-KVAVIS                          
060200     MOVE SPACE                TO UT-EKHT-FLLSBOK                         
060300                                  UT-EKHT-IDANALYS                        
060310                                  UT-EKHT-IDKST                           
060400                                  UT-EKHT-IDTRANS                         
060500                                  UT-EKHT-KDTRADP                         
060600                                  UT-EKHT-IDLEVNR                         
060700     MOVE SPACE                TO UT-EKHT-FLDCET                          
060710     MOVE SPACE                TO UT-EKHT-IDKUNDRF                        
060720     MOVE SPACE                TO UT-EKHT-IDFAKT-EXP                      
060800     .                                                                    
060900     EJECT                                                                
061000 Z-FINIT SECTION.                                                         
061100                                                                          
061200     CLOSE W47677                                                         
061300           W476UT                                                         
061400           W4762B                                                         
061500           W4762C                                                         
061600                                                                          
061700     MOVE 'S' TO POSTSUM-OPKOD                                            
061800     CALL POSTSUM USING POSTSUM-PARM                                      
061900     .                                                                    
062000     EJECT                                                                
062100 S01-LAES-INFIL  SECTION.                                                 
062200     SKIP2                                                                
062300     READ W47677 INTO IN-AREA                                             
062400     AT END                                                               
062500        MOVE JA TO W47677-EOF                                             
062600                                                                          
062700     NOT AT END                                                           
062800        MOVE 'W47677' TO POSTSUM-FDNAMN                                   
062900        MOVE 'W47677D1' TO POSTSUM-DDNAMN2                                
063000        MOVE IN-IDPTYP      TO POSTSUM-TRANSTYP                           
063100        CALL POSTSUM USING POSTSUM-PARM                                   
063200                                                                          
063300     END-READ                                                             
063400     .                                                                    
063500     EJECT                                                                
063600 S02-SKRIV-W476UT SECTION.                                                
063700     SKIP2                                                                
063800     WRITE UT-FAKT-RAD    FROM UT-AREA                                    
063900                                                                          
064000     MOVE 'W476UT'              TO POSTSUM-FDNAMN                         
064100     MOVE 'W47677D2'            TO POSTSUM-DDNAMN2                        
064200     MOVE UT-IDPTYP             TO POSTSUM-TRANSTYP                       
064300     CALL POSTSUM USING POSTSUM-PARM                                      
064400     .                                                                    
064500     EJECT                                                                
064600                                                                          
064700 S03-SKRIV-W4762B SECTION.                                                
064800     SKIP2                                                                
064900     WRITE UT-FAKT-RAD2   FROM UT-AREA                                    
065000                                                                          
065100     MOVE 'W4762B'              TO POSTSUM-FDNAMN                         
065200     MOVE 'W47677D3'            TO POSTSUM-DDNAMN2                        
065300     MOVE UT-IDPTYP             TO POSTSUM-TRANSTYP                       
065400     CALL POSTSUM USING POSTSUM-PARM                                      
065500     .                                                                    
065600     EJECT                                                                
065700 S04-FLYTTA-SALDOLOGG-DATA SECTION.                                       
065800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
065900     ACCEPT WS-TID                   FROM TIME                            
066000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
066100     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
066200     MOVE 9                   TO LOGG-IDSEKVNR                            
066300     MOVE 'DISC'              TO LOGG-IDHUVTYP                            
066400     MOVE 'REF'               TO LOGG-IDSUBTYP                            
066500     MOVE 'W4767700'          TO LOGG-IDPGM                               
066600     MOVE SPACE               TO LOGG-IDTRANS                             
066700     MOVE 'W4767700'          TO LOGG-IDUSER                              
066800     MOVE SPACE               TO LOGG-REF                                 
066900     MOVE IN-IDDISTR          TO LOGG-IDDISTR                             
067000     MOVE IN-IDKUNDNR         TO LOGG-IDKUNDNR                            
067100     MOVE ZERO                TO LOGG-IDORDNR5                            
067200     MOVE IN-IDFAKT           TO LOGG-IDFAKT                              
067300     MOVE IN-IDARTNR          TO LOGG-IDARTNR                             
067400     MOVE WC-CDC-SE           TO LOGG-IDDC                                
067500*   ---SALDOFÖRÄNDRINGAR PÅ WDK611                                        
067600*   ---LOGGAS PÅ WDL9                                                     
067700     MOVE WDK6-CLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                           
067800     MOVE '+'                 TO LOGG-IDTECKEN-KVAKS-PAV                  
067900     MOVE WDK6-CLAG-KVLS      TO LOGG-KVLS                                
068000     MOVE WDK6-CLAG-KVEFRS    TO LOGG-KVEFRS                              
068100     COMPUTE LOGG-KVAKS       =  WDK6-CLAG-KVAKS-CDC                      
068200                              +  WDK6-CLAG-KVAKS-T                        
068300     MOVE SPACE               TO LOGG-IDTECKEN-KVLS                       
068400     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
068500     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
068600     MOVE IN-KVLEVANM         TO LOGG-KVART-SALDO                         
068700     MOVE 0                   TO LOGG-DAREGDAT-LADD                       
068800     PERFORM S05-ISRT-SALDOLOGG                                           
068900     .                                                                    
069000     EJECT                                                                
069100 S05-ISRT-SALDOLOGG SECTION.                                              
069200     PERFORM IMS-ISRT-WDL901                                              
069300     IF SEGMENT-FINNS-REDAN                                               
069400       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
069500         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
069600         PERFORM IMS-ISRT-WDL901                                          
069700       END-PERFORM                                                        
069800     END-IF                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 S06-SKRIV-W4762C SECTION.                                                
070200                                                                          
070300     WRITE UT-SAPPOST FROM UT-SAP-AREA                                    
070400                                                                          
070500     MOVE UT-EKHT-KDEKHHT TO POSTSUM-TRANSTYP                             
070600     MOVE 'W4762C' TO POSTSUM-FDNAMN                                      
070700     MOVE 'W47677D4' TO POSTSUM-DDNAMN2                                   
070800     CALL POSTSUM USING POSTSUM-PARM                                      
070900     .                                                                    
071000     EJECT                                                                
074000* --- IMS SEKTIONER ---                                                   
074100     SKIP3                                                                
074200 IMS-GU-WDK601 SECTION.                                                   
074300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
074400          DELIMITED BY SIZE INTO SSA1                                     
074500     MOVE '  ' TO GODK-STATUSKODER                                        
074600     CALL CBLTDLI USING GU                                                
074700                        WDK6-PCB                                          
074800                        DLI-IO-WDK601                                     
074900                        SSA1                                              
075000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
075100     PERFORM IMS-STATUSKONTROLL                                           
075200     .                                                                    
075300     SKIP3                                                                
075400 IMS-GHNP-WDK611 SECTION.                                                 
075500     MOVE 'WDK611   ' TO SSA1                                             
075600     MOVE '  ' TO GODK-STATUSKODER                                        
075700     CALL CBLTDLI USING GHNP                                              
075800                        WDK6-PCB                                          
075900                        DLI-IO-WDK611                                     
076000                        SSA1                                              
076100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
076200     PERFORM IMS-STATUSKONTROLL                                           
076300     .                                                                    
076400     SKIP3                                                                
076500 IMS-REPL-WDK611 SECTION.                                                 
076600                                                                          
076700     MOVE '  '              TO GODK-STATUSKODER                           
076800     CALL CBLTDLI USING REPL                                              
076900                        WDK6-PCB                                          
077000                        DLI-IO-WDK611                                     
077100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
077200     PERFORM IMS-STATUSKONTROLL                                           
077300     .                                                                    
077400     EJECT                                                                
077500 IMS-GHU-LOPA11 SECTION.                                                  
077600     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
077700          DELIMITED BY SIZE INTO SSA1                                     
077800     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
077900          DELIMITED BY SIZE INTO SSA2                                     
078000     MOVE '    ' TO GODK-STATUSKODER                                      
078100     CALL CBLTDLI USING GHU                                               
078200                        LOPA-PCB                                          
078300                        DLI-IO-LOPA11                                     
078400                        SSA1                                              
078500                        SSA2                                              
078600     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
078700     PERFORM IMS-STATUSKONTROLL                                           
078800     .                                                                    
078900     SKIP3                                                                
079000 IMS-REPL-LOPA11 SECTION.                                                 
079100     MOVE '    ' TO GODK-STATUSKODER                                      
079200     CALL CBLTDLI USING REPL                                              
079300                        LOPA-PCB                                          
079400                        DLI-IO-LOPA11                                     
079500     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
079600     PERFORM IMS-STATUSKONTROLL                                           
079700     .                                                                    
079800     EJECT                                                                
079900 IMS-GU-WDL201   SECTION.                                                 
080000     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
080100          DELIMITED BY SIZE INTO SSA1                                     
080200     MOVE '  GE' TO GODK-STATUSKODER                                      
080300     CALL CBLTDLI USING GU                                                
080400                        WDL2-PCB                                          
080500                        DLI-IO-WDL201                                     
080600                        SSA1                                              
080700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
080800     PERFORM IMS-STATUSKONTROLL                                           
080900     .                                                                    
081000     SKIP3                                                                
081100 IMS-ISRT-WDL201   SECTION.                                               
081200     MOVE 'WDL201   ' TO SSA1                                             
081300     MOVE '  ' TO GODK-STATUSKODER                                        
081400     CALL CBLTDLI USING ISRT                                              
081500                        WDL2-PCB                                          
081600                        DLI-IO-WDL201                                     
081700                        SSA1                                              
081800     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
081900     PERFORM IMS-STATUSKONTROLL                                           
082000     .                                                                    
082100     SKIP3                                                                
082200 IMS-ISRT-WDL211   SECTION.                                               
082300     MOVE 'WDL211   ' TO SSA1                                             
082400     MOVE '  II' TO GODK-STATUSKODER                                      
082500     CALL CBLTDLI USING ISRT                                              
082600                        WDL2-PCB                                          
082700                        DLI-IO-WDL211                                     
082800                        SSA1                                              
082900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
083000     PERFORM IMS-STATUSKONTROLL                                           
083100     .                                                                    
083200     SKIP3                                                                
083300 IMS-ISRT-WDL221   SECTION.                                               
083400     MOVE 'WDL221   ' TO SSA1                                             
083500     MOVE '  ' TO GODK-STATUSKODER                                        
083600     CALL CBLTDLI USING ISRT                                              
083700                        WDL2-PCB                                          
083800                        DLI-IO-WDL221                                     
083900                        SSA1                                              
084000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
084100     PERFORM IMS-STATUSKONTROLL                                           
084200     .                                                                    
084300     EJECT                                                                
084400 IMS-ISRT-WDL901 SECTION.                                                 
084500                                                                          
084600     MOVE 'WDL901   ' TO SSA1                                             
084700     MOVE '  II' TO GODK-STATUSKODER                                      
084800     CALL CBLTDLI USING ISRT WDL9-PCB WDL901-AREA SSA1                    
084900     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
085000     PERFORM IMS-STATUSKONTROLL                                           
085100     .                                                                    
085200     EJECT                                                                
086600 IMS-STATUSKONTROLL SECTION.                                              
086700     SKIP2                                                                
086800     SET STATUS-IX TO 1                                                   
086900     SEARCH GODK-STATUS                                                   
087000       AT END                                                             
087100         MOVE 'IMS-FEL   ' TO FELTEXT-STR                                 
087200         DISPLAY FELTEXT                                                  
087300         CALL FELLOG                                                      
087400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
087500         CONTINUE                                                         
087600     END-SEARCH                                                           
087700     .                                                                    
