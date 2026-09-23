000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0062300.                                                
000300 AUTHOR.         ANDERSSON BERT.                                          
000400 DATE-WRITTEN.   06/01/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        NEW DISPATCH SCREEN                                              
000900*        VISAR, TAR BORT SAMT STARTAR TRANSAKTIONER                       
001000*        PÅ MEDDELANDE-KOMMUNIKATIONS-DATABASEN                           
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDP7                                       
001300*        PROGRAMMET UPPDATERAR WDP8                                       
001400*                                                                         
001500*        PROGRAMMET LÄSER     WDB6                                        
001510*        PROGRAMMET LÄSER     WDP8                                        
001600*        PROGRAMMET LÄSER     WDP8A1                                      
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W0T623                                              
002000*        MID:         W0I62301                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W0O62301                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
003200 77  IDPGM                       PIC X(08)   VALUE 'W0062300'.            
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FILLER                      PIC X(08)   VALUE 'FELTEXT '.            
003500 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
003600 77  FILLER                      PIC X(08)   VALUE 'PGMPOS  '.            
003700 77  PGMPOS                      PIC X(32)   VALUE SPACE.                 
003800 77  FILLER                      PIC X(08)   VALUE 'IMSPOS  '.            
003900 77  IMSPOS                      PIC X(32)   VALUE SPACE.                 
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  WS-KDKOMSTA-F               PIC X       VALUE 'F'.                   
004500 77  WS-SEQA-TIREGDAT            PIC 9(6)   VALUE ZERO.                   
004600 77  WS-SEQA-TIKLOCK             PIC 9(8)   VALUE ZERO.                   
004700 77  WS-MID-TIREGDAT             PIC X(6)   VALUE ZERO.                   
004800 77  WS-MID-TIKLOCK              PIC X(8)   VALUE ZERO.                   
004900                                                                          
005000 77  WS-IDDISTR-REFILL           PIC 9(4)   VALUE ZERO.                   
005100 77  WS-IDDISTR-RETUR            PIC 9(4)   VALUE ZERO.                   
005200 77  WS-IDDISTR-QRETUR           PIC 9(4)   VALUE ZERO.                   
005300 77  WS-IDDISTR-SKROT            PIC 9(4)   VALUE ZERO.                   
005400 77  WS-IDDISTR-QSKROT           PIC 9(4)   VALUE ZERO.                   
005500 77  WS-IDDISTR-RSKROT           PIC 9(4)   VALUE ZERO.                   
005600                                                                          
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006500     88  NYCKLAR-OK                          VALUE 'J'.                   
006600     88  NYCKLAR-FEL                         VALUE 'N'.                   
006700                                                                          
006800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006900     88  ALLT-OK                             VALUE 'J'.                   
007000                                                                          
007100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007200     88  EGEN-MID                            VALUE '0623'.                
007300     88  GODK-MID                            VALUE '0621' '0622'          
007400                                                   '0623' '0624'          
007500                                                   '0625' '0626'          
007600                                                   '0627' '0628'          
007700                                                   '0629'.                
007800     88  HELP-MID                            VALUE '0551'.                
007900                                                                          
008000 01  W-VIMSID.                                                            
008100   03  W-IMSID                   PIC X(4)    VALUE SPACE.                 
008200   03  FILLER                    PIC X(4)    VALUE SPACE.                 
008300                                                                          
008400 01  WS-STYR.                                                             
008500   03  WS-NOD                    PIC X(1)    VALUE ' '.                   
008600   03  WS-DATE                   PIC X(1)    VALUE ' '.                   
008700   03  WS-TIME                   PIC X(1)    VALUE ' '.                   
008800                                                                          
008900     EJECT                                                                
009000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009100 01  GENERELLA-SUBPROGRAM.                                                
009200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009800*01 -COPY WMEDAREA                                                        
009900     SKIP3                                                                
010000 01  MESSAGE-CODES.                                                       
010100     03 INF-PRESS-PF11           PIC X(3)    VALUE '003'.                 
010200     03 INF-FIRST-PAGE           PIC X(3)    VALUE '006'.                 
010300     03 INF-UPDATE-DONE          PIC X(3)    VALUE '101'.                 
010400     03 INF-MORE-INFO-EXISTS     PIC X(3)    VALUE '105'.                 
010500     03 ERR-CORR-HILITE-FLDS     PIC X(3)    VALUE '001'.                 
010600     03 ERR-UPDATE-NOT-ALLOWED   PIC X(3)    VALUE '007'.                 
010700     03 ERR-EMPTY                PIC X(3)    VALUE '010'.                 
010800     03 ERR-PF11-AND-NO-DATA     PIC X(3)    VALUE '011'.                 
010900     03 ERR-WRONG-KEY            PIC X(3)    VALUE '401'.                 
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011400     SKIP3                                                                
011500*01 -COPY WMSGINIT                                                        
011600     EJECT                                                                
011700*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
011800*                                                                         
011900 01  SPAR-AREA.                                                           
012000     03  SPAR-IDTRANS           PIC X(4)    VALUE '0623'.                 
012100     EJECT                                                                
012200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012500     SKIP3                                                                
012600*01  MID -COPY W0I62301                                                   
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012900     SKIP3                                                                
013000*01  -COPY WMSGAREA                                                       
013100     EJECT                                                                
013200     03  MOD REDEFINES MSG-AREA.                                          
013300*      05  -COPY W0O62301                                                 
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013600     SKIP3                                                                
013700*01  -COPY WMFSAREA                                                       
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)  VALUE 'MSG/ALT-AREA'.         
014000*01  -COPY WMSGKOM                                                        
014100     EJECT                                                                
014200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500     SKIP3                                                                
014600 01  NYCKLAR-TILL-DLI.                                                    
014700   03  W-WDP8A1KY-MIN.                                                    
014800     05  W-TIREGDAT-MIN          PIC S9(7)   VALUE ZERO COMP-3.           
014900     05  W-TIKLOCK-MIN           PIC S9(9)   VALUE ZERO COMP-3.           
015000     05  W-IDSNDNOD-MIN          PIC X(8)    VALUE LOW-VALUE.             
015100     05  W-IDSNDJOB-MIN          PIC X(8)    VALUE LOW-VALUE.             
015200*                                                                         
015300   03  W-WDP8A1KY-MAX.                                                    
015400     05  W-TIREGDAT-MAX          PIC S9(7)   VALUE ZERO COMP-3.           
015500     05  W-TIKLOCK-MAX           PIC S9(9)   VALUE ZERO COMP-3.           
015600     05  W-IDSNDNOD-MAX          PIC X(8)    VALUE LOW-VALUE.             
015700     05  W-IDSNDJOB-MAX          PIC X(8)    VALUE LOW-VALUE.             
015800*                                                                         
015900   03  W-WDP801KY-X.                                                      
016000     05  W-IDSNDNOD              PIC X(8)     VALUE SPACE.                
016100     05  W-IDSNDJOB              PIC X(8)     VALUE SPACE.                
016200     05  W-TIREGDAT              PIC S9(7)    VALUE ZERO COMP-3.          
016300     05  W-TIKLOCK               PIC S9(9)    VALUE ZERO COMP-3.          
016400*                                                                         
016500   03  W-IDRADNR-X.                                                       
016600     05  W-IDRADNR               PIC S9(5)    VALUE ZERO COMP-3.          
016700*                                                                         
016800   03  W-KDKOMSTA                PIC X(1)    VALUE SPACE.                 
016900*                                                                         
017000   03  W-IDDC-X.                                                          
017100     05  W-IDDC                  PIC X(2)    VALUE SPACE.                 
017200     SKIP2                                                                
017300*    --- STATUS-KOD FRÅN IMS                                              
017400 01  STATUS-WS                   PIC XX.                                  
017500     88  SEGMENT-FINNS                       VALUE '  '.                  
017600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017800     SKIP2                                                                
017900 01  GODK-STATUSKODER.                                                    
018000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100     SKIP3                                                                
018200 01  SSA1                        PIC X(192).                              
018300 01  SSA2                        PIC X(1088).                             
018400     EJECT                                                                
018500*    --- IMS FUNKTIONSKODER                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800*    ---  DLI INPUT-OUTPUT AREA                                           
018900 01  FILLER                      PIC X(16)                                
019000                                     VALUE 'DLI-IO-P801'.                 
019100 01  DLI-IO-P801.                                                         
019200*  03  -COPY WDP801                                                       
019300     EJECT                                                                
019400 01  FILLER                      PIC X(16)                                
019500                                     VALUE 'DLI-IO-P811'.                 
019600 01  DLI-IO-P811.                                                         
019700*  03  -COPY WDP811                                                       
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)                                
020000                                     VALUE 'DLI-IO-P8A1'.                 
020100 01  DLI-IO-P8A1.                                                         
020200*  05  -COPY WDP8A1                                                       
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)                                
020500                                     VALUE 'DLI-IO-WDB6'.                 
020600 01  DLI-IO-WDB6.                                                         
020700*  05  -COPY WDB601                                                       
020800     EJECT                                                                
020900 LINKAGE SECTION.                                                         
021000*01  -COPY W0009      -PRE MSG-                                           
021100                                                                          
021200*01  -COPY W0009      -PRE ALT-                                           
021300     SKIP2                                                                
021400*01    -COPY W0008     -PRE WDP7-                                         
021500     05  FILLER                  PIC X.                                   
021600     SKIP2                                                                
021700*01    -COPY W0008     -PRE WDP8-                                         
021800     05  FILLER                  PIC X.                                   
021900     SKIP2                                                                
022000*01    -COPY W0008     -PRE WDP8A-                                        
022100     05  FILLER                  PIC X.                                   
022200     SKIP2                                                                
022300*01    -COPY W0008     -PRE WDB6-                                         
022400     05  FILLER                  PIC X.                                   
022500     SKIP2                                                                
022600 PROCEDURE DIVISION  USING MSG-PCB   ALT-PCB WDP7-PCB                     
022700                           WDP8-PCB WDP8A-PCB WDB6-PCB.                   
022800 MAIN SECTION.                                                            
022900     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB WDP7-PCB                     
023000                           WDP8-PCB WDP8A-PCB WDB6-PCB.                   
023100                                                                          
023200     PERFORM IMS-GET-MSG                                                  
023300     IF SEGMENT-FINNS                                                     
023400       PERFORM A-INIT                                                     
023500       PERFORM B-KOLLA-NYCKLAR                                            
023600                                                                          
023700       IF NYCKLAR-OK                                                      
023800         IF MFS-UPDATE                                                    
023900           PERFORM G-KOLLA-INPUT                                          
024000                                                                          
024100           IF INDATA-OK                                                   
024200             PERFORM H-UPPDATERA                                          
024300           END-IF                                                         
024400         ELSE                                                             
024500           PERFORM C-SAMMA-SIDA                                           
024600         END-IF                                                           
024700                                                                          
024800         IF INDATA-OK                                                     
024900           PERFORM F-LAES-VISA-INFO                                       
025000         END-IF                                                           
025100       END-IF                                                             
025200                                                                          
025300       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O62301 + 4                      
025400       PERFORM IMS-INSERT-MSG                                             
025500     END-IF                                                               
025600                                                                          
025700     MOVE ZERO TO RETURN-CODE                                             
025800     GOBACK                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 A-INIT SECTION.                                                          
026200     MOVE 'STA A-INIT           SEC '      TO PGMPOS                      
026300                                                                          
026400     IF MSG-DUBBLA-TRANSKODER                                             
026500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I62301                 
026600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
026700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026800     ELSE                                                                 
026900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I62301                  
027000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027200     END-IF                                                               
027300                                                                          
027400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
027500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
027600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027700                                                                          
027800     MOVE LOW-VALUE TO MSG-AREA                                           
027900     MOVE 'W0O62301' TO MFS-IDMOD                                         
028000     MOVE '0623' TO MOD-IDTRANS                                           
028100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028200                                                                          
028300     IF EGEN-MID OR HELP-MID                                              
028400       IF  MID-TIREGDAT-IN    = ALL '+'                                   
028500       AND MID-TIKLOCK-IN     = ALL '+'                                   
028600         CONTINUE                                                         
028700       ELSE                                                               
028800         MOVE SPACE TO MFS-KDTRTYP                                        
028900         MOVE '7' TO MFS-IDPFK                                            
029000       END-IF                                                             
029100     ELSE                                                                 
029200       IF NOT MFS-UPD-X                                                   
029300         MOVE SPACE TO MFS-KDTRTYP                                        
029400         MOVE '7' TO MFS-IDPFK                                            
029500       END-IF                                                             
029600     END-IF                                                               
029700     MOVE 'END A-INIT           SEC '      TO PGMPOS                      
029800     .                                                                    
029900     EJECT                                                                
030000 B-KOLLA-NYCKLAR SECTION.                                                 
030100     MOVE 'STA B-KOLLA-NYCKLAR  SEC '      TO PGMPOS                      
030200                                                                          
030300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030400     MOVE '001'             TO MSGI-KDCALL                                
030500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
030600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
030700     MOVE '0623'            TO MSGI-IDTRANS                               
030800                                                                          
030900     IF GODK-MID                                                          
031000       IF MID-TIREGDAT-IN = '++++++'                                      
031100         MOVE MID-TIREGDAT-UT TO WS-MID-TIREGDAT                          
031200         MOVE MID-TIREGDAT-UT TO MSGI-TIREGDAT                            
031300       ELSE                                                               
031400         MOVE MID-TIREGDAT-IN TO WS-MID-TIREGDAT                          
031500         MOVE MID-TIREGDAT-IN TO MSGI-TIREGDAT                            
031600       END-IF                                                             
031700       IF MID-TIKLOCK-IN = '++++++++'                                     
031800         MOVE MID-TIKLOCK-UT TO WS-MID-TIKLOCK                            
031900         MOVE MID-TIKLOCK-UT TO MSGI-TIKLOCK                              
032000       ELSE                                                               
032100         MOVE MID-TIKLOCK-IN TO WS-MID-TIKLOCK                            
032200         MOVE MID-TIKLOCK-IN TO MSGI-TIKLOCK                              
032300       END-IF                                                             
032400     ELSE                                                                 
032500       MOVE SPACE           TO MID-IDSNDNOD                               
032600       MOVE ZERO            TO MID-TIREGDAT-IN                            
032700       MOVE ZERO            TO MID-TIKLOCK-IN                             
032800     END-IF                                                               
032900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
033000                                                                          
033100*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
033200     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
033300                                                                          
033400     MOVE JA TO NYCKLAR-SW                                                
033500                                                                          
033600     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-IN                              
033700     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-UT                              
033800     INSPECT MSGI-TIREGDAT   REPLACING LEADING SPACE BY ZERO              
033900     IF MSGI-TIREGDAT   NUMERIC AND MSGI-TIREGDAT   > ZERO                
034000       MOVE WS-MID-TIREGDAT TO W-TIREGDAT                                 
034100     ELSE                                                                 
034200*        STRING '*' WS-MID-TIREGDAT                                       
034300*        DELIMITED BY SIZE INTO MOD-TEMFSINF                              
034400       MOVE NEJ TO NYCKLAR-SW                                             
034500     END-IF                                                               
034600                                                                          
034700     MOVE MFS-RENSA-FAELT TO MOD-TIKLOCK-IN                               
034800     MOVE MFS-RENSA-FAELT TO MOD-TIKLOCK-UT                               
034900     INSPECT MSGI-TIKLOCK   REPLACING LEADING SPACE BY ZERO               
035000     IF MSGI-TIKLOCK   NUMERIC AND MSGI-TIKLOCK   > ZERO                  
035100       MOVE WS-MID-TIKLOCK TO W-TIKLOCK                                   
035200     ELSE                                                                 
035300*        STRING '*' WS-MID-TIKLOCK                                        
035400*               '*' MID-TIKLOCK-IN                                        
035500*               '*' MID-TIKLOCK-UT                                        
035600*               '*' MSGI-TIKLOCK                                          
035700*        DELIMITED BY SIZE INTO MOD-TEMFSINF                              
035800       MOVE NEJ TO NYCKLAR-SW                                             
035900     END-IF                                                               
036000                                                                          
036100*    -- KONTROLL AV WDP801KY                                              
036200*    MOVE MFS-RENSA-FAELT TO MOD-WDP801KY-IN                              
036300                                                                          
036400     IF GODK-MID OR NYCKLAR-OK                                            
036500       MOVE MSGI-TIREGDAT TO MOD-TIREGDAT-UT                              
036600       MOVE MSGI-TIKLOCK  TO MOD-TIKLOCK-UT                               
036700       INSPECT MOD-TIREGDAT-UT REPLACING LEADING SPACE BY ZERO            
036800       INSPECT MOD-TIKLOCK-UT  REPLACING LEADING SPACE BY ZERO            
036900     ELSE                                                                 
037000       MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-UT                            
037100                               MOD-TIKLOCK-UT                             
037200     END-IF                                                               
037300                                                                          
037400     IF NYCKLAR-FEL                                                       
037500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
037600       CALL WMEDKONV USING MED-WMEDAREA                                   
037700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
037800       PERFORM MFS-RENSA-FAELT-IN                                         
037900       PERFORM MFS-RENSA-FAELT-UT                                         
038000                                                                          
038100     END-IF                                                               
038200     MOVE 'END B-KOLLA-NYCKLAR  SEC '      TO PGMPOS                      
038300     .                                                                    
038400     EJECT                                                                
038500 C-SAMMA-SIDA        SECTION.                                             
038600     MOVE 'STA E-SAMMA-SIDA     SEC '      TO PGMPOS                      
038700                                                                          
038800     IF EGEN-MID                                                          
038900       IF MID-KDCMDVAL = '+++'                                            
039000         MOVE MFS-RENSA-FAELT     TO MOD-KDCMDVAL                         
039100       ELSE                                                               
039200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
039300         CALL WMEDKONV USING MED-WMEDAREA                                 
039400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
039500         PERFORM CA-MID-INDATA-TILL-MOD                                   
039600       END-IF                                                             
039700     ELSE                                                                 
039800       PERFORM MFS-RENSA-FAELT-IN                                         
039900     END-IF                                                               
040000     MOVE 'END E-SAMMA-SIDA     SEC '      TO PGMPOS                      
040100     .                                                                    
040200     EJECT                                                                
040300 CA-MID-INDATA-TILL-MOD SECTION.                                          
040400     MOVE 'STA CA-MID-INDATA    SEC '      TO PGMPOS                      
040500                                                                          
040600* * * * * FÖR VARJE MID-FÄLT                                              
040700* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
040800* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
040900* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
041000     IF MID-KDCMDVAL = '+++'                                              
041100       MOVE MFS-RENSA-FAELT       TO MOD-KDCMDVAL                         
041200     ELSE                                                                 
041300       MOVE MID-KDCMDVAL          TO MOD-KDCMDVAL                         
041400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR                    
041500     END-IF                                                               
041600     MOVE 'END CA-MID-INDATA    SEC '      TO PGMPOS                      
041700     .                                                                    
041800     EJECT                                                                
041900                                                                          
042000 F-LAES-VISA-INFO SECTION.                                                
042100     MOVE 'STA F-LAES-VISA-INFO SEC '      TO PGMPOS                      
042200                                                                          
042300     PERFORM FA-LAES-WDP8A1                                               
042400                                                                          
042500     IF SEGMENT-FINNS                                                     
042600       MOVE SPACE                TO MOD-KDCMDVAL                          
042700       MOVE SEQA-TIREGDAT        TO WS-SEQA-TIREGDAT                      
042800       MOVE WS-SEQA-TIREGDAT        TO MOD-TIREGDAT                       
042900                                                                          
043000       MOVE SEQA-TIKLOCK         TO WS-SEQA-TIKLOCK                       
043100       MOVE WS-SEQA-TIKLOCK         TO MOD-TIKLOCK                        
043200                                                                          
043300       MOVE SEQA-IDUSER          TO MOD-IDUSER                            
043400       MOVE SEQA-IDSNDNOD        TO MOD-IDSNDNOD                          
043500                                                                          
043600       MOVE SEQA-IDSNDJOB        TO MOD-IDSNDJOB                          
043700       MOVE SEQA-KDKOMSTA        TO MOD-KDKOMSTA                          
043800       MOVE SEQA-IDMFSMED        TO MOD-IDMFSMED                          
043900                                                                          
044000       PERFORM FB-SHOW-MESSAGE                                            
044100                                                                          
044200     ELSE                                                                 
044300       IF MFS-UPDATE                                                      
044400         CONTINUE                                                         
044500       ELSE                                                               
044600         MOVE ERR-EMPTY TO MED-IDMFSFEL                                   
044700       END-IF                                                             
044800       CALL WMEDKONV USING MED-WMEDAREA                                   
044900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
045000       PERFORM MFS-RENSA-FAELT-UT                                         
045100     END-IF                                                               
045200                                                                          
045300     MOVE 'END F-LAES-VISA-INFO SEC '      TO PGMPOS                      
045400     .                                                                    
045500     EJECT                                                                
045600 FA-LAES-WDP8A1    SECTION.                                               
045700     MOVE 'STA FA-LAES-WDP8A1    SEC'      TO PGMPOS                      
045800                                                                          
045900     MOVE MSGI-TIREGDAT             TO W-TIREGDAT-MIN                     
046000     MOVE MSGI-TIKLOCK              TO W-TIKLOCK-MIN                      
046100     MOVE LOW-VALUE                 TO W-IDSNDNOD-MIN                     
046200     MOVE LOW-VALUE                 TO W-IDSNDJOB-MIN                     
046300                                                                          
046400     MOVE MSGI-TIREGDAT             TO W-TIREGDAT-MAX                     
046500     MOVE MSGI-TIKLOCK              TO W-TIKLOCK-MAX                      
046600     MOVE HIGH-VALUE                TO W-IDSNDNOD-MAX                     
046700     MOVE HIGH-VALUE                TO W-IDSNDJOB-MAX                     
046800                                                                          
046900     MOVE 'F'                       TO WS-KDKOMSTA-F                      
047000                                                                          
047100     PERFORM IMS-GU-WDP8A1KY                                              
047200     MOVE 'END FA-LAES-WDP8A1    SEC'      TO PGMPOS                      
047300     .                                                                    
047400     EJECT                                                                
047500 FB-SHOW-MESSAGE   SECTION.                                               
047600     MOVE 'STA FB-SHOW-MESSAGE  SEC '      TO PGMPOS                      
047700                                                                          
047800     EVALUATE SEQA-IDMFSMED                                               
047900     WHEN '054'                                                           
048000       IF MSGI-IDLAND-SPR = 'GB'                                          
048100         move 'Order Missing            ' to mod-feltext                  
048200       ELSE                                                               
048300         move 'Order saknas             ' to mod-feltext                  
048400       END-IF                                                             
048500     WHEN '063'                                                           
048600       IF MSGI-IDLAND-SPR = 'GB'                                          
048700         move 'Customer info missing' to mod-feltext                      
048800       ELSE                                                               
048900         move 'Kundinfo saknas'     to mod-feltext                        
049000       END-IF                                                             
049100     WHEN '067'                                                           
049200       IF MSGI-IDLAND-SPR = 'GB'                                          
049300         MOVE                                                             
049400         'Further info. is required before this item is accepted'         
049500         TO MOD-FELTEXT                                                   
049600       ELSE                                                               
049700         move 'Speciell orderinformation behövs för denna artikel'        
049800         TO MOD-FELTEXT                                                   
049900       END-IF                                                             
050000     WHEN '087'                                                           
050100       IF MSGI-IDLAND-SPR = 'GB'                                          
050200         move 'Wrong Transport Info            '    to mod-feltext        
050400       ELSE                                                               
050500         move 'Fel transport info             '     to mod-feltext        
050600       END-IF                                                             
050700     WHEN '094'                                                           
050800       IF MSGI-IDLAND-SPR = 'GB'                                          
050900         move 'Error in order'        to mod-feltext                      
051000       ELSE                                                               
051100         move 'Order fel   '          to mod-feltext                      
051200       END-IF                                                             
051300     WHEN '94A'                                                           
051400       IF MSGI-IDLAND-SPR = 'GB'                                          
051500         move 'Error in district no' to mod-feltext                       
051600       ELSE                                                               
051700         move 'Fel distrikts nr' to mod-feltext                           
051800       END-IF                                                             
051900     WHEN '94B'                                                           
052000       IF MSGI-IDLAND-SPR = 'GB'                                          
052100         move 'Error in customer no' to mod-feltext                       
052200       ELSE                                                               
052300         move 'Fel kundnummer ' to mod-feltext                            
052400       END-IF                                                             
052500     WHEN '94C'                                                           
052600       IF MSGI-IDLAND-SPR = 'GB'                                          
052700         move 'Error in order number' to mod-feltext                      
052800       ELSE                                                               
052900         move 'Fel i order nummer' to mod-feltext                         
053000       END-IF                                                             
053100     WHEN '94D'                                                           
053200       IF MSGI-IDLAND-SPR = 'GB'                                          
053300         move 'Error in order class' to mod-feltext                       
053400       ELSE                                                               
053500         move 'Fel i orderklass ' to mod-feltext                          
053600       END-IF                                                             
053700     WHEN '94E'                                                           
053800       IF MSGI-IDLAND-SPR = 'GB'                                          
053900         move 'Error in freight code' to mod-feltext                      
054000       ELSE                                                               
054100         move 'Fel i frakt-kod' to mod-feltext                            
054200       END-IF                                                             
054300     WHEN '94F'                                                           
054400       IF MSGI-IDLAND-SPR = 'GB'                                          
054500         move 'Order backordered' to mod-feltext                          
054600       ELSE                                                               
054700         move 'Order restnoterad' to mod-feltext                          
054800       END-IF                                                             
054900     WHEN '940'                                                           
055000       IF MSGI-IDLAND-SPR = 'GB'                                          
055100         move 'Error in account no'     to mod-feltext                    
055200       ELSE                                                               
055300         move 'Fel konto-nummer    '     to mod-feltext                   
055400       END-IF                                                             
055500     WHEN '941'                                                           
055600       IF MSGI-IDLAND-SPR = 'GB'                                          
055700         move 'Error in analys number' to mod-feltext                     
055800       ELSE                                                               
055900         move 'Fel analys-nummer' to mod-feltext                          
056000       END-IF                                                             
056100     WHEN '942'                                                           
056200       IF MSGI-IDLAND-SPR = 'GB'                                          
056300         move 'Error in cost center' to mod-feltext                       
056400       ELSE                                                               
056500         move 'Fel kostnads-ställe  ' to mod-feltext                      
056600       END-IF                                                             
056700     WHEN '943'                                                           
056800       IF MSGI-IDLAND-SPR = 'GB'                                          
056900         move 'error in company no' to mod-feltext                        
057000       ELSE                                                               
057100         move 'Fel i företagskod  ' to mod-feltext                        
057200       END-IF                                                             
057300     WHEN '941'                                                           
057400       IF MSGI-IDLAND-SPR = 'GB'                                          
057500         move 'Error in invoice type' to mod-feltext                      
057600       ELSE                                                               
057700         move 'Fel fakuratyp       ' to mod-feltext                       
057800       END-IF                                                             
057900     WHEN '945'                                                           
058000       IF MSGI-IDLAND-SPR = 'GB'                                          
058100         move 'Error in consolidation' to mod-feltext                     
058200       ELSE                                                               
058300         move 'Fel i order konsolidation' to mod-feltext                  
058400       END-IF                                                             
058500     WHEN '946'                                                           
058600       IF MSGI-IDLAND-SPR = 'GB'                                          
058700         move 'Error in tpo-type'    to mod-feltext                       
058800       ELSE                                                               
058900         move 'Fel TPO-typ      '    to mod-feltext                       
059000       END-IF                                                             
059100     WHEN '947'                                                           
059200       IF MSGI-IDLAND-SPR = 'GB'                                          
059300         move 'Error in tpo-date'    to mod-feltext                       
059400       ELSE                                                               
059500         move 'Fel TPO-datum '    to mod-feltext                          
059600       END-IF                                                             
059700     WHEN '948'                                                           
059800       IF MSGI-IDLAND-SPR = 'GB'                                          
059900         move 'Error in RFS-date'    to mod-feltext                       
060000       ELSE                                                               
060100         move 'Fel RFS-datum'    to mod-feltext                           
060200       END-IF                                                             
060300     WHEN '949'                                                           
060400       IF MSGI-IDLAND-SPR = 'GB'                                          
060500         move 'Error in customs inv' to mod-feltext                       
060600       ELSE                                                               
060700         move 'Fel i tull-fakturering ' to mod-feltext                    
060800       END-IF                                                             
060900     WHEN '194'                                                           
061000       IF MSGI-IDLAND-SPR = 'GB'                                          
061100         move 'Error in national sign' to mod-feltext                     
061200       ELSE                                                               
061300         move 'Fel i nationalitets-tecken' to mod-feltext                 
061400       END-IF                                                             
061500     WHEN '294'                                                           
061600       IF MSGI-IDLAND-SPR = 'GB'                                          
061700         move 'Error in district center' to mod-feltext                   
061800       ELSE                                                               
061900         move 'Fel i lagertillhörighet' to mod-feltext                    
062000       END-IF                                                             
062100     WHEN '394'                                                           
062200       IF MSGI-IDLAND-SPR = 'GB'                                          
062300         move 'Error in stockupdating' to mod-feltext                     
062400       ELSE                                                               
062500         move 'Fel i lagersaldo      ' to mod-feltext                     
062600       END-IF                                                             
062700     WHEN '494'                                                           
062800       IF MSGI-IDLAND-SPR = 'GB'                                          
062900         move 'Error in campaign reference' to mod-feltext                
063000       ELSE                                                               
063100         move 'Fel i kampanj referens' to mod-feltext                     
063200       END-IF                                                             
063300     WHEN '95A'                                                           
063400       IF MSGI-IDLAND-SPR = 'GB'                                          
063500         move 'Error in district no' to mod-feltext                       
063600       ELSE                                                               
063700         move 'Fel i distrikts nummer' to mod-feltext                     
063800       END-IF                                                             
063900     WHEN '95B'                                                           
064000       IF MSGI-IDLAND-SPR = 'GB'                                          
064100         move 'Error in order number' to mod-feltext                      
064200       ELSE                                                               
064300         move 'Fel i order-nummer' to mod-feltext                         
064400       END-IF                                                             
064500     WHEN '95C'                                                           
064600       IF MSGI-IDLAND-SPR = 'GB'                                          
064700         move 'Error in order class' to mod-feltext                       
064800       ELSE                                                               
064900         move 'Fel i order-klass' to mod-feltext                          
065000       END-IF                                                             
065100     WHEN '95D'                                                           
065200       IF MSGI-IDLAND-SPR = 'GB'                                          
065300         move 'error in account'    to mod-feltext                        
065400       ELSE                                                               
065500         move 'Fel i konto-nummer'    to mod-feltext                      
065600       END-IF                                                             
065700     WHEN '95E'                                                           
065800       IF MSGI-IDLAND-SPR = 'GB'                                          
065900         move 'Error in error analys no' to mod-feltext                   
066000       ELSE                                                               
066100         move 'Fel i analys nummer ' to mod-feltext                       
066200       END-IF                                                             
066300     WHEN '95F'                                                           
066400       IF MSGI-IDLAND-SPR = 'GB'                                          
066500         move 'Error in cost center' to mod-feltext                       
066600       ELSE                                                               
066700         move 'Fel i kostnadsställe' to mod-feltext                       
066800       END-IF                                                             
066900     WHEN '950'                                                           
067000       IF MSGI-IDLAND-SPR = 'GB'                                          
067100         move 'Error in company NO' to mod-feltext                        
067200       ELSE                                                               
067300         move 'Fel i företagskod'   to mod-feltext                        
067400       END-IF                                                             
067500     WHEN '951'                                                           
067600       IF MSGI-IDLAND-SPR = 'GB'                                          
067700         move 'Error in campaign ref' to mod-feltext                      
067800       ELSE                                                               
067900         move 'Fel i kampanj-referens' to mod-feltext                     
068000       END-IF                                                             
068100     WHEN '952'                                                           
068200       IF MSGI-IDLAND-SPR = 'GB'                                          
068300         move 'Error in invoice type' to mod-feltext                      
068400       ELSE                                                               
068500         move 'Fel i fatura-typ    ' to mod-feltext                       
068600       END-IF                                                             
068700     WHEN '953'                                                           
068800       IF MSGI-IDLAND-SPR = 'GB'                                          
068900         move 'Error in TPO-type' to mod-feltext                          
069000       ELSE                                                               
069100         move 'Fel i order TPO-typ ' to mod-feltext                       
069200       END-IF                                                             
069300     WHEN '954'                                                           
069400       IF MSGI-IDLAND-SPR = 'GB'                                          
069500         move 'Error in TPO-date' to mod-feltext                          
069600       ELSE                                                               
069700         move 'Fel order TPO-datum' to mod-feltext                        
069800       END-IF                                                             
069900     WHEN '223'                                                           
070000       IF MSGI-IDLAND-SPR = 'GB'                                          
070100         move 'TVA-instruction missing' to mod-feltext                    
070200       ELSE                                                               
070300         move 'TVA-instruktion saknas ' to mod-feltext                    
070400       END-IF                                                             
070500     WHEN '703'                                                           
070600       IF MSGI-IDLAND-SPR = 'GB'                                          
070700         move 'Order already registered' to mod-feltext                   
070800       ELSE                                                               
070900         move 'Order redan registerad' to mod-feltext                     
071000       END-IF                                                             
071100     WHEN '401'                                                           
071200       IF MSGI-IDLAND-SPR = 'GB'                                          
071300         move 'Wrong keys'            to mod-feltext                      
071400       ELSE                                                               
071500         move 'Felaktiga nycklar'     to mod-feltext                      
071600       END-IF                                                             
071700     WHEN '701'                                                           
071800       IF MSGI-IDLAND-SPR = 'GB'                                          
071900         move 'Order head missing'    to mod-feltext                      
072000       ELSE                                                               
072100         move 'Order-huvud saknas '    to mod-feltext                     
072200       END-IF                                                             
072300     WHEN '718'                                                           
072400       IF MSGI-IDLAND-SPR = 'GB'                                          
072500         move 'Order already completed' to mod-feltext                    
072600       ELSE                                                               
072700         move 'Order redan kompletterad' to mod-feltext                   
072800       END-IF                                                             
072900     WHEN OTHER                                                           
073000       IF MSGI-IDLAND-SPR = 'GB'                                          
073100         move 'Error text missing'      to mod-feltext                    
073200       ELSE                                                               
073300         move 'Feltext saknas '         to mod-feltext                    
073400       END-IF                                                             
073500     END-EVALUATE                                                         
073600     MOVE 'END FB-SHOW-MESSAGE  SEC '      TO PGMPOS                      
073700     .                                                                    
073800     EJECT                                                                
073900 G-KOLLA-INPUT SECTION.                                                   
074000     MOVE 'STA G-KOLLA-INPUT SEC    '      TO PGMPOS                      
074100                                                                          
074200     MOVE JA  TO INDATA-SW                                                
074300                                                                          
074400     IF MID-KDCMDVAL = 'D  ' OR 'DEL' OR 'R  ' OR 'STA'                   
074500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR                     
074600     ELSE                                                                 
074700       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
074800       MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR                     
074900       MOVE NEJ TO INDATA-SW                                              
075000       MOVE MFS-ROER-EJ-FAELT    TO MOD-IDMFSMED                          
075100       MOVE MFS-ROER-EJ-FAELT    TO MOD-FELTEXT                           
075200     END-IF                                                               
075300                                                                          
075400     MOVE MID-IDSNDNOD           TO W-IDSNDNOD                            
075500     MOVE MID-IDSNDJOB           TO W-IDSNDJOB                            
075600                                                                          
075700     MOVE MID-TIREGDAT           TO WS-SEQA-TIREGDAT                      
075800     MOVE WS-SEQA-TIREGDAT       TO W-TIREGDAT                            
075900     MOVE MID-TIKLOCK            TO WS-SEQA-TIKLOCK                       
076000     MOVE WS-SEQA-TIKLOCK        TO W-TIKLOCK                             
076100                                                                          
076200     IF INDATA-OK                                                         
076300       PERFORM IMS-GU-WDP801                                              
076400       IF SEGMENT-SAKNAS                                                  
076500         MOVE NEJ TO INDATA-SW                                            
076600         MOVE ERR-EMPTY TO MED-IDMFSFEL                                   
076700       ELSE                                                               
076800                                                                          
076900         MOVE +0 TO W-IDRADNR                                             
077000         PERFORM IMS-GNP-P811-TRANS                                       
077100         IF SEGMENT-FINNS                                                 
077200           MOVE TRAN-TRANSDATA(51:2)       TO W-IDDC                      
077300           PERFORM IMS-GU-WDB601                                          
077400                                                                          
077500           IF SEGMENT-FINNS                                               
077600             MOVE DCS-IDDISTR-REFILL  TO WS-IDDISTR-REFILL                
077700             MOVE DCS-IDDISTR-RETUR   TO WS-IDDISTR-RETUR                 
077800             MOVE DCS-IDDISTR-QRETUR  TO WS-IDDISTR-QRETUR                
077900             MOVE DCS-IDDISTR-SKROT   TO WS-IDDISTR-SKROT                 
078000             MOVE DCS-IDDISTR-QSKROT  TO WS-IDDISTR-QSKROT                
078100             MOVE DCS-IDDISTR-RSKROT  TO WS-IDDISTR-RSKROT                
078200                                                                          
078300             IF TRAN-TRANSDATA(18:4) = WS-IDDISTR-REFILL                  
078400             OR WS-IDDISTR-RETUR                                          
078500             OR WS-IDDISTR-QRETUR                                         
078600             OR WS-IDDISTR-SKROT                                          
078700             OR WS-IDDISTR-QSKROT                                         
078800             OR WS-IDDISTR-RSKROT                                         
078900                                                                          
079000               MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                
079100               STRING 'DISTRICT: '                                        
079200                      TRAN-TRANSDATA(18:4)                                
079300                      ' NOT ALLOWED TO BE DELETED.'                       
079400               DELIMITED BY SIZE INTO MOD-TEMFSINF                        
079500               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR               
079600               MOVE NEJ TO INDATA-SW                                      
079700               MOVE MFS-ROER-EJ-FAELT  TO MOD-IDMFSMED                    
079800               MOVE MFS-ROER-EJ-FAELT  TO MOD-FELTEXT                     
080200             END-IF                                                       
080600           END-IF                                                         
080700         END-IF                                                           
080800       END-IF                                                             
080900     END-IF                                                               
081000                                                                          
081100     IF INDATA-FEL                                                        
081200       CALL WMEDKONV USING MED-WMEDAREA                                   
081300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
081400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
081500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
081600     END-IF                                                               
081700     MOVE 'END G-KOLLA-INPUT SEC    '      TO PGMPOS                      
081800     .                                                                    
081900     EJECT                                                                
082000 H-UPPDATERA    SECTION.                                                  
082100     MOVE 'STA H-UPPDATERA       SEC'      TO PGMPOS                      
082200                                                                          
082300     PERFORM IMS-GHU-WDP801                                               
082500                                                                          
082600     IF MID-KDCMDVAL = 'D  ' OR 'DEL'                                     
082700       PERFORM IMS-DLET-WDP801                                            
082800     ELSE                                                                 
082900                                                                          
083000       IF MID-KDCMDVAL = 'R  ' OR 'STA'                                   
083100         PERFORM HA-STARTA                                                
083300       END-IF                                                             
083400     END-IF                                                               
083500                                                                          
083600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
083700     CALL WMEDKONV USING MED-WMEDAREA                                     
083800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
083900     PERFORM MFS-FORM-ATTR                                                
084000     PERFORM MFS-RENSA-FAELT-IN                                           
084100* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
084200     MOVE 'END H-UPPDATERA       SEC'      TO PGMPOS                      
084300     .                                                                    
084400     EJECT                                                                
084500 HA-STARTA SECTION.                                                       
084600     MOVE 'STA HA-STARTA         SEC'      TO PGMPOS                      
084700                                                                          
085400     MOVE 'K' TO KOM-KDKOMSTA                                             
085500     MOVE ' ' TO KOM-KDKOMBEH                                             
085600     PERFORM IMS-REPL-P801-ROT                                            
085700     MOVE +54 TO MSG-KOM-KVLL                                             
085800     MOVE LOW-VALUE TO MSG-KOM-KDZ1                                       
085900                       MSG-KOM-KDZ2                                       
086000     MOVE 'W0T693X ' TO MSG-KOM-KDTRANS                                   
086100     MOVE KOM-IDCPYTXT TO MSG-KOM-IDCPYTXT                                
086200     MOVE KOM-IDSNDNOD TO MSG-KOM-IDSNDNOD                                
086300     MOVE KOM-IDSNDJOB TO MSG-KOM-IDSNDJOB                                
086400     MOVE KOM-TIREGDAT TO MSG-KOM-TIREGDAT                                
086500     MOVE KOM-TIKLOCK  TO MSG-KOM-TIKLOCK                                 
086600     MOVE SPACE TO MSG-KOM-IDMFSMED                                       
086700                   MSG-KOM-KDSVAR                                         
086800     PERFORM IMS-INSERT-KOM-MSG                                           
087500     MOVE 'END HA-STARTA         SEC'      TO PGMPOS                      
087600     .                                                                    
087700     EJECT                                                                
087800 MFS-RENSA-FAELT-UT SECTION.                                              
087900                                                                          
088000*    --- ALLA UTDATA-FÄLT                                                 
088100     MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL                                 
088200                             MOD-IDSNDNOD                                 
088300                             MOD-TIREGDAT                                 
088400                             MOD-TIKLOCK                                  
088500                             MOD-IDUSER                                   
088600                             MOD-KDKOMSTA                                 
088700                             MOD-IDSNDJOB                                 
088800     .                                                                    
088900     SKIP3                                                                
089000 MFS-RENSA-FAELT-IN SECTION.                                              
089100                                                                          
089200*    --- ALLA INDATA-FÄLT                                                 
089300     MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL                                 
089400                             MOD-IDSNDNOD                                 
089500                             MOD-TIREGDAT                                 
089600                             MOD-TIKLOCK                                  
089700                             MOD-IDUSER                                   
089800                             MOD-KDKOMSTA                                 
089900                             MOD-IDSNDJOB                                 
090000     .                                                                    
090100     EJECT                                                                
090200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
090300                                                                          
090400*    --- ALLA UTDATA-FÄLT                                                 
090500     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL                               
090600                               MOD-IDSNDNOD                               
090700                               MOD-TIREGDAT                               
090800                               MOD-TIKLOCK                                
090900                               MOD-IDUSER                                 
091000                               MOD-KDKOMSTA                               
091100                               MOD-IDSNDJOB                               
091200     .                                                                    
091300     SKIP3                                                                
091400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
091500                                                                          
091600*    --- ALLA INDATA-FÄLT                                                 
091700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL                               
091800     .                                                                    
091900     EJECT                                                                
092000 MFS-FORM-ATTR SECTION.                                                   
092100                                                                          
092200*    --- ALLA INDATA-FÄLT                                                 
092300     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR                         
092400     .                                                                    
092500     SKIP2                                                                
092600*MFS-LAES-IN-IGEN SECTION.                                                
092700*                                                                         
092800*    --- ALLA INDATA-FÄLT                                                 
092900*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR                      
093000*    .                                                                    
093100*    EJECT                                                                
093200* --- IMS SEKTIONER ---                                                   
093300     SKIP3                                                                
093400 IMS-GET-MSG SECTION.                                                     
093500     MOVE 'IMS-GET-MSG           SEC'      TO IMSPOS                      
093600                                                                          
093700     MOVE '  QC' TO GODK-STATUSKODER                                      
093800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
093900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200     SKIP3                                                                
094300 IMS-INSERT-MSG SECTION.                                                  
094400     MOVE 'IMS-INSERT-MSG        SEC'      TO IMSPOS                      
094500                                                                          
094600     IF MSGI-IDLAND-SPR = 'GB'                                            
094700       MOVE 'N' TO MFS-KDHUVOMR                                           
094800     END-IF                                                               
094900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095000     MOVE SPACE TO GODK-STATUSKODER                                       
095100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
095200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     EJECT                                                                
095600 IMS-INSERT-KOM-MSG SECTION.                                              
095700     MOVE 'IMS-INSERT-KOM-MSG    SEC'      TO IMSPOS                      
095800                                                                          
095900     MOVE SPACE TO GODK-STATUSKODER                                       
096000     CALL CBLTDLI USING ISRT ALT-PCB MSG-KOM-WMSGKOM                      
096100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
096200     PERFORM IMS-STATUSKONTROLL                                           
096300     .                                                                    
096400     EJECT                                                                
096500 IMS-GHU-WDP801   SECTION.                                                
096600     MOVE 'IMS-GHU-WDP801      SEC'      TO IMSPOS                        
096700                                                                          
096800     STRING 'WDP801  (WDP801KY =' W-WDP801KY-X ')'                        
096900          DELIMITED BY SIZE INTO SSA1                                     
097000     MOVE '    ' TO GODK-STATUSKODER                                      
097100     CALL CBLTDLI USING GHU WDP8-PCB DLI-IO-P801 SSA1                     
097200     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
097300     PERFORM IMS-STATUSKONTROLL                                           
097400     .                                                                    
097500                                                                          
097600 IMS-GU-WDP801   SECTION.                                                 
097700     MOVE 'IMS-GU-WDP801      SEC'      TO IMSPOS                         
097800                                                                          
097900     STRING 'WDP801  (WDP801KY =' W-WDP801KY-X ')'                        
098000          DELIMITED BY SIZE INTO SSA1                                     
098100     MOVE '  GE' TO GODK-STATUSKODER                                      
098200     CALL CBLTDLI USING GU WDP8-PCB DLI-IO-P801 SSA1                      
098300     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
098400     PERFORM IMS-STATUSKONTROLL                                           
098500     .                                                                    
098600                                                                          
098700 IMS-DLET-WDP801 SECTION.                                                 
098800     MOVE 'IMS-DLET-WDP801     SEC'      TO IMSPOS                        
098900                                                                          
099000     MOVE '  ' TO GODK-STATUSKODER                                        
099100     CALL CBLTDLI USING DLET WDP8-PCB DLI-IO-P801                         
099200     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
099300     PERFORM IMS-STATUSKONTROLL                                           
099400     .                                                                    
099500 IMS-GU-WDP8A1KY                SECTION.                                  
099600     MOVE 'IMS-GU-WDP8A1KY       SEC'      TO IMSPOS                      
099700                                                                          
099800     STRING 'WDP8A1  (WDP8A1KY=>' W-WDP8A1KY-MIN                          
099900                    '&WDP8A1KY<=' W-WDP8A1KY-MAX                          
100000                    '&KDKOMSTA =' WS-KDKOMSTA-F ')'                       
100100             DELIMITED BY SIZE INTO SSA1                                  
100200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
100300     CALL CBLTDLI USING GU WDP8A-PCB DLI-IO-P8A1 SSA1                     
100400     MOVE WDP8A-STATUS-CODE TO STATUS-WS                                  
100500     PERFORM IMS-STATUSKONTROLL                                           
100600     .                                                                    
100700     SKIP3                                                                
100800 IMS-GNP-P811-TRANS SECTION.                                              
100900     MOVE 'IMS-GNP-P811-TRANS    SEC'      TO IMSPOS                      
101000                                                                          
101100     STRING 'WDP811  (IDRADNR =>' W-IDRADNR-X ')'                         
101200          DELIMITED BY SIZE INTO SSA1                                     
101300     MOVE '  GE' TO GODK-STATUSKODER                                      
101400     CALL CBLTDLI USING GNP WDP8-PCB DLI-IO-P811 SSA1                     
101500     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
101600     PERFORM IMS-STATUSKONTROLL                                           
101700     .                                                                    
101800                                                                          
103000 IMS-REPL-P801-ROT SECTION.                                               
103100     MOVE 'IMS-REPL-P801-ROT     SEC'      TO IMSPOS                      
103200                                                                          
103300     MOVE '  ' TO GODK-STATUSKODER                                        
103400     CALL CBLTDLI USING REPL WDP8-PCB DLI-IO-P801                         
103500     MOVE WDP8-STATUS-CODE TO STATUS-WS                                   
103600     PERFORM IMS-STATUSKONTROLL                                           
103700     .                                                                    
103800                                                                          
103900 IMS-GU-WDB601 SECTION.                                                   
104000     MOVE 'IMS-GHU-WDB601        SEC'      TO IMSPOS                      
104100                                                                          
104200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
104300          DELIMITED BY SIZE INTO SSA1                                     
104400     MOVE '  GE' TO GODK-STATUSKODER                                      
104500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB6 SSA1                      
104600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     .                                                                    
104900     SKIP3                                                                
105000 IMS-STATUSKONTROLL SECTION.                                              
105100                                                                          
105200     SET STATUS-IX TO 1                                                   
105300     SEARCH GODK-STATUS                                                   
105400       AT END                                                             
105500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
105600         DELIMITED BY SIZE INTO FELTEXT                                   
105700         CALL FELLOG                                                      
105800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
105900         CONTINUE                                                         
106000     END-SEARCH                                                           
106100     .                                                                    
