000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5016400.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/01/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAM SOM LÄSER DETALJINFO. FRÅN SALDODB(WDL9).                
000900*        START FRÅN BILD 5162/5163 OCH ÅTERHOPP TILL SAMMA BILD           
001000*        M.H.A "ENTER"-TRYCKNING.                                         
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLLOGA (WDL9)                              
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W5T164                                              
001700*        MID:         W5I164N1                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W5O164N1                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W5016400'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
003900     88  ALLT-OK                             VALUE 'J'.                   
004000                                                                          
004100 77  BYT-SW                      PIC X       VALUE 'J'.                   
004200     88  BYT-BILD                            VALUE 'J'.                   
004300     88  BYT-EJ-BILD                         VALUE 'N'.                   
004400                                                                          
004500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004600     88  NYCKLAR-OK                          VALUE 'J'.                   
004700     88  NYCKLAR-FEL                         VALUE 'N'.                   
004800                                                                          
004900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005000     88  EGEN-MID                            VALUE '5164'.                
005100     88  GODK-MID                            VALUE '5162'                 
005200                                                   '5163'                 
005300                                                   '5164'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500     EJECT                                                                
005600*    ---AREA FÖR ATT HÄMTA UPP MSGI-SPAR-AREA TILL PGM                    
005700 01  SPAR-AREA.                                                           
005800     03  SPAR-IDARTNR            PIC S9(9)   COMP-3.                      
005900     03  SPAR-DAREGDAT           PIC 9(8).                                
006000     03  SPAR-TIKLOCK            PIC S9(9)   COMP-3.                      
006100     03  SPAR-IDSEKVNR           PIC S9(3)   COMP-3.                      
006200     03  SPAR-IDARTNR-UT         PIC 9(9).                                
006300     03  SPAR-IDDC               PIC X(2).                                
006400     03  SPAR-IDHUVTYP           PIC X(4).                                
006500     03  SPAR-IDSUBTYP           PIC X(3).                                
006600     03  SPAR-TIREGDAT-FOM       PIC 9(6).                                
006700     03  SPAR-TIREGDAT-TOM       PIC 9(6).                                
006800     03  SPAR-IDTRANS            PIC X(4).                                
006900     03  FILLER                  PIC X(389).                              
007000     03  SPAR-BILD               PIC X(4).                                
007100     EJECT                                                                
007200*    ARBETSAREA FÖR REFERENSFÄLT                                          
007300 01  SPAR-UREF.                                                           
007400     03  SPAR-UREF1                    PIC X(25)   VALUE SPACE.           
007500     03  SPAR-IDGMTREF REDEFINES SPAR-UREF1.                              
007600         05  SPAR-IDDISTR              PIC 9(5).                          
007700         05  FILLER                    PIC X.                             
007800         05  SPAR-IDKUNDNR             PIC 9(7).                          
007900         05  FILLER                    PIC X.                             
008000         05  SPAR-IDKUNDRF             PIC X(10).                         
008100         05  SPAR-IDKUNDRF-FILLER REDEFINES SPAR-IDKUNDRF.                
008200             07  SPAR-IDORDNR5         PIC 9(5).                          
008300     03  SPAR-IDLEVREF-FILLER REDEFINES SPAR-UREF1.                       
008400         05  SPAR-IDLEVREF.                                               
008500             07  SPAR-IDLOPNRM         PIC 9(9).                          
008600             07  FILLER                PIC X.                             
008700             07  SPAR-IDLEVNR          PIC X(5).                          
008800             07  FILLER                PIC X(9).                          
008900     03  SPAR-IDPRCREF-FILLER REDEFINES SPAR-UREF1.                       
009000         05  SPAR-IDPRCREF.                                               
009100             07  SPAR-IDPRCBAS         PIC X(3).                          
009200             07  FILLER                PIC X.                             
009300             07  SPAR-IDPRCVAR         PIC X.                             
009400             07  FILLER                PIC X.                             
009500             07  SPAR-IDUSER-IDPRCREF  PIC X(8).                          
009600             07  FILLER                PIC X(5).                          
009700     03  SPAR-IDLBREF-FILLER REDEFINES SPAR-UREF1.                        
009800         05  SPAR-IDLBREF.                                                
009900             07  SPAR-TIFAKT           PIC 9(7).                          
010000             07  FILLER                PIC X.                             
010100             07  SPAR-IDLBBET          PIC X(12).                         
010200             07  FILLER                PIC X.                             
010300     03  SPAR-IDANSTNR-FILLER REDEFINES SPAR-UREF1.                       
010400         05  SPAR-IDANSTREF.                                              
010500             07  SPAR-IDANSTNR         PIC 9(5).                          
010600             07  FILLER                PIC X(20).                         
010700     03  SPAR-IDDC-SEND-FILLER REDEFINES SPAR-UREF1.                      
010800         05  SPAR-IDDC-SEND            PIC X(2).                          
010900         05  FILLER                    PIC X(15).                         
011000                                                                          
011100     03  SPAR-UREF2                    PIC X(25)   VALUE SPACE.           
011200     03  SPAR-IDFAKT-FILLER REDEFINES SPAR-UREF2.                         
011300         05  SPAR-IDFAKT               PIC 9(7).                          
011400         05  FILLER                    PIC X(14).                         
011500     03  SPAR-IDKOLLI-FILLER REDEFINES SPAR-UREF2.                        
011600         05  SPAR-IDKOLLI              PIC 9(5).                          
011700         05  FILLER                    PIC X(15).                         
011800     03  SPAR-IDPRODREF-FILLER REDEFINES SPAR-UREF2.                      
011900         05  SPAR-IDPRODREF.                                              
012000             07  SPAR-IDPRODNR         PIC 9(7).                          
012100             07  FILLER                PIC X.                             
012200             07  SPAR-IDPLKLST         PIC 9(3).                          
012300             07  FILLER                PIC X(12).                         
012400     03  SPAR-IDFS-FILLER REDEFINES SPAR-UREF2.                           
012500         05  SPAR-IDFS             PIC X(8).                              
012600         05  FILLER                PIC X(10).                             
012700     03  SPAR-IDRAPPNR-FILLER REDEFINES SPAR-UREF2.                       
012800         05  SPAR-IDRAPPNR         PIC 9(7).                              
012900         05  FILLER                PIC X(11).                             
013000     03  SPAR-IDAVINR-FILLER REDEFINES SPAR-UREF2.                        
013100         05  SPAR-IDAVINR          PIC 9(7).                              
013200         05  FILLER                PIC X(14).                             
013300     03  SPAR-IDKR-FILLER REDEFINES SPAR-UREF2.                           
013400         05  SPAR-IDKR             PIC 9(5).                              
013500         05  FILLER                PIC X(13).                             
013600     03  SPAR-IDCLEARREF-FILLER REDEFINES SPAR-UREF2.                     
013700         05  SPAR-IDCLEARREF       PIC 9(7).                              
013800         05  FILLER                PIC X(14).                             
013900     EJECT                                                                
014000*    ---GENERELLA ARBETSFÄLT                                              
014100 01  WS-IDDISTR                  PIC 9(5).                                
014200 01  WS-IDKUNDNR                 PIC 9(7).                                
014300 01  WS-IDFAKT                   PIC 9(7).                                
014400 01  WS-IDKOLLI                  PIC 9(5).                                
014500 01  WS-IDANSTNR                 PIC 9(5).                                
014600 01  WS-IDPRODNR                 PIC 9(7).                                
014700 01  WS-IDPRCBAS                 PIC X(3).                                
014800 01  WS-IDUSER-IDPRCREF          PIC X(8).                                
014900 01  WS-IDPLKLST                 PIC 9(3).                                
015000 01  WS-IDORDNR5                 PIC 9(5).                                
015100 01  WS-IDKUNDRF                 PIC X(10).                               
015200 01  WS-IDRAPPNR                 PIC 9(7).                                
015300 01  WS-IDAVINR                  PIC 9(7).                                
015400 01  WS-IDKR                     PIC 9(5).                                
015500 01  WS-IDLOPNRM                 PIC 9(9).                                
015600 01  WS-IDFS                     PIC X(8).                                
015700 01  WS-TIFAKT                   PIC 9(7).                                
015800 01  WS-IDLBBET                  PIC X(12).                               
015900 01  WS-IDARTNR                  PIC 9(9).                                
016000 01  WS-IDCLEARREF               PIC 9(7).                                
016100 01  WS-IDDC-SEND                PIC X(2).                                
016200 01  W-DAREGDAT                  PIC 9(8).                                
016300 01  W-TID                       PIC S9(9)   COMP-3.                      
016400 01  WS-TID                      PIC 9(9).                                
016500     EJECT                                                                
016600 01  W-AREA.                                                              
016700     03  WS-DAREGDAT-AAR         PIC 9(2).                                
016800     03  WS-DAREGDAT-TI          PIC 9(6).                                
016900                                                                          
017000                                                                          
017100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017200 01  GENERELLA-SUBPROGRAM.                                                
017300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
017400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017700     EJECT                                                                
017800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017900*01 -COPY WMEDAREA                                                        
018000     SKIP3                                                                
018100 01  MESSAGE-CODES.                                                       
018200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018300     03  INF-PRESS-ENTER         PIC X(3)    VALUE '283'.                 
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018600*                                                                         
018700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018800     SKIP3                                                                
018900*01 -COPY WMSGINIT                                                        
019000     EJECT                                                                
019100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019200*                                                                         
019300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019400     SKIP3                                                                
019500*01  MID -COPY W5I16401                                                   
019600     EJECT                                                                
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019900     SKIP3                                                                
020000*01  -COPY WMSGAREA                                                       
020100     EJECT                                                                
020200     03  MOD REDEFINES MSG-AREA.                                          
020300*      05  -COPY W5O16401                                                 
020400     EJECT                                                                
020500 01  W-PROG-TO-PROG-SW-5162.                                              
020600     03  M-SW-LL-5162            PIC S9(4)   VALUE +240 COMP SYNC.        
020700     03  M-SW-Z1-Z2-5162         PIC X(2)    VALUE LOW-VALUE.             
020800     03  M-SW-KDTRANS-5162       PIC X(8)    VALUE 'W5T162  '.            
020900     03  M-SW-IDTRANS-5162       PIC X(4)    VALUE '5164'.                
021000     03  M-SW-KDMFSTYP-5162      PIC X(1)    VALUE '2'.                   
021100                                                                          
021200*    03  MID -COPY W5I16201 -PRE 5162-                                    
021300     EJECT                                                                
021400 01  W-PROG-TO-PROG-SW-5163.                                              
021500     03  M-SW-LL-5163            PIC S9(4)   VALUE +240 COMP SYNC.        
021600     03  M-SW-Z1-Z2-5163         PIC X(2)    VALUE LOW-VALUE.             
021700     03  M-SW-KDTRANS-5163       PIC X(8)    VALUE 'W5T163  '.            
021800     03  M-SW-IDTRANS-5163       PIC X(4)    VALUE '5164'.                
021900     03  M-SW-KDMFSTYP-5163      PIC X(1)    VALUE '2'.                   
022000                                                                          
022100*    03  MID -COPY W5I16301 -PRE 5163-                                    
022200     EJECT                                                                
022300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022400     SKIP3                                                                
022500*01  -COPY WMFSAREA                                                       
022600     EJECT                                                                
022700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022800*                                                                         
022900     EJECT                                                                
023000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023100     SKIP3                                                                
023200 01  NYCKLAR-TILL-DLI.                                                    
023300     03  W-WDL901KY-X.                                                    
023400         05  W-IDARTNR-X         PIC S9(9) COMP-3.                        
023500         05  W-DAREGDAT-X        PIC 9(8).                                
023600         05  W-TIKLOCK-X         PIC S9(9) COMP-3.                        
023700         05  W-IDSEKVNR-X        PIC S9(3) COMP-3.                        
023800     SKIP2                                                                
023900     03  W-IDARTNR-WDD3-X.                                                
024000         05  W-IDARTNR-WDD3      PIC S9(9)  VALUE ZERO COMP-3.            
024100     03  W-IDSKYLT-X.                                                     
024200         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
024300     SKIP2                                                                
024400*    --- STATUS-KOD FRÅN IMS                                              
024500 01  STATUS-WS                   PIC XX.                                  
024600     88  SEGMENT-FINNS                       VALUE '  '.                  
024700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024900     SKIP2                                                                
025000 01  GODK-STATUSKODER.                                                    
025100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025200     SKIP3                                                                
025300 01  SSA1                        PIC X(64).                               
025400 01  SSA2                        PIC X(64).                               
025500     EJECT                                                                
025600*    --- IMS FUNKTIONSKODER                                               
025700*01  -COPY W0003                                                          
025800     EJECT                                                                
025900*    ---  DLI INPUT-OUTPUT AREA                                           
026000                                                                          
026100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
026200 01  DLI-IO-WLLOGA01.                                                     
026300*    03  -COPY WDL901                                                     
026400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA01'.                    
026500 01  DLI-IO-WLBENA01.                                                     
026600*    03  -COPY WDD311                                                     
026700     EJECT                                                                
026800 LINKAGE SECTION.                                                         
026900*01  -COPY W0009   -PRE MSG-                                              
027000     EJECT                                                                
027100*01  -COPY W0009   -PRE ALT1-                                             
027200     EJECT                                                                
027300*01  -COPY W0009   -PRE ALT2-                                             
027400     EJECT                                                                
027500*01  -COPY W0008   -PRE USEA-                                             
027600     05  FILLER                  PIC X.                                   
027700                                                                          
027800*01  -COPY W0008  -PRE LOGA-                                              
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100*01  -COPY W0008  -PRE BENA-                                              
028200     05  FILLER                  PIC X.                                   
028300     EJECT                                                                
028400 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB USEA-PCB             
028500                           LOGA-PCB BENA-PCB.                             
028600 MAIN SECTION.                                                            
028700     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB USEA-PCB             
028800                           LOGA-PCB BENA-PCB.                             
028900                                                                          
029000     PERFORM IMS-GET-MSG                                                  
029100     IF SEGMENT-FINNS                                                     
029200       PERFORM A-INIT                                                     
029300       PERFORM B-KOLLA-NYCKLAR                                            
029400       IF NYCKLAR-OK                                                      
029500          IF MFS-ENTER AND EGEN-MID                                       
029600             PERFORM C-BYT-BILD                                           
029700          END-IF                                                          
029800          IF ALLT-OK                                                      
029900             PERFORM F-LAES-VISA-INFO                                     
030000          END-IF                                                          
030100       END-IF                                                             
030200       IF BYT-EJ-BILD                                                     
030300          COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16401 + 4                   
030400          PERFORM IMS-INSERT-MSG                                          
030500       END-IF                                                             
030600     END-IF                                                               
030700                                                                          
030800     MOVE ZERO TO RETURN-CODE                                             
030900     GOBACK                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 A-INIT SECTION.                                                          
031300                                                                          
031400     IF MSG-DUBBLA-TRANSKODER                                             
031500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I16401                 
031600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031800     ELSE                                                                 
031900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I16401                  
032000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
032100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032200     END-IF                                                               
032300                                                                          
032400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032700                                                                          
032800     MOVE LOW-VALUE TO MSG-AREA                                           
032900     MOVE 'W5O164N1' TO MFS-IDMOD                                         
033000     MOVE '5164' TO MOD-IDTRANS                                           
033100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033200                                                                          
033300                                                                          
033400     IF EGEN-MID OR HELP-MID                                              
033500       CONTINUE                                                           
033600     ELSE                                                                 
033700       MOVE SPACE TO MFS-KDTRTYP                                          
033800       MOVE '7' TO MFS-IDPFK                                              
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 B-KOLLA-NYCKLAR SECTION.                                                 
034300                                                                          
034400     MOVE JA TO NYCKLAR-SW                                                
034500                                                                          
034600* ---HÄR GÖRS INGEN KONTROLL AV NYCKLAR EFTERSOM MAN                      
034700* ---INTE HAR NÅGRA VALBARA FÄLT. DÄRFÖR FLYTTAS                          
034800* ---MID-AREAN TILL MOD-AREAN DIREKT                                      
034900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
035000     MOVE '001'             TO MSGI-KDCALL                                
035100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
035200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
035300     MOVE '5164'            TO MSGI-IDTRANS                               
035400                                                                          
035500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035600                                                                          
035700     IF GODK-MID                                                          
035800        MOVE MSGI-SPAR-AREA      TO SPAR-AREA                             
035900        MOVE MID-IDARTNR-UT      TO WS-IDARTNR                            
036000        INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE                
036100        MOVE WS-IDARTNR          TO MOD-IDARTNR-UT                        
036200        MOVE MID-IDDC-UT         TO MOD-IDDC-UT                           
036300        MOVE MID-IDHUVTYP-UT     TO MOD-IDHUVTYP-UT                       
036400        MOVE MID-IDSUBTYP-UT     TO MOD-IDSUBTYP-UT                       
036500        MOVE MID-TIREGDAT-FOM-UT TO MOD-TIREGDAT-FOM-UT                   
036600        MOVE MID-TIREGDAT-TOM-UT TO MOD-TIREGDAT-TOM-UT                   
036700        MOVE MID-IDTRANS-UT      TO MOD-IDTRANS-UT                        
036800     END-IF                                                               
036900                                                                          
037000* ---ANVÄNDS FÖR ATT SKICKA TILLBAKA MID-VÄRDEN                           
037100* ---TILL 5162/5163-BILDEN                                                
037200     IF NOT GODK-MID            OR                                        
037300       (GODK-MID AND                                                      
037400        SPAR-BILD NOT = '5162' AND '5163')                                
037500       MOVE NEJ TO NYCKLAR-SW                                             
037600     ELSE                                                                 
037700       IF NOT EGEN-MID                                                    
037800          MOVE MID-IDARTNR-UT      TO SPAR-IDARTNR-UT                     
037900          MOVE MID-IDDC-UT         TO SPAR-IDDC                           
038000          MOVE MID-IDHUVTYP-UT     TO SPAR-IDHUVTYP                       
038100          MOVE MID-IDSUBTYP-UT     TO SPAR-IDSUBTYP                       
038200          MOVE MID-TIREGDAT-FOM-UT TO SPAR-TIREGDAT-FOM                   
038300          MOVE MID-TIREGDAT-TOM-UT TO SPAR-TIREGDAT-TOM                   
038400          MOVE MID-IDTRANS-UT      TO SPAR-IDTRANS                        
038500       END-IF                                                             
038600     END-IF                                                               
038700     IF MSGI-IDLAND-SPR = 'SE'                                            
038800        MOVE 'S' TO MED-IDSKYLT                                           
038900        MOVE 'S' TO W-IDSKYLT                                             
039000     ELSE                                                                 
039100        IF MSGI-IDLAND-SPR = 'GB'                                         
039200            MOVE 'GB' TO MED-IDSKYLT                                      
039300            MOVE 'GB' TO W-IDSKYLT                                        
039400        ELSE                                                              
039500            MOVE 'US' TO MED-IDSKYLT                                      
039600            MOVE 'US' TO W-IDSKYLT                                        
039700        END-IF                                                            
039800     END-IF                                                               
039900     MOVE NEJ TO BYT-SW                                                   
040000     MOVE JA TO ALLT-SW                                                   
040100                                                                          
040200     IF NYCKLAR-FEL                                                       
040300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
040400       CALL WMEDKONV USING MED-WMEDAREA                                   
040500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
040600       PERFORM MFS-RENSA-FAELT-UT                                         
040700     END-IF                                                               
040800     .                                                                    
040900     EJECT                                                                
041000 C-BYT-BILD SECTION.                                                      
041100     SKIP2                                                                
041200     MOVE JA TO BYT-SW                                                    
041300     MOVE NEJ TO ALLT-SW                                                  
041400                                                                          
041500* ---SKICKAR VÄRDE TILL 5162-MID FÖR ATT SEDAN                            
041600* ---STARTA UPP 5162-BILDEN                                               
041700     IF SPAR-BILD = '5162'                                                
041800       MOVE LOW-VALUE            TO 5162-MID-W5I16201                     
041900       MOVE SPAR-IDARTNR-UT      TO 5162-MID-IDARTNR-IN                   
042000       MOVE SPAR-IDDC            TO 5162-MID-IDDC-IN                      
042100       MOVE SPAR-IDHUVTYP        TO 5162-MID-IDHUVTYP-IN                  
042200       MOVE SPAR-IDSUBTYP        TO 5162-MID-IDSUBTYP-IN                  
042300       MOVE SPAR-TIREGDAT-FOM    TO 5162-MID-TIREGDAT-IN1                 
042400       MOVE SPAR-TIREGDAT-TOM    TO 5162-MID-TIREGDAT-IN2                 
042500       MOVE SPAR-IDTRANS         TO 5162-MID-IDTRANS-IN                   
042600       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16401 + 17                     
042700       PERFORM IMS-INSERT-ALT-MSG-5162                                    
042800     ELSE                                                                 
042900                                                                          
043000* ---SKICKAR VÄRDE TILL 5163-MID FÖR ATT SEDAN                            
043100* ---STARTA UPP 5163-BILDEN                                               
043200       MOVE LOW-VALUE            TO 5163-MID-W5I16301                     
043300       MOVE SPAR-IDARTNR-UT      TO 5163-MID-IDARTNR-IN                   
043400       MOVE SPAR-IDDC            TO 5163-MID-IDDC-IN                      
043500       MOVE SPAR-IDHUVTYP        TO 5163-MID-IDHUVTYP-IN                  
043600       MOVE SPAR-IDSUBTYP        TO 5163-MID-IDSUBTYP-IN                  
043700       MOVE SPAR-TIREGDAT-FOM    TO 5163-MID-TIREGDAT-IN1                 
043800       MOVE SPAR-TIREGDAT-TOM    TO 5163-MID-TIREGDAT-IN2                 
043900       MOVE SPAR-IDTRANS         TO 5163-MID-IDSALDO-IN                   
044000       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16401 + 17                     
044100       PERFORM IMS-INSERT-ALT-MSG-5163                                    
044200     END-IF                                                               
044300     .                                                                    
044400     EJECT                                                                
044500 F-LAES-VISA-INFO SECTION.                                                
044600                                                                          
044700     MOVE SPAR-IDARTNR  TO W-IDARTNR-WDD3                                 
044800     PERFORM IMS-GET-BENA                                                 
044900     IF SEGMENT-FINNS                                                     
045000        MOVE TEXT-BEART TO MOD-BEART-UT                                   
045100     ELSE                                                                 
045200        MOVE 'UNKNOWN'  TO MOD-BEART-UT                                   
045300     END-IF                                                               
045400                                                                          
045500     MOVE SPAR-IDARTNR      TO W-IDARTNR-X                                
045600     MOVE SPAR-DAREGDAT     TO W-DAREGDAT-X                               
045700     MOVE SPAR-TIKLOCK      TO W-TIKLOCK-X                                
045800     MOVE SPAR-IDSEKVNR     TO W-IDSEKVNR-X                               
045900     PERFORM IMS-GET-LOGA                                                 
046000     IF SEGMENT-FINNS                                                     
046100* ---RÄKNAR OM 9KOMPL-DATUM TILL RIKTIGT DATUM                            
046200        COMPUTE W-DAREGDAT  = LOGG-DAREGDAT-9KOMPL - 99999999             
046300        MOVE W-DAREGDAT     TO W-AREA                                     
046400        COMPUTE W-TID       = LOGG-TIKLOCK-9KOMPL - 999999999             
046500        MOVE LOGG-IDDC               TO MOD-IDDC                          
046600        MOVE LOGG-IDHUVTYP           TO MOD-IDHUVTYP                      
046700        MOVE LOGG-IDSUBTYP           TO MOD-IDSUBTYP                      
046800        MOVE LOGG-IDTRANS            TO MOD-IDTRANS-UT                    
046900        MOVE LOGG-IDTRANS            TO MOD-IDTRANS-UT2                   
047000        MOVE LOGG-KVART-SALDO        TO MOD-KVART-SALDO                   
047100        MOVE LOGG-IDTECKEN-KVAKS-PAV TO MOD-IDTECKEN-KVAKS-PAV            
047200        MOVE LOGG-KVAKS-PAV          TO MOD-KVAKS-PAV                     
047300        MOVE LOGG-IDTECKEN-KVAKS     TO MOD-IDTECKEN-KVAKS                
047400        MOVE LOGG-KVAKS              TO MOD-KVAKS                         
047500        MOVE LOGG-IDTECKEN-KVEFRS    TO MOD-IDTECKEN-KVEFRS               
047600        MOVE LOGG-KVEFRS             TO MOD-KVEFRS                        
047700        MOVE LOGG-IDTECKEN-KVLS      TO MOD-IDTECKEN-KVLS                 
047800        MOVE LOGG-KVLS               TO MOD-KVLS                          
047900        MOVE LOGG-IDPGM              TO MOD-IDPGM                         
048000        MOVE LOGG-IDUSER             TO MOD-IDUSER                        
048100        MOVE WS-DAREGDAT-TI          TO MOD-TIREGDAT                      
048200        MOVE W-TID                   TO WS-TID                            
048300        INSPECT WS-TID REPLACING LEADING ZERO BY SPACE                    
048400        MOVE WS-TID                  TO MOD-TIKLOCK                       
048500        MOVE LOGG-DAREGDAT-LADD      TO MOD-LADD-DAT                      
048600        PERFORM S01-VILKEN-BILD                                           
048700        MOVE SPAR-UREF1              TO MOD-REF1                          
048800        MOVE SPAR-UREF2              TO MOD-REF2                          
048900        IF LOGG-IDPGM = 'W4073900'                                        
049000          MOVE '100/22'              TO MOD-REF2(19:6)                    
049100        END-IF                                                            
049200                                                                          
049300        MOVE INF-PRESS-ENTER TO MED-IDMFSINF                              
049400        CALL WMEDKONV USING MED-WMEDAREA                                  
049500        MOVE MED-MFSINF      TO MOD-TEMFSINF                              
049600                                                                          
049700     ELSE                                                                 
049800        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
049900        CALL WMEDKONV USING MED-WMEDAREA                                  
050000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
050100        PERFORM MFS-RENSA-FAELT-UT                                        
050200     END-IF                                                               
050300       MOVE '002'      TO MSGI-KDCALL                                     
050400       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
050500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
050600     .                                                                    
050700     EJECT                                                                
050800                                                                          
050900 S01-VILKEN-BILD SECTION.                                                 
051000     EVALUATE LOGG-IDPGM                                                  
051100     WHEN 'W3018200'                                                      
051200        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
051300        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
051400        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
051500     WHEN 'W3018300'                                                      
051600        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
051700        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
051800        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
051900        MOVE LOGG-IDKOLLI         TO WS-IDKOLLI                           
052000        INSPECT WS-IDKOLLI REPLACING LEADING ZERO BY SPACE                
052100        MOVE WS-IDKOLLI           TO SPAR-IDKOLLI                         
052200     WHEN 'W3018400'                                                      
052300        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
052400        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
052500        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
052600     WHEN 'W4030400'                                                      
052700        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
052800        INSPECT WS-IDDISTR  REPLACING LEADING ZERO BY SPACE               
052900        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
053000        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
053100        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
053200        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
053300        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
053400        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
053500        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
053600        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
053700        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
053800        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
053900     WHEN 'W4031300'                                                      
054000        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
054100        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
054200        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
054300        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
054400        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
054500        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
054600        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
054700        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
054800        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
054900        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
055000        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
055100        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
055200     WHEN 'W4035900'                                                      
055300        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
055400        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
055500        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
055600        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
055700        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
055800        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
055900        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
056000        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
056100        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
056200        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
056300        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
056400        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
056500     WHEN 'W4037100'                                                      
056600        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
056700        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
056800        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
056900        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
057000        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
057100        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
057200        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
057300        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
057400        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
057500     WHEN 'W4037500'                                                      
057600        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
057700        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
057800        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
057900        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
058000        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
058100        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
058200        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
058300        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
058400        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
058500        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
058600        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
058700        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
058800        MOVE LOGG-IDPLKLST        TO WS-IDPLKLST                          
058900        INSPECT WS-IDPLKLST REPLACING LEADING ZERO BY SPACE               
059000        MOVE WS-IDPLKLST          TO SPAR-IDPLKLST                        
059010     WHEN 'WL013400'                                                      
059020        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
059030        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
059040        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
059050        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
059060        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
059070        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
059080        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
059090        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
059091        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
059092        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
059093        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
059094        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
059095        MOVE LOGG-IDPLKLST        TO WS-IDPLKLST                          
059096        INSPECT WS-IDPLKLST REPLACING LEADING ZERO BY SPACE               
059097        MOVE WS-IDPLKLST          TO SPAR-IDPLKLST                        
059100     WHEN 'W411DEAV'                                                      
059200        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
059300        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
059400        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
059500        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
059600        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
059700        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
059800        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
059900        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
060000        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
060100        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
060200        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
060300        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
060400        MOVE LOGG-IDPLKLST        TO WS-IDPLKLST                          
060500        INSPECT WS-IDPLKLST REPLACING LEADING ZERO BY SPACE               
060600        MOVE WS-IDPLKLST          TO SPAR-IDPLKLST                        
060700     WHEN 'W4039700'                                                      
060800        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
060900        INSPECT WS-IDDISTR  REPLACING LEADING ZERO BY SPACE               
061000        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
061100        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
061200        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
061300        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
061400        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
061500        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
061600        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
061700        MOVE LOGG-IDPLKLST        TO WS-IDPLKLST                          
061800        INSPECT WS-IDPLKLST REPLACING LEADING ZERO BY SPACE               
061900        MOVE WS-IDPLKLST          TO SPAR-IDPLKLST                        
062000        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
062100        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
062200        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
062300     WHEN 'W4039800'                                                      
062400        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
062500        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
062600        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
062700        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
062800        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
062900        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
063000        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
063100        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
063200        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
063300        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
063400        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
063500        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
063600     WHEN 'W403AVSP'                                                      
063700        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
063800        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
063900        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
064000        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
064100        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
064200        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
064300        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
064400        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
064500        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
064600        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
064700        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
064800        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
064900     WHEN 'W4766500'                                                      
065000        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
065100        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
065200        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
065300        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
065400        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
065500        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
065600        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
065700        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
065800        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
065900        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
066000        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
066100        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
066200     WHEN 'W4766800'                                                      
066300*  W4766800 SKALL BORT ERSATT AV W4766500*******                          
066400        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
066500        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
066600        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
066700        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
066800        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
066900        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
067000        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
067100        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
067200        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
067300        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
067400        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
067500        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
067600     WHEN 'W4063600'                                                      
067700        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
067800        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
067900        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
068000        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
068100        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
068200        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
068300        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
068400        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
068500        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
068600        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
068700        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
068800        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
068900     WHEN 'W4073900'                                                      
069000        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
069100        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
069200        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
069300        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
069400        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
069500        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
069600        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
069700        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
069800        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
069900     WHEN 'W4079200'                                                      
070000        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
070100        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
070200        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
070300        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
070400        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
070500        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
070600        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
070700        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
070800        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
070900     WHEN 'W4079700'                                                      
071000        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
071100        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
071200        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
071300        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
071400        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
071500        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
071600        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
071700        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
071800        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
071900     WHEN 'W5010600'                                                      
072000     WHEN 'WL017100'                                                      
072100        MOVE LOGG-UREF1           TO SPAR-UREF1                           
072200        MOVE SPACE                TO SPAR-UREF2                           
072300     WHEN 'W5010800'                                                      
072400     WHEN 'WL017200'                                                      
072500        MOVE LOGG-IDUSER-IDPRCREF TO WS-IDUSER-IDPRCREF                   
072600        INSPECT WS-IDUSER-IDPRCREF REPLACING LEADING ZERO BY SPACE        
072700        MOVE WS-IDUSER-IDPRCREF   TO SPAR-IDUSER-IDPRCREF                 
072800        MOVE LOGG-IDPRODNR        TO WS-IDPRODNR                          
072900        INSPECT WS-IDPRODNR REPLACING LEADING ZERO BY SPACE               
073000        MOVE WS-IDPRODNR          TO SPAR-IDPRODNR                        
073100     WHEN 'W5010900'                                                      
073200        MOVE LOGG-IDDC-SEND       TO WS-IDDC-SEND                         
073300        INSPECT WS-IDDC-SEND REPLACING LEADING ZERO BY SPACE              
073400        MOVE WS-IDDC-SEND         TO SPAR-IDDC-SEND                       
073500        MOVE LOGG-IDCLEARREF      TO WS-IDCLEARREF                        
073600        INSPECT WS-IDCLEARREF REPLACING LEADING ZERO BY SPACE             
073700        MOVE WS-IDCLEARREF        TO SPAR-IDCLEARREF                      
073800     WHEN 'W5015100'                                                      
073900        IF LOGG-REF NOT = SPACE                                           
074000          MOVE LOGG-IDLOPNRM      TO WS-IDLOPNRM                          
074100          INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE             
074200          MOVE WS-IDLOPNRM        TO SPAR-IDLOPNRM                        
074300          MOVE LOGG-IDAVINR       TO WS-IDAVINR                           
074400          INSPECT WS-IDAVINR REPLACING LEADING ZERO BY SPACE              
074500          MOVE WS-IDAVINR         TO SPAR-IDAVINR                         
074600        END-IF                                                            
074700     WHEN 'W5030800'                                                      
074800     WHEN 'WL017800'                                                      
074900        MOVE LOGG-UREF1           TO SPAR-UREF1                           
075000        MOVE LOGG-UREF2           TO SPAR-UREF2                           
075100     WHEN 'W5016600'                                                      
075200        MOVE LOGG-UREF1           TO SPAR-UREF1                           
075300        MOVE LOGG-UREF2           TO SPAR-UREF2                           
075400     WHEN 'W6011C00'                                                      
075500        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
075600        MOVE LOGG-IDAVINR         TO WS-IDAVINR                           
075700        INSPECT WS-IDAVINR REPLACING LEADING ZERO BY SPACE                
075800        MOVE WS-IDAVINR           TO SPAR-IDAVINR                         
075900     WHEN 'W6011D00'                                                      
076000        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
076100        MOVE LOGG-IDKR            TO WS-IDKR                              
076200        INSPECT WS-IDKR REPLACING LEADING ZERO BY SPACE                   
076300        MOVE WS-IDKR              TO SPAR-IDKR                            
076400     WHEN 'W6011800'                                                      
076500        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
076600        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
076700        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
076800        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
076900        MOVE SPACE                TO SPAR-IDAVINR-FILLER                  
077000     WHEN 'W6011900'                                                      
077100        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
077200        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
077300        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
077400     WHEN 'W6019200'                                                      
077500        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
077600        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
077700        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
077800        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
077900        MOVE LOGG-IDFS            TO WS-IDFS                              
078000        INSPECT WS-IDFS REPLACING LEADING ZERO BY SPACE                   
078100        MOVE WS-IDFS              TO SPAR-IDFS                            
078200     WHEN 'W6019300'                                                      
078300        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
078400        MOVE LOGG-IDLOPNRM        TO WS-IDLOPNRM                          
078500        INSPECT WS-IDLOPNRM REPLACING LEADING ZERO BY SPACE               
078600        MOVE WS-IDLOPNRM          TO SPAR-IDLOPNRM                        
078700        MOVE LOGG-IDFS            TO WS-IDFS                              
078800        INSPECT WS-IDFS REPLACING LEADING ZERO BY SPACE                   
078900        MOVE WS-IDFS              TO SPAR-IDFS                            
079000     WHEN 'WL010100'                                                      
079100     WHEN 'W6030100'                                                      
079200        MOVE LOGG-TIFAKT          TO WS-TIFAKT                            
079300        INSPECT WS-TIFAKT REPLACING LEADING ZERO BY SPACE                 
079400        MOVE WS-TIFAKT            TO SPAR-TIFAKT                          
079500        MOVE LOGG-IDLBBET         TO WS-IDLBBET                           
079600        INSPECT WS-IDLBBET REPLACING LEADING ZERO BY SPACE                
079700        MOVE WS-IDLBBET           TO SPAR-IDLBBET                         
079800        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
079900        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
080000        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
080100     WHEN 'WL010200'                                                      
080200     WHEN 'W6030200'                                                      
080300        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
080400        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
080500        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
080600     WHEN 'WL010300'                                                      
080700     WHEN 'W6030300'                                                      
080710     WHEN 'W6033000'                                                      
080800        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
080900        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
081000        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
081100        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
081200        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
081300        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
081400        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
081500        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
081600        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
081700     WHEN 'WL010900'                                                      
081800     WHEN 'W6030900'                                                      
081900        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
082000        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
082100        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
082200        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
082300        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
082400        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
082500        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
082600        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
082700        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
082800     WHEN 'W0110200'                                                      
082900        MOVE SPACE                TO SPAR-UREF1                           
083000        MOVE SPACE                TO SPAR-UREF2                           
083100     WHEN 'W2170400'                                                      
083200        MOVE SPACE                TO SPAR-UREF1                           
083300        MOVE SPACE                TO SPAR-UREF2                           
083400     WHEN 'W3712300'                                                      
083500        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
083600        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
083700        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
083800        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
083900        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
084000        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
084100        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
084200        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
084300        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
084400     WHEN 'W3712700'                                                      
084500        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
084600        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
084700        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
084800        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
084900        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
085000        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
085100        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
085200        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
085300        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
085400     WHEN 'W3713300'                                                      
085500        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
085600        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
085700        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
085800        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
085900        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
086000        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
086100        MOVE LOGG-IDORDNR5        TO WS-IDORDNR5                          
086200        INSPECT WS-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
086300        MOVE WS-IDORDNR5          TO SPAR-IDORDNR5                        
086400     WHEN 'W4183500'                                                      
086500        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
086600        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
086700        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
086800        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
086900        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
087000        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
087100        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
087200        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
087300        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
087400     WHEN 'W4183300'                                                      
087500        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
087600        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
087700        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
087800        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
087900        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
088000        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
088100        MOVE LOGG-IDRAPPNR        TO WS-IDRAPPNR                          
088200        INSPECT WS-IDRAPPNR REPLACING LEADING ZERO BY SPACE               
088300        MOVE WS-IDRAPPNR          TO SPAR-IDRAPPNR                        
088400     WHEN 'W4752A00'                                                      
088500     WHEN 'W4769100'                                                      
088600        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
088700        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
088800        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
088900        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
089000        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
089100        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
089200        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
089300        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
089400        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
089500        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
089600        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
089700        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
089800     WHEN 'W4752B00'                                                      
089900        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
090000        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
090100        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
090200        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
090300        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
090400        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
090500        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
090600        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
090700        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
090800     WHEN 'W4752000'                                                      
090900        MOVE SPACE                TO SPAR-UREF1                           
091000        MOVE SPACE                TO SPAR-UREF2                           
091100     WHEN 'W4752099' THRU 'W4752100'                                      
091200        MOVE LOGG-IDDISTR         TO WS-IDDISTR                           
091300        INSPECT WS-IDDISTR REPLACING LEADING ZERO BY SPACE                
091400        MOVE WS-IDDISTR           TO SPAR-IDDISTR                         
091500        MOVE LOGG-IDKUNDNR        TO WS-IDKUNDNR                          
091600        INSPECT WS-IDKUNDNR REPLACING LEADING ZERO BY SPACE               
091700        MOVE WS-IDKUNDNR          TO SPAR-IDKUNDNR                        
091800        MOVE LOGG-IDKUNDRF        TO WS-IDKUNDRF                          
091900        INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE               
092000        MOVE WS-IDKUNDRF          TO SPAR-IDKUNDRF                        
092100        MOVE LOGG-IDFAKT          TO WS-IDFAKT                            
092200        INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                 
092300        MOVE WS-IDFAKT            TO SPAR-IDFAKT                          
092400     WHEN 'W5610700'                                                      
092500        MOVE SPACE                TO SPAR-UREF1                           
092600        MOVE SPACE                TO SPAR-UREF2                           
092700     WHEN 'W6110600'                                                      
092800        MOVE LOGG-IDLEVNR         TO SPAR-IDLEVNR                         
092900        MOVE LOGG-IDFS            TO WS-IDFS                              
093000        INSPECT WS-IDFS REPLACING LEADING ZERO BY SPACE                   
093100        MOVE WS-IDFS              TO SPAR-IDFS                            
093200     END-EVALUATE                                                         
093300     .                                                                    
093400     EJECT                                                                
093500 MFS-RENSA-FAELT-UT SECTION.                                              
093600                                                                          
093700*    --- ALLA UTDATA-FÄLT                                                 
093800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                               
093900                             MOD-IDDC-UT                                  
094000                             MOD-IDHUVTYP-UT                              
094100                             MOD-IDSUBTYP-UT                              
094200                             MOD-TIREGDAT-FOM-UT                          
094300                             MOD-TIREGDAT-TOM-UT                          
094400                             MOD-IDTRANS-UT                               
094500                             MOD-BEART-UT                                 
094600                             MOD-IDDC                                     
094700                             MOD-IDHUVTYP                                 
094800                             MOD-IDSUBTYP                                 
094900                             MOD-IDTRANS-UT2                              
095000                             MOD-IDPGM                                    
095100                             MOD-IDUSER                                   
095200                             MOD-TIREGDAT                                 
095300                             MOD-TIKLOCK                                  
095400                             MOD-REF1                                     
095500                             MOD-REF2                                     
095600     .                                                                    
095700     SKIP3                                                                
095800* --- IMS SEKTIONER ---                                                   
095900     SKIP3                                                                
096000 IMS-GET-MSG SECTION.                                                     
096100                                                                          
096200     MOVE '  QC' TO GODK-STATUSKODER                                      
096300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
096400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096500     PERFORM IMS-STATUSKONTROLL                                           
096600     .                                                                    
096700     SKIP3                                                                
096800 IMS-INSERT-MSG SECTION.                                                  
096900                                                                          
097000     IF ENGLISH-TEXT                                                      
097100       MOVE 'N' TO MFS-KDHUVOMR                                           
097200     END-IF                                                               
097300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
097400     MOVE SPACE TO GODK-STATUSKODER                                       
097500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
097600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097700     PERFORM IMS-STATUSKONTROLL                                           
097800     .                                                                    
097900     EJECT                                                                
098000 IMS-INSERT-ALT-MSG-5162 SECTION.                                         
098100                                                                          
098200     MOVE SPACE TO GODK-STATUSKODER                                       
098300     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW-5162              
098400     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
098500     PERFORM IMS-STATUSKONTROLL                                           
098600     .                                                                    
098700     EJECT                                                                
098800 IMS-INSERT-ALT-MSG-5163 SECTION.                                         
098900                                                                          
099000     MOVE SPACE TO GODK-STATUSKODER                                       
099100     CALL CBLTDLI USING ISRT ALT2-PCB W-PROG-TO-PROG-SW-5163              
099200     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
099300     PERFORM IMS-STATUSKONTROLL                                           
099400     .                                                                    
099500     EJECT                                                                
099600 IMS-GET-LOGA SECTION.                                                    
099700                                                                          
099800     STRING 'WLLOGA01(WDL901KY =' W-WDL901KY-X ')'                        
099900          DELIMITED BY SIZE INTO SSA1                                     
100000     MOVE '  GE' TO GODK-STATUSKODER                                      
100100     CALL CBLTDLI USING GU LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
100200     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     EJECT                                                                
100600 IMS-GET-BENA SECTION.                                                    
100700                                                                          
100800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
100900          DELIMITED BY SIZE INTO SSA1                                     
101000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
101100          DELIMITED BY SIZE INTO SSA2                                     
101200     MOVE '  GE' TO GODK-STATUSKODER                                      
101300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1 SSA2             
101400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     EJECT                                                                
101800 IMS-STATUSKONTROLL SECTION.                                              
101900                                                                          
102000     SET STATUS-IX TO 1                                                   
102100     SEARCH GODK-STATUS                                                   
102200       AT END                                                             
102300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
102400         DELIMITED BY SIZE INTO FELTEXT                                   
102500         CALL FELLOG                                                      
102600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
102700         CONTINUE                                                         
102800     END-SEARCH                                                           
102900     .                                                                    
