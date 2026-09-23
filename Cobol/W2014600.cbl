000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2014600.                                                
000300 AUTHOR.         GÖRAN KJELLSON  GUIDE                                    
000400 DATE-WRITTEN.   FEBRUARI 2004                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LISTNING AV ARTIKLAR SOM SAKNAR FÖRPACKNINGSINSTRUKTION          
000900*                                                                         
001000*        PROGRAMMET LÄSER   WDP7                                          
001100*                           USERDATABAS                                   
001200*        PROGRAMMET LÄSER   WDG2  (WLXXAV HTR 1142 KDSEGKEY = 4)          
001300*                           NYA ARTIKLAR UTAN FP-INSTRUKTION              
001400*        PROGRAMMET LÄSER   WDK6  (WDK601, WDK611)                        
001500*                           ARTIKELINFORMATION                            
001600*        PROGRAMMET LÄSER   WDD2  (WDD201)                                
001700*                           NYA ARTIKLAR FRÅN PV OCH LV                   
001800*        PROGRAMMET LÄSER   WDP3A (WDP3A1)                                
001900*                           PERSONKODER INDEX=ARTIKELINTERVALL            
002000*        PROGRAMMET LÄSER   WDP3  (WDP311)                                
002100*                           PERSONKODSREGISTER                            
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W2T146                                              
002500*        MID:         W2I14601                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W2O14601                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                          PIC X(08) VALUE 'W2014600'.           
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                        PIC X(80) VALUE SPACE.                
004000 77  RKOD-FELLOG                    PIC S9(4) COMP VALUE +33.             
004100                                                                          
004200 77  JA                             PIC X     VALUE 'J'.                  
004300 77  NEJ                            PIC X     VALUE 'N'.                  
004400 77  CURR-SECTION                   PIC X(16) VALUE 'MAIN'.               
004500 77  CURR-IMS-SECTION               PIC X(16) VALUE SPACE.                
004600 77  SW-TRAEFF                      PIC X     VALUE SPACE.                
004700                                                                          
004800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004900 77  INDX                           PIC S9(4) VALUE +0  COMP SYNC.        
005000 77  MAX-INDX                       PIC S9(4) VALUE +11 COMP SYNC.        
005100*77  MAX-INDX                       PIC S9(4) VALUE +2  COMP SYNC.        
005200                                                                          
005300 77  WS-CURR-AAVVD                  PIC 9(5)  VALUE ZERO.                 
005400 77  WS2000-CURR-AAVVD              PIC 9(7)  VALUE ZERO.                 
005500                                                                          
005600 77  WS-TIFINLV-FOM                 PIC 9(5)  VALUE ZERO.                 
005700 77  WS-TIFINLV-TOM                 PIC 9(5)  VALUE ZERO.                 
005800 77  WS2000-TIFINLV-FOM             PIC 9(7)  VALUE ZERO.                 
005900 77  WS2000-TIFINLV-TOM             PIC 9(7)  VALUE ZERO.                 
006000 77  ARTWS-TIFINLV                  PIC 9(5)  VALUE ZERO.                 
006100 77  ART2000-TIFINLV                PIC 9(7)  VALUE ZERO.                 
006200                                                                          
006300 77  WS-KVANTAL-TOT                 PIC 9(3)  VALUE ZERO.                 
006400 77  WS-KVANTAL-TOT-CHECK           PIC 9(3)  VALUE ZERO.                 
006500 77  WS-KVANTAL-INLEV               PIC 9(3)  VALUE ZERO.                 
006600 77  WS-KVANTAL-PISK                PIC 9(3)  VALUE ZERO.                 
006700                                                                          
006800 77  NYCKLAR-SW                     PIC X     VALUE 'J'.                  
006900     88  NYCKLAR-OK                           VALUE 'J'.                  
007000     88  NYCKLAR-FEL                          VALUE 'N'.                  
007100                                                                          
007200 77  W-IDTRANS                      PIC X(4)  VALUE SPACE.                
007300     88  EGEN-MID                             VALUE '2146'.               
007400     88  HELP-MID                             VALUE '0551'.               
007500                                                                          
007600     EJECT                                                                
007700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007800 01  GENERELLA-SUBPROGRAM.                                                
007900     03  WMEDKONV                   PIC X(8) VALUE 'WMEDKONV'.            
008000     03  WDATKONV                   PIC X(8) VALUE 'WDATKONV'.            
008100     03  W005INIT                   PIC X(8) VALUE 'W005INIT'.            
008200     03  CBLTDLI                    PIC X(8) VALUE 'CBLTDLI '.            
008300     03  FELLOG                     PIC X(8) VALUE 'FELLOG  '.            
008400     03  ABEND                      PIC X(8) VALUE 'ABEND   '.            
008500     EJECT                                                                
008600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008700*01 -COPY WMEDAREA                                                        
008800                                                                          
008900*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
009000*01 -COPY WDATAREA                                                        
009100                                                                          
009200 01  MESSAGE-CODES.                                                       
009300     03  ERR-CORR-HILITE-FLDS       PIC X(3) VALUE '001'.                 
009400     03  ERR-KEYS-ARE-MISSING       PIC X(3) VALUE '005'.                 
009500     03  INF-FIRST-PAGE             PIC X(3) VALUE '006'.                 
009600     03  INF-MORE-INFO-EXISTS       PIC X(3) VALUE '105'.                 
009700     03  INF-LAST-PAGE              PIC X(3) VALUE '106'.                 
009800     03  ERR-LAST-PAGE-SHOWN        PIC X(3) VALUE '115'.                 
009900     03  ERR-WRONG-KEY              PIC X(3) VALUE '401'.                 
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010200*                                                                         
010300 01  FILLER                         PIC X(16) VALUE 'WMSGINIT'.           
010400     SKIP3                                                                
010500*01 -COPY WMSGINIT                                                        
010600     EJECT                                                                
010700                                                                          
010800 01  WS-DAT                         PIC 9(6).                             
010900 01  FILLER REDEFINES WS-DAT.                                             
011000     03 WS-YEAR                     PIC 9(2).                             
011100     03 WS-MONTH                    PIC 9(2).                             
011200     03 WS-DAYS                     PIC 9(2).                             
011300 01  WS-TID                         PIC 9(6).                             
011400 01  FILLER REDEFINES WS-TID.                                             
011500     03 WS-HOURS                    PIC 9(2).                             
011600     03 WS-MINUTES                  PIC 9(2).                             
011700     03 WS-SECONDS                  PIC 9(2).                             
011800                                                                          
011900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
012000 01  SPAR-AREA.                                                           
012100     03  SPAR-IDTRANS                      PIC X(4)  VALUE '2146'.        
012200     03  SPAR-IDARTNR-ENTER                PIC S9(9) COMP-3.              
012300     03  SPAR-IDARTNR-NEXT                 PIC S9(9) COMP-3.              
012400     03  SPAR-KVANTAL-TOT                  PIC 9(3)  VALUE ZERO.          
012500     03  SPAR-KVANTAL-INLEV                PIC 9(3)  VALUE ZERO.          
012600     03  SPAR-KVANTAL-PISK                 PIC 9(3)  VALUE ZERO.          
012700                                                                          
012800                                                                          
012900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013100     SKIP3                                                                
013200*01  MID -COPY W2I14601                                                   
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013500     SKIP3                                                                
013600*01  -COPY WMSGAREA                                                       
013700     EJECT                                                                
013800     03  MOD REDEFINES MSG-AREA.                                          
013900*      05  -COPY W2O14601                                                 
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014200     SKIP3                                                                
014300*01  -COPY WMFSAREA                                                       
014400     EJECT                                                                
014500                                                                          
014600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014800     SKIP3                                                                
014900 01  NYCKLAR-TILL-DLI.                                                    
015000                                                                          
015100     03  W-1141KEY-X.                                                     
015200         05  FILLER               PIC X(04)  VALUE '1141'.                
015300         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
015400                                                                          
015500     03  W-1142KEY-X.                                                     
015600         05  FILLER               PIC X(01)  VALUE '4'.                   
015700                                                                          
015800     03  W-IDARTNR-FOM-X.                                                 
015900         05  W-IDARTNR-FOM           PIC S9(9) VALUE ZERO COMP-3.         
016000                                                                          
016100     03  W-IDARTNR-X.                                                     
016200         05  W-IDARTNR               PIC S9(9) VALUE ZERO COMP-3.         
016300                                                                          
016400     03  W-KDSEGKEY-X.                                                    
016500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
016600                                                                          
016700     03  W-WDP3A1-MIN.                                                    
016800         05  W-IDLAND-MIN          PIC X(2)    VALUE SPACE.               
016900         05  W-IDARTNRF-MIN        PIC S9(9)   VALUE ZERO COMP-3.         
017000         05  W-IDARTNRT-MIN        PIC S9(9)   VALUE ZERO COMP-3.         
017100         05  W-KDARBTYP-A-MIN      PIC X(8)    VALUE SPACE.               
017200     03  W-WDP3A1-MAX.                                                    
017210         05  W-IDLAND-MAX          PIC X(2)    VALUE SPACE.               
017300         05  W-IDARTNRF-MAX        PIC S9(9)   VALUE ZERO COMP-3.         
017400         05  W-IDARTNRT-MAX        PIC S9(9)   VALUE ZERO COMP-3.         
017500         05  W-KDARBTYP-A-MAX      PIC X(8)    VALUE SPACE.               
017600                                                                          
017700     03  W-KDARBTYP-X.                                                    
017800         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
017900                                                                          
018000     03  W-IDPERSON-X.                                                    
018100         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
018200     SKIP2                                                                
018300*    --- STATUS-KOD FRÅN IMS                                              
018400 01  STATUS-WS                   PIC XX.                                  
018500     88  SEGMENT-FINNS                       VALUE '  '.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     88  BASEN-SLUT                          VALUE 'GB'.                  
018800     SKIP2                                                                
018900 01  GODK-STATUSKODER.                                                    
019000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019100     SKIP3                                                                
019200 01  SSA1                        PIC X(96).                               
019300 01  SSA2                        PIC X(64).                               
019400     EJECT                                                                
019500*    --- IMS FUNKTIONSKODER                                               
019600*01  -COPY W0003                                                          
019700     EJECT                                                                
019800*    ---  DLI INPUT-OUTPUT AREA                                           
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG2  '.                      
020000 01  DLI-IO-WDG2.                                                         
020100*    03  WDG2 -COPY WDGX1142                                              
020200                                                                          
020300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
020400 01  DLI-IO-WDD201.                                                       
020500*    03  -COPY WDD201  -PRE D2-                                           
020600                                                                          
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
020800 01  DLI-IO-WDK601.                                                       
020900*    03  -COPY WDK601                                                     
021000                                                                          
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
021200 01  DLI-IO-WDK611.                                                       
021300*    03  -COPY WDK611                                                     
021400                                                                          
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP3A '.                      
021600 01  DLI-IO-AREA-WDP3A.                                                   
021700*    03  -COPY WDP3A1                                                     
021800                                                                          
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
022000 01  DLI-IO-AREA-WDP311.                                                  
022100*    03  -COPY WDP311                                                     
022200                                                                          
022300                                                                          
022400 LINKAGE SECTION.                                                         
022500*01  -COPY W0009  -PRE MSG-                                               
022600*01  -COPY W0008  -PRE WDP7-                                              
022700     05  FILLER                  PIC X.                                   
022800*01  -COPY W0008  -PRE WDG2-                                              
022900     05  FILLER                  PIC X.                                   
023000*01  -COPY W0008  -PRE WDK6-                                              
023100     05  FILLER                  PIC X.                                   
023200*01  -COPY W0008  -PRE WDD2-                                              
023300     05  FILLER                  PIC X.                                   
023400*01  -COPY W0008  -PRE WDP3A-                                             
023500     05  FILLER                  PIC X.                                   
023600*01  -COPY W0008  -PRE WDP3-                                              
023700     05  FILLER                  PIC X.                                   
023800                                                                          
023900 PROCEDURE DIVISION  USING MSG-PCB                                        
024000                           WDP7-PCB WDG2-PCB  WDK6-PCB                    
024100                           WDD2-PCB WDP3A-PCB WDP3-PCB.                   
024200                                                                          
024300 MAIN SECTION.                                                            
024400     ENTRY 'DLITCBL' USING MSG-PCB                                        
024500                           WDP7-PCB WDG2-PCB  WDK6-PCB                    
024600                           WDD2-PCB WDP3A-PCB WDP3-PCB.                   
024700                                                                          
024800     PERFORM IMS-GET-MSG                                                  
024900     IF SEGMENT-FINNS                                                     
025000        PERFORM A-INIT                                                    
025100        PERFORM B-KONTROLLERA-NYCKLAR                                     
025200        IF NYCKLAR-OK                                                     
025300           IF MFS-FIRST                                                   
025400              PERFORM C-FIRST-PAGE                                        
025500           ELSE                                                           
025600              IF MFS-NEXT                                                 
025700                 PERFORM D-NEXT-PAGE                                      
025800              ELSE                                                        
025900                 PERFORM E-SAME-PAGE                                      
026000              END-IF                                                      
026100           END-IF                                                         
026200                                                                          
026300           PERFORM F-READ-SHOW-INFO                                       
026400        END-IF                                                            
026500        PERFORM IMS-INSERT-MSG                                            
026600     END-IF                                                               
026700                                                                          
026800     MOVE ZERO TO RETURN-CODE                                             
026900     GOBACK                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 A-INIT SECTION.                                                          
027300     MOVE 'A-INIT          ' TO CURR-SECTION.                             
027400     IF MSG-DUBBLA-TRANSKODER                                             
027500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I14601                 
027600       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
027700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027800     ELSE                                                                 
027900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I14601                  
028000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
028100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028200     END-IF                                                               
028300                                                                          
028400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
028600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028700                                                                          
028800     MOVE LOW-VALUE TO MSG-AREA                                           
028900     MOVE 'W2O146N1' TO MFS-IDMOD                                         
029000     MOVE '2146' TO MOD-IDTRANS                                           
029100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029200                                                                          
029300     IF EGEN-MID OR HELP-MID                                              
029400       CONTINUE                                                           
029500     ELSE                                                                 
029600       MOVE SPACE TO MFS-KDTRTYP                                          
029700       MOVE '7' TO MFS-IDPFK                                              
029800     END-IF                                                               
029900                                                                          
030000     COMPUTE MSG-KVLL = LENGTH OF MOD-W2O14601 + 4                        
030100                                                                          
030200     ACCEPT WS-DAT     FROM DATE                                          
030300     ACCEPT WS-TID     FROM TIME                                          
030400                                                                          
030500     MOVE "AAMMDD"       TO DAT-KDDATFORM                                 
030600     MOVE WS-DAT         TO DAT-I-TIDATUM                                 
030700     CALL WDATKONV USING    DAT-KDDATFORM,                                
030800                            DAT-I-TIDATUM,                                
030900                            DAT-O-TIDATUM,                                
031000                            DAT-KDSVAR                                    
031100                                                                          
031200     IF DAT-KDSVAR-OK                                                     
031300        MOVE DAT-TIAAVVD TO WS-CURR-AAVVD                                 
031400     END-IF                                                               
031500                                                                          
031600     MOVE LOW-VALUE  TO W-WDP3A1-MIN                                      
031700     MOVE HIGH-VALUE TO W-WDP3A1-MAX                                      
031800     .                                                                    
031900     EJECT                                                                
032000 B-KONTROLLERA-NYCKLAR     SECTION.                                       
032100     MOVE 'B-KONTROLLERA-NY' TO CURR-SECTION.                             
032200                                                                          
032300     MOVE ALL '+' TO MSGI-WMSGINIT                                        
032400     MOVE '001' TO MSGI-KDCALL                                            
032500     MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                             
032600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032700     MOVE '2146' TO MSGI-IDTRANS                                          
032800     IF EGEN-MID                                                          
032900       MOVE MID-TIFINLV-FOM-IN TO MSGI-TIFINLV-FOM                        
033000       MOVE MID-TIFINLV-TOM-IN TO MSGI-TIFINLV-TOM                        
033100       MOVE MID-IDPROJ-IN      TO MSGI-IDPROJ                             
033200     END-IF                                                               
033300                                                                          
033400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
033500     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
033600                                                                          
033700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
033800*  EFTER FÖRLÄNGNING AV SPAR-IDARTNR BEHÖVS LITE FIX FÖR ATT              
033900*  INTE FÅ 0C7                                                            
034000     IF SPAR-IDARTNR-ENTER NOT NUMERIC                                    
034100        MOVE ZERO TO SPAR-IDARTNR-ENTER                                   
034200     END-IF                                                               
034300     IF SPAR-IDARTNR-NEXT NOT NUMERIC                                     
034400        MOVE ZERO TO SPAR-IDARTNR-NEXT                                    
034500     END-IF                                                               
034600*  ÄVEN NYA ANTALSFÄLT BEHÖVER FIXAS....                                  
034700     IF  SPAR-KVANTAL-TOT NOT NUMERIC                                     
034800         MOVE ZERO TO SPAR-KVANTAL-TOT                                    
034900     END-IF                                                               
035000     IF  SPAR-KVANTAL-INLEV NOT NUMERIC                                   
035100         MOVE ZERO TO SPAR-KVANTAL-INLEV                                  
035200     END-IF                                                               
035300     IF  SPAR-KVANTAL-PISK NOT NUMERIC                                    
035400         MOVE ZERO TO SPAR-KVANTAL-PISK                                   
035500     END-IF                                                               
035600*  SÅ, NU BÖR DET INTE ABENDA                                             
035700                                                                          
035800     IF SPAR-IDTRANS NOT = '2146'                                         
035900        MOVE '2146'    TO SPAR-IDTRANS                                    
036000        MOVE ZERO      TO SPAR-IDARTNR-ENTER                              
036100        MOVE ZERO      TO SPAR-IDARTNR-NEXT                               
036200     END-IF                                                               
036300     MOVE JA TO NYCKLAR-SW                                                
036400                                                                          
036500     MOVE MFS-RENSA-FAELT TO MOD-TIFINLV-FOM-IN                           
036600     IF MID-TIFINLV-FOM-IN NOT = ALL '+'                                  
036700       MOVE '7' TO MFS-IDPFK                                              
036800       MOVE SPACE TO MFS-KDTRTYP                                          
036900     END-IF                                                               
037000                                                                          
037100     MOVE MFS-RENSA-FAELT TO MOD-TIFINLV-TOM-IN                           
037200     IF MID-TIFINLV-TOM-IN NOT = ALL '+'                                  
037300       MOVE '7' TO MFS-IDPFK                                              
037400       MOVE SPACE TO MFS-KDTRTYP                                          
037500     END-IF                                                               
037600                                                                          
037700     MOVE MFS-RENSA-FAELT TO MOD-IDPROJ-IN                                
037800     IF MID-IDPROJ-IN NOT = ALL '+'                                       
037900       MOVE '7' TO MFS-IDPFK                                              
038000       MOVE SPACE TO MFS-KDTRTYP                                          
038100     END-IF                                                               
038200                                                                          
038300     IF MSGI-TIFINLV-FOM = SPACE   OR                                     
038400        MSGI-TIFINLV-FOM = ALL '+' OR                                     
038500       (MSGI-TIFINLV-FOM NUMERIC AND MSGI-TIFINLV-FOM > ZERO)             
038600        IF MSGI-TIFINLV-FOM NUMERIC AND MSGI-TIFINLV-FOM > ZERO           
038700           MOVE MSGI-TIFINLV-FOM TO WS-TIFINLV-FOM                        
038800        ELSE                                                              
038900           MOVE ZERO             TO WS-TIFINLV-FOM                        
039000        END-IF                                                            
039100     ELSE                                                                 
039200       MOVE NEJ TO NYCKLAR-SW                                             
039300     END-IF                                                               
039400                                                                          
039500     IF MSGI-TIFINLV-TOM = SPACE   OR                                     
039600        MSGI-TIFINLV-TOM = ALL '+' OR                                     
039700       (MSGI-TIFINLV-TOM NUMERIC AND MSGI-TIFINLV-TOM > ZERO)             
039800        IF MSGI-TIFINLV-TOM NUMERIC AND MSGI-TIFINLV-TOM > ZERO           
039900           MOVE MSGI-TIFINLV-TOM TO WS-TIFINLV-TOM                        
040000        ELSE                                                              
040100           MOVE 99999            TO WS-TIFINLV-TOM                        
040200        END-IF                                                            
040300     ELSE                                                                 
040400       MOVE NEJ TO NYCKLAR-SW                                             
040500     END-IF                                                               
040600                                                                          
040700     IF NYCKLAR-FEL                                                       
040800        MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                               
040900        CALL WMEDKONV USING MED-WMEDAREA                                  
041000        MOVE MED-MFSFEL     TO MOD-TEMFSFEL                               
041100        PERFORM MFS-RENSA-FAELT-UT                                        
041200     ELSE                                                                 
041300        IF NYCKLAR-OK                                                     
041400           MOVE MSGI-TIFINLV-FOM TO MOD-TIFINLV-FOM-UT                    
041500           MOVE MSGI-TIFINLV-TOM TO MOD-TIFINLV-TOM-UT                    
041600           MOVE MSGI-IDPROJ       TO MOD-IDPROJ-UT                        
041700        ELSE                                                              
041800           MOVE MFS-RENSA-FAELT   TO MOD-TIFINLV-FOM-UT                   
041900                                     MOD-TIFINLV-TOM-UT                   
042000                                     MOD-IDPROJ-UT                        
042100        END-IF                                                            
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 C-FIRST-PAGE SECTION.                                                    
042600     MOVE 'C-FIRST-PAGE    ' TO CURR-SECTION.                             
042700                                                                          
042800     MOVE ZERO           TO W-IDARTNR-FOM                                 
042900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
043000     CALL WMEDKONV USING MED-WMEDAREA                                     
043100     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
043200     .                                                                    
043300     EJECT                                                                
043400 D-NEXT-PAGE SECTION.                                                     
043500     MOVE 'D-NEXT-PAGE     ' TO CURR-SECTION.                             
043600                                                                          
043700     IF SPAR-IDTRANS = '2146'                                             
043800        MOVE SPAR-IDARTNR-NEXT     TO W-IDARTNR-FOM                       
043900     ELSE                                                                 
044000        MOVE ERR-LAST-PAGE-SHOWN   TO MED-IDMFSFEL                        
044100        CALL WMEDKONV USING MED-WMEDAREA                                  
044200        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 E-SAME-PAGE SECTION.                                                     
044700     MOVE 'E-SAME-PAGE     ' TO CURR-SECTION.                             
044800                                                                          
044900     MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR-FOM                             
045000     .                                                                    
045100     EJECT                                                                
045200 F-READ-SHOW-INFO SECTION.                                                
045300     MOVE 'F-READ-SHOW-INFO' TO CURR-SECTION.                             
045400                                                                          
045500     PERFORM IMS-01-GU-WDG201                                             
045600                                                                          
045700     IF SEGMENT-FINNS                                                     
045800        PERFORM IMS-02-GNP-WDG202                                         
045900        IF NOT MFS-FIRST                                                  
046000           PERFORM UNTIL SEGMENT-SAKNAS                                   
046100                      OR BASEN-SLUT                                       
046200                      OR 1142-IDARTNR = W-IDARTNR-FOM                     
046300*    HÄR GÖRS ANNROP PÅ FB- FÖR ATT FÅ TIKIGA TOTALER                     
046400              MOVE ZERO TO INDX                                           
046500              PERFORM FB-FIXA-TOTALER                                     
046600              PERFORM IMS-02-GNP-WDG202                                   
046700           END-PERFORM                                                    
046800        END-IF                                                            
046900     END-IF                                                               
047000     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
047100        MOVE ERR-KEYS-ARE-MISSING TO MED-MFSFEL                           
047200        CALL WMEDKONV USING MED-WMEDAREA                                  
047300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
047400     ELSE                                                                 
047500        MOVE 1142-IDARTNR  TO SPAR-IDARTNR-ENTER                          
047600        MOVE +1 TO INDX                                                   
047700        PERFORM UNTIL INDX > MAX-INDX                                     
047800           IF SEGMENT-FINNS                                               
047900              PERFORM FA-REDIGERA-ARTIKELRAD                              
048000              PERFORM IMS-02-GNP-WDG202                                   
048100           ELSE                                                           
048200              MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                  
048300                                      MOD-IDPROJ  (INDX)                  
048400                                      MOD-KDEMBKOD(INDX)                  
048500                                      MOD-TIFINLV (INDX)                  
048600                                      MOD-FLPISK  (INDX)                  
048700                                      MOD-TIREGDAT(INDX)                  
048800                                      MOD-IDNAMN  (INDX)                  
048900              ADD 1 TO INDX                                               
049000           END-IF                                                         
049100        END-PERFORM                                                       
049200                                                                          
049300        IF SEGMENT-FINNS                                                  
049400           MOVE 1142-IDARTNR         TO SPAR-IDARTNR-NEXT                 
049500           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
049600           CALL WMEDKONV USING MED-WMEDAREA                               
049700           MOVE MED-MFSINF           TO MOD-TEMFSINF                      
049800                                                                          
049900           IF MFS-FIRST                                                   
050000*             SNURRA IGENOM FÖR ATT RÄKNA TOTALER                         
050100              MOVE WS-KVANTAL-TOT TO WS-KVANTAL-TOT-CHECK                 
050200              PERFORM UNTIL SEGMENT-SAKNAS                                
050300                 PERFORM FB-FIXA-TOTALER                                  
050400                 PERFORM IMS-02-GNP-WDG202                                
050500              END-PERFORM                                                 
050600              IF WS-KVANTAL-TOT = WS-KVANTAL-TOT-CHECK                    
050700                 MOVE SPAR-IDARTNR-ENTER   TO SPAR-IDARTNR-NEXT           
050800                 MOVE INF-LAST-PAGE        TO MED-IDMFSINF                
050900                 CALL WMEDKONV USING MED-WMEDAREA                         
051000                 MOVE MED-MFSINF           TO MOD-TEMFSINF                
051100              END-IF                                                      
051200              MOVE WS-KVANTAL-TOT     TO SPAR-KVANTAL-TOT                 
051300              MOVE WS-KVANTAL-INLEV   TO SPAR-KVANTAL-INLEV               
051400              MOVE WS-KVANTAL-PISK    TO SPAR-KVANTAL-PISK                
051500           END-IF                                                         
051600        ELSE                                                              
051700           MOVE WS-KVANTAL-TOT     TO SPAR-KVANTAL-TOT                    
051800           MOVE WS-KVANTAL-INLEV   TO SPAR-KVANTAL-INLEV                  
051900           MOVE WS-KVANTAL-PISK    TO SPAR-KVANTAL-PISK                   
052000           MOVE SPAR-IDARTNR-ENTER   TO SPAR-IDARTNR-NEXT                 
052100           MOVE INF-LAST-PAGE        TO MED-IDMFSINF                      
052200           CALL WMEDKONV USING MED-WMEDAREA                               
052300           MOVE MED-MFSINF           TO MOD-TEMFSINF                      
052400        END-IF                                                            
052500                                                                          
052600        MOVE SPAR-KVANTAL-TOT   TO MOD-KVANTAL-TOT                        
052700        MOVE SPAR-KVANTAL-INLEV TO MOD-KVANTAL-INLEV                      
052800        MOVE SPAR-KVANTAL-PISK  TO MOD-KVANTAL-PISK                       
052900*GK*    MOVE WS-KVANTAL-TOT   TO MOD-KVANTAL-TOT                          
053000*GK*    MOVE WS-KVANTAL-INLEV TO MOD-KVANTAL-INLEV                        
053100*GK*    MOVE WS-KVANTAL-PISK  TO MOD-KVANTAL-PISK                         
053200                                                                          
053300       MOVE '002'     TO MSGI-KDCALL                                      
053400       MOVE '2146'    TO SPAR-IDTRANS                                     
053500       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
053600       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
053700     END-IF                                                               
053800     .                                                                    
053900     EJECT                                                                
054000 FA-REDIGERA-ARTIKELRAD SECTION.                                          
054100     MOVE 'FA-REDIGERA-ARTI' TO CURR-SECTION.                             
054200                                                                          
054300     MOVE 1142-IDARTNR TO W-IDARTNR                                       
054400     PERFORM IMS-03-GU-WDD201                                             
054401                                                                          
054410     IF SEGMENT-FINNS                                                     
054500                                                                          
054600       IF (MSGI-IDPROJ = SPACE OR                                         
054700           MSGI-IDPROJ = D2-ART-IDPROJ)                                   
054800                                                                          
054900          PERFORM IMS-04-GU-WDK601                                        
054910          IF SEGMENT-FINNS                                                
055110                                                                          
055200            MOVE ART-TIFINLV TO ARTWS-TIFINLV                             
055300            MOVE ART-TIFINLV TO ART2000-TIFINLV                           
055400            MOVE WS-TIFINLV-FOM TO WS2000-TIFINLV-FOM                     
055500            MOVE WS-TIFINLV-TOM TO WS2000-TIFINLV-TOM                     
055600            IF ARTWS-TIFINLV(1:2) > 50                                    
055700              COMPUTE ART2000-TIFINLV = ART2000-TIFINLV + 1900000         
055800            ELSE                                                          
055900              COMPUTE ART2000-TIFINLV = ART2000-TIFINLV + 2000000         
056000            END-IF                                                        
056100            IF WS-TIFINLV-FOM(1:2) > 50                                   
056200             COMPUTE WS2000-TIFINLV-FOM =                                 
056210                     WS2000-TIFINLV-FOM + 1900000                         
056300            ELSE                                                          
056400             COMPUTE WS2000-TIFINLV-FOM =                                 
056410                     WS2000-TIFINLV-FOM + 2000000                         
056500            END-IF                                                        
056600            IF WS-TIFINLV-TOM(1:2) > 50                                   
056700             COMPUTE WS2000-TIFINLV-TOM =                                 
056710                     WS2000-TIFINLV-TOM + 1900000                         
056800            ELSE                                                          
056900             COMPUTE WS2000-TIFINLV-TOM =                                 
056910                     WS2000-TIFINLV-TOM + 2000000                         
057000            END-IF                                                        
057100*           IF ART-TIFINLV NOT < WS-TIFINLV-FOM AND                       
057200*              ART-TIFINLV NOT > WS-TIFINLV-TOM                           
057300            IF (ART2000-TIFINLV NOT < WS2000-TIFINLV-FOM AND              
057400                ART2000-TIFINLV NOT > WS2000-TIFINLV-TOM)                 
057500            OR (WS-TIFINLV-FOM = ZERO AND                                 
057600                WS-TIFINLV-TOM = 99999)                                   
057700               PERFORM FAA-FP-TEKNIKER                                    
057800                                                                          
057900               IF INDX > ZERO AND                                         
058000                  INDX NOT > MAX-INDX                                     
058100                  MOVE D2-ART-IDARTNR TO MOD-IDARTNR (INDX)               
058200                  MOVE D2-ART-IDPROJ TO MOD-IDPROJ (INDX)                 
058201                                                                          
058210                  PERFORM IMS-05-GU-WDK611                                
058220                  IF SEGMENT-FINNS                                        
058300                    MOVE CLAG-KDEMBKOD-2 TO MOD-KDEMBKOD(INDX)            
058310                  ELSE                                                    
058320                    MOVE MFS-RENSA-FAELT TO MOD-KDEMBKOD(INDX)            
058330                  END-IF                                                  
058340                                                                          
058400                  MOVE ART-TIFINLV TO MOD-TIFINLV (INDX)                  
058500                  MOVE D2-ART-FLPISK TO MOD-FLPISK (INDX)                 
058600                  MOVE ART-TIREGDAT TO MOD-TIREGDAT(INDX)                 
058700                  MOVE PERS-IDNAMN TO MOD-IDNAMN  (INDX)                  
058800               END-IF                                                     
058900               ADD +1           TO WS-KVANTAL-TOT                         
059000                                                                          
059100               MOVE ART-TIFINLV TO ARTWS-TIFINLV                          
059200               MOVE ART-TIFINLV TO ART2000-TIFINLV                        
059300               MOVE WS-CURR-AAVVD TO WS2000-CURR-AAVVD                    
059400*              IF ART-TIFINLV < WS-CURR-AAVVD                             
059500               IF ARTWS-TIFINLV(1:2) > 50                                 
059600                  COMPUTE ART2000-TIFINLV =                               
059610                          ART2000-TIFINLV + 1900000                       
059700               ELSE                                                       
059800                  COMPUTE ART2000-TIFINLV =                               
059810                          ART2000-TIFINLV + 2000000                       
059900               END-IF                                                     
060000               IF WS-CURR-AAVVD(1:2) > 50                                 
060100                  COMPUTE WS2000-CURR-AAVVD                               
060200                                     = WS2000-CURR-AAVVD + 1900000        
060300               ELSE                                                       
060400                  COMPUTE WS2000-CURR-AAVVD                               
060500                                     = WS2000-CURR-AAVVD + 2000000        
060600               END-IF                                                     
060700               IF ART2000-TIFINLV < WS2000-CURR-AAVVD                     
060800                  ADD +1        TO WS-KVANTAL-INLEV                       
060900               END-IF                                                     
061000               IF D2-ART-FLPISK = JA                                      
061100                  ADD +1        TO WS-KVANTAL-PISK                        
061200               END-IF                                                     
061300               ADD +1 TO INDX                                             
061400            END-IF                                                        
061410          END-IF                                                          
061500       END-IF                                                             
061510     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 FAA-FP-TEKNIKER    SECTION.                                              
061900     MOVE 'FAA-FP-TEKNIKER ' TO CURR-SECTION.                             
062000                                                                          
062100     MOVE NEJ                TO SW-TRAEFF                                 
062200     MOVE SPACE              TO PERS-IDNAMN                               
062300     MOVE 'CDC     '         TO W-KDARBTYP                                
062400                                                                          
062500     PERFORM IMS-06-GU-WDP3A                                              
062600     PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = 'J'                      
062700        IF SEQA-IDARTNR-TOM < W-IDARTNR                                   
062800           PERFORM IMS-07-GN-WDP3A                                        
062900        ELSE                                                              
063000           IF SEQA-IDARTNR-FOM <= W-IDARTNR AND                           
063100              SEQA-IDARTNR-TOM >= W-IDARTNR                               
063200              MOVE 'J' TO SW-TRAEFF                                       
063300           ELSE                                                           
063400              MOVE 'GE' TO STATUS-WS                                      
063500           END-IF                                                         
063600        END-IF                                                            
063700     END-PERFORM                                                          
063800     IF SW-TRAEFF = 'J'                                                   
063900        IF SEQA-IDPERSON > +0                                             
064000           MOVE 'CDC     '                 TO W-KDARBTYP                  
064100           MOVE SEQA-IDPERSON              TO W-IDPERSON                  
064200           PERFORM IMS-08-GU-WDP311                                       
064300           IF SEGMENT-SAKNAS                                              
064400              MOVE SPACE        TO PERS-IDNAMN                            
064500           END-IF                                                         
064600        END-IF                                                            
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 FB-FIXA-TOTALER   SECTION.                                               
065100     MOVE 'FB-FIXA-TOTALER ' TO CURR-SECTION.                             
065200                                                                          
065300     MOVE 1142-IDARTNR TO W-IDARTNR                                       
065400     PERFORM IMS-03-GU-WDD201                                             
065500                                                                          
065600     IF (MSGI-IDPROJ = SPACE OR                                           
065700         MSGI-IDPROJ = D2-ART-IDPROJ)                                     
065800                                                                          
065900        PERFORM IMS-04-GU-WDK601                                          
066000        IF SEGMENT-FINNS                                                  
066100          MOVE ART-TIFINLV  TO ARTWS-TIFINLV                              
066200          MOVE ART-TIFINLV  TO ART2000-TIFINLV                            
066300          MOVE WS-TIFINLV-FOM TO WS2000-TIFINLV-FOM                       
066400          MOVE WS-TIFINLV-TOM TO WS2000-TIFINLV-TOM                       
066500          IF ARTWS-TIFINLV(1:2) > 50                                      
066600            COMPUTE ART2000-TIFINLV = ART2000-TIFINLV + 1900000           
066700          ELSE                                                            
066800            COMPUTE ART2000-TIFINLV = ART2000-TIFINLV + 2000000           
066900          END-IF                                                          
067000          IF WS-TIFINLV-FOM(1:2) > 50                                     
067100           COMPUTE WS2000-TIFINLV-FOM =                                   
067110                   WS2000-TIFINLV-FOM + 1900000                           
067200          ELSE                                                            
067300           COMPUTE WS2000-TIFINLV-FOM =                                   
067310                   WS2000-TIFINLV-FOM + 2000000                           
067400          END-IF                                                          
067500          IF WS-TIFINLV-TOM(1:2) > 50                                     
067600           COMPUTE WS2000-TIFINLV-TOM =                                   
067610                   WS2000-TIFINLV-TOM + 1900000                           
067700          ELSE                                                            
067800           COMPUTE WS2000-TIFINLV-TOM =                                   
067810                   WS2000-TIFINLV-TOM + 2000000                           
067900          END-IF                                                          
068000          IF (ART2000-TIFINLV NOT < WS2000-TIFINLV-FOM AND                
068100              ART2000-TIFINLV NOT > WS2000-TIFINLV-TOM)                   
068200          OR (WS-TIFINLV-FOM = ZERO AND                                   
068300              WS-TIFINLV-TOM = 99999)                                     
068400                                                                          
068500             ADD +1             TO WS-KVANTAL-TOT                         
068600                                                                          
068700             MOVE ART-TIFINLV  TO ARTWS-TIFINLV                           
068800             MOVE ART-TIFINLV  TO ART2000-TIFINLV                         
068900             MOVE WS-CURR-AAVVD TO WS2000-CURR-AAVVD                      
069000                                                                          
069100             IF ARTWS-TIFINLV(1:2) > 50                                   
069200                COMPUTE ART2000-TIFINLV =                                 
069210                        ART2000-TIFINLV + 1900000                         
069300             ELSE                                                         
069400                COMPUTE ART2000-TIFINLV =                                 
069410                        ART2000-TIFINLV + 2000000                         
069500             END-IF                                                       
069600             IF WS-CURR-AAVVD(1:2) > 50                                   
069700                COMPUTE WS2000-CURR-AAVVD                                 
069800                                  = WS2000-CURR-AAVVD + 1900000           
069900             ELSE                                                         
070000                COMPUTE WS2000-CURR-AAVVD                                 
070100                                  = WS2000-CURR-AAVVD + 2000000           
070200             END-IF                                                       
070300             IF ART2000-TIFINLV < WS2000-CURR-AAVVD                       
070400                ADD +1          TO WS-KVANTAL-INLEV                       
070500             END-IF                                                       
070600             IF D2-ART-FLPISK = JA                                        
070700                ADD +1          TO WS-KVANTAL-PISK                        
070800             END-IF                                                       
070900             ADD +1 TO INDX                                               
071000          END-IF                                                          
071010        END-IF                                                            
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400 MFS-RENSA-FAELT-UT SECTION.                                              
071500                                                                          
071600     MOVE +1 TO INDX                                                      
071700     PERFORM UNTIL INDX > MAX-INDX                                        
071800       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                         
071900                               MOD-IDPROJ  (INDX)                         
072000                               MOD-KDEMBKOD(INDX)                         
072100                               MOD-TIFINLV (INDX)                         
072200                               MOD-FLPISK  (INDX)                         
072300                               MOD-TIREGDAT(INDX)                         
072400                               MOD-IDNAMN  (INDX)                         
072500       ADD 1 TO INDX                                                      
072600     END-PERFORM                                                          
072700     .                                                                    
072800     SKIP3                                                                
072900* --- IMS SEKTIONER                                                       
073000 IMS-GET-MSG SECTION.                                                     
073100                                                                          
073200     MOVE '  QC' TO GODK-STATUSKODER                                      
073300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
073400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
073500     PERFORM IMS-STATUSKONTROLL                                           
073600     .                                                                    
073700     EJECT                                                                
073800 IMS-INSERT-MSG SECTION.                                                  
073900                                                                          
074000     MOVE SPACE TO GODK-STATUSKODER                                       
074100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
074200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074300     PERFORM IMS-STATUSKONTROLL                                           
074400     .                                                                    
074500     EJECT                                                                
074600 IMS-01-GU-WDG201      SECTION.                                           
074700     MOVE 'IMS-01'  TO CURR-IMS-SECTION                                   
074800                                                                          
074900     STRING 'WDG201  (WDGXKEY  =' W-1141KEY-X ')'                         
075000             DELIMITED BY SIZE INTO SSA1                                  
075100                                                                          
075200     MOVE '  GE'           TO GODK-STATUSKODER                            
075300     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDG2 SSA1                      
075400     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
075500     PERFORM IMS-STATUSKONTROLL                                           
075600     .                                                                    
075700 IMS-02-GNP-WDG202     SECTION.                                           
075800     MOVE 'IMS-02'  TO CURR-IMS-SECTION                                   
075900                                                                          
076000     STRING 'WDG202  (KDSEGKEY =' W-1142KEY-X ')'                         
076100             DELIMITED BY SIZE INTO SSA1                                  
076200                                                                          
076300     MOVE '  GEGB'         TO GODK-STATUSKODER                            
076400     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDG2 SSA1                     
076500     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800 IMS-03-GU-WDD201      SECTION.                                           
076900     MOVE 'IMS-03'  TO CURR-IMS-SECTION                                   
077000                                                                          
077100     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
077200          DELIMITED BY SIZE INTO SSA1                                     
077300     MOVE '  GE'           TO GODK-STATUSKODER                            
077400     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
077500     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
077600     PERFORM IMS-STATUSKONTROLL                                           
077700     .                                                                    
077800 IMS-04-GU-WDK601      SECTION.                                           
077900     MOVE 'IMS-04'  TO CURR-IMS-SECTION                                   
078000                                                                          
078100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
078200          DELIMITED BY SIZE INTO SSA1                                     
078300     MOVE '  GE'           TO GODK-STATUSKODER                            
078400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
078500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
078600     PERFORM IMS-STATUSKONTROLL                                           
078700     .                                                                    
078800 IMS-05-GU-WDK611      SECTION.                                           
078900     MOVE 'IMS-05'  TO CURR-IMS-SECTION                                   
079000                                                                          
079100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
079200          DELIMITED BY SIZE INTO SSA1                                     
079300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
079400          DELIMITED BY SIZE INTO SSA2                                     
079500     MOVE '  GE'           TO GODK-STATUSKODER                            
079600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
079700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
079800     PERFORM IMS-STATUSKONTROLL                                           
079900     .                                                                    
080000 IMS-06-GU-WDP3A SECTION.                                                 
080100     MOVE 'IMS-06'  TO CURR-IMS-SECTION                                   
080200                                                                          
080300     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
080400                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
080500                    '&KDARBTYP =' W-KDARBTYP ')'                          
080600           DELIMITED BY SIZE INTO SSA1                                    
080700     MOVE '  GE'            TO GODK-STATUSKODER                           
080800     CALL CBLTDLI USING GU WDP3A-PCB DLI-IO-AREA-WDP3A SSA1               
080900     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
081000     PERFORM IMS-STATUSKONTROLL                                           
081100     .                                                                    
081200                                                                          
081300 IMS-07-GN-WDP3A SECTION.                                                 
081400     MOVE 'IMS-07'  TO CURR-IMS-SECTION                                   
081500                                                                          
081600     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
081700                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
081800                    '&KDARBTYP =' W-KDARBTYP ')'                          
081900            DELIMITED BY SIZE INTO SSA1                                   
082000     MOVE '  GEGB'          TO GODK-STATUSKODER                           
082100     CALL CBLTDLI USING GN WDP3A-PCB DLI-IO-AREA-WDP3A SSA1               
082200     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
082300     PERFORM IMS-STATUSKONTROLL                                           
082400     .                                                                    
082500 IMS-08-GU-WDP311      SECTION.                                           
082600     MOVE 'IMS-08'  TO CURR-IMS-SECTION                                   
082700                                                                          
082800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
082900          DELIMITED BY SIZE INTO SSA1                                     
083000     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
083100          DELIMITED BY SIZE INTO SSA2                                     
083200     MOVE '  GE'           TO GODK-STATUSKODER                            
083300     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-AREA-WDP311 SSA1 SSA2          
083400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
083500     PERFORM IMS-STATUSKONTROLL                                           
083600     .                                                                    
083700                                                                          
083800                                                                          
083900 IMS-STATUSKONTROLL SECTION.                                              
084000                                                                          
084100     SET STATUS-IX TO 1                                                   
084200     SEARCH GODK-STATUS                                                   
084300       AT END                                                             
084400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
084500         DELIMITED BY SIZE INTO FELTEXT                                   
084600         CALL FELLOG                                                      
084700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
084800         CONTINUE                                                         
084900     END-SEARCH                                                           
085000     .                                                                    
085100     EJECT                                                                
