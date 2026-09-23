000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4066810.                                                
000400 AUTHOR.         ARCHANA BHAT.                                            
000500 DATE-WRITTEN.   14/04/22.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER HÄNDELSEBAS OCH KONTROLLERAR INMATNING. OM INDATA          
001000*        OK SÅ STARTAS ETT BAKGRUNDS-MPP SOM SKRIVER 'BILL OF             
001100*        LADING' FÖR NDC:ERNA.                                            
001200*                                                                         
001300*        PROGRAMMET LÄSER      WL4463 (WDR4)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T668                                              
001700*        MID:         W4I66801                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O66801                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W4066800'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FILLER                      PIC X(08) VALUE 'ERRORTEX'.              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
003800 77  WS-UPPD                     PIC 9(3)    VALUE ZERO.                  
003900 77  IX                          PIC S9(4)   VALUE +0 COMP SYNC.          
004000 77  INDX                        PIC S9(4)   VALUE +0 COMP SYNC.          
004100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004200 77  FILLER                      PIC X(08)   VALUE 'date::::'.            
004300 77  WS-DATUM                    PIC X(6)    VALUE ZERO.                  
004400 77  WS-IDTRPTNR                 PIC Z(2)9.                               
004500 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
004600 77  WS-IDTIDZON                 PIC 9(2)    VALUE ZERO.                  
004700                                                                          
004800     EJECT                                                                
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  INPUT-SW                    PIC X       VALUE 'J'.                   
005600     88  INMATNING-OK                        VALUE 'J'.                   
005700     88  INMATNING-EJ-OK                     VALUE 'N'.                   
005800                                                                          
005900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006000     88  INDATA-OK                           VALUE 'J'.                   
006100     88  INDATA-EJ-OK                        VALUE 'N'.                   
006200                                                                          
006300 77  SHOW-SW                     PIC X       VALUE 'N'.                   
006400     88  SHOWED                              VALUE 'J'.                   
006500                                                                          
006600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  EGEN-MID                            VALUE '4668'.                
006800     88  GODK-MID                            VALUE '4661' '4662'          
006900                                                   '4663' '4664'          
007000                                                   '4665' '4666'          
007100                                                   '4667' '4668'          
007200                                                   '4669'.                
007300     88  HELP-MID                            VALUE '0551'.                
007400 01  ALL-SPACE.                                                           
007500     03 FILLER                   PIC X(80)   VALUE SPACE.                 
007600 01  ALL-PLUS.                                                            
007700     03 FILLER                   PIC X(80)   VALUE ALL '+'.               
007800*      --- VALID IDDC CODES                                               
007900*                                                                         
008000*01    -COPY WWDC99                                                       
008100       EJECT                                                              
008200     EJECT                                                                
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
009000     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009300*01 -COPY WMEDAREA                                                        
009400     SKIP3                                                                
009500 01  MESSAGE-CODES.                                                       
009600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
009700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
009800     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '025'.                 
009900                                                                          
010000     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
010100     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '376'.                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'WORKAREA'.            
010400*01  -COPY WORKAREA                                                       
010500     EJECT                                                                
010600*                                                                         
010700 01  FILLER                      PIC X(08)   VALUE 'TIDZAREA'.            
010800*01  -COPY WL01TIDZ  -PRE TIDZ-                                           
010900     EJECT                                                                
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011500     SKIP3                                                                
011600*01 -COPY WMSGINIT                                                        
011700     SKIP3                                                                
011800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
011900*01 -COPY WDATAREA                                                        
012000     EJECT                                                                
012100 01  W-PROG-TO-PROG-SW-USA.                                               
012200     03  M-SW-LL-1               PIC S9(4)   VALUE +100 COMP SYNC.        
012300     03  M-SW-Z1-Z2-1            PIC X(2)    VALUE LOW-VALUE.             
012400     03  M-SW-KDTRANS-1          PIC X(8)    VALUE 'W4T688  '.            
012500     03  M-SW-IDTRANS-1          PIC X(4)    VALUE '4668'.                
012600     03  M-SW-KDMFSTYP-1         PIC X(1)    VALUE '1'.                   
012700                                                                          
012800     03 MID -COPY W4I68801 -PRE 4688-                                     
012900     EJECT                                                                
013000 01  W-PROG-TO-PROG-SW-JAP.                                               
013100     03  M-SW-LL-1               PIC S9(4)   VALUE +100 COMP SYNC.        
013200     03  M-SW-Z1-Z2-1            PIC X(2)    VALUE LOW-VALUE.             
013300     03  M-SW-KDTRANS-1          PIC X(8)    VALUE 'W4T681  '.            
013400     03  M-SW-IDTRANS-1          PIC X(4)    VALUE '4668'.                
013500     03  M-SW-KDMFSTYP-1         PIC X(1)    VALUE '1'.                   
013600                                                                          
013700     03 MID -COPY W4I68801 -PRE 4681-                                     
013800     EJECT                                                                
013900 01  W-PROG-TO-PROG-SW-AUS.                                               
014000     03  M-SW-LL-1               PIC S9(4)   VALUE +100 COMP SYNC.        
014100     03  M-SW-Z1-Z2-1            PIC X(2)    VALUE LOW-VALUE.             
014200     03  M-SW-KDTRANS-1          PIC X(8)    VALUE 'W4T682  '.            
014300     03  M-SW-IDTRANS-1          PIC X(4)    VALUE '4668'.                
014400     03  M-SW-KDMFSTYP-1         PIC X(1)    VALUE '1'.                   
014500                                                                          
014600     03 MID -COPY W4I68801 -PRE 4682-                                     
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014900     SKIP3                                                                
015000*01  -COPY WMFSAREA                                                       
015100     EJECT                                                                
015200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015300*                                                                         
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015600     SKIP3                                                                
015700 01  NYCKLAR-TILL-DLI.                                                    
015800     03  W-WDGXKEY-X.                                                     
015900         05  W-IDHTYP            PIC X(4)    VALUE '4463'.                
016000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016200     SKIP2                                                                
016300     03  W-DASKEPPN-X.                                                    
016400         05  W-DASKEPPN          PIC  9(8)   VALUE ZERO.                  
016500                                                                          
016600     03  W-DASKEPPN-MIN-X.                                                
016700         05  W-DASKEPPN-MIN      PIC  9(8)   VALUE ZERO.                  
016800                                                                          
016900     03  W-IDDC-B6-X.                                                     
017000         05 W-IDDC-B6            PIC X(2).                                
017100                                                                          
017200     EJECT                                                                
017300*    --- STATUS-KOD FRÅN IMS                                              
017400 01  STATUS-WS                   PIC XX.                                  
017500     88  SEGMENT-FINNS                       VALUE '  '.                  
017600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017800     SKIP2                                                                
017900 01  GODK-STATUSKODER.                                                    
018000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100     SKIP3                                                                
018200 01  SSA1                        PIC X(64).                               
018300 01  SSA2                        PIC X(64).                               
018400     EJECT                                                                
018500*    --- IMS FUNKTIONSKODER                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800*    ---  DLI INPUT-OUTPUT AREA                                           
018900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4463'.         
019000     SKIP3                                                                
019100 01  DLI-IO-AREA-4463.                                                    
019200     03  WL446301.                                                        
019300*        05  -COPY WDGX4463                                               
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4464'.         
019600     SKIP3                                                                
019700 01  DLI-IO-AREA-4464.                                                    
019800     03  WL446311.                                                        
019900*        05  -COPY WDGX4464                                               
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4466'.         
020200     SKIP3                                                                
020300 01  DLI-IO-AREA-4466.                                                    
020400     03  WL446321.                                                        
020500*        05  -COPY WDGX4466                                               
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
020800 01   DLI-IO-AREA-B601.                                                   
020900*     03  -COPY WDB601                                                    
021000     EJECT                                                                
021100                                                                          
021200 LINKAGE SECTION.                                                         
021300                                                                          
021400 01  REQU-AREA.                                                           
021500*    03 -COPY WZ01REQU                                                    
021600*    03 -COPY W40668I1                                                    
021700     EJECT                                                                
021800 01  RESP-AREA.                                                           
021900*    03 -COPY WZ01RESP                                                    
022000*    03 -COPY W40668O1                                                    
022100     EJECT                                                                
022200 01  MAX-KVRADER                 PIC S9(4) COMP.                          
022300*01  -COPY W0009   -PRE ALT-USA-                                          
022400     EJECT                                                                
022500*01  -COPY W0009   -PRE ALT-JAP-                                          
022600     EJECT                                                                
022700*01  -COPY W0009   -PRE ALT-AUS-                                          
022800     EJECT                                                                
022900*01  -COPY W0008   -PRE USEA-                                             
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE 4463-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE WDB6-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
023900                           ALT-USA-PCB ALT-JAP-PCB                        
024000                           ALT-AUS-PCB 4463-PCB WDB6-PCB.                 
024100                                                                          
024200     PERFORM A-INIT                                                       
024300     PERFORM B-KOLLA-NYCKLAR                                              
024400     IF NYCKLAR-OK                                                        
024500       PERFORM C-RENSA-GAMLA-4463                                         
024600                                                                          
024700       IF REQU-UPDATE                                                     
024800         PERFORM G-KOLLA-INPUT                                            
024900         IF INDATA-OK                                                     
025000           PERFORM H-STARTA-PRINTPROGRAM                                  
025100         END-IF                                                           
025200                                                                          
025300       ELSE                                                               
025400         IF REQU-FIRST                                                    
025500           PERFORM D-FOERSTA-SIDAN                                        
025600         ELSE                                                             
025700           PERFORM E-SAMMA-SIDA                                           
025800         END-IF                                                           
025900       END-IF                                                             
026000                                                                          
026100       PERFORM F-LAES-VISA-INFO                                           
026200     END-IF                                                               
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 A-INIT SECTION.                                                          
026700                                                                          
026800     IF REQU-KVRADER NOT NUMERIC                                          
026900        MOVE ZERO TO REQU-KVRADER                                         
027000     END-IF                                                               
027100                                                                          
027200     MOVE ALL '+'                TO RESP-W40668O1                         
027300     MOVE REQU-KVRADER           TO RESP-KVRADER                          
027400                                                                          
027500     PERFORM MFS-FORM-ATTR                                                
027600                                                                          
027700     MOVE 001                    TO RESP-IDMSGVER                         
027800     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
027900                                    RESP-IDMSG-INFO                       
028000                                    RESP-IDELMT-ERROR                     
028100     ACCEPT DAGENS-DATUM FROM DATE                                        
028200     ACCEPT DAGENS-TID   FROM TIME                                        
028300     PERFORM MFS-RENSA-FAELT-UT                                           
028400     .                                                                    
028500     EJECT                                                                
028600 B-KOLLA-NYCKLAR SECTION.                                                 
028700                                                                          
028800     MOVE JA TO NYCKLAR-SW                                                
028900     MOVE  REQU-IDDC-KEY    TO WS-IDDC                                    
029000                               W-IDDC                                     
029100                                                                          
029200     IF NDC-US OR NDC-CA OR NDC-JP OR NDC-AU                              
029300       PERFORM BA-VALIDATE-IDDC                                           
029400     ELSE                                                                 
029500       MOVE NEJ             TO NYCKLAR-SW                                 
029600       MOVE 'IDDC'          TO RESP-IDELMT-ERROR                          
029700     END-IF                                                               
029800                                                                          
029900*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
030000*WL01TIDZ. DETTA SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC               
030100*                                                                         
030200     PERFORM BB-CALL-WL01TIDZ                                             
030300                                                                          
030400     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
030500     IF REQU-TIDATUM-KEY   = ALL '+'                                      
030600       MOVE 'IDAG'             TO DAT-KDDATFORM                           
030700       MOVE TIDZ-MSGI-TILOKDAT TO WS-DATUM                                
030800     ELSE                                                                 
030900                                                                          
031000       IF REQU-TIDATUM-KEY NOT NUMERIC                                    
031100         INSPECT REQU-TIDATUM-KEY                                         
031200                 REPLACING LEADING SPACE BY ZERO                          
031300         IF REQU-TIDATUM-KEY  NOT NUMERIC                                 
031400           MOVE NEJ TO NYCKLAR-SW                                         
031500           MOVE 'DASKEPPN'         TO RESP-IDELMT-ERROR                   
031600           MOVE 'IDAG'             TO DAT-KDDATFORM                       
031700           MOVE TIDZ-MSGI-TILOKDAT TO WS-DATUM                            
031800         ELSE                                                             
031900           MOVE REQU-TIDATUM-KEY TO WS-DATUM                              
032000         END-IF                                                           
032100       ELSE                                                               
032200         MOVE REQU-TIDATUM-KEY TO WS-DATUM                                
032300       END-IF                                                             
032400     END-IF                                                               
032500                                                                          
032600     MOVE WS-DATUM          TO DAT-I-TIDATUM                              
032700     CALL WDATKONV       USING DAT-KDDATFORM                              
032800                               DAT-I-TIDATUM                              
032900                               DAT-O-TIDATUM                              
033000                               DAT-KDSVAR                                 
033100     IF DAT-KDSVAR-OK                                                     
033200       MOVE DAT-TIAAMMDD    TO W-DASKEPPN                                 
033300       MOVE DAT-TISEKEL     TO W-DASKEPPN (1:2)                           
033400     ELSE                                                                 
033500       MOVE NEJ             TO NYCKLAR-SW                                 
033600       MOVE 'DASKEPPN'      TO RESP-IDELMT-ERROR                          
033700     END-IF                                                               
033800                                                                          
033900                                                                          
034000     IF NYCKLAR-FEL                                                       
034100       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
034200       MOVE ZERO          TO RESP-KVRADER                                 
034300       PERFORM MFS-RENSA-FAELT-IN                                         
034400       PERFORM MFS-RENSA-FAELT-UT                                         
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 BA-VALIDATE-IDDC SECTION.                                                
034900     MOVE SPACE           TO WS-IDDC                                      
035000     MOVE REQU-IDDC-KEY   TO WS-IDDC                                      
035100                             W-IDDC-B6                                    
035200*                                                                         
035300     PERFORM IMS-GU-WDB601                                                
035400     IF SEGMENT-SAKNAS                                                    
035500       MOVE SPACE TO DCS-KDDC                                             
035600       MOVE 'IDDC'          TO RESP-IDELMT-ERROR                          
035700       MOVE ZERO            TO RESP-KVRADER                               
035800       MOVE NEJ             TO NYCKLAR-SW                                 
035900     ELSE                                                                 
036000       MOVE DCS-IDTIDZON    TO WS-IDTIDZON                                
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400                                                                          
036500 BB-CALL-WL01TIDZ      SECTION.                                           
036600                                                                          
036700*CALL FOR LOCAL TIME                                                      
036800                                                                          
036900     MOVE '011'                    TO TIDZ-MSGI-KDCALL                    
037000     MOVE WS-IDTIDZON              TO TIDZ-MSGI-IDTIDZON                  
037000     MOVE DCS-IDDC                 TO TIDZ-MSGI-IDDC                      
037100     MOVE DAGENS-DATUM             TO TIDZ-MSGI-TILOKDAT                  
037200     MOVE DAGENS-TID               TO TIDZ-MSGI-TILOKTID                  
037300     CALL WL01TIDZ USING              TIDZ-MSGI-WL01TIDZ                  
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 C-RENSA-GAMLA-4463 SECTION.                                              
037800                                                                          
037900** ALLA SEGM. SOM ÄR ÄLDRE ÄN 5 ARB.DAGAR RENSAS.                         
038000     MOVE REQU-IDDC-KEY   TO WORK-IDDC                                    
038100     MOVE 5               TO WORK-KVWORKD                                 
038200     MOVE TIDZ-MSGI-TILOKDAT TO WORK-TIAAMMDD-TOM                         
038300     MOVE 003             TO WORK-KDCALL                                  
038400     CALL WORKDAY      USING WORK-KDCALL                                  
038500                             WORK-DATE-AREA                               
038600                             WORK-KDSVAR                                  
038700     IF WORK-KDSVAR-FEL                                                   
038800        MOVE 'FEL FRÅN WORKDAY I C-SECTION'                               
038900                          TO FELTEXT                                      
039000        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
039100     END-IF                                                               
039200                                                                          
039300     PERFORM IMS-GU-WL446301-DC                                           
039400                                                                          
039500     MOVE WORK-TIAAMMDD-FOM TO W-DASKEPPN-MIN                             
039600     IF WORK-TIAAMMDD-FOM NOT = ZERO                                      
039700       IF WORK-TIAAMMDD-FOM < 500000                                      
039800         MOVE 20           TO W-DASKEPPN-MIN (1:2)                        
039900       ELSE                                                               
040000         IF WORK-TIAAMMDD-FOM < 999999                                    
040100           MOVE 19         TO W-DASKEPPN-MIN (1:2)                        
040200         ELSE                                                             
040300           MOVE 99999999   TO W-DASKEPPN-MIN                              
040400         END-IF                                                           
040500       END-IF                                                             
040600     END-IF                                                               
040700                                                                          
040800     PERFORM IMS-GHNP-WL446311                                            
040900     MOVE +1               TO WS-UPPD                                     
041000                                                                          
041100     PERFORM UNTIL SEGMENT-SAKNAS OR WS-UPPD > 113                        
041200                                                                          
041300       PERFORM IMS-DLET-WL446311                                          
041400       ADD +1              TO WS-UPPD                                     
041500       PERFORM IMS-GHNP-WL446311                                          
041600     END-PERFORM                                                          
041700     .                                                                    
041800 D-FOERSTA-SIDAN SECTION.                                                 
041900                                                                          
042000     PERFORM MFS-RENSA-FAELT-IN                                           
042100     .                                                                    
042200     EJECT                                                                
042300 E-SAMMA-SIDA SECTION.                                                    
042400                                                                          
042500     MOVE JA TO INPUT-SW                                                  
042600     MOVE +1 TO INDX                                                      
042700     PERFORM UNTIL INDX > REQU-KVRADER                                    
042800       IF REQU-KDSVAR-LINE(INDX) = ALL '+' OR                             
042900          REQU-KDSVAR-LINE(INDX) = SPACE                                  
043000                                                                          
043100         MOVE JA TO INPUT-SW                                              
043200         ADD +1 TO INDX                                                   
043300       ELSE                                                               
043400         MOVE NEJ TO INPUT-SW                                             
043500         MOVE +9999 TO INDX                                               
043600       END-IF                                                             
043700     END-PERFORM                                                          
043800                                                                          
043900     IF INMATNING-OK                                                      
044000       PERFORM MFS-RENSA-FAELT-IN                                         
044100     ELSE                                                                 
044200       MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                             
044300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
044400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
044500       PERFORM MFS-LAES-IN-IGEN                                           
044600       PERFORM EA-MID-INDATA-TILL-MOD                                     
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 EA-MID-INDATA-TILL-MOD SECTION.                                          
045100                                                                          
045200     MOVE +1 TO INDX                                                      
045300     PERFORM UNTIL INDX > REQU-KVRADER                                    
045400       IF REQU-KDSVAR-LINE (INDX) NOT  = ALL '+'                          
045500         MOVE REQU-KDSVAR-LINE(INDX)    TO                                
045600                                   RESP-KDSVAR-LINE(INDX)                 
045700         MOVE MFS-ADD-LAES-IN-FAELT     TO                                
045800                                   RESP-KDSVAR-LINE-ATTR(INDX)            
045900         MOVE REQU-IDTRPTNR-LINE(INDX)  TO                                
046000                                   RESP-IDTRPTNR-LINE(INDX)               
046100         MOVE REQU-IDLBBET-LINE(INDX)   TO                                
046200                                   RESP-IDLBBET-LINE(INDX)                
046300       ELSE                                                               
046400         MOVE ALL-SPACE             TO RESP-KDSVAR-LINE(INDX)             
046500                                       RESP-IDTRPTNR-LINE(INDX)           
046600                                       RESP-IDLBBET-LINE(INDX)            
046700       END-IF                                                             
046800       ADD +1 TO INDX                                                     
046900     END-PERFORM                                                          
047000     .                                                                    
047100     EJECT                                                                
047200 F-LAES-VISA-INFO SECTION.                                                
047300                                                                          
047400     PERFORM IMS-GU-WL446301-DC                                           
047500     IF SEGMENT-SAKNAS                                                    
047600       MOVE ERR-KEYS-MISSING TO RESP-IDMSG-ERROR                          
047700       MOVE 'IDDC'           TO RESP-IDELMT-ERROR                         
047800       MOVE 0                TO RESP-KVRADER                              
047900       PERFORM MFS-RENSA-FAELT-UT                                         
048000     ELSE                                                                 
048100       PERFORM IMS-GU-WL446311-TID                                        
048200       IF SEGMENT-SAKNAS                                                  
048300         MOVE ERR-KEYS-MISSING TO RESP-IDMSG-ERROR                        
048400         MOVE 'DASKEPPN'       TO RESP-IDELMT-ERROR                       
048500         MOVE 0                TO RESP-KVRADER                            
048600         PERFORM MFS-RENSA-FAELT-UT                                       
048700       ELSE                                                               
048800         MOVE +1 TO INDX                                                  
048900         MOVE 0  TO RESP-KVRADER                                          
049000         PERFORM IMS-GNP-WL446321-TRANSP                                  
049100         PERFORM UNTIL INDX > MAX-KVRADER                                 
049200           IF SEGMENT-FINNS                                               
049300             MOVE NEJ TO SHOW-SW                                          
049400             MOVE +1  TO IX                                               
049500                                                                          
049600             PERFORM UNTIL IX = INDX OR (SHOWED)                          
049700               MOVE 4466-IDTRPTNR    TO WS-IDTRPTNR                       
049800               IF WS-IDTRPTNR  = RESP-IDTRPTNR-LINE(IX) AND               
049900                  4466-IDLBBET = RESP-IDLBBET-LINE(IX)                    
050000                 MOVE JA             TO SHOW-SW                           
050100               END-IF                                                     
050200               ADD +1 TO IX                                               
050300             END-PERFORM                                                  
050400                                                                          
050500             IF NOT SHOWED                                                
050600               MOVE 4466-IDTRPTNR TO RESP-IDTRPTNR-LINE(INDX)             
050700               MOVE 4466-IDLBBET  TO RESP-IDLBBET-LINE(INDX)              
050800               ADD +1             TO INDX                                 
050900                                     RESP-KVRADER                         
051000             END-IF                                                       
051100             PERFORM IMS-GNP-WL446321-TRANSP                              
051200                                                                          
051300           ELSE                                                           
051400             MOVE ALL-SPACE          TO RESP-IDTRPTNR-LINE(INDX)          
051500                                        RESP-IDLBBET-LINE(INDX)           
051600             MOVE MFS-STAENG-FAELT   TO                                   
051700                                     RESP-KDSVAR-LINE-ATTR(INDX)          
051800             ADD +1 TO INDX                                               
051900           END-IF                                                         
052000         END-PERFORM                                                      
052100       END-IF                                                             
052200     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500 G-KOLLA-INPUT SECTION.                                                   
052600                                                                          
052700     MOVE +1 TO INDX                                                      
052800     PERFORM UNTIL INDX > REQU-KVRADER                                    
052900       IF REQU-KDSVAR-LINE(INDX) = '+' OR SPACE                           
053000          MOVE ALL-SPACE       TO RESP-KDSVAR-LINE (INDX)                 
053100          MOVE NEJ             TO INDATA-SW                               
053200       ELSE                                                               
053300         IF REQU-KDSVAR-LINE(INDX) = 'P' OR 'p' OR 'X' OR 'x' OR          
053400                                'F' OR 'f'                                
053500           MOVE MFS-ALFA-FAELT-RAETT TO                                   
053600                                   RESP-KDSVAR-LINE-ATTR(INDX)            
053700           PERFORM H-STARTA-PRINTPROGRAM                                  
053800                                                                          
053900         ELSE                                                             
054000           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDSVAR-LINE-ATTR(INDX)         
054100           MOVE ALL-PLUS           TO RESP-KDSVAR-LINE(INDX)              
054200           MOVE NEJ                TO INDATA-SW                           
054300           MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                    
054400           MOVE 'KDSVAR'           TO RESP-IDELMT-ERROR                   
054500         END-IF                                                           
054600       END-IF                                                             
054700       ADD +1 TO INDX                                                     
054800     END-PERFORM                                                          
054900     .                                                                    
055000     EJECT                                                                
055100 H-STARTA-PRINTPROGRAM SECTION.                                           
055200                                                                          
055300     IF NDC-US OR NDC-CA                                                  
055400      MOVE LOW-VALUE                 TO 4688-MID-W4I68801                 
055500      MOVE REQU-IDDC-KEY             TO 4688-MID-IDDC                     
055600      MOVE WS-DATUM                  TO 4688-MID-TIDATUM                  
055700      MOVE REQU-KDSVAR-LINE (INDX)   TO 4688-MID-KDSVAR                   
055800      INSPECT REQU-IDTRPTNR-LINE (INDX) REPLACING LEADING                 
055900                                   SPACE BY ZERO                          
056000      MOVE REQU-IDTRPTNR-LINE (INDX) TO 4688-MID-IDTRPTNR                 
056100      MOVE REQU-IDLBBET-LINE (INDX)  TO 4688-MID-IDLBBET                  
056200      MOVE REQU-IDUSER               TO 4688-MID-IDUSER                   
056300                                                                          
056400      PERFORM IMS-PURG-ALT-MSG-USA                                        
056500                                                                          
056600      MOVE INF-PRINT-REQUESTED       TO RESP-IDMSG-INFO                   
056700     END-IF                                                               
056800                                                                          
056900     IF NDC-JP                                                            
057000      MOVE LOW-VALUE                 TO 4681-MID-W4I68801                 
057100      MOVE REQU-IDDC-KEY             TO 4681-MID-IDDC                     
057200      MOVE WS-DATUM                  TO 4681-MID-TIDATUM                  
057300      MOVE REQU-KDSVAR-LINE (INDX)   TO 4681-MID-KDSVAR                   
057400      INSPECT REQU-IDTRPTNR-LINE (INDX) REPLACING LEADING                 
057500                                   SPACE BY ZERO                          
057600      MOVE REQU-IDTRPTNR-LINE (INDX) TO 4681-MID-IDTRPTNR                 
057700      MOVE REQU-IDLBBET-LINE (INDX)  TO 4681-MID-IDLBBET                  
057800                                                                          
057900      PERFORM IMS-INSERT-ALT-MSG-JAP                                      
058000                                                                          
058100      MOVE INF-PRINT-REQUESTED       TO RESP-IDMSG-INFO                   
058200     END-IF                                                               
058300                                                                          
058400     IF NDC-AU                                                            
058500      MOVE LOW-VALUE                 TO 4682-MID-W4I68801                 
058600      MOVE REQU-IDDC-KEY             TO 4682-MID-IDDC                     
058700      MOVE WS-DATUM                  TO 4682-MID-TIDATUM                  
058800      MOVE REQU-KDSVAR-LINE (INDX)   TO 4682-MID-KDSVAR                   
058900      INSPECT REQU-IDTRPTNR-LINE (INDX) REPLACING LEADING                 
059000                                  SPACE BY ZERO                           
059100      MOVE REQU-IDTRPTNR-LINE (INDX) TO 4682-MID-IDTRPTNR                 
059200      MOVE REQU-IDLBBET-LINE (INDX)  TO 4682-MID-IDLBBET                  
059300                                                                          
059400      PERFORM IMS-INSERT-ALT-MSG-AUS                                      
059500                                                                          
059600      MOVE INF-PRINT-REQUESTED       TO RESP-IDMSG-INFO                   
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100 MFS-RENSA-FAELT-UT SECTION.                                              
060200                                                                          
060300*    --- ALLA UTDATA-FÄLT                                                 
060400     MOVE +1 TO INDX                                                      
060500     PERFORM UNTIL INDX > MAX-KVRADER                                     
060600       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
060700       ADD +1 TO INDX                                                     
060800     END-PERFORM                                                          
060900     .                                                                    
061000     SKIP3                                                                
061100 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
061200                                                                          
061300*    --- ALLA UTDATA-FÄLT                                                 
061400     MOVE ALL-SPACE       TO RESP-KDSVAR-LINE(INDX)                       
061500                             RESP-IDTRPTNR-LINE(INDX)                     
061600                             RESP-IDLBBET-LINE(INDX)                      
061700     .                                                                    
061800     EJECT                                                                
061900 MFS-RENSA-FAELT-IN SECTION.                                              
062000                                                                          
062100*    --- ALLA UTDATA-FÄLT                                                 
062200     MOVE +1 TO INDX                                                      
062300     PERFORM UNTIL INDX > MAX-KVRADER                                     
062400       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
062500       ADD +1 TO INDX                                                     
062600     END-PERFORM                                                          
062700     .                                                                    
062800     SKIP3                                                                
062900 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
063000                                                                          
063100*    --- ALLA UTDATA-FÄLT                                                 
063200     MOVE ALL-SPACE       TO RESP-KDSVAR-LINE(INDX)                       
063300     .                                                                    
063400     EJECT                                                                
063500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
063600                                                                          
063700*    --- ALLA UTDATA-FÄLT                                                 
063800     MOVE +1 TO INDX                                                      
063900     PERFORM UNTIL INDX > MAX-KVRADER                                     
064000       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
064100       ADD +1 TO INDX                                                     
064200     END-PERFORM                                                          
064300     .                                                                    
064400     EJECT                                                                
064500 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
064600                                                                          
064700*    --- ALLA UTDATA-FÄLT                                                 
064800     MOVE ALL-PLUS          TO RESP-KDSVAR-LINE(INDX)                     
064900                               RESP-IDTRPTNR-LINE(INDX)                   
065000                               RESP-IDLBBET-LINE(INDX)                    
065100     .                                                                    
065200     EJECT                                                                
065300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
065400                                                                          
065500*    --- ALLA UTDATA-FÄLT                                                 
065600     MOVE +1 TO INDX                                                      
065700     PERFORM UNTIL INDX > MAX-KVRADER                                     
065800       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
065900       ADD +1 TO INDX                                                     
066000     END-PERFORM                                                          
066100     .                                                                    
066200     EJECT                                                                
066300 MFS-ROER-EJ-RAD-FAELT-IN SECTION.                                        
066400                                                                          
066500*    --- ALLA UTDATA-FÄLT                                                 
066600     MOVE ALL-PLUS          TO RESP-KDSVAR-LINE(INDX)                     
066700     .                                                                    
066800     EJECT                                                                
066900 MFS-LAES-IN-IGEN SECTION.                                                
067000                                                                          
067100*    --- ALLA INDATA-FÄLT                                                 
067200     MOVE +1 TO INDX                                                      
067300     PERFORM UNTIL INDX > MAX-KVRADER                                     
067400       IF REQU-KDSVAR-LINE(INDX) NOT = ALL '+'                            
067500         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
067600                                  RESP-KDSVAR-LINE-ATTR(INDX)             
067700       END-IF                                                             
067800       ADD +1 TO INDX                                                     
067900     END-PERFORM                                                          
068000     .                                                                    
068100     EJECT                                                                
068200 MFS-FORM-ATTR SECTION.                                                   
068300                                                                          
068400*    --- ALLA INDATA-FÄLT                                                 
068500     MOVE +1                   TO INDX                                    
068600     PERFORM UNTIL INDX > MAX-KVRADER                                     
068700       MOVE MFS-FORMATETS-ATTR TO RESP-KDSVAR-LINE-ATTR(INDX)             
068800                                                                          
068900       ADD +1 TO INDX                                                     
069000     END-PERFORM                                                          
069100     .                                                                    
069200* --- IMS SEKTIONER ---                                                   
069300     SKIP3                                                                
069400 IMS-PURG-ALT-MSG-USA SECTION.                                            
069500                                                                          
069600     MOVE SPACE TO GODK-STATUSKODER                                       
069700     CALL CBLTDLI USING PURG ALT-USA-PCB W-PROG-TO-PROG-SW-USA            
069800     MOVE ALT-USA-STATUS-CODE TO STATUS-WS                                
069900     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
070100                                                                          
070200 IMS-INSERT-ALT-MSG-JAP SECTION.                                          
070300                                                                          
070400     MOVE SPACE TO GODK-STATUSKODER                                       
070500     CALL CBLTDLI USING ISRT ALT-JAP-PCB W-PROG-TO-PROG-SW-JAP            
070600     MOVE ALT-JAP-STATUS-CODE TO STATUS-WS                                
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900     EJECT                                                                
071000 IMS-INSERT-ALT-MSG-AUS SECTION.                                          
071100                                                                          
071200     MOVE SPACE TO GODK-STATUSKODER                                       
071300     CALL CBLTDLI USING ISRT ALT-AUS-PCB W-PROG-TO-PROG-SW-AUS            
071400     MOVE ALT-AUS-STATUS-CODE TO STATUS-WS                                
071500     PERFORM IMS-STATUSKONTROLL                                           
071600     .                                                                    
071700     EJECT                                                                
071800 IMS-GU-WL446301-DC  SECTION.                                             
071900                                                                          
072000     STRING 'WL446301(WDGXKEY  =' W-WDGXKEY-X ')'                         
072100          DELIMITED BY SIZE INTO SSA1                                     
072200     MOVE '  GE' TO GODK-STATUSKODER                                      
072300     CALL CBLTDLI USING GU 4463-PCB DLI-IO-AREA-4463 SSA1                 
072400     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
072500     PERFORM IMS-STATUSKONTROLL                                           
072600     .                                                                    
072700     SKIP2                                                                
072800 IMS-GHNP-WL446311 SECTION.                                               
072900                                                                          
073000     STRING 'WL446311(DASKEPPN <' W-DASKEPPN-MIN-X ')'                    
073100          DELIMITED BY SIZE INTO SSA1                                     
073200     MOVE '  GE' TO GODK-STATUSKODER                                      
073300     CALL CBLTDLI USING GHNP 4463-PCB DLI-IO-AREA-4464 SSA1               
073400     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
073500     PERFORM IMS-STATUSKONTROLL                                           
073600     .                                                                    
073700     SKIP3                                                                
073800 IMS-DLET-WL446311 SECTION.                                               
073900                                                                          
074000     MOVE '  ' TO GODK-STATUSKODER                                        
074100     CALL CBLTDLI USING DLET 4463-PCB DLI-IO-AREA-4464                    
074200     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
074300     PERFORM IMS-STATUSKONTROLL                                           
074400     .                                                                    
074500     EJECT                                                                
074600 IMS-GU-WL446311-TID SECTION.                                             
074700                                                                          
074800     STRING 'WL446311(DASKEPPN =' W-DASKEPPN-X ')'                        
074900          DELIMITED BY SIZE INTO SSA1                                     
075000     MOVE '  GE' TO GODK-STATUSKODER                                      
075100     CALL CBLTDLI USING GU 4463-PCB DLI-IO-AREA-4464 SSA1                 
075200     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
075300     PERFORM IMS-STATUSKONTROLL                                           
075400     .                                                                    
075500     EJECT                                                                
075600 IMS-GNP-WL446321-TRANSP SECTION.                                         
075700                                                                          
075800     MOVE 'WL446321 ' TO SSA1                                             
075900     MOVE '  GE' TO GODK-STATUSKODER                                      
076000     CALL CBLTDLI USING GNP 4463-PCB DLI-IO-AREA-4466 SSA1                
076100     MOVE 4463-STATUS-CODE TO STATUS-WS                                   
076200     PERFORM IMS-STATUSKONTROLL                                           
076300     .                                                                    
076400     EJECT                                                                
076500 IMS-GU-WDB601    SECTION.                                                
076600                                                                          
076700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
076800          DELIMITED BY SIZE INTO SSA1                                     
076900     MOVE '  GE' TO GODK-STATUSKODER                                      
077000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
077100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
077200     PERFORM IMS-STATUSKONTROLL                                           
077300     .                                                                    
077400     EJECT                                                                
077500 IMS-STATUSKONTROLL SECTION.                                              
077600                                                                          
077700     SET STATUS-IX TO 1                                                   
077800     SEARCH GODK-STATUS                                                   
077900       AT END                                                             
078000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
078100         DELIMITED BY SIZE INTO FELTEXT                                   
078200         CALL FELLOG                                                      
078300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078400         CONTINUE                                                         
078500     END-SEARCH                                                           
078600     .                                                                    
