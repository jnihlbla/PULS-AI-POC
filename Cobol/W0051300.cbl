000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0051300.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   91/02/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MPP-INIT-REGISTRERING                                            
000900*        VISAR, FÖRÄNDRAR EGNA                                            
001000*        USER-UPPGIFTER PÅ MPP-INIT-DATABASEN                             
001100*                                                                         
001200*        PROGRAMMET UPPATERAR WLUSEA (WDP7)                               
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T513                                              
001600*        MID:         W0I51301                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W0O51301                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W0051300'.            
003000 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
003100                                                                          
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
003800 77  WS-DATE                     PIC 9(6)    VALUE ZERO.                  
003900 77  WS-TIME                     PIC 9(8)    VALUE ZERO.                  
004000                                                                          
004100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004200     88  INDATA-OK                           VALUE 'J'.                   
004300     88  INDATA-FEL                          VALUE 'N'.                   
004400                                                                          
004500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004600     88  EGEN-MID                            VALUE '0513'.                
004700     88  GODK-MID                            VALUE '0511' '0512'          
004800                                                   '0513' '0514'          
004900                                                   '0515' '0516'          
005000                                                   '0517' '0518'          
005100                                                   '0519' '0551'.         
005200                                                                          
005300                                                                          
005400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005500 01  GENERELLA-SUBPROGRAM.                                                
005600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000                                                                          
006100     EJECT                                                                
006200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006300*   -COPY WMEDAREA                                                        
006400                                                                          
006500 01  MESSAGE-CODES.                                                       
006600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007000                                                                          
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007300                                                                          
007400*01  -COPY WMSGINIT                                                       
007500                                                                          
007600     EJECT                                                                
007700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008000                                                                          
008100*01  MID -COPY W0I51301                                                   
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008400                                                                          
008500*01  -COPY WMSGAREA                                                       
008600     EJECT                                                                
008700     03  MOD REDEFINES MSG-AREA.                                          
008800*      05  -COPY W0O51301                                                 
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009100*01  -COPY WMFSAREA                                                       
009200     EJECT                                                                
009300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009400                                                                          
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700 01  NYCKLAR-TILL-DLI.                                                    
009800     03  W-WDP701KY-X.                                                    
009900         05  W-IDUSER            PIC X(8)     VALUE SPACE.                
010000     SKIP2                                                                
010100*    --- STATUS-KOD FRÅN IMS                                              
010200 01  STATUS-WS                   PIC XX.                                  
010300     88  SEGMENT-FINNS                       VALUE '  '.                  
010400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010600     SKIP2                                                                
010700 01  GODK-STATUSKODER.                                                    
010800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010900     SKIP3                                                                
011000 01  SSA1                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNKTIONSKODER                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-USER01'.         
011700                                                                          
011800 01  DLI-IO-USER01.                                                       
011900*    03  -COPY WDP701                                                     
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200*01  -COPY W0009      -PRE MSG-                                           
012300                                                                          
012400*01  -COPY W0008      -PRE USEA-                                          
012500     05  FILLER                  PIC X.                                   
012600     EJECT                                                                
012700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB.                              
012800 MAIN SECTION.                                                            
012900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB.                              
013000                                                                          
013100     PERFORM IMS-GET-MSG                                                  
013200     IF SEGMENT-FINNS                                                     
013300       PERFORM A-INIT                                                     
013400       PERFORM B-KOLLA-NYCKLAR                                            
013500       PERFORM IMS-GET-USEA-ROT                                           
013600       IF MFS-UPDATE                                                      
013700         PERFORM G-KOLLA-INPUT                                            
013800         IF INDATA-OK                                                     
013900           PERFORM H-UPPDATERA                                            
014000         END-IF                                                           
014100       END-IF                                                             
014200       PERFORM F-LAES-VISA-INFO                                           
014300       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O51301 + 4                      
014400       PERFORM IMS-INSERT-MSG                                             
014500     END-IF                                                               
014600                                                                          
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200                                                                          
015300     MOVE WHEN-COMPILED TO W-COMPILED                                     
015400     ACCEPT WS-DATE FROM DATE                                             
015500     ACCEPT WS-TIME FROM TIME                                             
015600                                                                          
015700     IF MSG-DUBBLA-TRANSKODER                                             
015800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I51301                 
015900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
016000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016100     ELSE                                                                 
016200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I51301                  
016300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
016400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
016500     END-IF                                                               
016600                                                                          
016700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
016800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
016900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017000                                                                          
017100     MOVE LOW-VALUE TO MSG-AREA                                           
017200     MOVE 'W0O51301' TO MFS-IDMOD                                         
017300     MOVE '0513' TO MOD-IDTRANS                                           
017400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
017500                                                                          
017600     IF NOT EGEN-MID                                                      
017700       MOVE SPACE TO MFS-KDTRTYP                                          
017800     END-IF                                                               
017900                                                                          
018000     IF ENGLISH-TEXT                                                      
018100       MOVE 'GB ' TO MED-IDSKYLT                                          
018200     ELSE                                                                 
018300       MOVE 'S  ' TO MED-IDSKYLT                                          
018400     END-IF                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 B-KOLLA-NYCKLAR SECTION.                                                 
018800                                                                          
018900     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
019000                                                                          
019100     IF MID-IDUSER-IN = ALL '+'                                           
019200       MOVE MID-IDUSER-UT TO WS-IDUSER                                    
019300     ELSE                                                                 
019400       MOVE MID-IDUSER-IN TO WS-IDUSER                                    
019500     END-IF                                                               
019600     IF WS-IDUSER = SPACE                                                 
019700       MOVE MSG-SIGNON-USERID TO WS-IDUSER                                
019800     END-IF                                                               
019900     MOVE WS-IDUSER TO W-IDUSER                                           
020000                                                                          
020100     IF GODK-MID                                                          
020200       MOVE WS-IDUSER TO MOD-IDUSER-UT                                    
020300     ELSE                                                                 
020400       MOVE MFS-RENSA-FAELT TO MOD-IDUSER-UT                              
020500     END-IF                                                               
020600     .                                                                    
020700     EJECT                                                                
020800 F-LAES-VISA-INFO SECTION.                                                
020900                                                                          
021000     IF SEGMENT-FINNS                                                     
021100       MOVE INIT-BEANST           TO MOD-BEANST                           
021200       MOVE INIT-IDAVD            TO MOD-IDAVD                            
021300       MOVE INIT-IDDC             TO MOD-IDDC                             
021400       MOVE INIT-IDFTG            TO MOD-IDFTG                            
021500       MOVE INIT-IDLAND-SPR       TO MOD-IDLAND-SPR                       
021600       MOVE INIT-IDLTERM          TO MOD-IDLTERM                          
021700       MOVE INIT-IDCSS            TO MOD-IDCSS                            
021800       MOVE INIT-IDSPRAK          TO MOD-IDSPRAK                          
021900       MOVE INIT-IDNODE           TO MOD-IDNODE                           
022000       MOVE INIT-IDRT-KEY         TO MOD-IDRT-KEY                         
022100       MOVE INIT-IDTFN            TO MOD-IDTFN                            
022200       MOVE INIT-IDTFX            TO MOD-IDTFX                            
022300       MOVE INIT-IDTIDZON         TO MOD-IDTIDZON                         
022400       MOVE INIT-KDMATT           TO MOD-KDMATT                           
022500       MOVE INIT-TIREGDAT-MPP     TO MOD-TIREGDAT                         
022600       MOVE INIT-TIUPPDAT         TO MOD-TIUPPDAT                         
022700       MOVE INIT-TIUPPTID         TO MOD-TIUPPTID                         
022800       PERFORM S01-KOLLA-TIDZON                                           
022900     ELSE                                                                 
023000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
023100       CALL WMEDKONV USING MED-WMEDAREA                                   
023200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023300       PERFORM MFS-RENSA-FAELT-UT                                         
023400     END-IF                                                               
023500     .                                                                    
023600     EJECT                                                                
023700 G-KOLLA-INPUT SECTION.                                                   
023800                                                                          
023900     IF WS-IDUSER = MSG-SIGNON-USERID                                     
024000       MOVE JA  TO INDATA-SW                                              
024100     ELSE                                                                 
024200       MOVE NEJ TO INDATA-SW                                              
024300     END-IF                                                               
024400                                                                          
024500     IF INDATA-FEL                                                        
024600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
024700       CALL WMEDKONV USING MED-WMEDAREA                                   
024800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024900     END-IF                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 H-UPPDATERA SECTION.                                                     
025300                                                                          
025400     IF SEGMENT-FINNS                                                     
025500       PERFORM HA-MOVE-DATA                                               
025600       PERFORM IMS-REPL-USEA                                              
025700     ELSE                                                                 
025800       MOVE NEJ TO INDATA-SW                                              
025900     END-IF                                                               
026000     IF INDATA-FEL                                                        
026100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
026200       CALL WMEDKONV USING MED-WMEDAREA                                   
026300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026400     ELSE                                                                 
026500       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
026600       CALL WMEDKONV USING MED-WMEDAREA                                   
026700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
026800     END-IF                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 HA-MOVE-DATA SECTION.                                                    
027200                                                                          
027300     IF MID-BEANST     NOT = ALL '+'                                      
027400       MOVE MID-BEANST     TO INIT-BEANST                                 
027500     END-IF                                                               
027600     IF MID-IDAVD      NOT = ALL '+'                                      
027700       MOVE MID-IDAVD      TO INIT-IDAVD                                  
027800     END-IF                                                               
027810                                                                          
027811*    -- UNDERHÅLL IDLAND-SPR VIA IDSPRAK                                  
027812*    -- ACCEPTERA LANDSKOD SOM INPUT OCH JUSTERA VID BEHOV                
027813     IF MID-IDSPRAK NOT = ALL '+'                                         
027815       MOVE MID-IDSPRAK      TO INIT-IDSPRAK INIT-IDLAND-SPR              
027816       EVALUATE MID-IDSPRAK                                               
027817        WHEN 'SV'  MOVE 'SE' TO INIT-IDLAND-SPR                           
027818        WHEN 'SE'  MOVE 'SV' TO INIT-IDSPRAK                              
027819        WHEN 'EN'  MOVE 'GB' TO INIT-IDLAND-SPR                           
027820        WHEN 'GB'  MOVE 'EN' TO INIT-IDSPRAK                              
027821        WHEN 'JA'  MOVE 'JP' TO INIT-IDLAND-SPR                           
027822        WHEN 'JP'  MOVE 'JA' TO INIT-IDSPRAK                              
027823        WHEN 'ZH'  MOVE 'TW' TO INIT-IDLAND-SPR                           
027824        WHEN 'TW'  MOVE 'ZH' TO INIT-IDSPRAK                              
027825       END-EVALUATE                                                       
027840*     -- TVINGA IN GB ELLER SE I LANDSKOD FÖR BAKÅTKOMPABILITET           
028000       IF INIT-IDLAND-SPR NOT = 'SE'                                      
028300         MOVE 'GB'           TO INIT-IDLAND-SPR                           
028400       END-IF                                                             
028500     END-IF                                                               
028510                                                                          
028600     IF MID-IDLTERM    NOT = ALL '+'                                      
028700       MOVE MID-IDLTERM    TO INIT-IDLTERM                                
028800     END-IF                                                               
028900     IF MID-IDCSS      NOT = ALL '+'                                      
029000       MOVE MID-IDCSS      TO INIT-IDCSS                                  
029100     END-IF                                                               
029500     IF MID-IDNODE     NOT = ALL '+'                                      
029600       MOVE MID-IDNODE     TO INIT-IDNODE                                 
029700     END-IF                                                               
029800     IF MID-IDTFN      NOT = ALL '+'                                      
029900       MOVE MID-IDTFN      TO INIT-IDTFN                                  
030000     END-IF                                                               
030100     IF MID-IDTFX      NOT = ALL '+'                                      
030200       MOVE MID-IDTFX      TO INIT-IDTFX                                  
030300     END-IF                                                               
030400     IF MID-IDTIDZON NUMERIC AND MID-IDTIDZON < '24'                      
030500       MOVE MID-IDTIDZON   TO INIT-IDTIDZON                               
030600     END-IF                                                               
030700     IF MID-KDMATT     NOT = ALL '+'                                      
030800       MOVE MID-KDMATT     TO INIT-KDMATT                                 
030900     END-IF                                                               
031000     MOVE WS-DATE          TO INIT-TIUPPDAT                               
031100     MOVE WS-TIME          TO INIT-TIUPPTID                               
031200     .                                                                    
031300     EJECT                                                                
031400 MFS-RENSA-FAELT-UT SECTION.                                              
031500                                                                          
031600     MOVE MFS-RENSA-FAELT           TO MOD-BEANST                         
031700                                       MOD-IDAVD                          
031800                                       MOD-IDDC                           
031900                                       MOD-IDFTG                          
032000                                       MOD-IDLAND-SPR                     
032100                                       MOD-IDLTERM                        
032200                                       MOD-IDCSS                          
032300                                       MOD-IDSPRAK                        
032400                                       MOD-IDNODE                         
032500                                       MOD-IDRT-KEY                       
032600                                       MOD-IDTFN                          
032700                                       MOD-IDTFX                          
032800                                       MOD-IDRT-KEY                       
032900                                       MOD-KDMATT                         
033000                                       MOD-TIREGDAT                       
033100                                       MOD-TIUPPDAT                       
033200                                       MOD-TIUPPTID                       
033300                                       MOD-TILOKDAT                       
033400                                       MOD-TILOKTID                       
033500     .                                                                    
033600     EJECT                                                                
033700 S01-KOLLA-TIDZON SECTION.                                                
033800                                                                          
033900     MOVE '011'             TO MSGI-KDCALL                                
034000     MOVE WS-IDUSER         TO MSGI-IDUSER                                
034100                               MSGI-IDLTERM-USER                          
034200     MOVE WS-DATE           TO MSGI-TILOKDAT                              
034300     MOVE WS-TIME           TO MSGI-TILOKTID                              
034310     MOVE '0513'            TO MSGI-IDTRANS                               
034400     CALL W005INIT USING   MSGI-WMSGINIT USEA-PCB                         
034500     IF INIT-KDMATT = 'U'                                                 
034600       STRING MSGI-TILOKDAT(3:2) '/'                                      
034700              MSGI-TILOKDAT(5:2) '/'                                      
034800              MSGI-TILOKDAT(1:2)                                          
034900          DELIMITED BY SIZE INTO MOD-TILOKDAT                             
035000     ELSE                                                                 
035100       STRING MSGI-TILOKDAT(1:2) '/'                                      
035200              MSGI-TILOKDAT(3:2) '/'                                      
035300              MSGI-TILOKDAT(5:2)                                          
035400          DELIMITED BY SIZE INTO MOD-TILOKDAT                             
035500     END-IF                                                               
035600     STRING MSGI-TILOKTID(1:2) ':'                                        
035700            MSGI-TILOKTID(3:2)                                            
035800        DELIMITED BY SIZE INTO MOD-TILOKTID                               
035900     .                                                                    
036000     EJECT                                                                
036100* --- IMS SEKTIONER ---                                                   
036200                                                                          
036300 IMS-GET-MSG SECTION.                                                     
036400                                                                          
036500     MOVE '  QC' TO GODK-STATUSKODER                                      
036600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
036700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     .                                                                    
037000     SKIP3                                                                
037100 IMS-INSERT-MSG SECTION.                                                  
037200                                                                          
037300     IF ENGLISH-TEXT                                                      
037400       MOVE 'N' TO MFS-KDHUVOMR                                           
037500     END-IF                                                               
037600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
037700     MOVE SPACE TO GODK-STATUSKODER                                       
037800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
037900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038000     PERFORM IMS-STATUSKONTROLL                                           
038100     .                                                                    
038200     EJECT                                                                
038300 IMS-GET-USEA-ROT SECTION.                                                
038400     STRING 'WLUSEA01(IDUSER   =' W-WDP701KY-X ')'                        
038500          DELIMITED BY SIZE INTO SSA1                                     
038600     MOVE '  GE' TO GODK-STATUSKODER                                      
038700     CALL CBLTDLI USING GHU USEA-PCB DLI-IO-USER01 SSA1                   
038800     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
038900     PERFORM IMS-STATUSKONTROLL                                           
039000     .                                                                    
039100     SKIP3                                                                
039200 IMS-REPL-USEA SECTION.                                                   
039300                                                                          
039400     MOVE '  ' TO GODK-STATUSKODER                                        
039500     CALL CBLTDLI USING REPL USEA-PCB DLI-IO-USER01                       
039600     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     EJECT                                                                
040000 IMS-STATUSKONTROLL SECTION.                                              
040100                                                                          
040200     SET STATUS-IX TO 1                                                   
040300     SEARCH GODK-STATUS                                                   
040400       AT END                                                             
040500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040600         DELIMITED BY SIZE INTO FELTEXT                                   
040700         CALL FELLOG                                                      
040800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040900         CONTINUE                                                         
041000     END-SEARCH                                                           
041100     .                                                                    
