000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0051100.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   91/02/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MPP-INIT-REGISTRERING                                            
000900*        VISAR, LÄGGER UPP, TAR BORT, FÖRÄNDRAR                           
001000*        USER-UPPGIFTER PÅ MPP-INIT-DATABASEN                             
001100*                                                                         
001200*        PROGRAMMET UPPATERAR WLUSEA (WDP7)                               
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W0T511                                              
001600*        MID:         W0I51101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W0O51101                                            
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800*    -- CHECKED BY WY2000                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(08)   VALUE 'W0051100'.            
003100 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
003200                                                                          
003300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
003900 77  WS-DATE                     PIC 9(6)    VALUE ZERO.                  
004000 77  WS-TIME                     PIC 9(8)    VALUE ZERO.                  
004010 77  WS-MEDKONV-MESSAGE          PIC X(3)    VALUE SPACE.                 
004100                                                                          
004200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004300     88  INDATA-OK                           VALUE 'J'.                   
004400     88  INDATA-FEL                          VALUE 'N'.                   
004500                                                                          
004600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004700     88  EGEN-MID                            VALUE '0511'.                
004800     88  GODK-MID                            VALUE '0511' '0512'          
004900                                                   '0513' '0514'          
005000                                                   '0515' '0516'          
005100                                                   '0517' '0518'          
005200                                                   '0519' '0551'.         
005300                                                                          
005400     EJECT                                                                
005500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005600 01  GENERELLA-SUBPROGRAM.                                                
005700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100                                                                          
006200     EJECT                                                                
006300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006400*   -COPY WMEDAREA                                                        
006500     SKIP3                                                                
006600 01  MESSAGE-CODES.                                                       
006700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007010     03  ERR-WRONG-DC            PIC X(3)    VALUE '340'.                 
007100                                                                          
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007400                                                                          
007500*01  -COPY WMSGINIT                                                       
007600                                                                          
007700     EJECT                                                                
007800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008100                                                                          
008200*01  MID -COPY W0I51101                                                   
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008500                                                                          
008600*01  -COPY WMSGAREA                                                       
008700     EJECT                                                                
008800     03  MOD REDEFINES MSG-AREA.                                          
008900*      05  -COPY W0O51101                                                 
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009200*01  -COPY WMFSAREA                                                       
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500                                                                          
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-WDP701KY-X.                                                    
010000         05  W-IDUSER            PIC X(8)     VALUE SPACE.                
010010     03  W-IDDC-B6-X.                                                     
010020         05 W-IDDC-B6                  PIC X(2).                          
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010700     SKIP2                                                                
010800 01  GODK-STATUSKODER.                                                    
010900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200     EJECT                                                                
011300*    --- IMS FUNKTIONSKODER                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600*    ---  DLI INPUT-OUTPUT AREA                                           
011700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-USER01'.         
011800                                                                          
011900 01  DLI-IO-USEA01.                                                       
012000*    03  -COPY WDP701                                                     
012010                                                                          
012020 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
012030 01   DLI-IO-AREA-B601.                                                   
012040*     03  -COPY WDB601                                                    
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012300*01  -COPY W0009      -PRE MSG-                                           
012400                                                                          
012500*01  -COPY W0008      -PRE USEA-                                          
012600     05  FILLER                  PIC X.                                   
012610                                                                          
012620*01  -COPY W0008      -PRE WDB6-                                          
012630     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDB6-PCB.                     
012900 MAIN SECTION.                                                            
013000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDB6-PCB.                     
013100                                                                          
013200     PERFORM IMS-GET-MSG                                                  
013300     IF SEGMENT-FINNS                                                     
013400       PERFORM A-INIT                                                     
013500       PERFORM B-KOLLA-NYCKLAR                                            
013600       IF MFS-UPDATE                                                      
013700         PERFORM G-KOLLA-INPUT                                            
013800         IF INDATA-OK                                                     
013900           PERFORM H-UPPDATERA                                            
014000         END-IF                                                           
014100       ELSE                                                               
014200         PERFORM F-LAES-VISA-INFO                                         
014300       END-IF                                                             
014400       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O51101 + 4                      
014500       PERFORM IMS-INSERT-MSG                                             
014600     END-IF                                                               
014700                                                                          
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 A-INIT SECTION.                                                          
015300                                                                          
015400     MOVE WHEN-COMPILED TO W-COMPILED                                     
015500     ACCEPT WS-DATE FROM DATE                                             
015600     ACCEPT WS-TIME FROM TIME                                             
015700                                                                          
015800     IF MSG-DUBBLA-TRANSKODER                                             
015900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I51101                 
016000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
016100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016200     ELSE                                                                 
016300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I51101                  
016400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
016500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
016600     END-IF                                                               
016700                                                                          
016800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
016900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
017000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017100                                                                          
017200     MOVE LOW-VALUE TO MSG-AREA                                           
017300     MOVE 'W0O51101' TO MFS-IDMOD                                         
017400     MOVE '0511' TO MOD-IDTRANS-UT                                        
017500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
017600                                                                          
017700     IF NOT EGEN-MID                                                      
017800       MOVE SPACE TO MFS-KDTRTYP                                          
017900     END-IF                                                               
018000                                                                          
018100     IF ENGLISH-TEXT                                                      
018200       MOVE 'GB ' TO MED-IDSKYLT                                          
018300     ELSE                                                                 
018400       MOVE 'S  ' TO MED-IDSKYLT                                          
018500     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 B-KOLLA-NYCKLAR SECTION.                                                 
018900                                                                          
019000     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-IN                                
019100                                                                          
019200     IF MID-IDUSER-IN = ALL '+'                                           
019300       MOVE MID-IDUSER-UT TO WS-IDUSER                                    
019400     ELSE                                                                 
019500       MOVE MID-IDUSER-IN TO WS-IDUSER                                    
019600     END-IF                                                               
019700     IF WS-IDUSER = SPACE                                                 
019800       MOVE MSG-SIGNON-USERID TO WS-IDUSER                                
019900     END-IF                                                               
020000     MOVE WS-IDUSER TO W-IDUSER                                           
020100                                                                          
020200     IF GODK-MID                                                          
020300       MOVE WS-IDUSER TO MOD-IDUSER-UT                                    
020400     ELSE                                                                 
020500       MOVE MFS-RENSA-FAELT TO MOD-IDUSER-UT                              
020600     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 F-LAES-VISA-INFO SECTION.                                                
021000                                                                          
021100     IF MID-KDCMDVAL = '+++'                                              
021200       PERFORM IMS-GET-USEA-ROT                                           
021300       IF SEGMENT-FINNS                                                   
021400         MOVE INIT-BEANST           TO MOD-BEANST                         
021500         MOVE INIT-IDAVD            TO MOD-IDAVD                          
021600         MOVE INIT-IDDC             TO MOD-IDDC                           
021700         MOVE INIT-IDFTG            TO MOD-IDFTG                          
021800         MOVE INIT-IDLAND-SPR       TO MOD-IDLAND-SPR                     
021900         MOVE INIT-IDLTERM          TO MOD-IDLTERM                        
022000         MOVE INIT-IDCSS            TO MOD-IDCSS                          
022100         MOVE INIT-IDSPRAK          TO MOD-IDSPRAK                        
022200         MOVE INIT-IDNODE           TO MOD-IDNODE                         
022300         MOVE INIT-IDRT-KEY         TO MOD-IDRT-KEY                       
022400         MOVE INIT-IDTFN            TO MOD-IDTFN                          
022500         MOVE INIT-IDTFX            TO MOD-IDTFX                          
022600         MOVE INIT-IDTIDZON         TO MOD-IDTIDZON                       
022700         MOVE INIT-IDTRANS          TO MOD-IDTRANS                        
022800         MOVE INIT-KDMATT           TO MOD-KDMATT                         
022900         MOVE INIT-KDMFSFOR         TO MOD-KDMFSFOR                       
023000         MOVE INIT-KDSVAR           TO MOD-KDSVAR                         
023100         MOVE INIT-TIREGDAT-MPP     TO MOD-TIREGDAT                       
023200         MOVE INIT-TIUPPDAT         TO MOD-TIUPPDAT                       
023300         MOVE INIT-TIUPPTID         TO MOD-TIUPPTID                       
023400         PERFORM S01-KOLLA-TIDZON                                         
023500       ELSE                                                               
023600         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
023700         CALL WMEDKONV USING MED-WMEDAREA                                 
023800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
023900         PERFORM MFS-RENSA-FAELT-UT                                       
024000       END-IF                                                             
024100       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL                               
024200     ELSE                                                                 
024300       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
024400       CALL WMEDKONV USING MED-WMEDAREA                                   
024500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
024600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024700       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL                             
024800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR                    
024900     END-IF                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 G-KOLLA-INPUT SECTION.                                                   
025300                                                                          
025400     MOVE JA  TO INDATA-SW                                                
025500     IF MID-KDCMDVAL = 'INS' OR 'REP' OR 'DEL'                            
025600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR                     
025700     ELSE                                                                 
025800       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR                       
025900       MOVE NEJ TO INDATA-SW                                              
026000     END-IF                                                               
026100                                                                          
026200     IF INDATA-FEL                                                        
026300       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
026400       CALL WMEDKONV USING MED-WMEDAREA                                   
026500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026600       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL                             
026700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR                    
026800     END-IF                                                               
026900     PERFORM MFS-ROER-EJ-FAELT-UT                                         
027000     .                                                                    
027100     EJECT                                                                
027200 H-UPPDATERA SECTION.                                                     
027300                                                                          
027400     IF MID-KDCMDVAL = 'INS'                                              
027500       MOVE SPACE TO INIT-WDP701                                          
027600       MOVE WS-IDUSER        TO INIT-IDUSER                               
027700       MOVE WS-DATE          TO INIT-TIREGDAT-MPP                         
027800       PERFORM HA-MOVE-DATA                                               
027900       PERFORM IMS-ISRT-USEA-ROT                                          
028000       IF STATUS-WS = 'II'                                                
028100         MOVE NEJ TO INDATA-SW                                            
028110         MOVE ERR-WRONG-KEY        TO WS-MEDKONV-MESSAGE                  
028200       END-IF                                                             
028300     ELSE                                                                 
028400       PERFORM IMS-GET-USEA-ROT                                           
028500       IF SEGMENT-FINNS                                                   
028600         IF MID-KDCMDVAL = 'REP'                                          
028700           PERFORM HA-MOVE-DATA                                           
028800           PERFORM IMS-REPL-USEA                                          
028900         ELSE                                                             
029000           PERFORM IMS-DLET-USEA                                          
029100         END-IF                                                           
029200       ELSE                                                               
029300         MOVE NEJ TO INDATA-SW                                            
029310         MOVE ERR-WRONG-KEY       TO WS-MEDKONV-MESSAGE                   
029400       END-IF                                                             
029500     END-IF                                                               
029600     IF INDATA-FEL                                                        
029700       MOVE WS-MEDKONV-MESSAGE    TO MED-IDMFSFEL                         
029800       CALL WMEDKONV USING MED-WMEDAREA                                   
029900       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
029910       IF WS-MEDKONV-MESSAGE = ERR-WRONG-DC                               
029920         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDC                           
029930         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-ATTR                      
030110       ELSE                                                               
030120         MOVE MFS-ROER-EJ-FAELT     TO MOD-KDCMDVAL                       
030130         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR                  
030140       END-IF                                                             
030200     ELSE                                                                 
030300       MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                         
030400       CALL WMEDKONV USING MED-WMEDAREA                                   
030500       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
030600       MOVE MFS-RENSA-FAELT       TO MOD-KDCMDVAL                         
030700       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR                       
030800     END-IF                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 HA-MOVE-DATA SECTION.                                                    
031200                                                                          
031300     MOVE MID-BEANST       TO INIT-BEANST                                 
031400     MOVE MID-IDAVD        TO INIT-IDAVD                                  
031410     MOVE MID-IDDC         TO W-IDDC-B6                                   
031411     PERFORM IMS-GU-WDB601                                                
031420     IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                              
031500       MOVE MID-IDDC       TO INIT-IDDC                                   
031510     ELSE                                                                 
031520       MOVE NEJ            TO INDATA-SW                                   
031530       MOVE ERR-WRONG-DC   TO WS-MEDKONV-MESSAGE                          
031540     END-IF                                                               
031550     MOVE MID-IDLAND-SPR   TO INIT-IDLAND-SPR                             
031600     MOVE MID-IDFTG        TO INIT-IDFTG                                  
031800     MOVE MID-IDLTERM      TO INIT-IDLTERM                                
031900     MOVE MID-IDCSS        TO INIT-IDCSS                                  
031910                                                                          
031930*    -- ACCEPTERA LANDSKOD SOM INPUT OCH RÄTTA VID BEHOV                  
032000     MOVE MID-IDSPRAK      TO INIT-IDSPRAK                                
032001     EVALUATE MID-IDSPRAK                                                 
032005      WHEN 'SE'  MOVE 'SV' TO INIT-IDSPRAK       MOD-IDSPRAK              
032007      WHEN 'GB'  MOVE 'EN' TO INIT-IDSPRAK       MOD-IDSPRAK              
032009      WHEN 'JP'  MOVE 'JA' TO INIT-IDSPRAK       MOD-IDSPRAK              
032011      WHEN 'TW'  MOVE 'ZH' TO INIT-IDSPRAK       MOD-IDSPRAK              
032012     END-EVALUATE                                                         
032020                                                                          
032100     MOVE MID-IDNODE       TO INIT-IDNODE                                 
032200     MOVE MID-IDRT-KEY     TO INIT-IDRT-KEY                               
032300     MOVE MID-IDTFN        TO INIT-IDTFN                                  
032400     MOVE MID-IDTFX        TO INIT-IDTFX                                  
032500     MOVE MID-KDMATT       TO INIT-KDMATT                                 
032600     IF MID-IDTIDZON NUMERIC AND MID-IDTIDZON < '24'                      
032700       MOVE MID-IDTIDZON   TO INIT-IDTIDZON                               
032800     ELSE                                                                 
032900       MOVE '11'           TO INIT-IDTIDZON                               
033000     END-IF                                                               
033100     MOVE WS-DATE          TO INIT-TIUPPDAT                               
033200     MOVE WS-TIME          TO INIT-TIUPPTID                               
033300     .                                                                    
033400     EJECT                                                                
033500 MFS-RENSA-FAELT-UT SECTION.                                              
033600                                                                          
033700     MOVE MFS-RENSA-FAELT           TO MOD-BEANST                         
033800                                       MOD-IDAVD                          
033900                                       MOD-IDDC                           
034000                                       MOD-IDFTG                          
034100                                       MOD-IDLAND-SPR                     
034200                                       MOD-IDLTERM                        
034300                                       MOD-IDCSS                          
034400                                       MOD-IDSPRAK                        
034500                                       MOD-IDNODE                         
034600                                       MOD-IDRT-KEY                       
034700                                       MOD-IDTFN                          
034800                                       MOD-IDTFX                          
034900                                       MOD-IDRT-KEY                       
035000                                       MOD-IDTRANS                        
035100                                       MOD-KDMATT                         
035200                                       MOD-KDMFSFOR                       
035300                                       MOD-KDSVAR                         
035400                                       MOD-TIREGDAT                       
035500                                       MOD-TIUPPDAT                       
035600                                       MOD-TIUPPTID                       
035700                                       MOD-TILOKDAT                       
035800                                       MOD-TILOKTID                       
035900     .                                                                    
036000     EJECT                                                                
036100 MFS-ROER-EJ-FAELT-UT SECTION.                                            
036200                                                                          
036300     MOVE MFS-ROER-EJ-FAELT         TO MOD-BEANST                         
036400                                       MOD-BEANST                         
036500                                       MOD-IDAVD                          
036600                                       MOD-IDDC                           
036700                                       MOD-IDFTG                          
036800                                       MOD-IDLAND-SPR                     
036900                                       MOD-IDLTERM                        
037000                                       MOD-KDMATT                         
037100                                       MOD-IDCSS                          
037200                                       MOD-IDSPRAK                        
037300                                       MOD-IDNODE                         
037400                                       MOD-IDRT-KEY                       
037500                                       MOD-IDTFN                          
037600                                       MOD-IDTFX                          
037700                                       MOD-IDTIDZON                       
037800                                       MOD-IDTRANS                        
037900                                       MOD-KDMFSFOR                       
038000                                       MOD-KDSVAR                         
038100                                       MOD-TIREGDAT                       
038200                                       MOD-TIUPPDAT                       
038300                                       MOD-TIUPPTID                       
038400                                       MOD-TILOKDAT                       
038500                                       MOD-TILOKTID                       
038600     .                                                                    
038700                                                                          
038800     EJECT                                                                
038900 S01-KOLLA-TIDZON SECTION.                                                
039000                                                                          
039100     MOVE '011'             TO MSGI-KDCALL                                
039200     MOVE WS-IDUSER         TO MSGI-IDUSER                                
039300                               MSGI-IDLTERM-USER                          
039400     MOVE WS-DATE           TO MSGI-TILOKDAT                              
039500     MOVE WS-TIME           TO MSGI-TILOKTID                              
039510     MOVE '0511'            TO MSGI-IDTRANS                               
039600     CALL W005INIT USING   MSGI-WMSGINIT USEA-PCB                         
039700     IF INIT-KDMATT = 'U'                                                 
039800       STRING MSGI-TILOKDAT(3:2) '/'                                      
039900              MSGI-TILOKDAT(5:2) '/'                                      
040000              MSGI-TILOKDAT(1:2)                                          
040100          DELIMITED BY SIZE INTO MOD-TILOKDAT                             
040200     ELSE                                                                 
040300       STRING MSGI-TILOKDAT(1:2) '/'                                      
040400              MSGI-TILOKDAT(3:2) '/'                                      
040500              MSGI-TILOKDAT(5:2)                                          
040600          DELIMITED BY SIZE INTO MOD-TILOKDAT                             
040700     END-IF                                                               
040800     STRING MSGI-TILOKTID(1:2) ':'                                        
040900            MSGI-TILOKTID(3:2)                                            
041000        DELIMITED BY SIZE INTO MOD-TILOKTID                               
041100     .                                                                    
041200     EJECT                                                                
041300* --- IMS SEKTIONER ---                                                   
041400                                                                          
041500 IMS-GET-MSG SECTION.                                                     
041600                                                                          
041700     MOVE '  QC' TO GODK-STATUSKODER                                      
041800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
041900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     .                                                                    
042200     SKIP3                                                                
042300 IMS-INSERT-MSG SECTION.                                                  
042400                                                                          
042500     IF INIT-IDLAND-SPR = 'GB'                                            
042600       MOVE '0' TO MFS-KDHUVOMR                                           
042700     END-IF                                                               
042800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
042900     MOVE SPACE TO GODK-STATUSKODER                                       
043000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
043100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043400     EJECT                                                                
043500 IMS-GET-USEA-ROT SECTION.                                                
043600     STRING 'WLUSEA01(IDUSER   =' W-WDP701KY-X ')'                        
043700          DELIMITED BY SIZE INTO SSA1                                     
043800     MOVE '  GE' TO GODK-STATUSKODER                                      
043900     CALL CBLTDLI USING GHU USEA-PCB DLI-IO-USEA01 SSA1                   
044000     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
044100     PERFORM IMS-STATUSKONTROLL                                           
044200     .                                                                    
044300     SKIP3                                                                
044400 IMS-ISRT-USEA-ROT SECTION.                                               
044500                                                                          
044600     MOVE 'WLUSEA01 ' TO SSA1                                             
044700     MOVE '  II' TO GODK-STATUSKODER                                      
044800     CALL CBLTDLI USING ISRT USEA-PCB DLI-IO-USEA01 SSA1                  
044900     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
045000     PERFORM IMS-STATUSKONTROLL                                           
045100     .                                                                    
045200     SKIP3                                                                
045300 IMS-REPL-USEA SECTION.                                                   
045400                                                                          
045500     MOVE '  ' TO GODK-STATUSKODER                                        
045600     CALL CBLTDLI USING REPL USEA-PCB DLI-IO-USEA01                       
045700     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     .                                                                    
046000     SKIP3                                                                
046100 IMS-DLET-USEA SECTION.                                                   
046200                                                                          
046300     MOVE '  ' TO GODK-STATUSKODER                                        
046400     CALL CBLTDLI USING DLET USEA-PCB DLI-IO-USEA01                       
046500     MOVE USEA-STATUS-CODE TO STATUS-WS                                   
046600     PERFORM IMS-STATUSKONTROLL                                           
046700     .                                                                    
046800     EJECT                                                                
046810 IMS-GU-WDB601    SECTION.                                                
046820     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
046830          DELIMITED BY SIZE INTO SSA1                                     
046840     MOVE '  GE' TO GODK-STATUSKODER                                      
046850     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
046860     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
046870     PERFORM IMS-STATUSKONTROLL                                           
046880     IF SEGMENT-SAKNAS                                                    
046890         MOVE SPACE TO DCS-KDDC                                           
046891     END-IF                                                               
046892     .                                                                    
046900 IMS-STATUSKONTROLL SECTION.                                              
047000                                                                          
047100     SET STATUS-IX TO 1                                                   
047200     SEARCH GODK-STATUS                                                   
047300       AT END                                                             
047400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047500         DELIMITED BY SIZE INTO FELTEXT                                   
047600         CALL FELLOG                                                      
047700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047800         CONTINUE                                                         
047900     END-SEARCH                                                           
048000     .                                                                    
