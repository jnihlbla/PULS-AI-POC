000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2042100.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   12/12/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VIEW AND UPDATE AGREEMENTS                                       
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDP7                                       
001100*        PROGRAMMET LÄSER      WDK6                                       
001200*        PROGRAMMET UPPDATERAR WDK7                                       
001300*        PROGRAMMET LÄSER      WDB6                                       
001400*        PROGRAMMET UPPDATERAR WDF1                                       
001500*        PROGRAMMET UPPDATERAR WDR3                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T421                                              
001900*        MID:         W2I42101                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O421N1                                            
002300                                                                          
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W2042100'.            
003000 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003200 77  CURRENT-DATE                PIC 9(6)    VALUE ZERO.                  
003300                                                                          
003400 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
003500*01 -COPY WWIDFTG                                                         
003600     EJECT                                                                
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  WS-FLPG                     PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004500 77  MID-IX                      PIC 9(4)    VALUE 0.                     
004600 77  MID-IX-MAX                  PIC 9(4)    VALUE 2.                     
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  AVTAL-SW                    PIC X       VALUE 'J'.                   
005800     88  AVTAL-OK                            VALUE 'J'.                   
005900     88  AVTAL-FEL                           VALUE 'N'.                   
006000                                                                          
006100 77  LEVNR-SW                    PIC X       VALUE 'J'.                   
006200     88  LEVNR-OK                            VALUE 'J'.                   
006300     88  LEVNR-FEL                           VALUE 'N'.                   
006400                                                                          
006500 01  W-KDFPKPRI                  PIC X       VALUE SPACE.                 
006600 01  W-DAPRLIST                  PIC 9(8)    VALUE ZERO.                  
006700 01  FILLER REDEFINES W-DAPRLIST.                                         
006800     03  W-DAPRLIST-SS           PIC 9(2).                                
006900     03  W-DAPRLIST-AAMMDD       PIC 9(6).                                
007000                                                                          
007100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007200     88  EGEN-MID                            VALUE '2421'.                
007300     88  GODK-MID                            VALUE '2421' '2422'          
007400                                                   '2423' '2424'          
007500                                                   '2425' '2426'          
007600                                                   '2427' '2428'          
007700                                                   '2429'.                
007800     88  HELP-MID                            VALUE '0551'.                
007900                                                                          
008000                                                                          
008100 01  WS-AA0101                   PIC 9(6).                                
008200 01  FILLER  REDEFINES WS-AA0101.                                         
008300     03  WS-AA                   PIC 9(2).                                
008400     03  WS-0101                 PIC 9(4).                                
008500                                                                          
008600 01  DIVERSE.                                                             
008700     03  WS-KDVALISO         PIC X(3)       VALUE SPACE.                  
008800     03  W-PRKURS            PIC S9(5)V9(5) VALUE +0   COMP-3.            
008900     03  W-REVALUTA          PIC S9(5)      VALUE +0   COMP-3.            
009000     03  W-RETULF            PIC S9(3)V9(4) VALUE +0.                     
009100     03  DAGENS-AAMMDD       PIC 9(6).                                    
009200     03  W-PRARTBEL          PIC S9(8)V9(5) VALUE +0.                     
009300     03  WS-PRARTBES-PR      PIC S9(7)V9(2) VALUE +0   COMP-3.            
009400     03  W-DATE-AAMM         PIC 9(4)    VALUE ZERO.                      
009800                                                                          
009900*01 -COPY WWPRODSL                                                        
010000                                                                          
010100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010200 01  GENERELLA-SUBPROGRAM.                                                
010300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010600     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
010700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010810     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
010900                                                                          
011000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011100*01 -COPY WMEDAREA                                                        
011200                                                                          
011300 01  MESSAGE-CODES.                                                       
011400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011800     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
011900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012100     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
012200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012300     03  ERR-DC-INVALID          PIC X(3)    VALUE '440'.                 
012400     03  ERR-DC-SAKNAS           PIC X(3)    VALUE '026'.                 
012500     03  ERR-LOCAL-PG            PIC X(3)    VALUE '511'.                 
012600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
012700                                                                          
012800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
012900*01 -COPY WDATAREA                                                        
013000                                                                          
013100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013200*                                                                         
013300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013400*01 -COPY WMSGINIT                                                        
013500                                                                          
013600*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
013900*01 -COPY W005WDK7                                                        
013910*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
013920*                                                                         
013930 01  FILLER                      PIC X(16)   VALUE 'W510CURR'.            
013940*01 -COPY W510CURR                                                        
014000                                                                          
014100*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
014200*                                                                         
014300 01  SPAR-AREA.                                                           
014400     03  SPAR-IDTRANS            PIC X(4)    VALUE '2421'.                
014500     03  SPAR-IDAVTAL-ENTER      PIC 9(12).                               
014600     03  SPAR-IDAVTAL-NEXT       PIC 9(12).                               
014700     03  SPAR-IDLEVNR-ENTER      PIC X(5).                                
014800     03  SPAR-IDLEVNR-NEXT       PIC X(5).                                
014900                                                                          
015000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015300*01  MID -COPY W2I42101                                                   
015400                                                                          
015500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015600*01  -COPY WMSGAREA                                                       
015700                                                                          
015800     03  MOD REDEFINES MSG-AREA.                                          
015900*      05  -COPY W2O42101                                                 
016000                                                                          
016100                                                                          
016200                                                                          
016300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016400*01  -COPY WMFSAREA                                                       
016500                                                                          
016600                                                                          
016700                                                                          
016800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016900*                                                                         
017000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017100 01  NYCKLAR-TILL-DLI.                                                    
017200     03  W-IDARTNR-X.                                                     
017300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017400     03  W-IDDC-X.                                                        
017500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017600     03  W-IDLAND-X.                                                      
017700         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
017800     03  W-WDK723KY-X.                                                    
017900         05  W-IDAVTAL           PIC S9(13)   VALUE ZERO COMP-3.          
018000         05  W-IDLEVNR-AVT       PIC  X(5)    VALUE SPACE.                
018100     03  W-IDBEST-X.                                                      
018200         05  W-IDBEST            PIC S9(13)   VALUE ZERO COMP-3.          
018300     03  W-IDLEVNR-X.                                                     
018400         05  W-IDLEVNR           PIC X(5)     VALUE SPACE.                
019300                                                                          
019400*    --- STATUS KODER FRÅN IMS                                            
019500 01  STATUS-WS                   PIC XX.                                  
019600     88  SEGMENT-FINNS                       VALUE '  '.                  
019700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019900                                                                          
020000 01  GODK-STATUSKODER.                                                    
020100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020200                                                                          
020300 01  ALL-SSA.                                                             
020400     03 SSA1                     PIC X(64).                               
020500     03 SSA2                     PIC X(64).                               
020600     03 SSA3                     PIC X(64).                               
020700                                                                          
020800                                                                          
020900                                                                          
021000*    --- IMS FUNKTIONSKODER                                               
021100*01  -COPY W0003                                                          
021200                                                                          
021300*    ---  DLI INPUT-OUTPUT AREA                                           
021400                                                                          
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
021600 01  DLI-IO-WDK601.                                                       
021700*    03  -COPY WDK601                                                     
021800                                                                          
021900                                                                          
022000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
022100 01  DLI-IO-WDK701.                                                       
022200*    03  -COPY WDK701                                                     
022300                                                                          
022400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
022500 01  DLI-IO-WDK711.                                                       
022600*    03  -COPY WDK711                                                     
022700                                                                          
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
022900 01  DLI-IO-WDK712.                                                       
023000*    03  -COPY WDK712                                                     
023100                                                                          
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
023300 01  DLI-IO-WDK722.                                                       
023400*    03  -COPY WDK722                                                     
023500                                                                          
023600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
023700 01  DLI-IO-WDK723.                                                       
023800*    03  -COPY WDK723                                                     
023900                                                                          
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
024100 01  DLI-IO-WDK724.                                                       
024200*    03  -COPY WDK724                                                     
024300                                                                          
024400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK725'.                      
024500 01  DLI-IO-WDK725.                                                       
024600*    03  -COPY WDK725                                                     
024700                                                                          
024800                                                                          
024900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
025000 01  DLI-IO-WDB601.                                                       
025100*    03  -COPY WDB601                                                     
025200                                                                          
025300                                                                          
025400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
025500 01  DLI-IO-WDF101.                                                       
025600*    03  -COPY WDF101                                                     
025700                                                                          
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF102'.                      
025900 01  DLI-IO-WDF102.                                                       
026000*    03  -COPY WDF102                                                     
026100                                                                          
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR301'.                      
026300 01  DLI-IO-WDR301.                                                       
026400*    03  -COPY WDR301                                                     
026500       05   -COPY W1145A  -RED FIL-WDR301-DATA -PRE 5A-                   
027200                                                                          
027300 LINKAGE SECTION.                                                         
027400*01  -COPY W0009  -PRE MSG-                                               
027500*01  -COPY W0008  -PRE USEA-                                              
027600     05  FILLER                  PIC X.                                   
027700                                                                          
027800*01  -COPY W0008  -PRE WDK6-                                              
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100*01  -COPY W0008  -PRE WDK7-                                              
028200     05  FILLER                  PIC X.                                   
028300                                                                          
028400*01  -COPY W0008  -PRE WDK7-2-                                            
028500     05  FILLER                  PIC X.                                   
028600                                                                          
028700*01  -COPY W0008  -PRE WDB6-                                              
028800     05  FILLER                  PIC X.                                   
028900                                                                          
029000*01  -COPY W0008  -PRE WDF1-                                              
029100     05  FILLER                  PIC X.                                   
029200                                                                          
029300*01  -COPY W0008  -PRE WDR3-                                              
029400     05  FILLER                  PIC X.                                   
029500                                                                          
029600*01  -COPY W0008  -PRE 9305-                                              
029700     05  FILLER                  PIC X.                                   
029800                                                                          
029900                                                                          
030000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB                      
030100                                   WDK7-PCB WDK7-2-PCB                    
030200                                   WDB6-PCB WDF1-PCB WDR3-PCB             
030300                                   9305-PCB.                              
030400 MAIN SECTION.                                                            
030500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB                      
030600                                   WDK7-PCB WDK7-2-PCB                    
030700                                   WDB6-PCB WDF1-PCB WDR3-PCB             
030800                                   9305-PCB.                              
030900                                                                          
031000     PERFORM IMS-GET-MSG                                                  
031100     IF SEGMENT-FINNS                                                     
031200        PERFORM A-INIT                                                    
031300        PERFORM B-KOLLA-NYCKLAR                                           
031400        IF NYCKLAR-OK                                                     
031500           IF MFS-UPDATE                                                  
031600              PERFORM G-KOLLA-INPUT                                       
031700              IF INDATA-OK                                                
031800                 PERFORM H-UPPDATERA                                      
031900              END-IF                                                      
032000           ELSE                                                           
032100              IF MFS-FIRST                                                
032200                 PERFORM C-FOERSTA-SIDA                                   
032300              ELSE                                                        
032400                 IF MFS-NEXT                                              
032500                    PERFORM D-NAESTA-SIDA                                 
032600                 ELSE                                                     
032700                    PERFORM E-SAMMA-SIDA                                  
032800                 END-IF                                                   
032900              END-IF                                                      
033000           END-IF                                                         
033100           IF INDATA-OK                                                   
033200              PERFORM F-LAES-VISA-INFO                                    
033300           END-IF                                                         
033400        END-IF                                                            
033500        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O42101 + 4                     
033600        PERFORM IMS-INSERT-MSG                                            
033700     END-IF                                                               
033800                                                                          
033900     MOVE ZERO TO RETURN-CODE                                             
034000     GOBACK                                                               
034100     .                                                                    
034200                                                                          
034300                                                                          
034400 A-INIT SECTION.                                                          
034500                                                                          
034600     MOVE 'A-INIT'          TO CURRENT-SECTION                            
034700                                                                          
034800     IF MSG-DUBBLA-TRANSKODER                                             
034900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I42101                
035000        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
035100        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
035200     ELSE                                                                 
035300        MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I42101                 
035400        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
035500        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
035600     END-IF                                                               
035700                                                                          
035800     MOVE MSG-KDTRTYP       TO MFS-KDTRTYP                                
035900     MOVE MSG-IDPFK         TO MFS-IDPFK                                  
036000     MOVE MFS-IDTRANS       TO W-IDTRANS                                  
036100                                                                          
036200     MOVE LOW-VALUE         TO MSG-AREA                                   
036300     MOVE 'W2O421N1'        TO MFS-IDMOD                                  
036400     MOVE '2421'            TO MOD-IDTRANS                                
036500     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL MOD-TEMFSINF                  
036600                                                                          
036700     IF EGEN-MID OR HELP-MID                                              
036800       CONTINUE                                                           
036900     ELSE                                                                 
037000       MOVE SPACE TO MFS-KDTRTYP                                          
037100       MOVE '7'   TO MFS-IDPFK                                            
037200     END-IF                                                               
037300                                                                          
037400     MOVE 'IDAG  '          TO DAT-KDDATFORM                              
037500     CALL WDATKONV USING DAT-KDDATFORM                                    
037600                         DAT-I-TIDATUM                                    
037700                         DAT-O-TIDATUM                                    
037800                         DAT-KDSVAR                                       
037900                                                                          
038000     MOVE DAT-TIAAMMDD      TO CURRENT-DATE                               
038100     MOVE DAT-TIAAMMDD(1:2) TO WS-AA                                      
038200     MOVE 0101              TO WS-0101                                    
038300     MOVE FUNCTION CURRENT-DATE (3:4) TO W-DATE-AAMM                      
038500     MOVE FUNCTION CURRENT-DATE (3:6) TO DAGENS-AAMMDD                    
038600     .                                                                    
038700                                                                          
038800                                                                          
038900 B-KOLLA-NYCKLAR SECTION.                                                 
039000                                                                          
039100     MOVE 'B-KOLLA-NYCKLAR '  TO CURRENT-SECTION                          
039200                                                                          
039300     MOVE ALL '+'             TO MSGI-WMSGINIT                            
039400     MOVE '001'               TO MSGI-KDCALL                              
039500     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
039600     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
039700     MOVE '2421'              TO MSGI-IDTRANS                             
039800     IF GODK-MID                                                          
039900        MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                             
040000        MOVE MID-IDDC-IN      TO MSGI-IDDC-KEY                            
040100     END-IF                                                               
040200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040300     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
040400                                                                          
040500*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
040600     MOVE 2                   TO MED-IDSKYLT                              
040700                                                                          
040800     MOVE JA                  TO NYCKLAR-SW                               
040900                                 INDATA-SW                                
041000     MOVE SPACE               TO MOD-TEMFSFEL                             
041100                                                                          
041200*    -- KONTROLL AV IDARTNR                                               
041300     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
041400                                                                          
041500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
041600        MOVE '7'             TO MFS-IDPFK                                 
041700        MOVE SPACE           TO MFS-KDTRTYP                               
041800     END-IF                                                               
041900     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
042000     IF MSGI-IDARTNR NUMERIC                                              
042100        MOVE MSGI-IDARTNR    TO W-IDARTNR                                 
042200        PERFORM IMS-GU-WDK601                                             
042300        IF SEGMENT-FINNS                                                  
042400           MOVE '-'          TO MOD-STRECK-1                              
042500           MOVE ART-REKSIFFR TO MOD-REKSIFFR                              
042600        ELSE                                                              
042700           MOVE SPACE        TO MOD-STRECK-1                              
042800                                MOD-REKSIFFR                              
042900           MOVE NEJ          TO NYCKLAR-SW                                
043000           MOVE ERR-ARTIKEL-SAKNAS                                        
043100                             TO MED-IDMFSFEL                              
043200           CALL WMEDKONV USING MED-WMEDAREA                               
043300           MOVE MED-MFSFEL   TO MOD-TEMFSFEL                              
043400        END-IF                                                            
043500     ELSE                                                                 
043600       MOVE NEJ              TO NYCKLAR-SW                                
043700     END-IF                                                               
043800                                                                          
043900*    -- KONTROLL AV IDDC                                                  
044000     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-IN                                
044100                                                                          
044200     IF MID-IDDC-IN NOT = ALL '+'                                         
044300       MOVE '7'             TO MFS-IDPFK                                  
044400       MOVE SPACE           TO MFS-KDTRTYP                                
044500     END-IF                                                               
044600     MOVE MSGI-IDDC-KEY     TO W-IDDC                                     
044700                                                                          
044800     PERFORM IMS-GU-WDB601                                                
044900     IF SEGMENT-FINNS                                                     
045000        MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                               
045100        IF DCS-NDC-CN                                                     
045200        OR DCS-USA                                                        
045300           PERFORM IMS-GU-WDK711                                          
045400           IF SEGMENT-FINNS                                               
045500             MOVE DCS-IDLEVNR-DC                                          
045600                               TO MOD-IDLEVNR-DC                          
045700           ELSE                                                           
045800             MOVE NEJ        TO NYCKLAR-SW                                
045900             MOVE 'PART MISSING ON DC' TO MOD-TEMFSFEL                    
046000           END-IF                                                         
046100        ELSE                                                              
046200           MOVE NEJ          TO NYCKLAR-SW                                
046300           MOVE ERR-DC-INVALID                                            
046400                             TO MED-IDMFSFEL                              
046500           CALL WMEDKONV USING MED-WMEDAREA                               
046600           MOVE MED-MFSFEL   TO MOD-TEMFSFEL                              
046700        END-IF                                                            
046800     ELSE                                                                 
046900        MOVE NEJ             TO NYCKLAR-SW                                
047000        MOVE ERR-DC-SAKNAS                                                
047100                          TO MED-IDMFSFEL                                 
047200        CALL WMEDKONV USING MED-WMEDAREA                                  
047300        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
047400     END-IF                                                               
047500                                                                          
047600     IF MID-IDARTNR-IN NOT = ALL '+'                                      
047700     OR MID-IDDC-IN    NOT = ALL '+'                                      
047800        MOVE ZERO            TO SPAR-IDAVTAL-ENTER                        
047900        MOVE ZERO            TO SPAR-IDAVTAL-NEXT                         
048000        MOVE SPACE           TO SPAR-IDLEVNR-ENTER                        
048100        MOVE SPACE           TO SPAR-IDLEVNR-NEXT                         
048200     END-IF                                                               
048300                                                                          
048400     IF GODK-MID OR NYCKLAR-OK                                            
048500       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
048600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
048700       MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                                
048800     ELSE                                                                 
048900       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
049000       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
049100     END-IF                                                               
049200                                                                          
049300     IF NYCKLAR-FEL                                                       
049400        IF MED-TEMFSFEL = SPACE                                           
049500           MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                           
049600           CALL WMEDKONV USING MED-WMEDAREA                               
049700           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
049800        END-IF                                                            
049900        PERFORM MFS-RENSA-FAELT-IN                                        
050000        PERFORM MFS-RENSA-FAELT-UT                                        
050100     END-IF                                                               
050200     .                                                                    
050300                                                                          
050400                                                                          
050500                                                                          
050600 C-FOERSTA-SIDA SECTION.                                                  
050700                                                                          
050800     MOVE 'C-FOERSTA-SIDA  '  TO CURRENT-SECTION                          
050900                                                                          
051000     MOVE ZERO               TO SPAR-IDAVTAL-ENTER                        
051100     MOVE ZERO               TO SPAR-IDAVTAL-NEXT                         
051200     MOVE SPACE              TO SPAR-IDLEVNR-ENTER                        
051300     MOVE SPACE              TO SPAR-IDLEVNR-NEXT                         
051400                                                                          
051500     MOVE INF-FIRST-PAGE     TO MED-IDMFSINF                              
051600     CALL WMEDKONV USING MED-WMEDAREA                                     
051700     MOVE MED-MFSINF         TO MOD-TEMFSINF                              
051800                                                                          
051900     PERFORM MFS-RENSA-FAELT-IN                                           
052000     .                                                                    
052100                                                                          
052200                                                                          
052300                                                                          
052400 D-NAESTA-SIDA SECTION.                                                   
052500                                                                          
052600     MOVE 'D-NEASTA-SIDA   '   TO CURRENT-SECTION                         
052700                                                                          
052800     IF SPAR-IDTRANS = '2421'                                             
052900        MOVE SPAR-IDAVTAL-NEXT TO W-IDAVTAL                               
053000        MOVE SPAR-IDLEVNR-NEXT TO W-IDLEVNR-AVT                           
053100     ELSE                                                                 
053200        PERFORM MFS-RENSA-FAELT-IN                                        
053300     END-IF                                                               
053400     .                                                                    
053500                                                                          
053600                                                                          
053700                                                                          
053800 E-SAMMA-SIDA SECTION.                                                    
053900                                                                          
054000     MOVE 'E-SAMMA-SIDA    '    TO CURRENT-SECTION                        
054100                                                                          
054200     IF SPAR-IDTRANS = '2421' OR '0551'                                   
054300        MOVE SPAR-IDAVTAL-ENTER TO W-IDAVTAL                              
054400        MOVE SPAR-IDLEVNR-ENTER TO W-IDLEVNR-AVT                          
054500        IF MID-INPUT = ALL '+'                                            
054600           PERFORM MFS-RENSA-FAELT-IN                                     
054700        ELSE                                                              
054800           MOVE NEJ             TO INDATA-SW                              
054900           MOVE INF-PRESS-PF11  TO MED-IDMFSINF                           
055000           CALL WMEDKONV USING MED-WMEDAREA                               
055100           MOVE MED-MFSINF      TO MOD-TEMFSINF                           
055200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
055300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
055400        END-IF                                                            
055500     ELSE                                                                 
055600        PERFORM MFS-RENSA-FAELT-IN                                        
055700     END-IF                                                               
055800     .                                                                    
055900                                                                          
056000                                                                          
056100                                                                          
056200 F-LAES-VISA-INFO SECTION.                                                
056300                                                                          
056400     MOVE 'F-LAES-VISA-INFO'    TO CURRENT-SECTION                        
056500                                                                          
056600     PERFORM FA-VISA-BEST-GRP1                                            
056700     PERFORM FB-VISA-AVTAL-GRP2                                           
056800     PERFORM FC-VISA-PRIS-GRP3                                            
056900                                                                          
057000     PERFORM MFS-RENSA-FAELT-IN                                           
057100     MOVE '002'               TO MSGI-KDCALL                              
057200     MOVE '2421'              TO SPAR-IDTRANS                             
057300     MOVE SPAR-AREA           TO MSGI-SPAR-AREA                           
057400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
057500     .                                                                    
057600                                                                          
057700                                                                          
057800                                                                          
057900 FA-VISA-BEST-GRP1 SECTION.                                               
058000                                                                          
058100     MOVE 'FA-VISA-BEST    '    TO CURRENT-SECTION                        
058200     PERFORM IMS-GU-WDK711                                                
058300     IF SEGMENT-FINNS                                                     
058400        MOVE 1 TO MID-IX                                                  
058500        PERFORM IMS-GNP-WDK725                                            
058600        PERFORM UNTIL SEGMENT-SAKNAS OR MID-IX > MID-IX-MAX               
058700           MOVE NBES-IDBEST     TO MOD-IDBEST(MID-IX)                     
058800           MOVE NBES-IDLEVNR-BEST                                         
058900                                TO MOD-IDLEVNR-BEST(MID-IX)               
059000           MOVE NBES-TIBEST     TO MOD-TIBEST(MID-IX)                     
059100           IF NBES-KDBEH-BEST = 5                                         
059200              MOVE 'CANCELLATION'                                         
059300                                TO MOD-KDBEH(MID-IX)                      
059400           ELSE                                                           
059500              MOVE SPACE        TO MOD-KDBEH(MID-IX)                      
059600           END-IF                                                         
059700           PERFORM IMS-GNP-WDK725                                         
059800           ADD 1 TO MID-IX                                                
059900        END-PERFORM                                                       
060000     END-IF                                                               
060100     .                                                                    
060200                                                                          
060300                                                                          
060400                                                                          
060500 FB-VISA-AVTAL-GRP2 SECTION.                                              
060600                                                                          
060700     MOVE 'FB-VISA-AVTAL   '    TO CURRENT-SECTION                        
060800                                                                          
060900     IF SPAR-IDAVTAL-ENTER = ZERO                                         
061000        MOVE INF-FIRST-PAGE     TO MED-IDMFSINF                           
061100        CALL WMEDKONV USING MED-WMEDAREA                                  
061200        MOVE MED-MFSINF         TO MOD-TEMFSINF                           
061300     END-IF                                                               
061400     PERFORM IMS-GU-WDK711                                                
061500     IF SEGMENT-FINNS                                                     
061600        MOVE 1 TO MID-IX                                                  
061700        PERFORM IMS-GNP-WDK723                                            
061800        PERFORM UNTIL SEGMENT-SAKNAS OR MID-IX > MID-IX-MAX               
061900           MOVE SAVT-IDAVTAL         TO MOD-IDAVTAL(MID-IX)               
062000           MOVE SAVT-IDLEVNR-AVT     TO MOD-IDLEVNR-AVT(MID-IX)           
062100           IF MID-IX = 1                                                  
062200              MOVE SAVT-IDAVTAL      TO SPAR-IDAVTAL-ENTER                
062300              MOVE SAVT-IDLEVNR-SHIP TO SPAR-IDLEVNR-ENTER                
062400           END-IF                                                         
062500           MOVE SAVT-IDLEVNR-SHIP                                         
062600                                     TO MOD-IDLEVNR-SHIP(MID-IX)          
062700           MOVE SAVT-TIAVTAL         TO MOD-TIAVTAL(MID-IX)               
062800                                                                          
062900           PERFORM FBA-KOLLA-KDFPKPRI                                     
063000           MOVE W-KDFPKPRI           TO MOD-KDFPKPRI(MID-IX)              
063100           PERFORM IMS-GNP-WDK723                                         
063200           ADD 1 TO MID-IX                                                
063300        END-PERFORM                                                       
063400     END-IF                                                               
063500*    IF NOT MFS-UPDATE                                                    
063600        IF SEGMENT-FINNS                                                  
063700           MOVE SAVT-IDAVTAL       TO SPAR-IDAVTAL-NEXT                   
063800           MOVE SAVT-IDLEVNR-AVT   TO SPAR-IDLEVNR-NEXT                   
063900           MOVE INF-MORE-INFO-EXISTS                                      
064000                                   TO MED-IDMFSFEL                        
064100           CALL WMEDKONV USING MED-WMEDAREA                               
064200           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
064300        ELSE                                                              
064400           MOVE SPAR-IDAVTAL-ENTER TO SPAR-IDAVTAL-NEXT                   
064500           MOVE SPAR-IDLEVNR-ENTER TO SPAR-IDLEVNR-NEXT                   
064600           MOVE INF-LAST-PAGE      TO MED-IDMFSINF                        
064700           CALL WMEDKONV USING MED-WMEDAREA                               
064800           MOVE MED-MFSINF         TO MOD-TEMFSINF                        
064900        END-IF                                                            
065000*    END-IF                                                               
065100     .                                                                    
065200                                                                          
065300                                                                          
065400                                                                          
065500 FBA-KOLLA-KDFPKPRI       SECTION.                                        
065600                                                                          
065700     MOVE 'FBA-KDFPKPRI    '    TO CURRENT-SECTION                        
065800                                                                          
065900     PERFORM IMS-GU-WDK711-2                                              
066000     IF SEGMENT-FINNS                                                     
066100        PERFORM IMS-GNP-WDK724-2                                          
066200        PERFORM UNTIL SEGMENT-SAKNAS                                      
066300                   OR SPRL-IDLEVNR-PR = SAVT-IDLEVNR-AVT                  
066400           PERFORM IMS-GNP-WDK724-2                                       
066500        END-PERFORM                                                       
066600     END-IF                                                               
066700     IF SEGMENT-FINNS                                                     
066800        MOVE SPRL-KDFPKPRI  TO W-KDFPKPRI                                 
066900     ELSE                                                                 
067000        MOVE SPACE          TO W-KDFPKPRI                                 
067100     END-IF                                                               
067200     .                                                                    
067300                                                                          
067400                                                                          
067500                                                                          
067600 FC-VISA-PRIS-GRP3 SECTION.                                               
067700                                                                          
067800     MOVE 'FC-VISA-PRIS    '     TO CURRENT-SECTION                       
067900                                                                          
068000     MOVE DCS-IDLANDX2           TO W-IDLAND                              
068100     PERFORM IMS-GU-WDK712                                                
068200     MOVE LART-PRMATRL           TO MOD-PRMATRL(1)                        
068300     IF LART-KDMATRPR = 1                                                 
068400        MOVE 'N'                 TO MOD-KDMATRPR(1)                       
068500     ELSE                                                                 
068600        IF LART-KDMATRPR = 2                                              
068700           MOVE 'Y'              TO MOD-KDMATRPR(1)                       
068800        END-IF                                                            
068900     END-IF                                                               
069000     PERFORM IMS-GU-WDK711                                                
069100     IF SEGMENT-FINNS                                                     
069200        MOVE 1 TO MID-IX                                                  
069300        PERFORM IMS-GNP-WDK724                                            
069400        PERFORM UNTIL SEGMENT-SAKNAS OR MID-IX > MID-IX-MAX               
069500          MOVE SPRL-IDLEVNR-PR  TO MOD-IDLEVNR-PR(MID-IX)                 
069600          COMPUTE W-DAPRLIST = 99999999 - SPRL-DAPRLIST-9KOMPL            
069700          MOVE W-DAPRLIST-AAMMDD                                          
069800                                TO MOD-DAPRLIST(MID-IX)                   
069900*         MOVE SPRL-PRARTBES-PR TO MOD-PRARTBES(MID-IX)                   
070000          MOVE SPRL-PRARTBEL-PR TO MOD-PRARTBEL(MID-IX)                   
070100          MOVE DCS-KDVALISO     TO MOD-KDVALISO-PRARTBES(MID-IX)          
070200          MOVE SPRL-PRARTBEL-PR TO W-PRARTBEL                             
070300          MOVE SPRL-KDVALISO    TO MOD-KDVALISO-PRARTBEL(MID-IX)          
070400          IF SPRL-SUINLEV-PR > ZERO                                       
070500             MOVE 'VAL'         TO MOD-STATUS(MID-IX)                     
070600          ELSE                                                            
070700             MOVE 'APPR'        TO MOD-STATUS(MID-IX)                     
070800          END-IF                                                          
070900**** RÄKNA OM PRISRADER MED AKTUELL MÅNADSKURS                            
071000          MOVE DCS-KDVALISO     TO CURR-KDVALISO-HUV                      
071100          MOVE SPRL-KDVALISO    TO WS-KDVALISO                            
071200          IF CURR-KDVALISO-HUV = WS-KDVALISO                              
071300            MOVE 1              TO W-PRKURS                               
071400            MOVE 1              TO W-REVALUTA                             
071500          ELSE                                                            
071600            MOVE WS-KDVALISO    TO CURR-KDVALISO-ROW                      
072610            MOVE 'M'            TO CURR-KDVALTYP                          
072611            MOVE W-DATE-AAMM    TO CURR-TIAAMM                            
072620            CALL W510CURR USING CURR-W510CURR 9305-PCB                    
072630            IF CURR-KDSVAR = ' '                                          
072640              MOVE CURR-PRKURS-NEW     TO W-PRKURS                        
072650              MOVE CURR-REVALUTA-TO    TO W-REVALUTA                      
072660            ELSE                                                          
072670              MOVE 1             TO W-PRKURS                              
072680              MOVE 1             TO W-REVALUTA                            
072690            END-IF                                                        
072700          END-IF                                                          
072800          MOVE SPRL-IDLEVNR-PR  TO W-IDLEVNR                              
072900          PERFORM IMS-GU-WDF101                                           
073000          IF SEGMENT-SAKNAS                                               
073100            MOVE 1 TO W-RETULF                                            
073200          ELSE                                                            
073300            MOVE DCS-IDLANDX2   TO W-IDLAND                               
073400            PERFORM IMS-GNP-WDF102                                        
073500            IF SEGMENT-FINNS                                              
073600              IF TULL-TITULF < DAGENS-AAMMDD                              
073700                MOVE TULL-RETULF-1 TO W-RETULF                            
073800              ELSE                                                        
073900                MOVE TULL-RETULF-2 TO W-RETULF                            
074000              END-IF                                                      
074100            ELSE                                                          
074200              MOVE 1 TO W-RETULF                                          
074300            END-IF                                                        
074400          END-IF                                                          
074500          COMPUTE WS-PRARTBES-PR ROUNDED =                                
074600                  W-PRARTBEL * W-RETULF                                   
074700                * W-PRKURS / W-REVALUTA                                   
074800          MOVE WS-PRARTBES-PR TO MOD-PRARTBES (MID-IX)                    
074900****                                                                      
075000          PERFORM IMS-GNP-WDK724                                          
075100          ADD 1 TO MID-IX                                                 
075200        END-PERFORM                                                       
075300     END-IF                                                               
075400     .                                                                    
075500                                                                          
075600                                                                          
075700                                                                          
075800 G-KOLLA-INPUT SECTION.                                                   
075900                                                                          
076000     MOVE 'G-KOLLA-INPUT   '    TO CURRENT-SECTION                        
076100                                                                          
076200     MOVE JA  TO INDATA-SW                                                
076300     IF MID-INPUT = ALL '+' OR SPACE                                      
076400        MOVE ERR-PF11-AND-NO-DATA    TO MED-IDMFSFEL                      
076500        CALL WMEDKONV USING MED-WMEDAREA                                  
076600        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
076700        PERFORM MFS-ROER-EJ-FAELT-IN                                      
076800        PERFORM MFS-ROER-EJ-FAELT-UT                                      
076900        MOVE NEJ                     TO INDATA-SW                         
077000     ELSE                                                                 
077100        PERFORM GH-AUTH-USER-CHECK                                        
077200        IF INDATA-OK                                                      
077300           PERFORM GA-KOLLA-INDATA-KOMBINATION                            
077400           IF INDATA-OK                                                   
077500              IF MID-IDAVTAL-IN  NOT = ALL '+'                            
077600              OR MID-IDLEVNR-AVT-IN NOT = ALL '+'                         
077700              OR MID-IDLEVNR-SHIP-IN NOT = ALL '+'                        
077800              OR MID-TIAVTAL-IN  NOT = ALL '+'                            
077900              OR MID-KDCMD-NC-IN NOT = ALL '+'                            
078000                                                                          
078100                 PERFORM GB-KOLLA-AVTAL                                   
078200                 PERFORM GC-KOLLA-LEVNR-AVT                               
078300                 PERFORM GD-KOLLA-LEVNR-SHIP                              
078400                 PERFORM GE-KOLLA-TIAVTAL                                 
078500                 PERFORM GF-KOLLA-KDCMD-NC                                
078600              ELSE                                                        
078700                 PERFORM GG-KOLLA-KDCMD-C                                 
078800              END-IF                                                      
078900           END-IF                                                         
079000                                                                          
079100           IF INDATA-FEL                                                  
079200              IF MED-IDMFSFEL NOT = ZERO                                  
079300                 IF WS-FLPG = JA                                          
079400                   MOVE ERR-LOCAL-PG         TO MED-IDMFSFEL              
079500                 ELSE                                                     
079600                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
079700                 END-IF                                                   
079800                 CALL WMEDKONV USING MED-WMEDAREA                         
079900                 MOVE MED-MFSFEL         TO MOD-TEMFSFEL                  
080000              END-IF                                                      
080100              PERFORM MFS-ROER-EJ-FAELT-UT                                
080200              PERFORM MFS-ROER-EJ-FAELT-IN                                
080300              IF MID-KDCMD-NC-IN = '+' OR SPACE                           
080400                 IF MID-IDAVTAL-IN NOT = ALL '+'                          
080500                 OR MID-IDLEVNR-AVT-IN NOT = ALL '+'                      
080600                 OR MID-IDLEVNR-SHIP-IN NOT = ALL '+'                     
080700                 OR MID-TIAVTAL-IN NOT = ALL '+'                          
080800                    MOVE '?'             TO MOD-KDCMD-NC-IN               
080900                 END-IF                                                   
081000              END-IF                                                      
081100           END-IF                                                         
081200        ELSE                                                              
081300           MOVE ERR-NOT-AUTHORIZED   TO MED-IDMFSFEL                      
081400           CALL WMEDKONV USING MED-WMEDAREA                               
081500           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
081600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
081700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
081800        END-IF                                                            
081900     END-IF                                                               
082000     .                                                                    
082100                                                                          
082200                                                                          
082300                                                                          
082400 GA-KOLLA-INDATA-KOMBINATION SECTION.                                     
082500                                                                          
082600     MOVE 'GA-KOLLA-KOMBINA'    TO CURRENT-SECTION                        
082700                                                                          
082800     IF (MID-IDAVTAL-IN  NOT = ALL '+'                                    
082900      OR MID-IDLEVNR-AVT-IN NOT = ALL '+'                                 
083000      OR MID-IDLEVNR-SHIP-IN NOT = ALL '+'                                
083100      OR MID-TIAVTAL-IN  NOT = ALL '+'                                    
083200      OR MID-KDCMD-NC-IN NOT = ALL '+')                                   
083300     AND MID-KDCMD-C-IN NOT = '+'                                         
083400                                                                          
083500        MOVE NEJ TO INDATA-SW                                             
083600     END-IF                                                               
083700     .                                                                    
083800                                                                          
083900                                                                          
084000                                                                          
084100 GB-KOLLA-AVTAL SECTION.                                                  
084200                                                                          
084300     MOVE 'GB-KOLLA-AVTAL  '    TO CURRENT-SECTION                        
084400                                                                          
084500     MOVE NEJ TO AVTAL-SW                                                 
084600     IF MID-IDAVTAL-IN = ALL '+'                                          
084700        MOVE MFS-NUM-FAELT-FEL    TO MOD-IDAVTAL-IN-ATTR                  
084800        MOVE NEJ TO INDATA-SW                                             
084900     ELSE                                                                 
085000        IF MID-IDAVTAL-IN NOT NUMERIC                                     
085100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDAVTAL-IN-ATTR                  
085200           MOVE NEJ TO INDATA-SW                                          
085300        ELSE                                                              
085400           MOVE MID-IDAVTAL-IN    TO W-IDAVTAL                            
085500           MOVE MFS-NUM-FAELT-RAETT                                       
085600                                  TO MOD-IDAVTAL-IN-ATTR                  
085700           MOVE JA                TO AVTAL-SW                             
085800        END-IF                                                            
085900     END-IF                                                               
086000     .                                                                    
086100                                                                          
086200                                                                          
086300                                                                          
086400 GC-KOLLA-LEVNR-AVT SECTION.                                              
086500                                                                          
086600     MOVE 'GC-KOLLA-LEVNR-A'    TO CURRENT-SECTION                        
086700                                                                          
086800     MOVE NEJ TO LEVNR-SW                                                 
086900     IF MID-IDLEVNR-AVT-IN = ALL '+' OR SPACE                             
087000        MOVE NEJ TO INDATA-SW                                             
087100        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-AVT-IN-ATTR              
087200     ELSE                                                                 
087300        MOVE MID-IDLEVNR-AVT-IN   TO W-IDLEVNR                            
087400        PERFORM IMS-GU-WDF101                                             
087500        IF SEGMENT-FINNS                                                  
087600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-AVT-IN-ATTR           
087700        ELSE                                                              
087800           MOVE NEJ TO INDATA-SW                                          
087900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-AVT-IN-ATTR             
088000        END-IF                                                            
088100     END-IF                                                               
088200     .                                                                    
088300                                                                          
088400                                                                          
088500                                                                          
088600 GD-KOLLA-LEVNR-SHIP SECTION.                                             
088700                                                                          
088800     MOVE 'GD-KOLLA-LEVNR-S'    TO CURRENT-SECTION                        
088900                                                                          
089000     IF MID-IDLEVNR-SHIP-IN = ALL '+' OR SPACE                            
089100        MOVE NEJ TO INDATA-SW                                             
089200        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-SHIP-IN-ATTR             
089300     ELSE                                                                 
089400        MOVE MID-IDLEVNR-SHIP-IN  TO W-IDLEVNR                            
089500        PERFORM IMS-GU-WDF101                                             
089600        IF SEGMENT-FINNS                                                  
089700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-SHIP-IN-ATTR          
089800        ELSE                                                              
089900           MOVE NEJ TO INDATA-SW                                          
090000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-SHIP-IN-ATTR            
090100        END-IF                                                            
090200     END-IF                                                               
090300     .                                                                    
090400                                                                          
090500                                                                          
090600                                                                          
090700 GE-KOLLA-TIAVTAL SECTION.                                                
090800                                                                          
090900     MOVE 'GE-KOLLA-TIAVTAL'    TO CURRENT-SECTION                        
091000                                                                          
091100     IF MID-TIAVTAL-IN = ALL '+'                                          
091200        MOVE MFS-NUM-FAELT-FEL    TO MOD-TIAVTAL-IN-ATTR                  
091300        MOVE NEJ TO INDATA-SW                                             
091400     ELSE                                                                 
091500        IF MID-TIAVTAL-IN NOT NUMERIC                                     
091600           MOVE MFS-NUM-FAELT-FEL TO MOD-TIAVTAL-IN-ATTR                  
091700           MOVE NEJ TO INDATA-SW                                          
091800        ELSE                                                              
091900           MOVE 'AAMMDD'          TO DAT-KDDATFORM                        
092000           MOVE MID-TIAVTAL-IN    TO DAT-I-TIDATUM                        
092100           CALL WDATKONV USING DAT-KDDATFORM                              
092200                               DAT-I-TIDATUM                              
092300                               DAT-O-TIDATUM                              
092400                               DAT-KDSVAR                                 
092500           IF  DAT-KDSVAR-OK                                              
092600           AND DAT-O-TIDATUM NOT = CURRENT-DATE                           
092700              MOVE MFS-NUM-FAELT-RAETT                                    
092800                                  TO MOD-TIAVTAL-IN-ATTR                  
092900           ELSE                                                           
093000              MOVE MFS-NUM-FAELT-FEL                                      
093100                                  TO MOD-TIAVTAL-IN-ATTR                  
093200              MOVE NEJ TO INDATA-SW                                       
093300           END-IF                                                         
093400        END-IF                                                            
093500     END-IF                                                               
093600     .                                                                    
093700                                                                          
093800                                                                          
093900                                                                          
094000 GF-KOLLA-KDCMD-NC SECTION.                                               
094100                                                                          
094200     MOVE 'GF-KOLLA-KDCMDNC'    TO CURRENT-SECTION                        
094300                                                                          
094400     IF MID-KDCMD-NC-IN = 'N' OR 'C'                                      
094500        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDCMD-NC-IN-ATTR                
094600     ELSE                                                                 
094700        MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDCMD-NC-IN-ATTR                
094800        MOVE NEJ                   TO INDATA-SW                           
094900     END-IF                                                               
095000                                                                          
095100     IF  MID-KDCMD-NC-IN = 'C'                                            
095200     AND AVTAL-OK                                                         
095300     AND LEVNR-OK                                                         
095400        MOVE MID-IDLEVNR-AVT-IN    TO W-IDLEVNR-AVT                       
095500        PERFORM IMS-GU-WDK723                                             
095600        IF SEGMENT-SAKNAS                                                 
095700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-NC-IN-ATTR                
095800           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDAVTAL-IN-ATTR                 
095900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-AVT-IN-ATTR             
096000           MOVE NEJ                TO INDATA-SW                           
096100        END-IF                                                            
096200     END-IF                                                               
096300     .                                                                    
096400                                                                          
096500                                                                          
096600                                                                          
096700 GG-KOLLA-KDCMD-C SECTION.                                                
096800                                                                          
096900     MOVE 'GG-KOLLA-KDCMD-C'    TO CURRENT-SECTION                        
097000                                                                          
097100     MOVE NEJ TO WS-FLPG                                                  
097200     IF MID-KDCMD-C-IN = ALL '+' OR SPACE                                 
097300        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDCMD-C-IN-ATTR                 
097400        MOVE SPACE                 TO MID-KDCMD-C-IN                      
097500     ELSE                                                                 
097600        IF   MID-KDCMD-C-IN = 'Y'                                         
097700           MOVE ART-KDPRODSL       TO TEST-KDPRODSL                       
097800           IF KDPRODSL-LOCAL                                              
097900* LOKALT PRODUKTSLAG                                                      
098000              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-C-IN-ATTR              
098100              MOVE NEJ                TO INDATA-SW                        
098200              MOVE JA                 TO WS-FLPG                          
098300           ELSE                                                           
098400              MOVE MFS-ALFA-FAELT-RAETT                                   
098500                                   TO MOD-KDCMD-C-IN-ATTR                 
098600           END-IF                                                         
098700        ELSE                                                              
098800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-C-IN-ATTR                 
098900           MOVE NEJ                TO INDATA-SW                           
099000        END-IF                                                            
099100     END-IF                                                               
099200     .                                                                    
099300                                                                          
099400 GH-AUTH-USER-CHECK SECTION.                                              
099500     MOVE 'GH-AUTH-USER-CHECK'  TO CURRENT-SECTION                        
099600                                                                          
099700     PERFORM IMS-GU-WDB601                                                
099800     IF SEGMENT-FINNS                                                     
099900       IF  DCS-NDC-CN                                                     
100000       OR  DCS-USA                                                        
100100         MOVE MSGI-IDFTG               TO WS-IDFTG                        
100200         IF (DCS-NDC-CN AND IDFTG-CN)                                     
100300         OR (DCS-NDC-NA AND IDFTG-US)                                     
100400         OR MSGI-IDFTG  = WC-IDFTG-PV                                     
100500            CONTINUE                                                      
100600         ELSE                                                             
100700            MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDCMD-NC-IN-ATTR            
100800            MOVE ERR-NOT-AUTHORIZED    TO MED-IDMFSFEL                    
100900            MOVE NEJ                   TO INDATA-SW                       
101000         END-IF                                                           
101100       ELSE                                                               
101200         MOVE MFS-ALPHA-FIELD-WRONG    TO MOD-KDCMD-NC-IN-ATTR            
101300         MOVE ERR-NOT-AUTHORIZED       TO MED-IDMFSFEL                    
101400         MOVE NEJ                      TO INDATA-SW                       
101500       END-IF                                                             
101600     ELSE                                                                 
101700       MOVE ERR-NOT-AUTHORIZED         TO MED-IDMFSFEL                    
101800       MOVE MFS-ALPHA-FIELD-WRONG      TO MOD-KDCMD-NC-IN-ATTR            
101900       MOVE NEJ                        TO INDATA-SW                       
102000     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102300                                                                          
102400                                                                          
102500 H-UPPDATERA SECTION.                                                     
102600                                                                          
102700     MOVE 'H-UPPDATERA     '    TO CURRENT-SECTION                        
102800                                                                          
102900     IF MID-KDCMD-NC-IN NOT = '+'                                         
103000     OR MID-KDCMD-C-IN  NOT = '+'                                         
103100        IF MID-KDCMD-NC-IN = 'N'                                          
103200           PERFORM HA-NYTT-AVTAL                                          
103300        ELSE                                                              
103400           IF MID-KDCMD-NC-IN = 'C'                                       
103500              PERFORM HB-DELETE-AVTAL                                     
103600           ELSE                                                           
103700              IF MID-KDCMD-C-IN = 'Y'                                     
103800                 PERFORM HC-CANCEL-AVTAL                                  
103900              END-IF                                                      
104000           END-IF                                                         
104100        END-IF                                                            
104200                                                                          
104300        MOVE ZERO             TO W-IDAVTAL                                
104400        MOVE SPACE            TO W-IDLEVNR-AVT                            
104500                                                                          
104600        MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                             
104700        CALL WMEDKONV USING MED-WMEDAREA                                  
104800        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
104900     END-IF                                                               
105000     .                                                                    
105100                                                                          
105200                                                                          
105300                                                                          
105400 HA-NYTT-AVTAL     SECTION.                                               
105500                                                                          
105600     MOVE 'HA-NYTT-AVTAL   ' TO CURRENT-SECTION                           
105700                                                                          
105800     MOVE MID-IDAVTAL-IN     TO W-IDAVTAL                                 
105900     MOVE MID-IDLEVNR-AVT-IN TO W-IDLEVNR-AVT                             
106000     PERFORM IMS-GHU-WDK723                                               
106100                                                                          
106200     MOVE MID-IDAVTAL-IN     TO SAVT-IDAVTAL                              
106300     MOVE MID-IDLEVNR-AVT-IN TO SAVT-IDLEVNR-AVT                          
106400     MOVE MID-IDLEVNR-SHIP-IN TO SAVT-IDLEVNR-SHIP                        
106500     MOVE MID-TIAVTAL-IN     TO SAVT-TIAVTAL                              
106600     IF SEGMENT-FINNS                                                     
106700        PERFORM IMS-REPL-WDK723                                           
106800     ELSE                                                                 
106900        MOVE ALL '+'         TO WDK7-W005WDK7                             
107000        MOVE 'WDK723'        TO WDK7-IDSEGM                               
107100        MOVE W-IDARTNR       TO WDK7-IDARTNR-KFB                          
107200        MOVE W-IDDC          TO WDK7-IDDC-KFB                             
107300                                WDK7-IDDC                                 
107400        MOVE SAVT-WDK723     TO WDK7-WDK723                               
107500        CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                        
107600                            WDK6-PCB WDK7-PCB                             
107700        PERFORM IMS-GHU-WDK722                                            
107800        IF SEGMENT-FINNS                                                  
107900          MOVE +1         TO XLAG-KDAVT                                   
108000          PERFORM IMS-REPL-WDK722                                         
108100        END-IF                                                            
108200     END-IF                                                               
108300     IF DCS-NDC-CN                                                        
108400       PERFORM HAA-UPDATE-TULLFAKTOR                                      
108500     END-IF                                                               
108600     IF DCS-USA                                                           
108700       PERFORM HAB-UPDATE-TULLFAKTOR-USA                                  
108800     END-IF                                                               
108900     .                                                                    
109000                                                                          
109100 HAA-UPDATE-TULLFAKTOR SECTION.                                           
109200     MOVE 'HAA-UPDATE-TULLFAKTOR ' TO CURRENT-SECTION                     
109300                                                                          
109400     MOVE W-IDLEVNR-AVT  TO W-IDLEVNR                                     
109500     PERFORM IMS-GU-WDF101                                                
109600     IF SEGMENT-FINNS                                                     
109700       MOVE DCS-IDLANDX2  TO W-IDLAND                                     
109800       PERFORM IMS-GNP-WDF102                                             
109900       IF SEGMENT-FINNS                                                   
110000         CONTINUE                                                         
110100       ELSE                                                               
110200         MOVE 171                  TO TULL-KDVALLEV                       
110300         MOVE WS-AA0101            TO TULL-TITULF                         
110400         MOVE 1.0300               TO TULL-RETULF-1                       
110500         MOVE 1.0300               TO TULL-RETULF-2                       
110600         MOVE 'CN'                 TO TULL-IDLANDX2                       
110700                                                                          
110800         PERFORM IMS-ISRT-WDF102                                          
110900       END-IF                                                             
111000     END-IF                                                               
111100     .                                                                    
111200                                                                          
111300                                                                          
111400                                                                          
111500 HAB-UPDATE-TULLFAKTOR-USA SECTION.                                       
111600     MOVE 'HAB-UPDATE-TULLFAKTOR-USA '  TO CURRENT-SECTION                
111700                                                                          
111800     MOVE W-IDLEVNR-AVT  TO W-IDLEVNR                                     
111900     PERFORM IMS-GU-WDF101                                                
112000     IF SEGMENT-FINNS                                                     
112100       MOVE DCS-IDLANDX2  TO W-IDLAND                                     
112200       PERFORM IMS-GNP-WDF102                                             
112300       IF SEGMENT-FINNS                                                   
112400         CONTINUE                                                         
112500       ELSE                                                               
112600         MOVE 121                  TO TULL-KDVALLEV                       
112700         MOVE WS-AA0101            TO TULL-TITULF                         
112800         MOVE 1.0500               TO TULL-RETULF-1                       
112900         MOVE 1.0500               TO TULL-RETULF-2                       
113000         MOVE 'US'                 TO TULL-IDLANDX2                       
113100                                                                          
113200         PERFORM IMS-ISRT-WDF102                                          
113300       END-IF                                                             
113400     END-IF                                                               
113500     .                                                                    
113600                                                                          
113700                                                                          
113800                                                                          
113900 HB-DELETE-AVTAL   SECTION.                                               
114000                                                                          
114100     MOVE 'HB-DELETE-AVTAL '    TO CURRENT-SECTION                        
114200                                                                          
114300     MOVE MID-IDAVTAL-IN     TO NBES-IDBEST                               
114400                                W-IDAVTAL                                 
114500     MOVE MID-IDLEVNR-AVT-IN TO NBES-IDLEVNR-BEST                         
114600                                W-IDLEVNR-AVT                             
114700     MOVE 5                  TO NBES-KDBEH-BEST                           
114800     MOVE CURRENT-DATE       TO NBES-TIBEST                               
114900                                                                          
115000                                                                          
115100     PERFORM IMS-GHU-WDK723                                               
115200     IF SEGMENT-FINNS                                                     
115300       PERFORM IMS-DLET-WDK723                                            
115400       MOVE ALL '+'          TO WDK7-W005WDK7                             
115500       MOVE 'WDK725'         TO WDK7-IDSEGM                               
115600       MOVE W-IDARTNR        TO WDK7-IDARTNR-KFB                          
115700       MOVE W-IDDC           TO WDK7-IDDC-KFB                             
115800                                  WDK7-IDDC                               
115900       MOVE NBES-WDK725      TO WDK7-WDK725                               
116000       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                         
116100                           WDK6-PCB WDK7-PCB                              
116200     END-IF                                                               
116300                                                                          
116400     PERFORM IMS-GU-WDK723-OKVAL                                          
116500     IF SEGMENT-SAKNAS                                                    
116600        PERFORM IMS-GHU-WDK722                                            
116700        IF SEGMENT-FINNS                                                  
116800           MOVE ZERO       TO XLAG-KDAVT                                  
116900           PERFORM IMS-REPL-WDK722                                        
117000        END-IF                                                            
117100     END-IF                                                               
117200     .                                                                    
117300                                                                          
117400 HC-CANCEL-AVTAL   SECTION.                                               
117500                                                                          
117600     MOVE 'HC-CANCEL-AVTAL '    TO CURRENT-SECTION                        
117700                                                                          
117800     PERFORM IMS-GU-WDK711                                                
117900     IF SEGMENT-FINNS                                                     
118000        PERFORM IMS-GNP-WDK723-OKVAL                                      
118100        PERFORM UNTIL SEGMENT-SAKNAS                                      
118200           MOVE SAVT-IDAVTAL      TO NBES-IDBEST                          
118300           MOVE SAVT-IDLEVNR-AVT  TO NBES-IDLEVNR-BEST                    
118400           MOVE 5                 TO NBES-KDBEH-BEST                      
118500           MOVE SAVT-TIAVTAL      TO NBES-TIBEST                          
118600                                                                          
118700           MOVE ALL '+'           TO WDK7-W005WDK7                        
118800           MOVE 'WDK725'          TO WDK7-IDSEGM                          
118900           MOVE W-IDARTNR         TO WDK7-IDARTNR-KFB                     
119000           MOVE W-IDDC            TO WDK7-IDDC-KFB                        
119100                                     WDK7-IDDC                            
119200           MOVE NBES-WDK725       TO WDK7-WDK725                          
119300           CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB                     
119400                               WDK6-PCB WDK7-2-PCB                        
119500           PERFORM IMS-GNP-WDK723-OKVAL                                   
119600        END-PERFORM                                                       
119700     END-IF                                                               
119800                                                                          
119900     PERFORM HCA-CANCELLATION-RQUEST                                      
120000     .                                                                    
120100                                                                          
120200                                                                          
120300                                                                          
120400 HCA-CANCELLATION-RQUEST SECTION.                                         
120500                                                                          
120600     MOVE 'HCA-CANCEL-REQ  '    TO CURRENT-SECTION                        
120700                                                                          
120800     MOVE IDPGM             TO FIL-IDPGM                                  
120900     ACCEPT FIL-TIREGDAT    FROM DATE                                     
121000     ACCEPT FIL-TIKLOCK     FROM TIME                                     
121100     MOVE 1                 TO FIL-IDSEKVNR                               
121200     MOVE 'W114'            TO FIL-CT-IDSYSTEM                            
121300     MOVE 'A'               TO FIL-CT-IDVTYP                              
121400     MOVE 'H02'             TO FIL-CT-IDPTYP                              
121500     MOVE W-IDDC            TO 5A-IDDC                                    
121600     MOVE W-IDARTNR         TO 5A-IDARTNR                                 
121700     PERFORM IMS-GU-WDK722                                                
121800     IF SEGMENT-FINNS                                                     
121900       MOVE XLAG-IDANSK     TO 5A-IDANSK                                  
122000     ELSE                                                                 
122100       MOVE ZERO            TO 5A-IDANSK                                  
122200     END-IF                                                               
122300                                                                          
122400     PERFORM IMS-ISRT-WDR301                                              
122500     IF SEGMENT-FINNS-REDAN                                               
122600        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
122700           ADD 1 TO FIL-IDSEKVNR                                          
122800           PERFORM IMS-ISRT-WDR301                                        
122900        END-PERFORM                                                       
123000     END-IF                                                               
123100     .                                                                    
123200                                                                          
123300 MFS-RENSA-FAELT-UT SECTION.                                              
123400                                                                          
123500     MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-DC                            
123600     MOVE 1 TO MID-IX                                                     
123700     PERFORM UNTIL MID-IX > MID-IX-MAX                                    
123800        MOVE MFS-RENSA-FAELT TO MOD-IDBEST(MID-IX)                        
123900                                MOD-IDLEVNR-BEST(MID-IX)                  
124000                                MOD-TIBEST(MID-IX)                        
124100                                MOD-KDBEH(MID-IX)                         
124200         MOD-IDAVTAL(MID-IX)                                              
124300                                MOD-IDLEVNR-AVT(MID-IX)                   
124400                                MOD-IDLEVNR-SHIP(MID-IX)                  
124500                                MOD-TIAVTAL(MID-IX)                       
124600                                MOD-KDFPKPRI(MID-IX)                      
124700                                MOD-IDLEVNR-PR(MID-IX)                    
124800                                MOD-DAPRLIST(MID-IX)                      
124900                                MOD-PRARTBES(MID-IX)                      
125000                                MOD-KDVALISO-PRARTBES(MID-IX)             
125100                                MOD-PRARTBEL(MID-IX)                      
125200                                MOD-KDVALISO-PRARTBEL(MID-IX)             
125300                                MOD-STATUS(MID-IX)                        
125400                                MOD-PRMATRL(MID-IX)                       
125500                                MOD-KDMATRPR(MID-IX)                      
125600        ADD 1 TO MID-IX                                                   
125700     END-PERFORM                                                          
125800                                                                          
125900     MOVE ZERO               TO SPAR-IDAVTAL-ENTER                        
126000     MOVE ZERO               TO SPAR-IDAVTAL-NEXT                         
126100     MOVE SPACE              TO SPAR-IDLEVNR-ENTER                        
126200     MOVE SPACE              TO SPAR-IDLEVNR-NEXT                         
126300     .                                                                    
126400                                                                          
126500                                                                          
126600 MFS-RENSA-FAELT-IN SECTION.                                              
126700                                                                          
126800     MOVE MFS-RENSA-FAELT TO MOD-IDAVTAL-IN                               
126900                             MOD-IDLEVNR-AVT-IN                           
127000                             MOD-IDLEVNR-SHIP-IN                          
127100                             MOD-TIAVTAL-IN                               
127200                             MOD-KDCMD-NC-IN                              
127300                             MOD-KDCMD-C-IN                               
127400     .                                                                    
127500                                                                          
127600                                                                          
127700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
127800                                                                          
127900     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDLEVNR-DC                          
128000     MOVE 1 TO MID-IX                                                     
128100     PERFORM UNTIL MID-IX > MID-IX-MAX                                    
128200        MOVE MFS-ROER-EJ-FAELT TO MOD-IDBEST(MID-IX)                      
128300                                  MOD-IDLEVNR-BEST(MID-IX)                
128400                                  MOD-TIBEST(MID-IX)                      
128500                                  MOD-KDBEH(MID-IX)                       
128600                                  MOD-IDAVTAL(MID-IX)                     
128700                                  MOD-IDLEVNR-AVT(MID-IX)                 
128800                                  MOD-IDLEVNR-SHIP(MID-IX)                
128900                                  MOD-TIAVTAL(MID-IX)                     
129000                                  MOD-KDFPKPRI(MID-IX)                    
129100                                  MOD-IDLEVNR-PR(MID-IX)                  
129200                                  MOD-DAPRLIST(MID-IX)                    
129300                                  MOD-PRARTBES(MID-IX)                    
129400                                  MOD-KDVALISO-PRARTBES(MID-IX)           
129500                                  MOD-PRARTBEL(MID-IX)                    
129600                                  MOD-KDVALISO-PRARTBEL(MID-IX)           
129700                                  MOD-STATUS(MID-IX)                      
129800                                  MOD-PRMATRL(MID-IX)                     
129900                                  MOD-KDMATRPR(MID-IX)                    
130000        ADD 1 TO MID-IX                                                   
130100     END-PERFORM                                                          
130200     .                                                                    
130300                                                                          
130400                                                                          
130500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
130600                                                                          
130700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDAVTAL-IN                             
130800                               MOD-IDLEVNR-AVT-IN                         
130900                               MOD-IDLEVNR-SHIP-IN                        
131000                               MOD-TIAVTAL-IN                             
131100                               MOD-KDCMD-NC-IN                            
131200                               MOD-KDCMD-C-IN                             
131300     .                                                                    
131400                                                                          
131500                                                                          
131600 MFS-FORM-ATTR SECTION.                                                   
131700                                                                          
131800     MOVE MFS-FORMATETS-ATTR TO MOD-IDAVTAL-IN-ATTR                       
131900                                MOD-IDLEVNR-AVT-IN-ATTR                   
132000                                MOD-IDLEVNR-SHIP-IN-ATTR                  
132100                                MOD-TIAVTAL-IN-ATTR                       
132200                                MOD-KDCMD-NC-IN-ATTR                      
132300                                MOD-KDCMD-C-IN-ATTR                       
132400     .                                                                    
132500                                                                          
132600                                                                          
132700 MFS-LAES-IN-IGEN SECTION.                                                
132800                                                                          
132900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDAVTAL-IN-ATTR                    
133000                                   MOD-IDLEVNR-AVT-IN-ATTR                
133100                                   MOD-IDLEVNR-SHIP-IN-ATTR               
133200                                   MOD-TIAVTAL-IN-ATTR                    
133300                                   MOD-KDCMD-NC-IN-ATTR                   
133400                                   MOD-KDCMD-C-IN-ATTR                    
133500     .                                                                    
133600                                                                          
133700                                                                          
133800                                                                          
133900* --- IMS SEKTIONER ---                                                   
134000                                                                          
134100 IMS-GET-MSG SECTION.                                                     
134200                                                                          
134300     MOVE '  QC'          TO GODK-STATUSKODER                             
134400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
134500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134600     PERFORM IMS-STATUSKONTROLL                                           
134700     .                                                                    
134800                                                                          
134900                                                                          
135000 IMS-INSERT-MSG SECTION.                                                  
135100                                                                          
135200     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
135300     MOVE SPACE           TO GODK-STATUSKODER                             
135400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
135500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135600     PERFORM IMS-STATUSKONTROLL                                           
135700     .                                                                    
135800                                                                          
135900                                                                          
136000 IMS-GU-WDK601 SECTION.                                                   
136100                                                                          
136200     MOVE 'IMS-GU-WDK601   '  TO CURRENT-IMS-SECTION                      
136300                                                                          
136400     MOVE SPACE               TO ALL-SSA                                  
136500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
136600          DELIMITED BY SIZE INTO SSA1                                     
136700     MOVE '  GE'              TO GODK-STATUSKODER                         
136800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
136900     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
137000     PERFORM IMS-STATUSKONTROLL                                           
137100     .                                                                    
137200                                                                          
137300                                                                          
137400                                                                          
137500 IMS-GU-WDK701 SECTION.                                                   
137600                                                                          
137700     MOVE 'IMS-GU-WDK701   '  TO CURRENT-IMS-SECTION                      
137800                                                                          
137900     MOVE SPACE               TO ALL-SSA                                  
138000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
138100          DELIMITED BY SIZE INTO SSA1                                     
138200     MOVE '  GE'              TO GODK-STATUSKODER                         
138300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
138400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
138500     PERFORM IMS-STATUSKONTROLL                                           
138600     .                                                                    
138700                                                                          
138800                                                                          
138900                                                                          
139000 IMS-GU-WDK711 SECTION.                                                   
139100                                                                          
139200     MOVE 'IMS-GU-WDK711   '  TO CURRENT-IMS-SECTION                      
139300                                                                          
139400     MOVE SPACE               TO ALL-SSA                                  
139500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
139600          DELIMITED BY SIZE INTO SSA1                                     
139700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
139800          DELIMITED BY SIZE INTO SSA2                                     
139900     MOVE '  GE'              TO GODK-STATUSKODER                         
140000     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
140100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
140200     PERFORM IMS-STATUSKONTROLL                                           
140300     .                                                                    
140400                                                                          
140500                                                                          
140600 IMS-GU-WDK711-2 SECTION.                                                 
140700                                                                          
140800     MOVE 'IMS-GU-WDK711-2 '  TO CURRENT-IMS-SECTION                      
140900                                                                          
141000     MOVE SPACE               TO ALL-SSA                                  
141100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
141200          DELIMITED BY SIZE INTO SSA1                                     
141300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
141400          DELIMITED BY SIZE INTO SSA2                                     
141500     MOVE '  GE'              TO GODK-STATUSKODER                         
141600     CALL CBLTDLI USING GU  WDK7-2-PCB DLI-IO-WDK711 SSA1 SSA2            
141700     MOVE WDK7-2-STATUS-CODE  TO STATUS-WS                                
141800     PERFORM IMS-STATUSKONTROLL                                           
141900     .                                                                    
142000                                                                          
142100                                                                          
142200                                                                          
142300 IMS-GU-WDK712 SECTION.                                                   
142400                                                                          
142500     MOVE 'IMS-GU-WDK712   '  TO CURRENT-IMS-SECTION                      
142600                                                                          
142700     MOVE SPACE               TO ALL-SSA                                  
142800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
142900          DELIMITED BY SIZE INTO SSA1                                     
143000     STRING 'WDK712  (IDLAND   =' W-IDLAND ')'                            
143100          DELIMITED BY SIZE INTO SSA2                                     
143200     MOVE '    '              TO GODK-STATUSKODER                         
143300     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
143400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
143500     PERFORM IMS-STATUSKONTROLL                                           
143600     .                                                                    
143700                                                                          
143800                                                                          
143900 IMS-GU-WDK722 SECTION.                                                   
144000                                                                          
144100     MOVE 'IMS-GU-WDK722   '  TO CURRENT-IMS-SECTION                      
144200                                                                          
144300     MOVE SPACE               TO ALL-SSA                                  
144400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
144500          DELIMITED BY SIZE INTO SSA1                                     
144600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
144700          DELIMITED BY SIZE INTO SSA2                                     
144800     MOVE 'WDK722 '           TO SSA3                                     
144900     MOVE '  GE'              TO GODK-STATUSKODER                         
145000     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
145100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400                                                                          
145500                                                                          
145600 IMS-GHU-WDK722 SECTION.                                                  
145700                                                                          
145800     MOVE 'IMS-GHU-WDK722  '  TO CURRENT-IMS-SECTION                      
145900                                                                          
146000     MOVE SPACE               TO ALL-SSA                                  
146100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
146200          DELIMITED BY SIZE INTO SSA1                                     
146300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
146400          DELIMITED BY SIZE INTO SSA2                                     
146500     MOVE 'WDK722 '           TO SSA3                                     
146600     MOVE '  GE'              TO GODK-STATUSKODER                         
146700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
146800     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
146900     PERFORM IMS-STATUSKONTROLL                                           
147000     .                                                                    
147100                                                                          
147200                                                                          
147300 IMS-REPL-WDK722 SECTION.                                                 
147400                                                                          
147500     MOVE 'IMS-REPL-WDK722 '  TO CURRENT-IMS-SECTION                      
147600                                                                          
147700     MOVE SPACE               TO ALL-SSA                                  
147800     MOVE '  '             TO GODK-STATUSKODER                            
147900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
148000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
148100     PERFORM IMS-STATUSKONTROLL                                           
148200     .                                                                    
148300                                                                          
148400                                                                          
148500                                                                          
148600 IMS-GNP-WDK723-OKVAL SECTION.                                            
148700                                                                          
148800     MOVE 'IMS-GNP-WDK723-O'  TO CURRENT-IMS-SECTION                      
148900                                                                          
149000     MOVE SPACE               TO ALL-SSA                                  
149100     MOVE 'WDK723 '           TO SSA1                                     
149200     MOVE '  GE'              TO GODK-STATUSKODER                         
149300     CALL CBLTDLI USING GNP  WDK7-PCB DLI-IO-WDK723 SSA1                  
149400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
149500     PERFORM IMS-STATUSKONTROLL                                           
149600     .                                                                    
149700                                                                          
149800                                                                          
149900 IMS-GNP-WDK723 SECTION.                                                  
150000                                                                          
150100     MOVE 'IMS-GNP-WDK723'    TO CURRENT-IMS-SECTION                      
150200                                                                          
150300     MOVE SPACE               TO ALL-SSA                                  
150400     STRING 'WDK723  (WDK723KY>=' W-WDK723KY-X ')'                        
150500          DELIMITED BY SIZE INTO SSA1                                     
150600     MOVE '  GE'              TO GODK-STATUSKODER                         
150700     CALL CBLTDLI USING GNP  WDK7-PCB DLI-IO-WDK723 SSA1                  
150800     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
150900     PERFORM IMS-STATUSKONTROLL                                           
151000     .                                                                    
151100                                                                          
151200                                                                          
151300                                                                          
151400 IMS-GU-WDK723 SECTION.                                                   
151500                                                                          
151600     MOVE 'IMS-GU-WDK723   '  TO CURRENT-IMS-SECTION                      
151700                                                                          
151800     MOVE SPACE               TO ALL-SSA                                  
151900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
152000          DELIMITED BY SIZE INTO SSA1                                     
152100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
152200          DELIMITED BY SIZE INTO SSA2                                     
152300     STRING 'WDK723  (WDK723KY =' W-WDK723KY-X ')'                        
152400          DELIMITED BY SIZE INTO SSA3                                     
152500     MOVE '  GE'              TO GODK-STATUSKODER                         
152600     CALL CBLTDLI USING GU   WDK7-PCB DLI-IO-WDK723 SSA1 SSA2 SSA3        
152700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     .                                                                    
153000                                                                          
153100                                                                          
153200 IMS-GU-WDK723-OKVAL SECTION.                                             
153300                                                                          
153400     MOVE 'IMS-GU-WDK723-O '  TO CURRENT-IMS-SECTION                      
153500                                                                          
153600     MOVE SPACE               TO ALL-SSA                                  
153700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
153800          DELIMITED BY SIZE INTO SSA1                                     
153900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
154000          DELIMITED BY SIZE INTO SSA2                                     
154100     MOVE   'WDK723 '         TO SSA3                                     
154200     MOVE '  GE'              TO GODK-STATUSKODER                         
154300     CALL CBLTDLI USING GU   WDK7-PCB DLI-IO-WDK723 SSA1 SSA2 SSA3        
154400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
154500     PERFORM IMS-STATUSKONTROLL                                           
154600     .                                                                    
154700                                                                          
154800                                                                          
154900                                                                          
155000 IMS-GHU-WDK723 SECTION.                                                  
155100                                                                          
155200     MOVE 'IMS-GHU-WDK723  '  TO CURRENT-IMS-SECTION                      
155300                                                                          
155400     MOVE SPACE               TO ALL-SSA                                  
155500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
155600          DELIMITED BY SIZE INTO SSA1                                     
155700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
155800          DELIMITED BY SIZE INTO SSA2                                     
155900     STRING 'WDK723  (WDK723KY =' W-WDK723KY-X ')'                        
156000          DELIMITED BY SIZE INTO SSA3                                     
156100     MOVE '  GE'              TO GODK-STATUSKODER                         
156200     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK723 SSA1 SSA2 SSA3        
156300     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
156400     PERFORM IMS-STATUSKONTROLL                                           
156500     .                                                                    
156600                                                                          
156700                                                                          
156800                                                                          
156900 IMS-GNP-WDK724 SECTION.                                                  
157000                                                                          
157100     MOVE 'IMS-GNP-WDK724  '  TO CURRENT-IMS-SECTION                      
157200                                                                          
157300     MOVE SPACE               TO ALL-SSA                                  
157400     MOVE 'WDK724 '           TO SSA1                                     
157500     MOVE '  GE'              TO GODK-STATUSKODER                         
157600     CALL CBLTDLI USING GNP  WDK7-PCB DLI-IO-WDK724 SSA1                  
157700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000                                                                          
158100                                                                          
158200 IMS-GNP-WDK724-2 SECTION.                                                
158300                                                                          
158400     MOVE 'IMS-GNP-WDK724-2'  TO CURRENT-IMS-SECTION                      
158500                                                                          
158600     MOVE SPACE               TO ALL-SSA                                  
158700     MOVE 'WDK724 '           TO SSA1                                     
158800     MOVE '  GE'              TO GODK-STATUSKODER                         
158900     CALL CBLTDLI USING GNP  WDK7-2-PCB DLI-IO-WDK724 SSA1                
159000     MOVE WDK7-2-STATUS-CODE  TO STATUS-WS                                
159100     PERFORM IMS-STATUSKONTROLL                                           
159200     .                                                                    
159300                                                                          
159400                                                                          
159500                                                                          
159600 IMS-REPL-WDK723 SECTION.                                                 
159700                                                                          
159800     MOVE 'IMS-REPL-WDK723 '  TO CURRENT-IMS-SECTION                      
159900                                                                          
160000     MOVE SPACE               TO ALL-SSA                                  
160100     MOVE '  '             TO GODK-STATUSKODER                            
160200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK723                       
160300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
160400     PERFORM IMS-STATUSKONTROLL                                           
160500     .                                                                    
160600                                                                          
160700                                                                          
160800                                                                          
160900 IMS-DLET-WDK723 SECTION.                                                 
161000                                                                          
161100     MOVE 'IMS-DLET-WDK723 '  TO CURRENT-IMS-SECTION                      
161200                                                                          
161300     MOVE SPACE               TO ALL-SSA                                  
161400     MOVE '  '             TO GODK-STATUSKODER                            
161500     CALL CBLTDLI USING DLET WDK7-PCB DLI-IO-WDK723                       
161600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
161700     PERFORM IMS-STATUSKONTROLL                                           
161800     .                                                                    
161900                                                                          
162000                                                                          
162100                                                                          
162200                                                                          
162300 IMS-GNP-WDK725 SECTION.                                                  
162400                                                                          
162500     MOVE 'IMS-GNP-WDK725  '  TO CURRENT-IMS-SECTION                      
162600                                                                          
162700     MOVE SPACE               TO ALL-SSA                                  
162800     MOVE 'WDK725 '           TO SSA1                                     
162900     MOVE '  GE'              TO GODK-STATUSKODER                         
163000     CALL CBLTDLI USING GNP  WDK7-PCB DLI-IO-WDK725 SSA1                  
163100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
163200     PERFORM IMS-STATUSKONTROLL                                           
163300     .                                                                    
163400                                                                          
163700 IMS-GU-WDB601 SECTION.                                                   
163800                                                                          
163900     MOVE 'IMS-GU-WDB601   '  TO CURRENT-IMS-SECTION                      
164000                                                                          
164100     MOVE SPACE               TO ALL-SSA                                  
164200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
164300          DELIMITED BY SIZE INTO SSA1                                     
164400     MOVE '  GE'              TO GODK-STATUSKODER                         
164500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
164600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
164700     PERFORM IMS-STATUSKONTROLL                                           
164800     .                                                                    
165100                                                                          
165200 IMS-GU-WDF101 SECTION.                                                   
165300                                                                          
165400     MOVE 'IMS-GU-WDF101   '  TO CURRENT-IMS-SECTION                      
165500                                                                          
165600     MOVE SPACE               TO ALL-SSA                                  
165700     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
165800          DELIMITED BY SIZE INTO SSA1                                     
165900     MOVE '  GE'              TO GODK-STATUSKODER                         
166000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
166100     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
166200     PERFORM IMS-STATUSKONTROLL                                           
166300     .                                                                    
166400                                                                          
166500 IMS-GNP-WDF102 SECTION.                                                  
166600     MOVE 'IMS-GNP-WDF102  ' TO CURRENT-IMS-SECTION                       
166700                                                                          
166800     MOVE SPACE               TO ALL-SSA                                  
166900     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
167000          DELIMITED BY SIZE INTO SSA1                                     
167100     MOVE '  GE' TO GODK-STATUSKODER                                      
167200     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF102 SSA1                   
167300     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     SKIP3                                                                
167700                                                                          
167800 IMS-ISRT-WDF102 SECTION.                                                 
167900     MOVE 'IMS-ISRT-WDF102 ' TO CURRENT-IMS-SECTION                       
168000                                                                          
168100     MOVE SPACE               TO ALL-SSA                                  
168200     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
168300          DELIMITED BY SIZE INTO SSA1                                     
168400     MOVE 'WDF102 '           TO SSA2                                     
168500     MOVE '  II' TO GODK-STATUSKODER                                      
168600     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF102 SSA1 SSA2             
168700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
168800     PERFORM IMS-STATUSKONTROLL                                           
168900     .                                                                    
169000                                                                          
169100 IMS-ISRT-WDR301   SECTION.                                               
169200                                                                          
169300     MOVE 'IMS-ISRT-WDR301 '  TO CURRENT-IMS-SECTION                      
169400                                                                          
169500     MOVE SPACE               TO ALL-SSA                                  
169600     MOVE 'WDR301 '           TO SSA1                                     
169700     MOVE '  II'              TO GODK-STATUSKODER                         
169800     CALL CBLTDLI USING ISRT WDR3-PCB DLI-IO-WDR301 SSA1                  
169900     MOVE WDR3-STATUS-CODE    TO STATUS-WS                                
170000     PERFORM IMS-STATUSKONTROLL                                           
170100     .                                                                    
170200                                                                          
171700 IMS-STATUSKONTROLL SECTION.                                              
171800                                                                          
171900     SET STATUS-IX TO 1                                                   
172000     SEARCH GODK-STATUS                                                   
172100       AT END                                                             
172200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
172300         DELIMITED BY SIZE INTO FELTEXT                                   
172400         CALL FELLOG                                                      
172500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
172600         CONTINUE                                                         
172700     END-SEARCH                                                           
172800     .                                                                    
