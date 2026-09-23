000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041700.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   96/04/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV FRAKT INFORMATION.                                
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLGMTC (WDB5)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T417                                              
001400*        MID:         W4I41701                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O41701                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400*    -COPY WY2000W1                                                       
002500     SKIP3                                                                
002600 77  IDPGM                       PIC X(08)   VALUE 'W4041700'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
003200 77  MAX-INDX                    PIC S9(4)   VALUE +11 COMP SYNC.         
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003600                                                                          
003700 77  WS-KDFRAKT                  PIC S9(3)   VALUE ZERO COMP-3.           
003800 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO COMP-3.           
003900 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO COMP-3.           
004000                                                                          
004100 77  WS-REFOERS                  PIC S9(2)V9(3) VALUE ZERO.               
004200 77  WS-PRLEGKST                 PIC S9(7)V9(2) VALUE ZERO.               
004300                                                                          
004400 77  WS-REFOERS-COPY             PIC 9(2).9(3) VALUE ZERO.                
004500 77  WS-PRLEGKST-COPY            PIC 9(7).9(2) VALUE ZERO.                
004600                                                                          
004700 77  W-KDTRPKAT                  PIC X         VALUE SPACE.               
004800 77  W-IDTRP-0                   PIC X(5)      VALUE SPACE.               
004900 77  W-IDTRP-1                   PIC X(5)      VALUE SPACE.               
005000 77  W-IDTRP-2                   PIC X(5)      VALUE SPACE.               
005100 77  W-IDTRP-3                   PIC X(5)      VALUE SPACE.               
005200 77  W-IDTRP-4                   PIC X(5)      VALUE SPACE.               
005300 77  W-KDFDKRAV                  PIC S9(3)      VALUE ZERO.               
005400 77  W-KDGRANS                   PIC S9(3)      VALUE ZERO.               
005500 77  W-REFOERS                   PIC S9(7)V9(2) VALUE ZERO.               
005600 77  W-PRLEGKST                  PIC S9(2)V9(3) VALUE ZERO.               
005700                                                                          
005800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005900                                                                          
006000 77  GAELLANDE-SW                PIC X       VALUE 'J'.                   
006100     88  GAELLANDE-KUND                      VALUE 'J'.                   
006200     88  STOPPAD-KUND                        VALUE 'N'.                   
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006500     88  INDATA-OK                           VALUE 'J'.                   
006600     88  INDATA-FEL                          VALUE 'N'.                   
006700                                                                          
006800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006900     88  NYCKLAR-OK                          VALUE 'J'.                   
007000     88  NYCKLAR-FEL                         VALUE 'N'.                   
007100                                                                          
007200                                                                          
007300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007400     88  EGEN-MID                            VALUE '4417'.                
007500     88  GODK-MID                            VALUE '4411' '4412'          
007600                                                   '4413' '4414'          
007700                                                   '4415' '4416'          
007800                                                   '4417' '4418'          
007900                                                   '4419'.                
008000     88  NYCKEL-MID                          VALUE '4415' '4418'.         
008100     88  HELP-MID                            VALUE '0551'.                
008200     EJECT                                                                
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009200 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA'.            
009300*01 -COPY WMEDAREA                                                        
009400     SKIP3                                                                
009500 01  MESSAGE-CODES.                                                       
009600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010300     03  ERR-TRPID-NOT-REG       PIC X(3)    VALUE '042'.                 
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010800     SKIP3                                                                
010900*01 -COPY WMSGINIT                                                        
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
011200     SKIP3                                                                
011300*01 -COPY WDECAREA                                                        
011400     SKIP3                                                                
011500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011600*                                                                         
011700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011800     SKIP3                                                                
011900*01  MID -COPY W4I41701                                                   
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012200     SKIP3                                                                
012300*01  -COPY WMSGAREA                                                       
012400     EJECT                                                                
012500     03  MOD REDEFINES MSG-AREA.                                          
012600*      05  -COPY W4O41701                                                 
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012900     SKIP3                                                                
013000*01  -COPY WMFSAREA                                                       
013100     EJECT                                                                
013200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013300*                                                                         
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600     SKIP3                                                                
013700 01  SPAR-AREA.                                                           
013800     03  SPAR-IDTRANS            PIC X(4)    VALUE SPACE.                 
013900     03  SPAR-WDB501KY-ENTER     PIC X(11)   VALUE SPACE.                 
014000     03  SPAR-WDB501KY-NEXT      PIC X(11)   VALUE SPACE.                 
014100     SKIP3                                                                
014200 01  NYCKLAR-TILL-BLAEDDRING.                                             
014300     03  W-MINKEY-IDDC           PIC X(2)    VALUE SPACE.                 
014400     03  W-MINKEY-KDFRAKT        PIC S9(3)   VALUE ZERO COMP-3.           
014500     03  W-MINKEY-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
014600     03  W-MINKEY-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
014700                                                                          
014800     SKIP3                                                                
014900 01  NYCKLAR-TILL-DLI.                                                    
015000     03  W-WDB501KY-X.                                                    
015100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015200         05  W-KDFRAKT           PIC S9(3)   VALUE ZERO COMP-3.           
015300         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
015400         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
015500     SKIP2                                                                
015600     03  W-WDB501KY-MIN-X.                                                
015700         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
015800         05  W-KDFRAKT-MIN       PIC S9(3)   VALUE ZERO COMP-3.           
015900         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
016000         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
016100     SKIP2                                                                
016200     03  W-WDB501KY-MAX-X.                                                
016300         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
016400         05  W-KDFRAKT-MAX       PIC S9(3)   VALUE ZERO COMP-3.           
016500         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
016600         05  W-IDKUNDNR-MAX      PIC S9(7) COMP-3 VALUE +9999999.         
016700     SKIP2                                                                
016800     03  W-WDB501KY-COPY-X.                                               
016900         05  W-IDDC-COPY         PIC X(2)    VALUE SPACE.                 
017000         05  W-KDFRAKT-COPY      PIC S9(3)   VALUE ZERO COMP-3.           
017100         05  W-IDDISTR-COPY      PIC S9(5)   VALUE ZERO COMP-3.           
017200         05  W-IDKUNDNR-COPY     PIC S9(7)   VALUE ZERO COMP-3.           
017300     SKIP2                                                                
017400     03  W-WDB501KY-DEF-X.                                                
017500         05  W-IDDC-DEF          PIC X(2)    VALUE SPACE.                 
017600         05  W-KDFRAKT-DEF       PIC S9(3)   VALUE ZERO COMP-3.           
017700         05  W-IDDISTR-DEF       PIC S9(5)   VALUE ZERO COMP-3.           
017800         05  W-IDKUNDNR-DEF      PIC S9(7)  VALUE +9999999 COMP-3.        
017900     SKIP2                                                                
018000     03  W-WDGX4431-X.                                                    
018100         05  W-IDHTYP            PIC X(4)    VALUE '4431'.                
018200         05  W-IDDC4431          PIC X(2)    VALUE ZERO.                  
018300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
018400     SKIP2                                                                
018500     03  W-IDGMT-X.                                                       
018600         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
018700         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
018800     SKIP3                                                                
018900     03  W-KDFKTYP-BADA-X.                                                
019000         05  W-KDFKTYP-BADA      PIC X(1)    VALUE '3'.                   
019100     SKIP2                                                                
019200     03  W-KDFKTYP-X.                                                     
019300         05  W-KDFKTYP           PIC X(1)    VALUE '1'.                   
019400     SKIP2                                                                
019500     03  W-KDFKTYP-2-X.                                                   
019600         05  W-KDFKTYP-2         PIC X(1)    VALUE '2'.                   
019700     SKIP2                                                                
019800     03  W-WDGX4432-X.                                                    
019900         05  W-IDTRP             PIC X(5).                                
020000                                                                          
020100     03  W-IDDC-B6-X.                                                     
020200         05 W-IDDC-B6                  PIC X(2).                          
020300                                                                          
020400     EJECT                                                                
020500                                                                          
020600*    --- STATUS-KOD FRÅN IMS                                              
020700 01  STATUS-WS                   PIC XX.                                  
020800     88  SEGMENT-FINNS                       VALUE '  '.                  
020900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021100     88  BAS-SLUT                            VALUE 'GB'.                  
021200     SKIP2                                                                
021300 01  GODK-STATUSKODER.                                                    
021400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021500     SKIP3                                                                
021600 01  SSA1                        PIC X(256).                              
021700 01  SSA2                        PIC X(64).                               
021800     EJECT                                                                
021900*    --- IMS FUNKTIONSKODER                                               
022000*01  -COPY W0003                                                          
022100     EJECT                                                                
022200*    ---  DLI INPUT-OUTPUT AREA                                           
022300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022400     SKIP3                                                                
022500                                                                          
022600 01  FILLER                      PIC X(16)   VALUE 'WDB501-AREA'.         
022700 01  DLI-IO-AREA.                                                         
022800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
022900     SKIP3                                                                
023000     03  WLGMTC01 REDEFINES IO-AREA.                                      
023100*        05  -COPY WDB501  -PRE GMTC-                                     
023200     EJECT                                                                
023300                                                                          
023400 01  FILLER                      PIC X(16)   VALUE                        
023500                                             'COPY-WDB501-AREA'.          
023600 01  DLI-IO-AREA-1.                                                       
023700     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
023800     SKIP3                                                                
023900     03  WLGMTC01 REDEFINES IO-AREA-1.                                    
024000*        05  -COPY WDB501  -PRE COPY-                                     
024100     EJECT                                                                
024200                                                                          
024300 01  DLI-IO-AREA-2.                                                       
024400     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
024500     SKIP3                                                                
024600     03  WLGMTC01 REDEFINES IO-AREA-2.                                    
024700*        05  -COPY WDB501  -PRE DEF-                                      
024800     EJECT                                                                
024900                                                                          
025000 01  FILLER                      PIC X(16)   VALUE 'XXKA-AREA'.           
025100 01  DLI-IO-AREA-3.                                                       
025200     03  IO-AREA-3               PIC X(150)  VALUE SPACE.                 
025300     SKIP3                                                                
025400     03  WLXXKA01 REDEFINES IO-AREA-3.                                    
025500*        05  -COPY WDGX4431 -PRE XXKA-                                    
025600     EJECT                                                                
025700     03  WLXXKA11 REDEFINES IO-AREA-3.                                    
025800*        05  -COPY WDGX4432 -PRE XXKA-                                    
025900     EJECT                                                                
026000                                                                          
026100 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
026200 01  DLI-IO-AREA-4.                                                       
026300     03  WLGMTA01.                                                        
026400*        05  -COPY WDB201 -PRE GMTA-                                      
026500                                                                          
026600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026700 01   DLI-IO-AREA-B601.                                                   
026800*     03  -COPY WDB601                                                    
026900                                                                          
027000     EJECT                                                                
027100                                                                          
027200 LINKAGE SECTION.                                                         
027300*01  -COPY W0009   -PRE MSG-                                              
027400*01  -COPY W0008   -PRE USEA-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01  -COPY W0008  -PRE GMTC-                                              
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01  -COPY W0008  -PRE GMTCU-                                             
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300*01  -COPY W0008  -PRE XXKA-                                              
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600*01  -COPY W0008  -PRE GMTA-                                              
028700     05  FILLER                  PIC X.                                   
028800     EJECT                                                                
028900*01  -COPY W0008  -PRE WDB6-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTC-PCB GMTCU-PCB            
029300                           XXKA-PCB GMTA-PCB WDB6-PCB.                    
029400 MAIN SECTION.                                                            
029500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTC-PCB GMTCU-PCB            
029600                           XXKA-PCB GMTA-PCB WDB6-PCB.                    
029700                                                                          
029800     PERFORM IMS-GET-MSG                                                  
029900     IF SEGMENT-FINNS                                                     
030000       PERFORM A-INIT                                                     
030100       PERFORM B-KOLLA-NYCKLAR                                            
030200       IF NYCKLAR-OK                                                      
030300         IF MFS-UPDATE                                                    
030400           PERFORM G-KOLLA-INPUT                                          
030500           IF INDATA-OK                                                   
030600             PERFORM H-UPPDATERA                                          
030700           END-IF                                                         
030800           PERFORM S10-SAMMA-SIDA-EFTER-UPPDAT                            
030900         ELSE                                                             
031000           IF MFS-FIRST                                                   
031100             PERFORM C-FOERSTA-SIDA                                       
031200           ELSE                                                           
031300             IF MFS-NEXT                                                  
031400               PERFORM D-NAESTA-SIDA                                      
031500             ELSE                                                         
031600               PERFORM E-SAMMA-SIDA                                       
031700             END-IF                                                       
031800           END-IF                                                         
031900         END-IF                                                           
032000         PERFORM F-LAES-VISA-INFO                                         
032100       END-IF                                                             
032200*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
032300*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
032400       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41701 + 4                      
032500       PERFORM IMS-INSERT-MSG                                             
032600     END-IF                                                               
032700                                                                          
032800     MOVE ZERO TO RETURN-CODE                                             
032900     GOBACK                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 A-INIT SECTION.                                                          
033300                                                                          
033400     IF MSG-DUBBLA-TRANSKODER                                             
033500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41701                 
033600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
033700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033800     ELSE                                                                 
033900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41701                  
034000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
034100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034200     END-IF                                                               
034300                                                                          
034400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
034500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
034600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
034700                                                                          
034800     MOVE LOW-VALUE TO MSG-AREA                                           
034900     MOVE 'W4O417N1' TO MFS-IDMOD                                         
035000     MOVE '4417' TO MOD-IDTRANS                                           
035100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
035200                                                                          
035300     IF EGEN-MID OR HELP-MID                                              
035400       CONTINUE                                                           
035500     ELSE                                                                 
035600       MOVE SPACE TO MFS-KDTRTYP                                          
035700       MOVE '7' TO MFS-IDPFK                                              
035800     END-IF                                                               
035900                                                                          
036000     ACCEPT DAGENS-DATUM FROM DATE                                        
036100     .                                                                    
036200     EJECT                                                                
036300 B-KOLLA-NYCKLAR SECTION.                                                 
036400                                                                          
036500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036600     MOVE '001'             TO MSGI-KDCALL                                
036700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
036800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036900     MOVE '4417'            TO MSGI-IDTRANS                               
037000     IF EGEN-MID                                                          
037100       MOVE MID-KDFRAKT-IN  TO MSGI-KDFRAKT                               
037200       MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                               
037300       MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                              
037400     END-IF                                                               
037500                                                                          
037600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037700     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
037800     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
037900     MOVE JA TO NYCKLAR-SW                                                
038000                                                                          
038100*    -- KONTROLL AV IDDC                                                  
038200     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
038300                                                                          
038400     IF MID-IDDC-IN = ALL '+'                                             
038500       IF EGEN-MID OR NYCKEL-MID                                          
038600         MOVE MID-IDDC-UT TO W-IDDC-B6                                    
038700       ELSE                                                               
038800         MOVE MSGI-IDDC TO W-IDDC-B6                                      
038900       END-IF                                                             
039000     ELSE                                                                 
039100       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
039200       MOVE '7'         TO MFS-IDPFK                                      
039300       MOVE SPACE       TO MFS-KDTRTYP                                    
039400     END-IF                                                               
039500     PERFORM IMS-GU-WDB601                                                
039600     IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                         
039700       MOVE NEJ TO NYCKLAR-SW                                             
039800     ELSE                                                                 
039900       MOVE DCS-IDDC TO W-IDDC                                            
040000                        W-IDDC-MIN                                        
040100                        W-IDDC-MAX                                        
040200                        W-IDDC-COPY                                       
040300                        W-IDDC-DEF                                        
040400                        W-IDDC4431                                        
040500     END-IF                                                               
040600                                                                          
040700                                                                          
040800*    -- KONTROLL AV KDFRAKT                                               
040900     MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-IN                               
041000                                                                          
041100     IF MID-KDFRAKT-IN NOT = ALL '+'                                      
041200       MOVE '7'         TO MFS-IDPFK                                      
041300       MOVE SPACE       TO MFS-KDTRTYP                                    
041400     END-IF                                                               
041500     IF MSGI-KDFRAKT NUMERIC                                              
041600       MOVE MSGI-KDFRAKT TO W-KDFRAKT                                     
041700                            W-KDFRAKT-MIN                                 
041800                            W-KDFRAKT-MAX                                 
041900                            W-KDFRAKT-COPY                                
042000                            W-KDFRAKT-DEF                                 
042100                            WS-KDFRAKT                                    
042200     ELSE                                                                 
042300       MOVE NEJ TO NYCKLAR-SW                                             
042400     END-IF                                                               
042500                                                                          
042600                                                                          
042700*    -- KONTROLL AV IDDISTR                                               
042800     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
042900                                                                          
043000     IF MID-IDDISTR-IN NOT = ALL '+'                                      
043100       MOVE '7'         TO MFS-IDPFK                                      
043200       MOVE SPACE       TO MFS-KDTRTYP                                    
043300     END-IF                                                               
043400     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > 0                         
043500       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
043600                            W-IDDISTR-MIN                                 
043700                            W-IDDISTR-MAX                                 
043800                            W-IDDISTR-COPY                                
043900                            W-IDDISTR-DEF                                 
044000                            W-IDDISTR-WDB2                                
044100                            WS-IDDISTR                                    
044200     ELSE                                                                 
044300       MOVE NEJ TO NYCKLAR-SW                                             
044400     END-IF                                                               
044500                                                                          
044600                                                                          
044700*    -- KONTROLL AV KUNDNR                                                
044800     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
044900                                                                          
045000     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
045100       MOVE '7'         TO MFS-IDPFK                                      
045200       MOVE SPACE       TO MFS-KDTRTYP                                    
045300     END-IF                                                               
045400                                                                          
045500     IF MSGI-IDKUNDNR NUMERIC                                             
045600       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
045700                             W-IDKUNDNR-MIN                               
045800                             W-IDKUNDNR-COPY                              
045900                             WS-IDKUNDNR                                  
046000     ELSE                                                                 
046100       MOVE NEJ TO NYCKLAR-SW                                             
046200     END-IF                                                               
046300                                                                          
046400     IF GODK-MID OR NYCKLAR-OK                                            
046500       MOVE DCS-IDDC            TO MOD-IDDC-UT                            
046600       MOVE MSGI-KDFRAKT        TO MOD-KDFRAKT-UT                         
046700       INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE             
046800       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
046900       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
047000       MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                        
047100       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
047200     ELSE                                                                 
047300       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
047400                               MOD-KDFRAKT-UT                             
047500                               MOD-IDDISTR-UT                             
047600                               MOD-IDKUNDNR-UT                            
047700     END-IF                                                               
047800                                                                          
047900     IF NYCKLAR-FEL                                                       
048000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
048100       CALL WMEDKONV USING MED-WMEDAREA                                   
048200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
048300       PERFORM MFS-RENSA-FAELT-IN                                         
048400       PERFORM MFS-RENSA-FAELT-UT                                         
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 C-FOERSTA-SIDA SECTION.                                                  
048900                                                                          
049000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
049100     CALL WMEDKONV USING MED-WMEDAREA                                     
049200     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
049300                                                                          
049400     PERFORM MFS-RENSA-FAELT-IN                                           
049500     .                                                                    
049600     EJECT                                                                
049700                                                                          
049800 D-NAESTA-SIDA SECTION.                                                   
049900                                                                          
050000     IF SPAR-IDTRANS = '4417'                                             
050100       MOVE SPAR-WDB501KY-NEXT   TO NYCKLAR-TILL-BLAEDDRING               
050200       MOVE W-MINKEY-IDDC        TO W-IDDC                                
050300                                    W-IDDC-MIN                            
050400                                    W-IDDC-MAX                            
050500                                    W-IDDC4431                            
050600       MOVE W-MINKEY-KDFRAKT     TO W-KDFRAKT                             
050700                                    W-KDFRAKT-MIN                         
050800                                    W-KDFRAKT-MAX                         
050900       MOVE W-MINKEY-IDDISTR     TO W-IDDISTR                             
051000                                    W-IDDISTR-MIN                         
051100                                    W-IDDISTR-MAX                         
051200                                    W-IDDISTR-WDB2                        
051300       MOVE W-MINKEY-IDKUNDNR    TO W-IDKUNDNR                            
051400                                    W-IDKUNDNR-MIN                        
051500     END-IF                                                               
051600     PERFORM MFS-RENSA-FAELT-IN                                           
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000 E-SAMMA-SIDA SECTION.                                                    
052100                                                                          
052200     PERFORM MFS-RENSA-FAELT-UT                                           
052300     IF EGEN-MID OR HELP-MID                                              
052400       IF MID-INPUT = ALL '+'                                             
052500         IF SPAR-IDTRANS = '4417'                                         
052600           MOVE SPAR-WDB501KY-ENTER                                       
052700                                 TO NYCKLAR-TILL-BLAEDDRING               
052800           MOVE DCS-IDDC         TO W-IDDC                                
052900                                    W-IDDC-MIN                            
053000                                    W-IDDC-MAX                            
053100                                    W-IDDC4431                            
053200           MOVE W-MINKEY-KDFRAKT TO W-KDFRAKT                             
053300                                    W-KDFRAKT-MIN                         
053400                                    W-KDFRAKT-MAX                         
053500           MOVE W-MINKEY-IDDISTR TO W-IDDISTR                             
053600                                    W-IDDISTR-MIN                         
053700                                    W-IDDISTR-MAX                         
053800                                    W-IDDISTR-WDB2                        
053900           MOVE W-MINKEY-IDKUNDNR TO W-IDKUNDNR                           
054000                                     W-IDKUNDNR-MIN                       
054100         END-IF                                                           
054200         PERFORM MFS-RENSA-FAELT-IN                                       
054300       ELSE                                                               
054400         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
054500         CALL WMEDKONV USING MED-WMEDAREA                                 
054600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
054700         PERFORM EA-MID-INDATA-TILL-MOD                                   
054800       END-IF                                                             
054900     ELSE                                                                 
055000       PERFORM MFS-RENSA-FAELT-IN                                         
055100     END-IF                                                               
055200     .                                                                    
055300     EJECT                                                                
055400 EA-MID-INDATA-TILL-MOD SECTION.                                          
055500                                                                          
055600     IF MID-KDCMD-IN NOT = ALL '+'                                        
055700       MOVE MID-KDCMD-IN          TO MOD-KDCMD-IN                         
055800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                    
055900     ELSE                                                                 
056000       MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-IN                         
056100     END-IF                                                               
056200                                                                          
056300     IF MID-KUNDNR-IN NOT = ALL '+'                                       
056400       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
056500       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
056600       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
056700       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
056800       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
056900         MOVE '9999999'           TO MID-KUNDNR-IN                        
057000         MOVE 'DEF    '           TO MOD-KUNDNR-IN                        
057100       ELSE                                                               
057200         MOVE MID-KUNDNR-IN       TO MOD-KUNDNR-IN                        
057300       END-IF                                                             
057400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KUNDNR-IN-ATTR                   
057500     ELSE                                                                 
057600       MOVE MFS-RENSA-FAELT       TO MOD-KUNDNR-IN                        
057700     END-IF                                                               
057800                                                                          
057900     IF MID-KDTRPKAT-IN NOT = ALL '+'                                     
058000       MOVE MID-KDTRPKAT-IN       TO MOD-KDTRPKAT-IN                      
058100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDTRPKAT-IN-ATTR                 
058200     ELSE                                                                 
058300       MOVE MFS-RENSA-FAELT       TO MOD-KDTRPKAT-IN                      
058400     END-IF                                                               
058500                                                                          
058600     IF MID-IDTRP-0-IN NOT = ALL '+'                                      
058700       MOVE MID-IDTRP-0-IN        TO MOD-IDTRP-0-IN                       
058800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDTRP-0-IN-ATTR                  
058900     ELSE                                                                 
059000       MOVE MFS-RENSA-FAELT       TO MOD-IDTRP-0-IN                       
059100     END-IF                                                               
059200                                                                          
059300     IF MID-IDTRP-1-IN NOT = ALL '+'                                      
059400       MOVE MID-IDTRP-1-IN        TO MOD-IDTRP-1-IN                       
059500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDTRP-1-IN-ATTR                  
059600     ELSE                                                                 
059700       MOVE MFS-RENSA-FAELT       TO MOD-IDTRP-1-IN                       
059800     END-IF                                                               
059900                                                                          
060000     IF MID-IDTRP-2-IN NOT = ALL '+'                                      
060100       MOVE MID-IDTRP-2-IN        TO MOD-IDTRP-2-IN                       
060200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDTRP-2-IN-ATTR                  
060300     ELSE                                                                 
060400       MOVE MFS-RENSA-FAELT       TO MOD-IDTRP-2-IN                       
060500     END-IF                                                               
060600                                                                          
060700     IF MID-IDTRP-3-IN NOT = ALL '+'                                      
060800       MOVE MID-IDTRP-3-IN        TO MOD-IDTRP-3-IN                       
060900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDTRP-3-IN-ATTR                  
061000     ELSE                                                                 
061100       MOVE MFS-RENSA-FAELT       TO MOD-IDTRP-3-IN                       
061200     END-IF                                                               
061300                                                                          
061400     IF MID-IDTRP-4-IN NOT = ALL '+'                                      
061500       MOVE MID-IDTRP-4-IN        TO MOD-IDTRP-4-IN                       
061600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDTRP-4-IN-ATTR                  
061700     ELSE                                                                 
061800       MOVE MFS-RENSA-FAELT       TO MOD-IDTRP-4-IN                       
061900     END-IF                                                               
062000                                                                          
062100     IF MID-KDFDKRAV-IN NOT = ALL '+'                                     
062200       MOVE MID-KDFDKRAV-IN        TO MOD-KDFDKRAV-IN                     
062300       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDFDKRAV-IN-ATTR                
062400     ELSE                                                                 
062500       MOVE MFS-RENSA-FAELT        TO MOD-KDFDKRAV-IN                     
062600     END-IF                                                               
062700                                                                          
062800     IF MID-KDGRANS-IN NOT = ALL '+'                                      
062900       MOVE MID-KDGRANS-IN         TO MOD-KDGRANS-IN                      
063000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDGRANS-IN-ATTR                 
063100     ELSE                                                                 
063200       MOVE MFS-RENSA-FAELT        TO MOD-KDGRANS-IN                      
063300     END-IF                                                               
063400                                                                          
063500     IF MID-REFOERS-IN NOT = ALL '+'                                      
063600       MOVE MID-REFOERS-IN        TO MOD-REFOERS-IN                       
063700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REFOERS-IN-ATTR                  
063800     ELSE                                                                 
063900       MOVE MFS-RENSA-FAELT       TO MOD-REFOERS-IN-ATTR                  
064000     END-IF                                                               
064100                                                                          
064200     IF MID-PRLEGKST-IN NOT = ALL '+'                                     
064300       MOVE MID-PRLEGKST-IN       TO MOD-PRLEGKST-IN                      
064400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRLEGKST-IN-ATTR                 
064500     ELSE                                                                 
064600       MOVE MFS-RENSA-FAELT       TO MOD-PRLEGKST-IN                      
064700     END-IF                                                               
064800     .                                                                    
064900     EJECT                                                                
065000 F-LAES-VISA-INFO SECTION.                                                
065100                                                                          
065200     PERFORM IMS-GU-WDB501                                                
065300                                                                          
065400     IF SEGMENT-SAKNAS                                                    
065500*        LÄMPLIGT FELMEDDELANDE                                           
065600*       CALL WMEDKONV USING MED-WMEDAREA                                  
065700*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
065800*       MOVE 'FK UPPG. SAKNAS ' TO MOD-TEMFSFEL                           
065900        PERFORM MFS-RENSA-FAELT-UT                                        
066000        MOVE W-WDB501KY-MIN-X        TO NYCKLAR-TILL-BLAEDDRING           
066100*       MOVE SPACE                     TO W-MINKEY-IDDC                   
066200*       MOVE ZERO                      TO W-MINKEY-KDFRAKT                
066300*                                         W-MINKEY-IDDISTR                
066400*                                         W-MINKEY-IDKUNDNR               
066500        MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB501KY-ENTER               
066600                                        SPAR-WDB501KY-NEXT                
066700        MOVE '002'     TO MSGI-KDCALL                                     
066800        MOVE '4417'    TO MSGI-IDTRANS                                    
066900                          SPAR-IDTRANS                                    
067000        MOVE SPAR-AREA TO MSGI-SPAR-AREA                                  
067100        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
067200                                                                          
067300     ELSE                                                                 
067400                                                                          
067500***** SPARA ENTER NYCKLAR PÅ USERBASEN                                    
067600                                                                          
067700       MOVE GMTC-FK-IDDC               TO W-MINKEY-IDDC                   
067800       MOVE GMTC-FK-KDFRAKT            TO W-MINKEY-KDFRAKT                
067900       MOVE GMTC-FK-IDDISTR            TO W-MINKEY-IDDISTR                
068000       MOVE GMTC-FK-IDKUNDNR           TO W-MINKEY-IDKUNDNR               
068100                                                                          
068200       MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB501KY-ENTER                
068300                                                                          
068400       MOVE +1 TO INDX                                                    
068500       PERFORM UNTIL INDX > MAX-INDX                                      
068600                                                                          
068700         IF SEGMENT-FINNS AND GMTC-FK-IDKUNDNR < 9999999                  
068800                                                                          
068900           MOVE GMTC-FK-IDKUNDNR       TO MOD-IDKUNDNR     (INDX)         
069000           MOVE GMTC-FK-KDTRPKAT       TO MOD-KDTRPKAT     (INDX)         
069100           MOVE GMTC-FK-IDTRP-0        TO MOD-IDTRP-0      (INDX)         
069200           MOVE GMTC-FK-IDTRP-1        TO MOD-IDTRP-1      (INDX)         
069300           MOVE GMTC-FK-IDTRP-2        TO MOD-IDTRP-2      (INDX)         
069400           MOVE GMTC-FK-IDTRP-3        TO MOD-IDTRP-3      (INDX)         
069500           MOVE GMTC-FK-IDTRP-4        TO MOD-IDTRP-4      (INDX)         
069600           MOVE GMTC-FK-KDFDKRAV       TO MOD-KDFDKRAV     (INDX)         
069700           MOVE GMTC-FK-KDGRANS        TO MOD-KDGRANS      (INDX)         
069800           MOVE GMTC-FK-REFOERS        TO MOD-REFOERS      (INDX)         
069900           MOVE GMTC-FK-PRLEGKST       TO MOD-PRLEGKST     (INDX)         
070000                                                                          
070100           ADD +1 TO INDX                                                 
070200           PERFORM IMS-GN-WDB501                                          
070300         ELSE                                                             
070400           MOVE MFS-RENSA-FAELT        TO MOD-IDKUNDNR     (INDX)         
070500                                        MOD-KDTRPKAT       (INDX)         
070600                                        MOD-IDTRP-0        (INDX)         
070700                                        MOD-IDTRP-1        (INDX)         
070800                                        MOD-IDTRP-2        (INDX)         
070900                                        MOD-IDTRP-3        (INDX)         
071000                                        MOD-IDTRP-4        (INDX)         
071100                                        MOD-KDFDKRAV       (INDX)         
071200                                        MOD-KDGRANS        (INDX)         
071300                                        MOD-REFOERS        (INDX)         
071400                                        MOD-PRLEGKST       (INDX)         
071500                                                                          
071600           ADD +1 TO INDX                                                 
071700         END-IF                                                           
071800       END-PERFORM                                                        
071900                                                                          
072000       IF SEGMENT-FINNS AND                                               
072100         GMTC-FK-IDKUNDNR < +9999999                                      
072200                                                                          
072300***** SPARA NEXT NYCKLAR PÅ USERBASEN                                     
072400                                                                          
072500         MOVE GMTC-FK-IDDC       TO W-MINKEY-IDDC                         
072600         MOVE GMTC-FK-KDFRAKT    TO W-MINKEY-KDFRAKT                      
072700         MOVE GMTC-FK-IDDISTR    TO W-MINKEY-IDDISTR                      
072800         MOVE GMTC-FK-IDKUNDNR TO W-MINKEY-IDKUNDNR                       
072900                                                                          
073000         IF MFS-UPDATE                                                    
073100           CONTINUE                                                       
073200         ELSE                                                             
073300           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
073400           CALL WMEDKONV USING MED-WMEDAREA                               
073500           MOVE MED-MFSINF TO MOD-TEMFSINF                                
073600         END-IF                                                           
073700       ELSE                                                               
073800         CONTINUE                                                         
073900       END-IF                                                             
074000                                                                          
074100       MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB501KY-NEXT                 
074200       MOVE '002'     TO MSGI-KDCALL                                      
074300       MOVE '4417'    TO MSGI-IDTRANS                                     
074400                         SPAR-IDTRANS                                     
074500       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
074600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
074700                                                                          
074800     END-IF                                                               
074900                                                                          
075000     PERFORM IMS-GU-WDB501-DEFAULT                                        
075100     IF SEGMENT-FINNS                                                     
075200******* DEFAULT LASNING                                                   
075300                                                                          
075400       MOVE DEF-FK-KDTRPKAT      TO MOD-KDTRPKAT-UT                       
075500       MOVE DEF-FK-IDTRP-0       TO MOD-IDTRP-0-UT                        
075600       MOVE DEF-FK-IDTRP-1       TO MOD-IDTRP-1-UT                        
075700       MOVE DEF-FK-IDTRP-2       TO MOD-IDTRP-2-UT                        
075800       MOVE DEF-FK-IDTRP-3       TO MOD-IDTRP-3-UT                        
075900       MOVE DEF-FK-IDTRP-4       TO MOD-IDTRP-4-UT                        
076000       MOVE DEF-FK-KDFDKRAV      TO MOD-KDFDKRAV-UT                       
076100       MOVE DEF-FK-KDGRANS       TO MOD-KDGRANS-UT                        
076200       MOVE DEF-FK-REFOERS       TO MOD-REFOERS-UT                        
076300       MOVE DEF-FK-PRLEGKST      TO MOD-PRLEGKST-UT                       
076400     ELSE                                                                 
076500       MOVE 'DEFAULT ROW MISSING '   TO MOD-TEMFSFEL                      
076600       PERFORM MFS-RENSA-FAELT-UT                                         
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000                                                                          
077100 G-KOLLA-INPUT SECTION.                                                   
077200                                                                          
077300     MOVE JA  TO INDATA-SW                                                
077400                                                                          
077500     IF MID-INPUT = ALL '+'                                               
077600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
077700       CALL WMEDKONV USING MED-WMEDAREA                                   
077800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
077900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
078000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
078100       MOVE NEJ TO INDATA-SW                                              
078200     ELSE                                                                 
078300      IF  WS-KDFRAKT = 0                                                  
078400      AND MID-KDCMD-IN = 'N'                                              
078500        MOVE 'FREIGHT CODE 0 NOT ALLOWED' TO MOD-TEMFSINF                 
078600        PERFORM MFS-RENSA-FAELT-IN                                        
078700        PERFORM MFS-ROER-EJ-FAELT-UT                                      
078800        MOVE NEJ TO INDATA-SW                                             
078900      ELSE                                                                
079000       IF MID-KDCMD-IN NOT = ALL '+'                                      
079100         IF MID-KDCMD-IN = 'N' OR 'E' OR 'D' OR 'C'                       
079200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                 
079300           IF MID-KDCMD-IN = 'N'                                          
079400             PERFORM GA-KONTROLLERA-NEW                                   
079500           ELSE                                                           
079600             IF MID-KDCMD-IN = 'E'                                        
079700               PERFORM GB-KONTROLLERA-EDIT                                
079800             ELSE                                                         
079900               IF MID-KDCMD-IN = 'D'                                      
080000                 PERFORM GC-KONTROLLERA-DELETE                            
080100               ELSE                                                       
080200                 PERFORM GD-KONTROLLERA-COPY                              
080300               END-IF                                                     
080400             END-IF                                                       
080500           END-IF                                                         
080600         ELSE                                                             
080700            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                  
080800            MOVE NEJ TO INDATA-SW                                         
080900         END-IF                                                           
081000       ELSE                                                               
081100          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-IN-ATTR                  
081200          MOVE NEJ TO INDATA-SW                                           
081300       END-IF                                                             
081400                                                                          
081500       IF INDATA-FEL                                                      
081600         IF GAELLANDE-KUND                                                
081700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
081800           CALL WMEDKONV USING MED-WMEDAREA                               
081900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
082000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
082100           PERFORM MFS-ROER-EJ-FAELT-IN                                   
082200         ELSE                                                             
082300*          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
082400*          CALL WMEDKONV USING MED-WMEDAREA                               
082500*          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
082600           MOVE ' GOODS RECEIVER NOT VALID ' TO MOD-TEMFSINF              
082700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
082800           PERFORM MFS-ROER-EJ-FAELT-IN                                   
082900         END-IF                                                           
083000       END-IF                                                             
083100      END-IF                                                              
083200     END-IF                                                               
083300     .                                                                    
083400     EJECT                                                                
083500                                                                          
083600 GA-KONTROLLERA-NEW SECTION.                                              
083700                                                                          
083800                                                                          
083900**** KONTROLL AV KUNDNR                                                   
084000                                                                          
084100     IF MID-KUNDNR-IN NOT = ALL '+'                                       
084200       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
084300       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
084400       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
084500       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
084600       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
084700         MOVE '9999999'           TO MID-KUNDNR-IN                        
084800       END-IF                                                             
084900       IF MID-KUNDNR-IN NOT NUMERIC                                       
085000         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KUNDNR-IN-ATTR                 
085100         MOVE NEJ TO INDATA-SW                                            
085200       ELSE                                                               
085300         MOVE MID-KUNDNR-IN         TO W-IDKUNDNR-COPY                    
085400                                       W-IDKUNDNR-WDB2                    
085500         IF MID-KUNDNR-IN = 9999999                                       
085600           PERFORM IMS-GU-WDB501-COPY-DEF                                 
085700         ELSE                                                             
085800           PERFORM IMS-GU-WDB501-COPY                                     
085900         END-IF                                                           
086000         IF SEGMENT-SAKNAS                                                
086100           IF MID-KUNDNR-IN = 9999999                                     
086200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR              
086300           ELSE                                                           
086400             PERFORM IMS-GU-WDB501-DEFAULT                                
086500             IF SEGMENT-FINNS                                             
086600               PERFORM S01-KOLLA-OM-GAELLANDE-KUND                        
086700               IF GAELLANDE-KUND                                          
086800                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR          
086900               ELSE                                                       
087000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR            
087100                 MOVE NEJ TO INDATA-SW                                    
087200               END-IF                                                     
087300             ELSE                                                         
087400               MOVE MFS-ALFA-FAELT-FEL  TO MOD-KUNDNR-IN-ATTR             
087500               MOVE NEJ TO INDATA-SW                                      
087600               MOVE ' DEFAULT ROW MISSING ' TO MOD-TEMFSINF               
087700             END-IF                                                       
087800           END-IF                                                         
087900         ELSE                                                             
088000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR                  
088100           MOVE NEJ TO INDATA-SW                                          
088200           MOVE 'GOODS RECEIVER ALREADY PRESENT ' TO MOD-TEMFSINF         
088300         END-IF                                                           
088400       END-IF                                                             
088500     ELSE                                                                 
088600       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
088700       MOVE NEJ TO INDATA-SW                                              
088800     END-IF                                                               
088900                                                                          
089000**** KONTROLL AV KDTRPKAT ( TRANSPORT KATEGORI )                          
089100                                                                          
089200     IF MID-KDTRPKAT-IN NOT = ALL '+'                                     
089300       IF MID-KDTRPKAT-IN = 'A' OR 'B' OR 'C'                             
089400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTRPKAT-IN-ATTR                
089500       ELSE                                                               
089600         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDTRPKAT-IN-ATTR                  
089700         MOVE NEJ TO INDATA-SW                                            
089800       END-IF                                                             
089900     ELSE                                                                 
090000       MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDTRPKAT-IN-ATTR                  
090100       MOVE NEJ TO INDATA-SW                                              
090200     END-IF                                                               
090300                                                                          
090400**** KONTROLL AV IDTRP-0 ( TRANSPORT IDENTITET KLASS 0)                   
090500                                                                          
090600     IF MID-KDTRPKAT-IN = 'C'        AND                                  
090700       (MID-IDTRP-0-IN = ALL '+' OR                                       
090800        MID-IDTRP-0-IN = '00000')                                         
090900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-0-IN-ATTR                  
091000     ELSE                                                                 
091100       IF MID-IDTRP-0-IN NOT = ALL '+'                                    
091200          MOVE MID-IDTRP-0-IN           TO W-IDTRP                        
091300          PERFORM IMS-GU-XXKA-WDR110                                      
091400          IF SEGMENT-FINNS                                                
091500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-0-IN-ATTR              
091600          ELSE                                                            
091700            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-0-IN-ATTR            
091800            MOVE NEJ TO INDATA-SW                                         
091900            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
092000            CALL WMEDKONV USING MED-WMEDAREA                              
092100            MOVE MED-MFSINF TO MOD-TEMFSINF                               
092200          END-IF                                                          
092300       ELSE                                                               
092400         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDTRP-0-IN-ATTR                   
092500         MOVE NEJ TO INDATA-SW                                            
092600       END-IF                                                             
092700     END-IF                                                               
092800                                                                          
092900**** KONTROLL AV IDTRP-1 ( TRANSPORT IDENTITET KLASS 1)                   
093000                                                                          
093100     IF MID-KDTRPKAT-IN = 'C'        AND                                  
093200       (MID-IDTRP-1-IN = ALL '+' OR                                       
093300        MID-IDTRP-1-IN = '00000')                                         
093400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-1-IN-ATTR                  
093500     ELSE                                                                 
093600       IF MID-IDTRP-1-IN NOT = ALL '+'                                    
093700          MOVE MID-IDTRP-1-IN           TO W-IDTRP                        
093800          PERFORM IMS-GU-XXKA-WDR110                                      
093900          IF SEGMENT-FINNS                                                
094000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-1-IN-ATTR              
094100          ELSE                                                            
094200            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-1-IN-ATTR            
094300            MOVE NEJ TO INDATA-SW                                         
094400            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
094500            CALL WMEDKONV USING MED-WMEDAREA                              
094600            MOVE MED-MFSINF TO MOD-TEMFSINF                               
094700          END-IF                                                          
094800       ELSE                                                               
094900         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDTRP-1-IN-ATTR                   
095000         MOVE NEJ TO INDATA-SW                                            
095100       END-IF                                                             
095200     END-IF                                                               
095300                                                                          
095400**** KONTROLL AV IDTRP-2 ( TRANSPORT IDENTITET KLASS 2)                   
095500                                                                          
095600     IF MID-KDTRPKAT-IN = 'C'        AND                                  
095700       (MID-IDTRP-2-IN = ALL '+' OR                                       
095800        MID-IDTRP-2-IN = '00000')                                         
095900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-2-IN-ATTR                  
096000     ELSE                                                                 
096100       IF MID-IDTRP-2-IN NOT = ALL '+'                                    
096200          MOVE MID-IDTRP-2-IN           TO W-IDTRP                        
096300          PERFORM IMS-GU-XXKA-WDR110                                      
096400          IF SEGMENT-FINNS                                                
096500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-2-IN-ATTR              
096600          ELSE                                                            
096700            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-2-IN-ATTR            
096800            MOVE NEJ TO INDATA-SW                                         
096900            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
097000            CALL WMEDKONV USING MED-WMEDAREA                              
097100            MOVE MED-MFSINF TO MOD-TEMFSINF                               
097200          END-IF                                                          
097300       ELSE                                                               
097400         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDTRP-2-IN-ATTR                   
097500         MOVE NEJ TO INDATA-SW                                            
097600       END-IF                                                             
097700     END-IF                                                               
097800                                                                          
097900**** KONTROLL AV IDTRP-3 ( TRANSPORT IDENTITET KLASS 3)                   
098000                                                                          
098100     IF MID-KDTRPKAT-IN = 'C'        AND                                  
098200       (MID-IDTRP-3-IN = ALL '+' OR                                       
098300        MID-IDTRP-3-IN = '00000')                                         
098400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-3-IN-ATTR                  
098500     ELSE                                                                 
098600       IF MID-IDTRP-3-IN NOT = ALL '+'                                    
098700          MOVE MID-IDTRP-3-IN           TO W-IDTRP                        
098800          PERFORM IMS-GU-XXKA-WDR110                                      
098900          IF SEGMENT-FINNS                                                
099000            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-3-IN-ATTR              
099100          ELSE                                                            
099200            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-3-IN-ATTR            
099300            MOVE NEJ TO INDATA-SW                                         
099400            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
099500            CALL WMEDKONV USING MED-WMEDAREA                              
099600            MOVE MED-MFSINF TO MOD-TEMFSINF                               
099700          END-IF                                                          
099800       ELSE                                                               
099900         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDTRP-3-IN-ATTR                   
100000         MOVE NEJ TO INDATA-SW                                            
100100       END-IF                                                             
100200     END-IF                                                               
100300                                                                          
100400**** KONTROLL AV IDTRP-4 ( TRANSPORT IDENTITET KLASS 4)                   
100500                                                                          
100600     IF MID-KDTRPKAT-IN = 'C'        AND                                  
100700       (MID-IDTRP-4-IN = ALL '+' OR                                       
100800        MID-IDTRP-4-IN = '00000')                                         
100900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-4-IN-ATTR                  
101000     ELSE                                                                 
101100       IF MID-IDTRP-4-IN NOT = ALL '+'                                    
101200          MOVE MID-IDTRP-4-IN           TO W-IDTRP                        
101300          PERFORM IMS-GU-XXKA-WDR110                                      
101400          IF SEGMENT-FINNS                                                
101500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-4-IN-ATTR              
101600          ELSE                                                            
101700            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-4-IN-ATTR            
101800            MOVE NEJ TO INDATA-SW                                         
101900            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
102000            CALL WMEDKONV USING MED-WMEDAREA                              
102100            MOVE MED-MFSINF TO MOD-TEMFSINF                               
102200          END-IF                                                          
102300       ELSE                                                               
102400         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDTRP-4-IN-ATTR                   
102500         MOVE NEJ TO INDATA-SW                                            
102600       END-IF                                                             
102700     END-IF                                                               
102800                                                                          
102900**** KONTROLL AV KDFDKRAV ( TRANSPORTFÖRPACKNINGSKOD )                    
103000                                                                          
103100     IF MID-KDFDKRAV-IN NOT = ALL '+'                                     
103200       IF MID-KDFDKRAV-IN NOT NUMERIC                                     
103300         MOVE MFS-NUM-FAELT-FEL       TO MOD-KDFDKRAV-IN-ATTR             
103400         MOVE NEJ TO INDATA-SW                                            
103500       ELSE                                                               
103600         IF MID-KDFDKRAV-IN <= 099                                        
103700           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFDKRAV-IN-ATTR               
103800         ELSE                                                             
103900           MOVE MFS-NUM-FAELT-FEL     TO MOD-KDFDKRAV-IN-ATTR             
104000           MOVE NEJ TO INDATA-SW                                          
104100         END-IF                                                           
104200       END-IF                                                             
104300     ELSE                                                                 
104400       MOVE MFS-NUM-FAELT-FEL       TO MOD-KDFDKRAV-IN-ATTR               
104500       MOVE NEJ TO INDATA-SW                                              
104600     END-IF                                                               
104700                                                                          
104800**** KONTROLL AV KDGRANS ( GRÄNSKOD )                                     
104900                                                                          
105000     IF MID-KDGRANS-IN NOT = ALL '+'                                      
105100       IF MID-KDGRANS-IN NUMERIC                                          
105200         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGRANS-IN-ATTR                  
105300       ELSE                                                               
105400         MOVE MFS-NUM-FAELT-FEL       TO MOD-KDGRANS-IN-ATTR              
105500         MOVE NEJ TO INDATA-SW                                            
105600       END-IF                                                             
105700     END-IF                                                               
105800                                                                          
105900**** KONTROLL AV REFOERS ( FÖRSÄKRINGSKOSTNADSFAKTOR )                    
106000                                                                          
106100     IF MID-REFOERS-IN NOT = ALL '+'                                      
106200       MOVE MID-REFOERS-IN      TO DEC-IDFRIDATA                          
106300       MOVE 2                   TO DEC-KVHELTAL                           
106400       MOVE 3                   TO DEC-KVDECIMAL                          
106500                                                                          
106600       CALL WDECEDIT USING DEC-WDECAREA                                   
106700                                                                          
106800       IF DEC-KDSVAR-OK                                                   
106900         MOVE DEC-IDEDITDATA    TO WS-REFOERS                             
107000         MOVE MFS-NUM-FAELT-RAETT TO MOD-REFOERS-IN-ATTR                  
107100       ELSE                                                               
107200         MOVE MFS-NUM-FAELT-FEL       TO MOD-REFOERS-IN-ATTR              
107300         MOVE NEJ TO INDATA-SW                                            
107400       END-IF                                                             
107500     END-IF                                                               
107600                                                                          
107700**** KONTROLL AV PRLEGKST ( LEGALISERINGSKOSTNAD )                        
107800                                                                          
107900     IF MID-PRLEGKST-IN NOT = ALL '+'                                     
108000       MOVE MID-PRLEGKST-IN     TO DEC-IDFRIDATA                          
108100       MOVE 7                   TO DEC-KVHELTAL                           
108200       MOVE 2                   TO DEC-KVDECIMAL                          
108300                                                                          
108400       CALL WDECEDIT USING DEC-WDECAREA                                   
108500                                                                          
108600       IF DEC-KDSVAR-OK                                                   
108700         MOVE DEC-IDEDITDATA    TO WS-PRLEGKST                            
108800         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRLEGKST-IN-ATTR                 
108900       ELSE                                                               
109000         MOVE MFS-NUM-FAELT-FEL       TO MOD-PRLEGKST-IN-ATTR             
109100         MOVE NEJ TO INDATA-SW                                            
109200       END-IF                                                             
109300     END-IF                                                               
109400                                                                          
109500     .                                                                    
109600     EJECT                                                                
109700                                                                          
109800 GB-KONTROLLERA-EDIT SECTION.                                             
109900                                                                          
110000******* KONTROLL AV KUNDNR                                                
110100                                                                          
110200     IF MID-KUNDNR-IN NOT = ALL '+'                                       
110300       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
110400       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
110500       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
110600       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
110700       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
110800         MOVE '9999999'           TO MID-KUNDNR-IN                        
110900       END-IF                                                             
111000       IF MID-KUNDNR-IN NUMERIC                                           
111100         MOVE MID-KUNDNR-IN          TO W-IDKUNDNR-COPY                   
111200         IF MID-KUNDNR-IN = 9999999                                       
111300           PERFORM IMS-GU-WDB501-COPY-DEF                                 
111400         ELSE                                                             
111500           PERFORM IMS-GU-WDB501-COPY                                     
111600         END-IF                                                           
111700         IF SEGMENT-FINNS                                                 
111800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
111900           MOVE COPY-FK-IDTRP-0      TO W-IDTRP-0                         
112000           MOVE COPY-FK-IDTRP-1      TO W-IDTRP-1                         
112100           MOVE COPY-FK-IDTRP-2      TO W-IDTRP-2                         
112200           MOVE COPY-FK-IDTRP-3      TO W-IDTRP-3                         
112300           MOVE COPY-FK-IDTRP-4      TO W-IDTRP-4                         
112400         ELSE                                                             
112500           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KUNDNR-IN-ATTR                
112600           MOVE NEJ TO INDATA-SW                                          
112700           MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSINF                 
112800         END-IF                                                           
112900       ELSE                                                               
113000         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KUNDNR-IN-ATTR                
113100         MOVE NEJ TO INDATA-SW                                            
113200       END-IF                                                             
113300     ELSE                                                                 
113400       MOVE MFS-ALFA-FAELT-FEL       TO MOD-KUNDNR-IN-ATTR                
113500       MOVE NEJ TO INDATA-SW                                              
113600     END-IF                                                               
113700                                                                          
113800**** KONTROLL AV KDTRPKAT ( TRANSPORT KATEGORI )                          
113900                                                                          
114000     IF MID-KDTRPKAT-IN NOT = ALL '+'                                     
114100       IF MID-KDTRPKAT-IN = 'A' OR 'B' OR 'C'                             
114200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTRPKAT-IN-ATTR                
114300       ELSE                                                               
114400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDTRPKAT-IN-ATTR                  
114500         MOVE NEJ TO INDATA-SW                                            
114600       END-IF                                                             
114700     END-IF                                                               
114800                                                                          
114900**** KONTROLL AV IDTRP-0 ( TRANSPORT IDENTITET KLASS 0)                   
115000                                                                          
115100     IF INDATA-OK                     AND                                 
115200       (MID-KDTRPKAT-IN = 'A'         OR                                  
115300        MID-KDTRPKAT-IN = 'B')        AND                                 
115400        W-IDTRP-0 = '00000'           AND                                 
115500        MID-IDTRP-0-IN  = ALL '+'                                         
115600        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDTRP-0-IN-ATTR              
115700        MOVE NEJ TO INDATA-SW                                             
115800     ELSE                                                                 
115900       IF MID-IDTRP-0-IN NOT = ALL '+'                                    
116000          MOVE MID-IDTRP-0-IN           TO W-IDTRP                        
116100          PERFORM IMS-GU-XXKA-WDR110                                      
116200          IF SEGMENT-FINNS                                                
116300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-0-IN-ATTR              
116400          ELSE                                                            
116500            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-0-IN-ATTR            
116600            MOVE NEJ TO INDATA-SW                                         
116700            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
116800            CALL WMEDKONV USING MED-WMEDAREA                              
116900            MOVE MED-MFSINF TO MOD-TEMFSINF                               
117000          END-IF                                                          
117100       END-IF                                                             
117200     END-IF                                                               
117300                                                                          
117400**** KONTROLL AV IDTRP-1 ( TRANSPORT IDENTITET KLASS 1)                   
117500                                                                          
117600     IF INDATA-OK                     AND                                 
117700       (MID-KDTRPKAT-IN = 'A'         OR                                  
117800        MID-KDTRPKAT-IN = 'B')        AND                                 
117900        W-IDTRP-1 = '00000'           AND                                 
118000        MID-IDTRP-1-IN  = ALL '+'                                         
118100        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDTRP-1-IN-ATTR              
118200        MOVE NEJ TO INDATA-SW                                             
118300     ELSE                                                                 
118400       IF MID-IDTRP-1-IN NOT = ALL '+'                                    
118500          MOVE MID-IDTRP-1-IN           TO W-IDTRP                        
118600          PERFORM IMS-GU-XXKA-WDR110                                      
118700          IF SEGMENT-FINNS                                                
118800            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-1-IN-ATTR              
118900          ELSE                                                            
119000            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-1-IN-ATTR            
119100            MOVE NEJ TO INDATA-SW                                         
119200            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
119300            CALL WMEDKONV USING MED-WMEDAREA                              
119400            MOVE MED-MFSINF TO MOD-TEMFSINF                               
119500          END-IF                                                          
119600       END-IF                                                             
119700     END-IF                                                               
119800                                                                          
119900**** KONTROLL AV IDTRP-2 ( TRANSPORT IDENTITET KLASS 2)                   
120000                                                                          
120100     IF INDATA-OK                     AND                                 
120200       (MID-KDTRPKAT-IN = 'A'         OR                                  
120300        MID-KDTRPKAT-IN = 'B')        AND                                 
120400        W-IDTRP-2 = '00000'           AND                                 
120500        MID-IDTRP-2-IN  = ALL '+'                                         
120600        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDTRP-2-IN-ATTR              
120700        MOVE NEJ TO INDATA-SW                                             
120800     ELSE                                                                 
120900       IF MID-IDTRP-2-IN NOT = ALL '+'                                    
121000          MOVE MID-IDTRP-2-IN           TO W-IDTRP                        
121100          PERFORM IMS-GU-XXKA-WDR110                                      
121200          IF SEGMENT-FINNS                                                
121300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-2-IN-ATTR              
121400          ELSE                                                            
121500            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-2-IN-ATTR            
121600            MOVE NEJ TO INDATA-SW                                         
121700            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
121800            CALL WMEDKONV USING MED-WMEDAREA                              
121900            MOVE MED-MFSINF TO MOD-TEMFSINF                               
122000          END-IF                                                          
122100       END-IF                                                             
122200     END-IF                                                               
122300                                                                          
122400**** KONTROLL AV IDTRP-3 ( TRANSPORT IDENTITET KLASS 3)                   
122500                                                                          
122600     IF INDATA-OK                     AND                                 
122700       (MID-KDTRPKAT-IN = 'A'         OR                                  
122800        MID-KDTRPKAT-IN = 'B')        AND                                 
122900        W-IDTRP-3 = '00000'           AND                                 
123000        MID-IDTRP-3-IN  = ALL '+'                                         
123100        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDTRP-3-IN-ATTR              
123200        MOVE NEJ TO INDATA-SW                                             
123300     ELSE                                                                 
123400       IF MID-IDTRP-3-IN NOT = ALL '+'                                    
123500          MOVE MID-IDTRP-3-IN           TO W-IDTRP                        
123600          PERFORM IMS-GU-XXKA-WDR110                                      
123700          IF SEGMENT-FINNS                                                
123800            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-3-IN-ATTR              
123900          ELSE                                                            
124000            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-3-IN-ATTR            
124100            MOVE NEJ TO INDATA-SW                                         
124200            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
124300            CALL WMEDKONV USING MED-WMEDAREA                              
124400            MOVE MED-MFSINF TO MOD-TEMFSINF                               
124500          END-IF                                                          
124600       END-IF                                                             
124700     END-IF                                                               
124800                                                                          
124900**** KONTROLL AV IDTRP-4 ( TRANSPORT IDENTITET KLASS 4)                   
125000                                                                          
125100     IF INDATA-OK                     AND                                 
125200       (MID-KDTRPKAT-IN = 'A'         OR                                  
125300        MID-KDTRPKAT-IN = 'B')        AND                                 
125400        W-IDTRP-4 = '00000'           AND                                 
125500        MID-IDTRP-4-IN  = ALL '+'                                         
125600        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDTRP-4-IN-ATTR              
125700        MOVE NEJ TO INDATA-SW                                             
125800     ELSE                                                                 
125900       IF MID-IDTRP-4-IN NOT = ALL '+'                                    
126000          MOVE MID-IDTRP-4-IN           TO W-IDTRP                        
126100          PERFORM IMS-GU-XXKA-WDR110                                      
126200          IF SEGMENT-FINNS                                                
126300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDTRP-4-IN-ATTR              
126400          ELSE                                                            
126500            MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDTRP-4-IN-ATTR            
126600            MOVE NEJ TO INDATA-SW                                         
126700            MOVE ERR-TRPID-NOT-REG TO MED-IDMFSINF                        
126800            CALL WMEDKONV USING MED-WMEDAREA                              
126900            MOVE MED-MFSINF TO MOD-TEMFSINF                               
127000          END-IF                                                          
127100       END-IF                                                             
127200     END-IF                                                               
127300                                                                          
127400**** KONTROLL AV KDFDKRAV ( TRANSPORTFÖRPACKNINGSKOD )                    
127500                                                                          
127600     IF MID-KDFDKRAV-IN NOT = ALL '+'                                     
127700       IF MID-KDFDKRAV-IN NOT NUMERIC                                     
127800         MOVE MFS-NUM-FAELT-FEL       TO MOD-KDFDKRAV-IN-ATTR             
127900         MOVE NEJ TO INDATA-SW                                            
128000       ELSE                                                               
128100         IF MID-KDFDKRAV-IN <= 099                                        
128200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFDKRAV-IN-ATTR               
128300         ELSE                                                             
128400           MOVE MFS-NUM-FAELT-FEL     TO MOD-KDFDKRAV-IN-ATTR             
128500           MOVE NEJ TO INDATA-SW                                          
128600         END-IF                                                           
128700       END-IF                                                             
128800     END-IF                                                               
128900                                                                          
129000**** KONTROLL AV KDGRANS ( GRÄNSKOD )                                     
129100                                                                          
129200     IF MID-KDGRANS-IN NOT = ALL '+'                                      
129300       IF MID-KDGRANS-IN NUMERIC                                          
129400         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDGRANS-IN-ATTR                  
129500       ELSE                                                               
129600         MOVE MFS-NUM-FAELT-FEL       TO MOD-KDGRANS-IN-ATTR              
129700         MOVE NEJ TO INDATA-SW                                            
129800       END-IF                                                             
129900     END-IF                                                               
130000                                                                          
130100**** KONTROLL AV REFOERS ( FÖRSÄKRINGSKOSTNADSFAKTOR )                    
130200                                                                          
130300     IF MID-REFOERS-IN NOT = ALL '+'                                      
130400       MOVE MID-REFOERS-IN      TO DEC-IDFRIDATA                          
130500       MOVE 2                   TO DEC-KVHELTAL                           
130600       MOVE 3                   TO DEC-KVDECIMAL                          
130700                                                                          
130800       CALL WDECEDIT USING DEC-WDECAREA                                   
130900                                                                          
131000       IF DEC-KDSVAR-OK                                                   
131100         MOVE DEC-IDEDITDATA    TO WS-REFOERS                             
131200         MOVE MFS-NUM-FAELT-RAETT TO MOD-REFOERS-IN-ATTR                  
131300       ELSE                                                               
131400         MOVE MFS-NUM-FAELT-FEL       TO MOD-REFOERS-IN-ATTR              
131500         MOVE NEJ TO INDATA-SW                                            
131600       END-IF                                                             
131700     END-IF                                                               
131800                                                                          
131900**** KONTROLL AV PRLEGKST ( LEGALISERINGSKOSTNAD )                        
132000                                                                          
132100     IF MID-PRLEGKST-IN NOT = ALL '+'                                     
132200       MOVE MID-PRLEGKST-IN     TO DEC-IDFRIDATA                          
132300       MOVE 7                   TO DEC-KVHELTAL                           
132400       MOVE 2                   TO DEC-KVDECIMAL                          
132500                                                                          
132600       CALL WDECEDIT USING DEC-WDECAREA                                   
132700                                                                          
132800       IF DEC-KDSVAR-OK                                                   
132900         MOVE DEC-IDEDITDATA    TO WS-PRLEGKST                            
133000         MOVE MFS-NUM-FAELT-RAETT TO MOD-PRLEGKST-IN-ATTR                 
133100       ELSE                                                               
133200         MOVE MFS-NUM-FAELT-FEL       TO MOD-PRLEGKST-IN-ATTR             
133300         MOVE NEJ TO INDATA-SW                                            
133400       END-IF                                                             
133500     END-IF                                                               
133600                                                                          
133700     .                                                                    
133800     EJECT                                                                
133900                                                                          
134000 GC-KONTROLLERA-DELETE SECTION.                                           
134100                                                                          
134200******* KONTROLL AV KUNDNR                                                
134300                                                                          
134400     IF MID-KUNDNR-IN NOT = ALL '+'                                       
134500       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
134600       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
134700       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
134800       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
134900       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
135000         MOVE '9999999'           TO MID-KUNDNR-IN                        
135100       END-IF                                                             
135200       IF MID-KUNDNR-IN NUMERIC                                           
135300        IF MID-KUNDNR-IN = 9999999                                        
135400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                  
135500         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-IN-ATTR                    
135600         MOVE NEJ TO INDATA-SW                                            
135700         MOVE 'DEFAULT ROW CAN NOT BE DELETED ' TO  MOD-TEMFSINF          
135800        ELSE                                                              
135900         MOVE MID-KUNDNR-IN          TO W-IDKUNDNR-COPY                   
136000         PERFORM IMS-GU-WDB501-COPY                                       
136100         IF SEGMENT-FINNS                                                 
136200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
136300         ELSE                                                             
136400           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KUNDNR-IN-ATTR                
136500           MOVE NEJ TO INDATA-SW                                          
136600           MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSINF                 
136700         END-IF                                                           
136800        END-IF                                                            
136900       ELSE                                                               
137000         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KUNDNR-IN-ATTR                
137100         MOVE NEJ TO INDATA-SW                                            
137200       END-IF                                                             
137300     ELSE                                                                 
137400       MOVE MFS-ALFA-FAELT-FEL       TO MOD-KUNDNR-IN-ATTR                
137500       MOVE NEJ TO INDATA-SW                                              
137600     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900                                                                          
138000 GD-KONTROLLERA-COPY SECTION.                                             
138100                                                                          
138200     IF MID-KUNDNR-IN NOT = ALL '+'                                       
138300       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
138400       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
138500       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
138600       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
138700       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
138800         MOVE '9999999'           TO MID-KUNDNR-IN                        
138900       END-IF                                                             
139000       IF MID-KUNDNR-IN NUMERIC                                           
139100         MOVE MID-KUNDNR-IN TO W-IDKUNDNR-COPY                            
139200         IF MID-KUNDNR-IN = 9999999                                       
139300           PERFORM IMS-GU-WDB501-COPY-DEF                                 
139400         ELSE                                                             
139500           PERFORM IMS-GU-WDB501-COPY                                     
139600         END-IF                                                           
139700         IF SEGMENT-FINNS                                                 
139800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
139900         ELSE                                                             
140000           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KUNDNR-IN-ATTR                
140100           MOVE NEJ TO INDATA-SW                                          
140200         END-IF                                                           
140300       ELSE                                                               
140400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR                    
140500         MOVE NEJ TO INDATA-SW                                            
140600       END-IF                                                             
140700     ELSE                                                                 
140800       MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR                      
140900       MOVE NEJ TO INDATA-SW                                              
141000     END-IF                                                               
141100     .                                                                    
141200     EJECT                                                                
141300                                                                          
141400 H-UPPDATERA SECTION.                                                     
141500                                                                          
141600     MOVE MID-KUNDNR-IN TO W-IDKUNDNR                                     
141700                                                                          
141800     PERFORM IMS-GHU-WDB501                                               
141900     IF SEGMENT-SAKNAS                                                    
142000       IF MID-KDCMD-IN = 'N'                                              
142100         PERFORM HA-ISRT-NEW-CUSTOMER                                     
142200       END-IF                                                             
142300     ELSE                                                                 
142400       IF MID-KDCMD-IN = 'N'                                              
142500         IF  GMTC-FK-KDFKTYP = '2'                                        
142600           MOVE '3'         TO GMTC-FK-KDFKTYP                            
142700         END-IF                                                           
142800         PERFORM HB-REPLACE-CUSTOMER                                      
142900       ELSE                                                               
143000         IF MID-KDCMD-IN = 'E'                                            
143100           PERFORM HB-REPLACE-CUSTOMER                                    
143200         ELSE                                                             
143300           IF MID-KDCMD-IN = 'D'                                          
143400             IF GMTC-FK-KDFKTYP = '3'                                     
143500               PERFORM HC-REPL-CUST-FKOD                                  
143600             ELSE                                                         
143700               PERFORM HC-DELETE-CUSTOMER                                 
143800             END-IF                                                       
143900           ELSE                                                           
144000             PERFORM HD-COPY-CUSTOMER                                     
144100           END-IF                                                         
144200         END-IF                                                           
144300       END-IF                                                             
144400     END-IF                                                               
144500                                                                          
144600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
144700     CALL WMEDKONV USING MED-WMEDAREA                                     
144800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
144900     PERFORM MFS-FORM-ATTR                                                
145000* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
145100     .                                                                    
145200     EJECT                                                                
145300                                                                          
145400 HA-ISRT-NEW-CUSTOMER SECTION.                                            
145500                                                                          
145600     MOVE DCS-IDDC            TO GMTC-FK-IDDC                             
145700     MOVE WS-KDFRAKT          TO GMTC-FK-KDFRAKT                          
145800     MOVE WS-IDDISTR          TO GMTC-FK-IDDISTR                          
145900     MOVE MID-KUNDNR-IN       TO GMTC-FK-IDKUNDNR                         
146000                                                                          
146100     MOVE MID-KDTRPKAT-IN     TO GMTC-FK-KDTRPKAT                         
146200                                                                          
146300     IF MID-IDTRP-0-IN NOT = ALL '+'                                      
146400       MOVE MID-IDTRP-0-IN      TO GMTC-FK-IDTRP-0                        
146500     ELSE                                                                 
146600       MOVE '00000'             TO GMTC-FK-IDTRP-0                        
146700     END-IF                                                               
146800                                                                          
146900     IF MID-IDTRP-1-IN NOT = ALL '+'                                      
147000       MOVE MID-IDTRP-1-IN      TO GMTC-FK-IDTRP-1                        
147100     ELSE                                                                 
147200       MOVE '00000'             TO GMTC-FK-IDTRP-1                        
147300     END-IF                                                               
147400                                                                          
147500     IF MID-IDTRP-2-IN NOT = ALL '+'                                      
147600       MOVE MID-IDTRP-2-IN      TO GMTC-FK-IDTRP-2                        
147700     ELSE                                                                 
147800       MOVE '00000'             TO GMTC-FK-IDTRP-2                        
147900     END-IF                                                               
148000                                                                          
148100     IF MID-IDTRP-3-IN NOT = ALL '+'                                      
148200       MOVE MID-IDTRP-3-IN      TO GMTC-FK-IDTRP-3                        
148300     ELSE                                                                 
148400       MOVE '00000'             TO GMTC-FK-IDTRP-3                        
148500     END-IF                                                               
148600                                                                          
148700     IF MID-IDTRP-4-IN NOT = ALL '+'                                      
148800       MOVE MID-IDTRP-4-IN      TO GMTC-FK-IDTRP-4                        
148900     ELSE                                                                 
149000       MOVE '00000'             TO GMTC-FK-IDTRP-4                        
149100     END-IF                                                               
149200                                                                          
149300     MOVE MID-KDFDKRAV-IN     TO GMTC-FK-KDFDKRAV                         
149400                                                                          
149500     IF MID-KDGRANS-IN NOT = ALL '+'                                      
149600       MOVE MID-KDGRANS-IN    TO GMTC-FK-KDGRANS                          
149700     ELSE                                                                 
149800       MOVE +0                TO GMTC-FK-KDGRANS                          
149900     END-IF                                                               
150000                                                                          
150100     IF MID-REFOERS-IN NOT = ALL '+'                                      
150200       MOVE WS-REFOERS        TO GMTC-FK-REFOERS                          
150300     ELSE                                                                 
150400       MOVE +0                TO GMTC-FK-REFOERS                          
150500     END-IF                                                               
150600                                                                          
150700     IF MID-PRLEGKST-IN NOT = ALL '+'                                     
150800       MOVE WS-PRLEGKST       TO GMTC-FK-PRLEGKST                         
150900     ELSE                                                                 
151000       MOVE +0                TO GMTC-FK-PRLEGKST                         
151100     END-IF                                                               
151200                                                                          
151300     MOVE SPACE               TO GMTC-FK-BEGMRK                           
151400                                                                          
151500     IF MID-KUNDNR-IN = 9999999                                           
151600       MOVE '0'               TO GMTC-FK-KDFKTYP                          
151700     ELSE                                                                 
151800       MOVE '1'               TO GMTC-FK-KDFKTYP                          
151900       PERFORM IMS-GU-WDB501-DEFAULT                                      
152000       IF SEGMENT-FINNS                                                   
152100         MOVE DEF-FK-BEGMRK     TO GMTC-FK-BEGMRK                         
152200       END-IF                                                             
152300     END-IF                                                               
152400                                                                          
152500     PERFORM IMS-ISRT-WDB501                                              
152600     PERFORM MFS-RENSA-FAELT-IN                                           
152700     .                                                                    
152800     EJECT                                                                
152900                                                                          
153000 HB-REPLACE-CUSTOMER SECTION.                                             
153100                                                                          
153200     IF MID-KDTRPKAT-IN NOT =    ALL '+'                                  
153300       MOVE MID-KDTRPKAT-IN TO GMTC-FK-KDTRPKAT                           
153400     END-IF                                                               
153500                                                                          
153600     IF MID-IDTRP-0-IN NOT = ALL '+'                                      
153700       MOVE MID-IDTRP-0-IN    TO GMTC-FK-IDTRP-0                          
153800     ELSE                                                                 
153900       IF MID-KDCMD-IN = 'N'                                              
154000         MOVE '00000'         TO GMTC-FK-IDTRP-0                          
154100       END-IF                                                             
154200     END-IF                                                               
154300                                                                          
154400     IF MID-IDTRP-1-IN NOT = ALL '+'                                      
154500       MOVE MID-IDTRP-1-IN    TO GMTC-FK-IDTRP-1                          
154600     ELSE                                                                 
154700       IF MID-KDCMD-IN = 'N'                                              
154800         MOVE '00000'         TO GMTC-FK-IDTRP-1                          
154900       END-IF                                                             
155000     END-IF                                                               
155100                                                                          
155200     IF MID-IDTRP-2-IN NOT = ALL '+'                                      
155300       MOVE MID-IDTRP-2-IN    TO GMTC-FK-IDTRP-2                          
155400     ELSE                                                                 
155500       IF MID-KDCMD-IN = 'N'                                              
155600         MOVE '00000'         TO GMTC-FK-IDTRP-2                          
155700       END-IF                                                             
155800     END-IF                                                               
155900                                                                          
156000     IF MID-IDTRP-3-IN NOT = ALL '+'                                      
156100       MOVE MID-IDTRP-3-IN    TO GMTC-FK-IDTRP-3                          
156200     ELSE                                                                 
156300       IF MID-KDCMD-IN = 'N'                                              
156400         MOVE '00000'         TO GMTC-FK-IDTRP-3                          
156500       END-IF                                                             
156600     END-IF                                                               
156700                                                                          
156800     IF MID-IDTRP-4-IN NOT = ALL '+'                                      
156900       MOVE MID-IDTRP-4-IN    TO GMTC-FK-IDTRP-4                          
157000     ELSE                                                                 
157100       IF MID-KDCMD-IN = 'N'                                              
157200         MOVE '00000'         TO GMTC-FK-IDTRP-4                          
157300       END-IF                                                             
157400     END-IF                                                               
157500                                                                          
157600     IF MID-KDFDKRAV-IN NOT = ALL '+'                                     
157700       MOVE MID-KDFDKRAV-IN TO GMTC-FK-KDFDKRAV                           
157800     END-IF                                                               
157900                                                                          
158000     IF MID-KDGRANS-IN NOT = ALL '+'                                      
158100       MOVE MID-KDGRANS-IN TO GMTC-FK-KDGRANS                             
158200     ELSE                                                                 
158300       IF MID-KDCMD-IN = 'N'                                              
158400         MOVE 0               TO GMTC-FK-KDGRANS                          
158500       END-IF                                                             
158600     END-IF                                                               
158700                                                                          
158800     IF MID-REFOERS-IN NOT = ALL '+'                                      
158900       MOVE WS-REFOERS TO GMTC-FK-REFOERS                                 
159000     ELSE                                                                 
159100       IF MID-KDCMD-IN = 'N'                                              
159200         MOVE 0               TO GMTC-FK-REFOERS                          
159300       END-IF                                                             
159400     END-IF                                                               
159500                                                                          
159600     IF MID-PRLEGKST-IN NOT = ALL '+'                                     
159700       MOVE WS-PRLEGKST TO GMTC-FK-PRLEGKST                               
159800     ELSE                                                                 
159900       IF MID-KDCMD-IN = 'N'                                              
160000         MOVE 0               TO GMTC-FK-PRLEGKST                         
160100       END-IF                                                             
160200     END-IF                                                               
160300                                                                          
160400     MOVE GMTC-FK-KDTRPKAT  TO W-KDTRPKAT                                 
160500     MOVE GMTC-FK-IDTRP-0   TO W-IDTRP-0                                  
160600     MOVE GMTC-FK-IDTRP-1   TO W-IDTRP-1                                  
160700     MOVE GMTC-FK-IDTRP-2   TO W-IDTRP-2                                  
160800     MOVE GMTC-FK-IDTRP-3   TO W-IDTRP-3                                  
160900     MOVE GMTC-FK-IDTRP-4   TO W-IDTRP-4                                  
161000     MOVE GMTC-FK-KDFDKRAV  TO W-KDFDKRAV                                 
161100     MOVE GMTC-FK-KDGRANS   TO W-KDGRANS                                  
161200     MOVE GMTC-FK-REFOERS   TO W-REFOERS                                  
161300     MOVE GMTC-FK-PRLEGKST  TO W-PRLEGKST                                 
161400                                                                          
161500     PERFORM IMS-REPL-WDB501                                              
161600                                                                          
161700     IF MID-KUNDNR-IN = 9999999                                           
161800        PERFORM HBA-REPLACE-ALLA-KUNDER                                   
161900     END-IF                                                               
162000                                                                          
162100     PERFORM MFS-RENSA-FAELT-IN                                           
162200     .                                                                    
162300     EJECT                                                                
162400                                                                          
162500 HBA-REPLACE-ALLA-KUNDER SECTION.                                         
162600                                                                          
162700     MOVE ZERO TO W-IDKUNDNR-MIN                                          
162800     PERFORM IMS-GHU-WDB501-FIRST                                         
162900     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
163000             BAS-SLUT                                                     
163100                                                                          
163200       MOVE W-KDTRPKAT  TO GMTC-FK-KDTRPKAT                               
163300       MOVE W-IDTRP-0   TO GMTC-FK-IDTRP-0                                
163400       MOVE W-IDTRP-1   TO GMTC-FK-IDTRP-1                                
163500       MOVE W-IDTRP-2   TO GMTC-FK-IDTRP-2                                
163600       MOVE W-IDTRP-3   TO GMTC-FK-IDTRP-3                                
163700       MOVE W-IDTRP-4   TO GMTC-FK-IDTRP-4                                
163800       MOVE W-KDFDKRAV  TO GMTC-FK-KDFDKRAV                               
163900       MOVE W-KDGRANS   TO GMTC-FK-KDGRANS                                
164000       MOVE W-REFOERS   TO GMTC-FK-REFOERS                                
164100       MOVE W-PRLEGKST  TO GMTC-FK-PRLEGKST                               
164200                                                                          
164300       PERFORM IMS-REPL-WDB501                                            
164400                                                                          
164500       PERFORM IMS-GHN-WDB501-NEXT                                        
164600     END-PERFORM                                                          
164700     .                                                                    
164800     EJECT                                                                
164900                                                                          
165000 HC-REPL-CUST-FKOD SECTION.                                               
165100                                                                          
165200     PERFORM IMS-GU-WDB501-DEFAULT                                        
165300     IF SEGMENT-FINNS                                                     
165400       MOVE DEF-FK-IDTRP-0    TO GMTC-FK-IDTRP-0                          
165500       MOVE DEF-FK-IDTRP-1    TO GMTC-FK-IDTRP-1                          
165600       MOVE DEF-FK-IDTRP-2    TO GMTC-FK-IDTRP-2                          
165700       MOVE DEF-FK-IDTRP-3    TO GMTC-FK-IDTRP-3                          
165800       MOVE DEF-FK-IDTRP-4    TO GMTC-FK-IDTRP-4                          
165900       MOVE DEF-FK-KDTRPKAT   TO GMTC-FK-KDTRPKAT                         
166000       MOVE DEF-FK-KDFDKRAV   TO GMTC-FK-KDFDKRAV                         
166100       MOVE DEF-FK-KDGRANS    TO GMTC-FK-KDGRANS                          
166200       MOVE DEF-FK-PRLEGKST   TO GMTC-FK-PRLEGKST                         
166300       MOVE DEF-FK-REFOERS    TO GMTC-FK-REFOERS                          
166400     END-IF                                                               
166500                                                                          
166600     MOVE '2'         TO GMTC-FK-KDFKTYP                                  
166700                                                                          
166800     PERFORM IMS-REPL-WDB501                                              
166900     PERFORM MFS-RENSA-FAELT-IN                                           
167000     .                                                                    
167100     EJECT                                                                
167200                                                                          
167300 HC-DELETE-CUSTOMER SECTION.                                              
167400                                                                          
167500     PERFORM IMS-DLET-WDB501                                              
167600     PERFORM MFS-RENSA-FAELT-IN                                           
167700     .                                                                    
167800     EJECT                                                                
167900                                                                          
168000 HD-COPY-CUSTOMER SECTION.                                                
168100                                                                          
168200                                                                          
168300     MOVE MFS-RENSA-FAELT     TO MOD-KUNDNR-IN                            
168400                                 MOD-KDCMD-IN                             
168500                                                                          
168600     MOVE COPY-FK-KDTRPKAT    TO MOD-KDTRPKAT-IN                          
168700     MOVE COPY-FK-IDTRP-0     TO MOD-IDTRP-0-IN                           
168800     MOVE COPY-FK-IDTRP-1     TO MOD-IDTRP-1-IN                           
168900     MOVE COPY-FK-IDTRP-2     TO MOD-IDTRP-2-IN                           
169000     MOVE COPY-FK-IDTRP-3     TO MOD-IDTRP-3-IN                           
169100     MOVE COPY-FK-IDTRP-4     TO MOD-IDTRP-4-IN                           
169200     MOVE COPY-FK-KDGRANS     TO MOD-KDGRANS-IN                           
169300     MOVE COPY-FK-KDFDKRAV    TO MOD-KDFDKRAV-IN                          
169400     MOVE COPY-FK-REFOERS     TO WS-REFOERS-COPY                          
169500     MOVE WS-REFOERS-COPY     TO MOD-REFOERS-IN                           
169600     MOVE COPY-FK-PRLEGKST    TO WS-PRLEGKST-COPY                         
169700     MOVE WS-PRLEGKST-COPY    TO MOD-PRLEGKST-IN                          
169800                                                                          
169900     PERFORM MFS-LAES-IN-IGEN                                             
170000     .                                                                    
170100     EJECT                                                                
170200                                                                          
170300 S01-KOLLA-OM-GAELLANDE-KUND SECTION.                                     
170400                                                                          
170500     PERFORM IMS-GU-WDB201                                                
170600     IF SEGMENT-FINNS                                                     
170700*      IF GMTA-GMT-TISTADAT > ZERO                                        
170800*        IF GMTA-GMT-TISTODAT > ZERO                                      
170900*          MOVE GMTA-GMT-TISTODAT   TO TMP1-YYMMDD                        
171000*          MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
171100*          PERFORM WY2000P1                                               
171200*          IF TMP1-YYMMDD > TMP2-YYMMDD                                   
171300*            MOVE JA TO GAELLANDE-SW                                      
171400*          ELSE                                                           
171500*            MOVE NEJ TO GAELLANDE-SW                                     
171600*          END-IF                                                         
171700*        ELSE                                                             
171800*          MOVE JA TO GAELLANDE-SW                                        
171900*        END-IF                                                           
172000*      ELSE                                                               
172100*        MOVE NEJ TO GAELLANDE-SW                                         
172200*      END-IF                                                             
172300       MOVE JA TO GAELLANDE-SW                                            
172400     ELSE                                                                 
172500       MOVE NEJ TO GAELLANDE-SW                                           
172600     END-IF                                                               
172700     .                                                                    
172800     EJECT                                                                
172900                                                                          
173000 S10-SAMMA-SIDA-EFTER-UPPDAT SECTION.                                     
173100                                                                          
173200***************** FÖR ATT VISA SAMMA SIDA VID UPPDATERING *****           
173300                                                                          
173400     PERFORM MFS-RENSA-FAELT-UT                                           
173500     IF EGEN-MID OR HELP-MID                                              
173600       IF SPAR-IDTRANS = '4417'                                           
173700         MOVE SPAR-WDB501KY-ENTER                                         
173800                             TO NYCKLAR-TILL-BLAEDDRING                   
173900         MOVE DCS-IDDC             TO W-IDDC                              
174000                                    W-IDDC-MIN                            
174100                                    W-IDDC-MAX                            
174200                                    W-IDDC4431                            
174300         MOVE W-MINKEY-KDFRAKT TO W-KDFRAKT                               
174400                                    W-KDFRAKT-MIN                         
174500                                    W-KDFRAKT-MAX                         
174600         MOVE W-MINKEY-IDDISTR   TO W-IDDISTR                             
174700                                    W-IDDISTR-MIN                         
174800                                    W-IDDISTR-MAX                         
174900                                    W-IDDISTR-WDB2                        
175000         MOVE W-MINKEY-IDKUNDNR  TO W-IDKUNDNR                            
175100                                    W-IDKUNDNR-MIN                        
175200       END-IF                                                             
175300     END-IF                                                               
175400     .                                                                    
175500     EJECT                                                                
175600                                                                          
175700 MFS-RENSA-FAELT-UT SECTION.                                              
175800                                                                          
175900*    --- ALLA UTDATA-FÄLT                                                 
176000     MOVE MFS-RENSA-FAELT TO MOD-KDTRPKAT-UT                              
176100                             MOD-IDTRP-0-UT                               
176200                             MOD-IDTRP-1-UT                               
176300                             MOD-IDTRP-2-UT                               
176400                             MOD-IDTRP-3-UT                               
176500                             MOD-IDTRP-4-UT                               
176600                             MOD-KDFDKRAV-UT                              
176700                             MOD-KDGRANS-UT                               
176800                             MOD-REFOERS-UT                               
176900                             MOD-PRLEGKST-UT                              
177000     MOVE +1 TO INDX                                                      
177100     PERFORM UNTIL INDX > MAX-INDX                                        
177200       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
177300       ADD +1 TO INDX                                                     
177400     END-PERFORM                                                          
177500     .                                                                    
177600     SKIP3                                                                
177700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
177800                                                                          
177900     MOVE MFS-RENSA-FAELT  TO MOD-IDKUNDNR     (INDX)                     
178000                              MOD-KDTRPKAT     (INDX)                     
178100                              MOD-IDTRP-0      (INDX)                     
178200                              MOD-IDTRP-1      (INDX)                     
178300                              MOD-IDTRP-2      (INDX)                     
178400                              MOD-IDTRP-3      (INDX)                     
178500                              MOD-IDTRP-4      (INDX)                     
178600                              MOD-KDFDKRAV     (INDX)                     
178700                              MOD-KDGRANS      (INDX)                     
178800                              MOD-REFOERS      (INDX)                     
178900                              MOD-PRLEGKST     (INDX)                     
179000     .                                                                    
179100     EJECT                                                                
179200                                                                          
179300 MFS-RENSA-FAELT-IN SECTION.                                              
179400                                                                          
179500*    --- ALLA INDATA-FÄLT                                                 
179600     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IN                                 
179700                             MOD-KUNDNR-IN                                
179800                             MOD-KDTRPKAT-IN                              
179900                             MOD-IDTRP-0-IN                               
180000                             MOD-IDTRP-1-IN                               
180100                             MOD-IDTRP-2-IN                               
180200                             MOD-IDTRP-3-IN                               
180300                             MOD-IDTRP-4-IN                               
180400                             MOD-KDFDKRAV-IN                              
180500                             MOD-KDGRANS-IN                               
180600                             MOD-REFOERS-IN                               
180700                             MOD-PRLEGKST-IN                              
180800     .                                                                    
180900     EJECT                                                                
181000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
181100                                                                          
181200*    --- ALLA UTDATA-FÄLT                                                 
181300                                                                          
181400*    --- ALLA UTDATA-FÄLT                                                 
181500     MOVE MFS-ROER-EJ-FAELT TO MOD-KDTRPKAT-UT                            
181600                               MOD-IDTRP-0-UT                             
181700                               MOD-IDTRP-1-UT                             
181800                               MOD-IDTRP-2-UT                             
181900                               MOD-IDTRP-3-UT                             
182000                               MOD-IDTRP-4-UT                             
182100                               MOD-KDFDKRAV-UT                            
182200                               MOD-KDGRANS-UT                             
182300                               MOD-REFOERS-UT                             
182400                               MOD-PRLEGKST-UT                            
182500                                                                          
182600     MOVE +1 TO INDX                                                      
182700     PERFORM UNTIL INDX > MAX-INDX                                        
182800       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
182900       ADD +1 TO INDX                                                     
183000     END-PERFORM                                                          
183100     .                                                                    
183200     SKIP3                                                                
183300 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
183400                                                                          
183500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR     (INDX)                    
183600                               MOD-KDTRPKAT     (INDX)                    
183700                               MOD-IDTRP-0      (INDX)                    
183800                               MOD-IDTRP-1      (INDX)                    
183900                               MOD-IDTRP-2      (INDX)                    
184000                               MOD-IDTRP-3      (INDX)                    
184100                               MOD-IDTRP-4      (INDX)                    
184200                               MOD-KDFDKRAV     (INDX)                    
184300                               MOD-KDGRANS      (INDX)                    
184400                               MOD-REFOERS      (INDX)                    
184500                               MOD-PRLEGKST     (INDX)                    
184600     .                                                                    
184700     EJECT                                                                
184800                                                                          
184900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
185000                                                                          
185100*    --- ALLA INDATA-FÄLT                                                 
185200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN                               
185300                               MOD-KUNDNR-IN                              
185400                               MOD-KDTRPKAT-IN                            
185500                               MOD-IDTRP-0-IN                             
185600                               MOD-IDTRP-1-IN                             
185700                               MOD-IDTRP-2-IN                             
185800                               MOD-IDTRP-3-IN                             
185900                               MOD-IDTRP-4-IN                             
186000                               MOD-KDFDKRAV-IN                            
186100                               MOD-KDGRANS-IN                             
186200                               MOD-REFOERS-IN                             
186300                               MOD-PRLEGKST-IN                            
186400     .                                                                    
186500     EJECT                                                                
186600 MFS-FORM-ATTR SECTION.                                                   
186700                                                                          
186800*    --- ALLA INDATA-FÄLT                                                 
186900     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-IN-ATTR                         
187000                                MOD-KUNDNR-IN-ATTR                        
187100                                MOD-KDTRPKAT-IN-ATTR                      
187200                                MOD-IDTRP-0-IN-ATTR                       
187300                                MOD-IDTRP-1-IN-ATTR                       
187400                                MOD-IDTRP-2-IN-ATTR                       
187500                                MOD-IDTRP-3-IN-ATTR                       
187600                                MOD-IDTRP-4-IN-ATTR                       
187700                                MOD-KDFDKRAV-IN-ATTR                      
187800                                MOD-KDGRANS-IN-ATTR                       
187900                                MOD-REFOERS-IN-ATTR                       
188000                                MOD-PRLEGKST-IN-ATTR                      
188100     .                                                                    
188200     SKIP2                                                                
188300 MFS-LAES-IN-IGEN SECTION.                                                
188400                                                                          
188500*    --- ALLA INDATA-FÄLT                                                 
188600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                      
188700                                   MOD-KUNDNR-IN-ATTR                     
188800                                   MOD-KDTRPKAT-IN-ATTR                   
188900                                   MOD-IDTRP-0-IN-ATTR                    
189000                                   MOD-IDTRP-1-IN-ATTR                    
189100                                   MOD-IDTRP-2-IN-ATTR                    
189200                                   MOD-IDTRP-3-IN-ATTR                    
189300                                   MOD-IDTRP-4-IN-ATTR                    
189400                                   MOD-KDFDKRAV-IN-ATTR                   
189500                                   MOD-KDGRANS-IN-ATTR                    
189600                                   MOD-REFOERS-IN-ATTR                    
189700                                   MOD-PRLEGKST-IN-ATTR                   
189800     .                                                                    
189900     EJECT                                                                
190000* --- IMS SEKTIONER ---                                                   
190100     SKIP3                                                                
190200 IMS-GET-MSG SECTION.                                                     
190300                                                                          
190400     MOVE '  QC' TO GODK-STATUSKODER                                      
190500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
190600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
190700     PERFORM IMS-STATUSKONTROLL                                           
190800     .                                                                    
190900     SKIP3                                                                
191000 IMS-INSERT-MSG SECTION.                                                  
191100                                                                          
191200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
191300       MOVE 'N' TO MFS-KDHUVOMR                                           
191400     END-IF                                                               
191500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
191600     MOVE SPACE TO GODK-STATUSKODER                                       
191700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
191800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
191900     PERFORM IMS-STATUSKONTROLL                                           
192000     .                                                                    
192100     EJECT                                                                
192200 IMS-GU-WDB501-DEFAULT SECTION.                                           
192300                                                                          
192400     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-DEF-X ')'                    
192500          DELIMITED BY SIZE INTO SSA1                                     
192600     MOVE '  GE' TO GODK-STATUSKODER                                      
192700     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-2 SSA1                    
192800     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
192900     PERFORM IMS-STATUSKONTROLL                                           
193000     .                                                                    
193100     SKIP2                                                                
193200 IMS-GU-WDB501-COPY-DEF SECTION.                                          
193300                                                                          
193400     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-COPY-X ')'                   
193500          DELIMITED BY SIZE INTO SSA1                                     
193600     MOVE '  GE' TO GODK-STATUSKODER                                      
193700     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-1 SSA1                    
193800     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
193900     PERFORM IMS-STATUSKONTROLL                                           
194000     .                                                                    
194100     SKIP2                                                                
194200 IMS-GU-WDB501-COPY SECTION.                                              
194300                                                                          
194400*BS  STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-COPY-X ')'                   
194500     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-COPY-X                       
194600                    '&KDFKTYP  =' W-KDFKTYP-X                             
194700                    '+WDB501KY =' W-WDB501KY-COPY-X                       
194800                    '&KDFKTYP  =' W-KDFKTYP-BADA-X ')'                    
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     MOVE '  GE' TO GODK-STATUSKODER                                      
195100     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-1 SSA1                    
195200     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
195300     PERFORM IMS-STATUSKONTROLL                                           
195400     .                                                                    
195500     SKIP2                                                                
195600 IMS-GHU-WDB501 SECTION.                                                  
195700                                                                          
195800     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X ')'                        
195900          DELIMITED BY SIZE INTO SSA1                                     
196000     MOVE '  GE' TO GODK-STATUSKODER                                      
196100     CALL CBLTDLI USING GHU GMTCU-PCB DLI-IO-AREA SSA1                    
196200     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
196300     PERFORM IMS-STATUSKONTROLL                                           
196400     .                                                                    
196500     SKIP2                                                                
196600 IMS-GN-WDB501 SECTION.                                                   
196700                                                                          
196800     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
196900                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
197000                    '&KDFKTYP  =' W-KDFKTYP-X                             
197100                    '+WDB501KY>=' W-WDB501KY-MIN-X                        
197200                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
197300                    '&KDFKTYP  =' W-KDFKTYP-BADA-X ')'                    
197400          DELIMITED BY SIZE INTO SSA1                                     
197500     MOVE '  GE' TO GODK-STATUSKODER                                      
197600     CALL CBLTDLI USING GN GMTC-PCB DLI-IO-AREA SSA1                      
197700     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
197800     PERFORM IMS-STATUSKONTROLL                                           
197900     .                                                                    
198000     SKIP2                                                                
198100 IMS-GU-WDB501 SECTION.                                                   
198200                                                                          
198300     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
198400                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
198500                    '&KDFKTYP  =' W-KDFKTYP-X                             
198600                    '+WDB501KY>=' W-WDB501KY-MIN-X                        
198700                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
198800                    '&KDFKTYP  =' W-KDFKTYP-BADA-X ')'                    
198900          DELIMITED BY SIZE INTO SSA1                                     
199000     MOVE '  GE' TO GODK-STATUSKODER                                      
199100     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA SSA1                      
199200     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
199300     PERFORM IMS-STATUSKONTROLL                                           
199400     .                                                                    
199500     SKIP2                                                                
199600 IMS-GHN-WDB501-NEXT SECTION.                                             
199700                                                                          
199800     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
199900                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
200000                    '&KDFKTYP  =' W-KDFKTYP-2-X ')'                       
200100          DELIMITED BY SIZE INTO SSA1                                     
200200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
200300     CALL CBLTDLI USING GHN GMTCU-PCB DLI-IO-AREA SSA1                    
200400     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
200500     PERFORM IMS-STATUSKONTROLL                                           
200600     .                                                                    
200700     SKIP2                                                                
200800 IMS-GHU-WDB501-FIRST SECTION.                                            
200900                                                                          
201000     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
201100                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
201200                    '&KDFKTYP  =' W-KDFKTYP-2-X ')'                       
201300          DELIMITED BY SIZE INTO SSA1                                     
201400     MOVE '  GE' TO GODK-STATUSKODER                                      
201500     CALL CBLTDLI USING GHU GMTCU-PCB DLI-IO-AREA SSA1                    
201600     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
201700     PERFORM IMS-STATUSKONTROLL                                           
201800     .                                                                    
201900     EJECT                                                                
202000 IMS-ISRT-WDB501 SECTION.                                                 
202100                                                                          
202200     MOVE 'WLGMTC01 ' TO SSA1                                             
202300     MOVE '  II' TO GODK-STATUSKODER                                      
202400     CALL CBLTDLI USING ISRT GMTCU-PCB DLI-IO-AREA SSA1                   
202500     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
202600     PERFORM IMS-STATUSKONTROLL                                           
202700     .                                                                    
202800     SKIP3                                                                
202900 IMS-REPL-WDB501 SECTION.                                                 
203000                                                                          
203100     MOVE '  ' TO GODK-STATUSKODER                                        
203200     CALL CBLTDLI USING REPL GMTCU-PCB DLI-IO-AREA                        
203300     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
203400     PERFORM IMS-STATUSKONTROLL                                           
203500     .                                                                    
203600     SKIP3                                                                
203700 IMS-DLET-WDB501 SECTION.                                                 
203800                                                                          
203900     MOVE '  ' TO GODK-STATUSKODER                                        
204000     CALL CBLTDLI USING DLET GMTCU-PCB DLI-IO-AREA                        
204100     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
204200     PERFORM IMS-STATUSKONTROLL                                           
204300     .                                                                    
204400     EJECT                                                                
204500 IMS-GU-XXKA-WDR110 SECTION.                                              
204600                                                                          
204700     STRING 'WLXXKA01(WDGXKEY  =' W-WDGX4431-X ')'                        
204800          DELIMITED BY SIZE INTO SSA1                                     
204900     STRING 'WLXXKA11(WDGXKEY  =' W-WDGX4432-X ')'                        
205000          DELIMITED BY SIZE INTO SSA2                                     
205100     MOVE '  GE' TO GODK-STATUSKODER                                      
205200     CALL CBLTDLI USING GHU XXKA-PCB DLI-IO-AREA-3 SSA1 SSA2              
205300     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
205400     PERFORM IMS-STATUSKONTROLL                                           
205500     .                                                                    
205600     SKIP3                                                                
205700 IMS-GU-WDB201 SECTION.                                                   
205800                                                                          
205900     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
206000          DELIMITED BY SIZE INTO SSA1                                     
206100     MOVE '  GE' TO GODK-STATUSKODER                                      
206200     CALL CBLTDLI USING GHU GMTA-PCB DLI-IO-AREA-4 SSA1                   
206300     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
206400     PERFORM IMS-STATUSKONTROLL                                           
206500     .                                                                    
206600     SKIP2                                                                
206700 IMS-GU-WDB601    SECTION.                                                
206800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
206900          DELIMITED BY SIZE INTO SSA1                                     
207000     MOVE '  GE' TO GODK-STATUSKODER                                      
207100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
207200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
207300     PERFORM IMS-STATUSKONTROLL                                           
207400     IF SEGMENT-SAKNAS                                                    
207500         MOVE SPACE TO DCS-KDDC                                           
207600     END-IF                                                               
207700     .                                                                    
207800 IMS-STATUSKONTROLL SECTION.                                              
207900                                                                          
208000     SET STATUS-IX TO 1                                                   
208100     SEARCH GODK-STATUS                                                   
208200       AT END                                                             
208300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
208400         DELIMITED BY SIZE INTO FELTEXT                                   
208500         CALL FELLOG                                                      
208600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
208700         CONTINUE                                                         
208800     END-SEARCH                                                           
208900     .                                                                    
209000     EJECT                                                                
