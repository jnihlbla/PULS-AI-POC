000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041800.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   96/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV FRAKT INFORMATION.                                
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLGMTC (WDB5)                              
001010*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001100*                              WDB6                                       
001110*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T418                                              
001400*        MID:         W4I41801                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O41801                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002301*    -COPY WY2000W1                                                       
002310     SKIP3                                                                
002400 77  IDPGM                       PIC X(08)   VALUE 'W4041800'.            
002500                                                                          
002600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002800                                                                          
002900 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
003000 77  MAX-INDX                    PIC S9(4)   VALUE +11 COMP SYNC.         
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003210 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003300                                                                          
003400 77  WS-KDFRAKT                  PIC S9(3)   VALUE ZERO COMP-3.           
003500 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO COMP-3.           
003600 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO COMP-3.           
003610                                                                          
003620 77  W-BEGMRK                    PIC X(60)   VALUE SPACE.                 
003700                                                                          
003800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003900                                                                          
004000 77  GAELLANDE-SW                PIC X       VALUE 'J'.                   
004100     88  GAELLANDE-KUND                      VALUE 'J'.                   
004200     88  STOPPAD-KUND                        VALUE 'N'.                   
004300                                                                          
004310 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004320     88  INDATA-OK                           VALUE 'J'.                   
004330     88  INDATA-FEL                          VALUE 'N'.                   
004340                                                                          
004400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004500     88  NYCKLAR-OK                          VALUE 'J'.                   
004600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '4418'.                
005700     88  GODK-MID                            VALUE '4411' '4412'          
005800                                                   '4413' '4414'          
005900                                                   '4415' '4416'          
006000                                                   '4417' '4418'          
006100                                                   '4419'.                
006200     88  NYCKEL-MID                          VALUE '4415' '4417'.         
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA'.            
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008500     03  ERR-TRPID-NOT-REG       PIC X(3)    VALUE '042'.                 
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009000     SKIP3                                                                
009100*01 -COPY WMSGINIT                                                        
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
009400     SKIP3                                                                
009500*01 -COPY WDECAREA                                                        
009600     SKIP3                                                                
009700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010000     SKIP3                                                                
010100*01  MID -COPY W4I41801                                                   
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010400     SKIP3                                                                
010500*01  -COPY WMSGAREA                                                       
010600     EJECT                                                                
010700     03  MOD REDEFINES MSG-AREA.                                          
010800*      05  -COPY W4O41801                                                 
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011100     SKIP3                                                                
011200*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800     SKIP3                                                                
011810 01  SPAR-AREA.                                                           
011820     03  SPAR-IDTRANS            PIC X(4)    VALUE SPACE.                 
011830     03  SPAR-WDB501KY-ENTER     PIC X(11)   VALUE SPACE.                 
011840     03  SPAR-WDB501KY-NEXT      PIC X(11)   VALUE SPACE.                 
011850     SKIP3                                                                
011900 01  NYCKLAR-TILL-BLAEDDRING.                                             
012000     03  W-MINKEY-IDDC           PIC X(2)    VALUE SPACE.                 
012100     03  W-MINKEY-KDFRAKT        PIC S9(3)   VALUE ZERO COMP-3.           
012200     03  W-MINKEY-IDDISTR        PIC S9(5)   VALUE ZERO COMP-3.           
012300     03  W-MINKEY-IDKUNDNR       PIC S9(7)   VALUE ZERO COMP-3.           
012400                                                                          
012500     SKIP3                                                                
012600 01  NYCKLAR-TILL-DLI.                                                    
012700     03  W-WDB501KY-X.                                                    
012800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012900         05  W-KDFRAKT           PIC S9(3)   VALUE ZERO COMP-3.           
013000         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
013100         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
013200     SKIP2                                                                
013300     03  W-WDB501KY-MIN-X.                                                
013400         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
013500         05  W-KDFRAKT-MIN       PIC S9(3)   VALUE ZERO COMP-3.           
013600         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
013700         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
013800     SKIP2                                                                
013900     03  W-WDB501KY-MAX-X.                                                
014000         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
014100         05  W-KDFRAKT-MAX       PIC S9(3)   VALUE ZERO COMP-3.           
014200         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
014300         05  W-IDKUNDNR-MAX      PIC S9(7) COMP-3 VALUE +9999999.         
014400     EJECT                                                                
014500     03  W-WDB501KY-COPY-X.                                               
014600         05  W-IDDC-COPY         PIC X(2)    VALUE SPACE.                 
014700         05  W-KDFRAKT-COPY      PIC S9(3)   VALUE ZERO COMP-3.           
014800         05  W-IDDISTR-COPY      PIC S9(5)   VALUE ZERO COMP-3.           
014900         05  W-IDKUNDNR-COPY     PIC S9(7)   VALUE ZERO COMP-3.           
015000     SKIP2                                                                
015100     03  W-WDB501KY-DEF-X.                                                
015200         05  W-IDDC-DEF          PIC X(2)    VALUE SPACE.                 
015300         05  W-KDFRAKT-DEF       PIC S9(3)   VALUE ZERO COMP-3.           
015400         05  W-IDDISTR-DEF       PIC S9(5)   VALUE ZERO COMP-3.           
015500         05  W-IDKUNDNR-DEF      PIC S9(7) VALUE +9999999 COMP-3.         
015600     SKIP3                                                                
015700     03  W-IDGMT-X.                                                       
015800         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
015810         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
015900                                                                          
015910     03  W-KDFKTYP-1-X.                                                   
015920         05  W-KDFKTYP-1         PIC X(1)    VALUE '1'.                   
015930                                                                          
015940     03  W-KDFKTYP-X.                                                     
015950         05  W-KDFKTYP           PIC X(1)    VALUE '2'.                   
015960                                                                          
016000     03  W-KDFKTYP-BADA-X.                                                
016100         05  W-KDFKTYP-BADA      PIC X(1)    VALUE '3'.                   
016110                                                                          
016120     03  W-IDDC-B6-X.                                                     
016130         05 W-IDDC-B6                  PIC X(2).                          
016200     EJECT                                                                
016300                                                                          
016400*    --- STATUS-KOD FRÅN IMS                                              
016500 01  STATUS-WS                   PIC XX.                                  
016600     88  SEGMENT-FINNS                       VALUE '  '.                  
016700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016810     88  BAS-SLUT                            VALUE 'GB'.                  
016900     SKIP2                                                                
017000 01  GODK-STATUSKODER.                                                    
017100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200     SKIP3                                                                
017300 01  SSA1                        PIC X(128).                              
017400 01  SSA2                        PIC X(64).                               
017500     EJECT                                                                
017600*    --- IMS FUNKTIONSKODER                                               
017700*01  -COPY W0003                                                          
017800     EJECT                                                                
017900*    ---  DLI INPUT-OUTPUT AREA                                           
018000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018100     SKIP2                                                                
018200 01  FILLER                      PIC X(16)   VALUE 'WDB501-AREAR'.        
018300 01  DLI-IO-AREA.                                                         
018700*    03  -COPY WDB501  -PRE GMTC-                                         
018800     EJECT                                                                
018900                                                                          
019000 01  FILLER                      PIC X(16)   VALUE                        
019100                                             'COPY-WDB501-AREA'.          
019200 01  DLI-IO-AREA-1.                                                       
019600*    03  -COPY WDB501  -PRE COPY-                                         
019700     EJECT                                                                
019800                                                                          
019900                                                                          
020000 01  FILLER                      PIC X(16)   VALUE                        
020100                                             'DEF-WDB501-AREA '.          
020200 01  DLI-IO-AREA-2.                                                       
020600*    03  -COPY WDB501  -PRE DEF-                                          
020700     EJECT                                                                
020800                                                                          
020810 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA '.        
020830 01  DLI-IO-AREA-3.                                                       
020860     03  WLGMTA01.                                                        
020870*        05  -COPY WDB201  -PRE GMTA-                                     
020871                                                                          
020872 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020873 01   DLI-IO-AREA-B601.                                                   
020874*     03  -COPY WDB601                                                    
020875                                                                          
020880     EJECT                                                                
020890                                                                          
020900 LINKAGE SECTION.                                                         
021000*01  -COPY W0009   -PRE MSG-                                              
021100*01  -COPY W0008   -PRE USEA-                                             
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008  -PRE GMTC-                                              
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700*01  -COPY W0008  -PRE GMTCU-                                             
021800     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
021910*01  -COPY W0008  -PRE GMTA-                                              
021920     05  FILLER                  PIC X.                                   
021930     EJECT                                                                
021940*01  -COPY W0008  -PRE WDB6-                                              
021950     05  FILLER                  PIC X.                                   
021960     EJECT                                                                
022000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTC-PCB GMTCU-PCB            
022010                           GMTA-PCB WDB6-PCB.                             
022100 MAIN SECTION.                                                            
022200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTC-PCB GMTCU-PCB            
022210                           GMTA-PCB WDB6-PCB.                             
022300                                                                          
022400     PERFORM IMS-GET-MSG                                                  
022500     IF SEGMENT-FINNS                                                     
022600       PERFORM A-INIT                                                     
022700       PERFORM B-KOLLA-NYCKLAR                                            
022800       IF NYCKLAR-OK                                                      
022900         IF MFS-UPDATE                                                    
023000           PERFORM G-KOLLA-INPUT                                          
023100           IF INDATA-OK                                                   
023200             PERFORM H-UPPDATERA                                          
023300           END-IF                                                         
023310           PERFORM S10-SAMMA-SIDA-EFTER-UPPDAT                            
023400         ELSE                                                             
023500           IF MFS-FIRST                                                   
023600             PERFORM C-FOERSTA-SIDA                                       
023700           ELSE                                                           
023800             IF MFS-NEXT                                                  
023900               PERFORM D-NAESTA-SIDA                                      
024000             ELSE                                                         
024100               PERFORM E-SAMMA-SIDA                                       
024200             END-IF                                                       
024300           END-IF                                                         
024400         END-IF                                                           
024500         PERFORM F-LAES-VISA-INFO                                         
024600       END-IF                                                             
024700*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
024800*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
024900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41801 + 4                      
025000       PERFORM IMS-INSERT-MSG                                             
025100     END-IF                                                               
025200                                                                          
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 A-INIT SECTION.                                                          
025800                                                                          
025900     IF MSG-DUBBLA-TRANSKODER                                             
026000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41801                 
026100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
026200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026300     ELSE                                                                 
026400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41801                  
026500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026700     END-IF                                                               
026800                                                                          
026900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
027000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
027100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027200                                                                          
027300     MOVE LOW-VALUE TO MSG-AREA                                           
027400     MOVE 'W4O418N1' TO MFS-IDMOD                                         
027500     MOVE '4418' TO MOD-IDTRANS                                           
027600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027700                                                                          
027800     IF EGEN-MID OR HELP-MID                                              
027900       CONTINUE                                                           
028000     ELSE                                                                 
028100       MOVE SPACE TO MFS-KDTRTYP                                          
028200       MOVE '7' TO MFS-IDPFK                                              
028300     END-IF                                                               
028310                                                                          
028320     ACCEPT DAGENS-DATUM FROM DATE                                        
028400     .                                                                    
028500     EJECT                                                                
028600 B-KOLLA-NYCKLAR SECTION.                                                 
028700                                                                          
028800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028900     MOVE '001'             TO MSGI-KDCALL                                
029000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029200     MOVE '4418'            TO MSGI-IDTRANS                               
029300     IF EGEN-MID                                                          
029400         MOVE MID-KDFRAKT-IN     TO MSGI-KDFRAKT                          
029500         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
029600         MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                         
029700     END-IF                                                               
029710                                                                          
029800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029900     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
029910     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
030000     MOVE JA TO NYCKLAR-SW                                                
030200                                                                          
030300*    -- KONTROLL AV IDDC                                                  
030400     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
030500                                                                          
030600     IF MID-IDDC-IN = ALL '+'                                             
030700       IF EGEN-MID OR NYCKEL-MID                                          
030800         MOVE MID-IDDC-UT TO W-IDDC-B6                                    
031000       ELSE                                                               
031100         MOVE MSGI-IDDC TO W-IDDC-B6                                      
031300       END-IF                                                             
031400     ELSE                                                                 
031500       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
031600       MOVE '7'         TO MFS-IDPFK                                      
031700       MOVE SPACE       TO MFS-KDTRTYP                                    
031800     END-IF                                                               
031810     PERFORM IMS-GU-WDB601                                                
031900                                                                          
032000     IF DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                         
032010       MOVE NEJ TO NYCKLAR-SW                                             
032020     ELSE                                                                 
032100       MOVE DCS-IDDC TO W-IDDC                                            
032200                        W-IDDC-MIN                                        
032300                        W-IDDC-MAX                                        
032400                        W-IDDC-COPY                                       
032500                        W-IDDC-DEF                                        
032800     END-IF                                                               
032900                                                                          
033000*    -- KONTROLL AV KDFRAKT                                               
033100     MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-IN                               
033200                                                                          
033300     IF MID-KDFRAKT-IN NOT = ALL '+'                                      
033400       MOVE '7'         TO MFS-IDPFK                                      
033500       MOVE SPACE       TO MFS-KDTRTYP                                    
033600     END-IF                                                               
033700     IF MSGI-KDFRAKT NUMERIC AND MSGI-KDFRAKT > 0                         
033800       MOVE MSGI-KDFRAKT TO W-KDFRAKT                                     
033900                            W-KDFRAKT-MIN                                 
034000                            W-KDFRAKT-MAX                                 
034100                            W-KDFRAKT-COPY                                
034200                            W-KDFRAKT-DEF                                 
034300                            WS-KDFRAKT                                    
034400     ELSE                                                                 
034500       MOVE NEJ TO NYCKLAR-SW                                             
034600     END-IF                                                               
034700                                                                          
034800                                                                          
034900*    -- KONTROLL AV IDDISTR                                               
035000     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
035100                                                                          
035200     IF MID-IDDISTR-IN NOT = ALL '+'                                      
035300       MOVE '7'         TO MFS-IDPFK                                      
035400       MOVE SPACE       TO MFS-KDTRTYP                                    
035500     END-IF                                                               
035600     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > 0                         
035700       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
035800                            W-IDDISTR-MIN                                 
035900                            W-IDDISTR-MAX                                 
036000                            W-IDDISTR-COPY                                
036100                            W-IDDISTR-DEF                                 
036110                            W-IDDISTR-WDB2                                
036200                            WS-IDDISTR                                    
036300     ELSE                                                                 
036400       MOVE NEJ TO NYCKLAR-SW                                             
036500     END-IF                                                               
036600                                                                          
036700                                                                          
036800*    -- KONTROLL AV KUNDNR                                                
036900     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
037000                                                                          
037100     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
037200       MOVE '7'         TO MFS-IDPFK                                      
037300       MOVE SPACE       TO MFS-KDTRTYP                                    
037400     END-IF                                                               
037500                                                                          
037600     IF MSGI-IDKUNDNR NUMERIC                                             
037700       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
037800                             W-IDKUNDNR-MIN                               
037900                             W-IDKUNDNR-COPY                              
038000                             WS-IDKUNDNR                                  
038100     ELSE                                                                 
038200       MOVE NEJ TO NYCKLAR-SW                                             
038300     END-IF                                                               
038400                                                                          
038500     IF GODK-MID OR NYCKLAR-OK                                            
038600       MOVE DCS-IDDC            TO MOD-IDDC-UT                            
038700       MOVE MSGI-KDFRAKT        TO MOD-KDFRAKT-UT                         
038800       INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE             
038900       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
039000       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
039100       MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                        
039200       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
039300     ELSE                                                                 
039400       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
039500                               MOD-KDFRAKT-UT                             
039600                               MOD-IDDISTR-UT                             
039700                               MOD-IDKUNDNR-UT                            
039800     END-IF                                                               
039900                                                                          
040000     IF NYCKLAR-FEL                                                       
040100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
040200       CALL WMEDKONV USING MED-WMEDAREA                                   
040300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
040400       PERFORM MFS-RENSA-FAELT-IN                                         
040500       PERFORM MFS-RENSA-FAELT-UT                                         
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900 C-FOERSTA-SIDA SECTION.                                                  
041000                                                                          
041100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
041200     CALL WMEDKONV USING MED-WMEDAREA                                     
041300     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
041400                                                                          
041500     PERFORM MFS-RENSA-FAELT-IN                                           
041600     .                                                                    
041700     EJECT                                                                
041800                                                                          
041900 D-NAESTA-SIDA SECTION.                                                   
042000                                                                          
042200     IF SPAR-IDTRANS = '4418'                                             
042210       MOVE SPAR-WDB501KY-NEXT   TO NYCKLAR-TILL-BLAEDDRING               
042300       MOVE W-MINKEY-IDDC        TO W-IDDC                                
042400                                    W-IDDC-MIN                            
042500                                    W-IDDC-MAX                            
042600       MOVE W-MINKEY-KDFRAKT     TO W-KDFRAKT                             
042700                                    W-KDFRAKT-MIN                         
042800                                    W-KDFRAKT-MAX                         
042900       MOVE W-MINKEY-IDDISTR     TO W-IDDISTR                             
043000                                    W-IDDISTR-MIN                         
043100                                    W-IDDISTR-MAX                         
043110                                    W-IDDISTR-WDB2                        
043200       MOVE W-MINKEY-IDKUNDNR    TO W-IDKUNDNR                            
043300                                    W-IDKUNDNR-MIN                        
043400     END-IF                                                               
043500     PERFORM MFS-RENSA-FAELT-IN                                           
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900 E-SAMMA-SIDA SECTION.                                                    
044000                                                                          
044200     PERFORM MFS-RENSA-FAELT-UT                                           
044300     IF EGEN-MID OR HELP-MID                                              
044500       IF MID-INPUT = ALL '+'                                             
044510         IF SPAR-IDTRANS = '4418'                                         
044520           MOVE SPAR-WDB501KY-ENTER                                       
044530                                 TO NYCKLAR-TILL-BLAEDDRING               
044700           MOVE DCS-IDDC         TO W-IDDC                                
044800                                    W-IDDC-MIN                            
044900                                    W-IDDC-MAX                            
045000           MOVE W-MINKEY-KDFRAKT TO W-KDFRAKT                             
045100                                    W-KDFRAKT-MIN                         
045200                                    W-KDFRAKT-MAX                         
045300           MOVE W-MINKEY-IDDISTR TO W-IDDISTR                             
045400                                    W-IDDISTR-MIN                         
045500                                    W-IDDISTR-MAX                         
045600           MOVE W-MINKEY-IDKUNDNR TO W-IDKUNDNR                           
045700                                     W-IDKUNDNR-MIN                       
045800         END-IF                                                           
045900         PERFORM MFS-RENSA-FAELT-IN                                       
046000       ELSE                                                               
046100         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
046200         CALL WMEDKONV USING MED-WMEDAREA                                 
046300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
046400         PERFORM EA-MID-INDATA-TILL-MOD                                   
046500       END-IF                                                             
046800     ELSE                                                                 
046900       PERFORM MFS-RENSA-FAELT-IN                                         
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 EA-MID-INDATA-TILL-MOD SECTION.                                          
047400                                                                          
047500     IF MID-KDCMD-IN NOT = ALL '+'                                        
047600       MOVE MID-KDCMD-IN TO MOD-KDCMD-IN                                  
047700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                    
047800     ELSE                                                                 
047900       MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-IN                         
048000     END-IF                                                               
048100                                                                          
048200     IF MID-KUNDNR-IN NOT = ALL '+'                                       
048210       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
048220       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
048230       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
048240       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
048250       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
048260         MOVE '9999999'           TO MID-KUNDNR-IN                        
048270         MOVE 'DEF    '           TO MOD-KUNDNR-IN                        
048280       ELSE                                                               
048290         MOVE MID-KUNDNR-IN       TO MOD-KUNDNR-IN                        
048291       END-IF                                                             
048400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KUNDNR-IN-ATTR                   
048500     ELSE                                                                 
048600       MOVE MFS-RENSA-FAELT       TO MOD-KUNDNR-IN                        
048700     END-IF                                                               
048800                                                                          
048900     IF MID-BEGMRK-RAD1-IN NOT = ALL '+'                                  
049000       MOVE MID-BEGMRK-RAD1-IN    TO MOD-BEGMRK-RAD1-IN                   
049100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMRK-RAD1-IN-ATTR              
049200     ELSE                                                                 
049300       MOVE MFS-RENSA-FAELT       TO MOD-BEGMRK-RAD1-IN                   
049400     END-IF                                                               
049500                                                                          
049600     IF MID-BEGMRK-RAD2-IN NOT = ALL '+'                                  
049700       MOVE MID-BEGMRK-RAD2-IN    TO MOD-BEGMRK-RAD2-IN                   
049800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMRK-RAD2-IN-ATTR              
049900     ELSE                                                                 
050000       MOVE MFS-RENSA-FAELT       TO MOD-BEGMRK-RAD2-IN                   
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400 F-LAES-VISA-INFO SECTION.                                                
050500                                                                          
050600     PERFORM IMS-GU-WDB501                                                
050700                                                                          
050800     IF SEGMENT-SAKNAS                                                    
050900*        LÄMPLIGT FELMEDDELANDE                                           
051000*       CALL WMEDKONV USING MED-WMEDAREA                                  
051100*       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
051200*       MOVE 'SEGMENT SAKNAS '       TO MOD-TEMFSFEL                      
051300        PERFORM MFS-RENSA-FAELT-UT                                        
051400        MOVE SPACE                   TO W-MINKEY-IDDC                     
051500        MOVE ZERO                    TO W-MINKEY-KDFRAKT                  
051600                                        W-MINKEY-IDDISTR                  
051700                                        W-MINKEY-IDKUNDNR                 
051810        MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB501KY-ENTER               
051820                                        SPAR-WDB501KY-NEXT                
051900        MOVE '002'     TO MSGI-KDCALL                                     
052000        MOVE '4418'    TO MSGI-IDTRANS                                    
052010                          SPAR-IDTRANS                                    
052020        MOVE SPAR-AREA TO MSGI-SPAR-AREA                                  
052100        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
052200                                                                          
052300     ELSE                                                                 
052400                                                                          
052500***** SPARA ENTER NYCKLAR PÅ USERBASEN                                    
052600                                                                          
052700       MOVE GMTC-FK-IDDC               TO W-MINKEY-IDDC                   
052800       MOVE GMTC-FK-KDFRAKT            TO W-MINKEY-KDFRAKT                
052900       MOVE GMTC-FK-IDDISTR            TO W-MINKEY-IDDISTR                
053000       MOVE GMTC-FK-IDKUNDNR           TO W-MINKEY-IDKUNDNR               
053100                                                                          
053210       MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB501KY-ENTER                
053300                                                                          
053400*****                                                                     
053500                                                                          
053600       MOVE +1 TO INDX                                                    
053700       PERFORM UNTIL INDX > MAX-INDX                                      
053800                                                                          
053900         IF SEGMENT-FINNS AND GMTC-FK-IDKUNDNR < +9999999                 
054000                                                                          
054100           MOVE GMTC-FK-IDKUNDNR       TO MOD-IDKUNDNR     (INDX)         
054200           MOVE GMTC-FK-BEGMRK-RAD1    TO MOD-BEGMRK-RAD1  (INDX)         
054300           MOVE GMTC-FK-BEGMRK-RAD2    TO MOD-BEGMRK-RAD2  (INDX)         
054400                                                                          
054500           ADD +1 TO INDX                                                 
054600           PERFORM IMS-GN-WDB501                                          
054700         ELSE                                                             
054800           MOVE MFS-RENSA-FAELT        TO MOD-IDKUNDNR     (INDX)         
054900                                        MOD-BEGMRK-RAD1    (INDX)         
055000                                        MOD-BEGMRK-RAD2    (INDX)         
055100                                                                          
055200           ADD +1 TO INDX                                                 
055300         END-IF                                                           
055400       END-PERFORM                                                        
055500                                                                          
055600       IF SEGMENT-FINNS AND                                               
055700         GMTC-FK-IDKUNDNR < +9999999                                      
055800                                                                          
055900***** SPARA NEXT NYCKLAR PÅ USERBASEN                                     
056000                                                                          
056100         MOVE GMTC-FK-IDDC       TO W-MINKEY-IDDC                         
056200         MOVE GMTC-FK-KDFRAKT    TO W-MINKEY-KDFRAKT                      
056300         MOVE GMTC-FK-IDDISTR    TO W-MINKEY-IDDISTR                      
056400         MOVE GMTC-FK-IDKUNDNR TO W-MINKEY-IDKUNDNR                       
056500                                                                          
056510         IF MFS-UPDATE                                                    
056520           CONTINUE                                                       
056530         ELSE                                                             
056600           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
056700           CALL WMEDKONV USING MED-WMEDAREA                               
056800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
056810         END-IF                                                           
056900       ELSE                                                               
057300         CONTINUE                                                         
057500       END-IF                                                             
057600                                                                          
057710       MOVE NYCKLAR-TILL-BLAEDDRING TO SPAR-WDB501KY-NEXT                 
057800       MOVE '002'     TO MSGI-KDCALL                                      
057900       MOVE '4418'    TO MSGI-IDTRANS                                     
057910                         SPAR-IDTRANS                                     
057920       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
058000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
058100                                                                          
058200     END-IF                                                               
058300                                                                          
058400     MOVE +9999999                TO W-IDKUNDNR                           
058500                                                                          
058600     PERFORM IMS-GU-WDB501-DEFAULT                                        
058700                                                                          
058800     IF SEGMENT-FINNS                                                     
058900       MOVE DEF-FK-BEGMRK-RAD1 TO MOD-BEGMRK-RAD1-UT                      
059000       MOVE DEF-FK-BEGMRK-RAD2 TO MOD-BEGMRK-RAD2-UT                      
059100     ELSE                                                                 
059200*        LÄMPLIGT FELMEDDELANDE                                           
059300*       CALL WMEDKONV USING MED-WMEDAREA                                  
059400*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
059500        MOVE 'DEFAULT ROW MISSING ' TO MOD-TEMFSFEL                       
059600        PERFORM MFS-RENSA-FAELT-UT                                        
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100 G-KOLLA-INPUT SECTION.                                                   
060200                                                                          
060300     MOVE JA  TO INDATA-SW                                                
060400     IF MID-INPUT = ALL '+'                                               
060500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
060600       CALL WMEDKONV USING MED-WMEDAREA                                   
060700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
060800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
060900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
061000       MOVE NEJ TO INDATA-SW                                              
061100     ELSE                                                                 
061200                                                                          
061300       IF MID-KDCMD-IN NOT = ALL '+'                                      
061400         IF MID-KDCMD-IN = 'N' OR 'E' OR 'D' OR 'C'                       
061500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                 
061600           IF MID-KDCMD-IN = 'N'                                          
061700             PERFORM GA-KONTROLLERA-NEW                                   
061800           ELSE                                                           
061900             IF MID-KDCMD-IN = 'E'                                        
062000               PERFORM GB-KONTROLLERA-EDIT                                
062100             ELSE                                                         
062200               IF MID-KDCMD-IN = 'D'                                      
062300                 PERFORM GC-KONTROLLERA-DELETE                            
062400               ELSE                                                       
062500                 PERFORM GD-KONTROLLERA-COPY                              
062600               END-IF                                                     
062700             END-IF                                                       
062800           END-IF                                                         
062900         ELSE                                                             
063000            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                  
063100            MOVE NEJ TO INDATA-SW                                         
063200         END-IF                                                           
063300       ELSE                                                               
063400          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-IN-ATTR                  
063500          MOVE NEJ TO INDATA-SW                                           
063600       END-IF                                                             
063700                                                                          
063800       IF INDATA-FEL                                                      
063810         IF GAELLANDE-KUND                                                
063900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
064000           CALL WMEDKONV USING MED-WMEDAREA                               
064100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
064200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
064300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
064310         ELSE                                                             
064311*          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
064312*          CALL WMEDKONV USING MED-WMEDAREA                               
064313*          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
064314           MOVE ' GOODS RECEIVER NOT VALID ' TO MOD-TEMFSINF              
064315           PERFORM MFS-ROER-EJ-FAELT-UT                                   
064316           PERFORM MFS-ROER-EJ-FAELT-IN                                   
064320         END-IF                                                           
064400       END-IF                                                             
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 GA-KONTROLLERA-NEW SECTION.                                              
065000                                                                          
065100**** KONTROLL AV KUNDNR                                                   
065200                                                                          
065300     IF MID-KUNDNR-IN NOT = ALL '+'                                       
065310       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
065320       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
065330       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
065340       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
065350       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
065360         MOVE '9999999'           TO MID-KUNDNR-IN                        
065370       END-IF                                                             
065400       IF MID-KUNDNR-IN NOT NUMERIC                                       
065500         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KUNDNR-IN-ATTR                 
065600         MOVE NEJ TO INDATA-SW                                            
065700       ELSE                                                               
065701        IF MID-KUNDNR-IN = 9999999                                        
065702         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                  
065703         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-IN-ATTR                    
065704         MOVE NEJ TO INDATA-SW                                            
065705        ELSE                                                              
065710         MOVE MID-KUNDNR-IN       TO W-IDKUNDNR-COPY                      
065720                                     W-IDKUNDNR-WDB2                      
065730         PERFORM IMS-GU-WDB501-COPY                                       
065740         IF SEGMENT-SAKNAS                                                
065780           PERFORM IMS-GU-WDB501-DEFAULT                                  
065790           IF SEGMENT-FINNS                                               
065791             PERFORM S01-KOLLA-OM-GAELLANDE-KUND                          
065792             IF GAELLANDE-KUND                                            
065793               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR            
065794             ELSE                                                         
065795               MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR              
065796               MOVE NEJ TO INDATA-SW                                      
065797             END-IF                                                       
065798           ELSE                                                           
065799             MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR                
065800             MOVE NEJ TO INDATA-SW                                        
065801             MOVE 'DEFAULT ROW MISSING ' TO MOD-TEMFSINF                  
065802           END-IF                                                         
065804         ELSE                                                             
065805           MOVE MFS-ALFA-FAELT-FEL TO MOD-KUNDNR-IN-ATTR                  
065806           MOVE NEJ TO INDATA-SW                                          
065807           MOVE 'GOODS RECEIVER ALREADY PRESENT ' TO MOD-TEMFSINF         
065808         END-IF                                                           
065809        END-IF                                                            
065900       END-IF                                                             
066000     ELSE                                                                 
066100       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
066200       MOVE NEJ TO INDATA-SW                                              
066300     END-IF                                                               
066400                                                                          
066500**** KONTROLL AV BEGMRK-RAD1   ( GODS MÄRKNING 1 )                        
066600                                                                          
066700     IF MID-BEGMRK-RAD1-IN NOT = ALL '+'                                  
066800       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMRK-RAD1-IN-ATTR               
066900     END-IF                                                               
067000                                                                          
067100**** KONTROLL AV BEGMRK-RAD2   ( GODS MÄRKNING 2 )                        
067200                                                                          
067300     IF MID-BEGMRK-RAD2-IN NOT = ALL '+'                                  
067400       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEGMRK-RAD2-IN-ATTR               
067500     END-IF                                                               
067600                                                                          
067700     .                                                                    
067800     EJECT                                                                
067900                                                                          
068000 GB-KONTROLLERA-EDIT SECTION.                                             
068100                                                                          
068200**** KONTROLL AV KUNDNR                                                   
068300                                                                          
068400     IF MID-KUNDNR-IN NOT = ALL '+'                                       
068500       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
068600       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
068700       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
068800       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
068900       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
069000         MOVE '9999999'           TO MID-KUNDNR-IN                        
069001       END-IF                                                             
069010       IF MID-KUNDNR-IN NUMERIC                                           
069020         MOVE MID-KUNDNR-IN          TO W-IDKUNDNR-COPY                   
069021         IF MID-KUNDNR-IN = 9999999                                       
069022           PERFORM IMS-GU-WDB501-COPY-DEF                                 
069023         ELSE                                                             
069024           PERFORM IMS-GU-WDB501-COPY                                     
069025         END-IF                                                           
069040         IF SEGMENT-FINNS                                                 
069050           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
069060         ELSE                                                             
069070           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KUNDNR-IN-ATTR                
069080           MOVE NEJ TO INDATA-SW                                          
069090           MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSINF                 
069091         END-IF                                                           
069092       ELSE                                                               
069093         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KUNDNR-IN-ATTR                
069094         MOVE NEJ TO INDATA-SW                                            
069095       END-IF                                                             
069100     ELSE                                                                 
069200       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
069300       MOVE NEJ TO INDATA-SW                                              
069400     END-IF                                                               
069500                                                                          
069600     IF MID-BEGMRK-RAD1-IN NOT = ALL '+'                                  
069700       MOVE MFS-ALFA-FAELT-RAETT    TO MOD-BEGMRK-RAD1-IN-ATTR            
069800     END-IF                                                               
069900                                                                          
070000     IF MID-BEGMRK-RAD2-IN NOT = ALL '+'                                  
070100       MOVE MFS-ALFA-FAELT-RAETT    TO MOD-BEGMRK-RAD2-IN-ATTR            
070200     END-IF                                                               
070300     .                                                                    
070400     EJECT                                                                
070500                                                                          
070600 GC-KONTROLLERA-DELETE SECTION.                                           
070700                                                                          
070800**** KONTROLL AV KUNDNR                                                   
070900                                                                          
071000     IF MID-KUNDNR-IN NOT = ALL '+'                                       
071601       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
071602       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
071603       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
071604       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
071605       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
071606         MOVE '9999999'           TO MID-KUNDNR-IN                        
071607       END-IF                                                             
071610       IF MID-KUNDNR-IN NUMERIC                                           
071611        IF MID-KUNDNR-IN = 9999999                                        
071612         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                  
071613         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-IN-ATTR                    
071614         MOVE NEJ TO INDATA-SW                                            
071615         MOVE 'DEFAULT ROW CAN NOT BE DELETED ' TO  MOD-TEMFSINF          
071616        ELSE                                                              
071620         MOVE MID-KUNDNR-IN          TO W-IDKUNDNR-COPY                   
071630         PERFORM IMS-GU-WDB501-COPY                                       
071640         IF SEGMENT-FINNS                                                 
071650           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
071660         ELSE                                                             
071670           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KUNDNR-IN-ATTR                
071680           MOVE NEJ TO INDATA-SW                                          
071690           MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSINF                 
071691         END-IF                                                           
071692        END-IF                                                            
071693       ELSE                                                               
071694         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KUNDNR-IN-ATTR                
071695         MOVE NEJ TO INDATA-SW                                            
071696       END-IF                                                             
071700     ELSE                                                                 
071800       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
071900       MOVE NEJ TO INDATA-SW                                              
072000     END-IF                                                               
072100                                                                          
072200     .                                                                    
072300     EJECT                                                                
072400                                                                          
072500 GD-KONTROLLERA-COPY SECTION.                                             
072600                                                                          
072700**** KONTROLL AV KUNDNR                                                   
072800                                                                          
072900     IF MID-KUNDNR-IN NOT = ALL '+'                                       
072910       IF  MID-KUNDNR-IN (1:3) = 'DEF'                                    
072920       OR  MID-KUNDNR-IN (2:3) = 'DEF'                                    
072930       OR  MID-KUNDNR-IN (3:3) = 'DEF'                                    
072940       OR  MID-KUNDNR-IN (4:3) = 'DEF'                                    
072950       OR  MID-KUNDNR-IN (5:3) = 'DEF'                                    
072960         MOVE '9999999'           TO MID-KUNDNR-IN                        
072970       END-IF                                                             
073000       IF MID-KUNDNR-IN NUMERIC                                           
073100         MOVE MID-KUNDNR-IN     TO W-IDKUNDNR-COPY                        
073110         IF MID-KUNDNR-IN = 9999999                                       
073120           PERFORM IMS-GU-WDB501-COPY-DEF                                 
073130         ELSE                                                             
073140           PERFORM IMS-GU-WDB501-COPY                                     
073150         END-IF                                                           
073300         IF SEGMENT-FINNS                                                 
073400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KUNDNR-IN-ATTR                
073500         ELSE                                                             
073600           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KUNDNR-IN-ATTR                
073700           MOVE NEJ TO INDATA-SW                                          
073800         END-IF                                                           
073900       ELSE                                                               
074000         MOVE MFS-ALFA-FAELT-FEL    TO MOD-KUNDNR-IN-ATTR                 
074100         MOVE NEJ TO INDATA-SW                                            
074200       END-IF                                                             
074300     ELSE                                                                 
074400       MOVE MFS-ALFA-FAELT-FEL      TO MOD-KUNDNR-IN-ATTR                 
074500       MOVE NEJ TO INDATA-SW                                              
074600     END-IF                                                               
074700                                                                          
074800     .                                                                    
074900     EJECT                                                                
075000                                                                          
075100 H-UPPDATERA SECTION.                                                     
075200                                                                          
075300     MOVE MID-KUNDNR-IN   TO W-IDKUNDNR                                   
075400                                                                          
075500     PERFORM IMS-GHU-WDB501                                               
075600     IF SEGMENT-SAKNAS                                                    
075700       IF MID-KDCMD-IN = 'N'                                              
075800         PERFORM HA-ISRT-NEW-CUSTOMER                                     
075900       END-IF                                                             
076000     ELSE                                                                 
076100       IF MID-KDCMD-IN = 'N'                                              
076200         PERFORM HA-REPL-CUST-FKOD                                        
076300       ELSE                                                               
076400         IF MID-KDCMD-IN = 'E'                                            
076500           PERFORM HB-REPLACE-CUSTOMER                                    
076600         ELSE                                                             
076700           IF MID-KDCMD-IN = 'D'                                          
076800             IF GMTC-FK-KDFKTYP = '3'                                     
076900               PERFORM HC-REPL-CUST-MARK                                  
077000             ELSE                                                         
077100               PERFORM HC-DELETE-CUSTOMER                                 
077200             END-IF                                                       
077300           ELSE                                                           
077400             PERFORM HD-COPY-CUSTOMER                                     
077500           END-IF                                                         
077600         END-IF                                                           
077700       END-IF                                                             
077800     END-IF                                                               
077900                                                                          
078000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
078100     CALL WMEDKONV USING MED-WMEDAREA                                     
078200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
078300     PERFORM MFS-FORM-ATTR                                                
078400* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
080400     .                                                                    
080500     EJECT                                                                
080600                                                                          
080700 HA-ISRT-NEW-CUSTOMER SECTION.                                            
080800                                                                          
080900     MOVE DCS-IDDC             TO GMTC-FK-IDDC                            
081000     MOVE WS-KDFRAKT           TO GMTC-FK-KDFRAKT                         
081100     MOVE WS-IDDISTR           TO GMTC-FK-IDDISTR                         
081200     MOVE MID-KUNDNR-IN        TO GMTC-FK-IDKUNDNR                        
081300                                                                          
081400     IF MID-BEGMRK-RAD1-IN NOT =    ALL '+'                               
081500       MOVE MID-BEGMRK-RAD1-IN TO GMTC-FK-BEGMRK-RAD1                     
081600     ELSE                                                                 
081700       MOVE SPACE              TO GMTC-FK-BEGMRK-RAD1                     
081800     END-IF                                                               
081900                                                                          
082000     IF MID-BEGMRK-RAD2-IN NOT =    ALL '+'                               
082100       MOVE MID-BEGMRK-RAD2-IN TO GMTC-FK-BEGMRK-RAD2                     
082200     ELSE                                                                 
082300       MOVE SPACE              TO GMTC-FK-BEGMRK-RAD2                     
082400     END-IF                                                               
082500                                                                          
082600     MOVE '2'                  TO GMTC-FK-KDFKTYP                         
082700                                                                          
082800     PERFORM IMS-GU-WDB501-DEFAULT                                        
082900     IF SEGMENT-FINNS                                                     
083000       MOVE DEF-FK-IDTRP-0     TO GMTC-FK-IDTRP-0                         
083100       MOVE DEF-FK-IDTRP-1     TO GMTC-FK-IDTRP-1                         
083200       MOVE DEF-FK-IDTRP-2     TO GMTC-FK-IDTRP-2                         
083300       MOVE DEF-FK-IDTRP-3     TO GMTC-FK-IDTRP-3                         
083400       MOVE DEF-FK-IDTRP-4     TO GMTC-FK-IDTRP-4                         
083500       MOVE DEF-FK-KDTRPKAT    TO GMTC-FK-KDTRPKAT                        
083600       MOVE DEF-FK-KDFDKRAV    TO GMTC-FK-KDFDKRAV                        
083700       MOVE DEF-FK-KDGRANS     TO GMTC-FK-KDGRANS                         
083800       MOVE DEF-FK-PRLEGKST    TO GMTC-FK-PRLEGKST                        
083900       MOVE DEF-FK-REFOERS     TO GMTC-FK-REFOERS                         
084000     END-IF                                                               
084100                                                                          
084200     PERFORM IMS-ISRT-WDB501                                              
084300     PERFORM MFS-RENSA-FAELT-IN                                           
084400     .                                                                    
084500     EJECT                                                                
084600                                                                          
084700 HA-REPL-CUST-FKOD SECTION.                                               
084800                                                                          
084900     IF MID-BEGMRK-RAD1-IN NOT =    ALL '+'                               
085000       MOVE MID-BEGMRK-RAD1-IN TO GMTC-FK-BEGMRK-RAD1                     
085100     ELSE                                                                 
085200       MOVE SPACE              TO GMTC-FK-BEGMRK-RAD1                     
085300     END-IF                                                               
085400                                                                          
085500     IF MID-BEGMRK-RAD2-IN NOT =    ALL '+'                               
085600       MOVE MID-BEGMRK-RAD2-IN TO GMTC-FK-BEGMRK-RAD2                     
085700     ELSE                                                                 
085800       MOVE SPACE              TO GMTC-FK-BEGMRK-RAD2                     
085900     END-IF                                                               
086000                                                                          
086100     MOVE '3'                  TO GMTC-FK-KDFKTYP                         
086200                                                                          
086300     PERFORM IMS-REPL-WDB501                                              
086400     PERFORM MFS-RENSA-FAELT-IN                                           
086500                                                                          
086600     .                                                                    
086700     EJECT                                                                
086800                                                                          
086900 HB-REPLACE-CUSTOMER SECTION.                                             
087000                                                                          
087100                                                                          
087200     IF MID-BEGMRK-RAD1-IN NOT =    ALL '+'                               
087300       MOVE MID-BEGMRK-RAD1-IN TO GMTC-FK-BEGMRK-RAD1                     
087400     END-IF                                                               
087500                                                                          
087600     IF MID-BEGMRK-RAD2-IN NOT =    ALL '+'                               
087700       MOVE MID-BEGMRK-RAD2-IN TO GMTC-FK-BEGMRK-RAD2                     
087800     END-IF                                                               
087900                                                                          
087910     MOVE GMTC-FK-BEGMRK TO W-BEGMRK                                      
087920                                                                          
088000     PERFORM IMS-REPL-WDB501                                              
088010                                                                          
088020     IF MID-KUNDNR-IN = 9999999                                           
088030        PERFORM HBA-REPLACE-ALLA-KUNDER                                   
088040     END-IF                                                               
088050                                                                          
088100     PERFORM MFS-RENSA-FAELT-IN                                           
088200     .                                                                    
088210     EJECT                                                                
088220                                                                          
088230 HBA-REPLACE-ALLA-KUNDER SECTION.                                         
088240                                                                          
088250     MOVE ZERO TO W-IDKUNDNR-MIN                                          
088260     PERFORM IMS-GHU-WDB501-FIRST                                         
088270     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
088280             BAS-SLUT                                                     
088290                                                                          
088291       MOVE W-BEGMRK    TO GMTC-FK-BEGMRK                                 
088301                                                                          
088302       PERFORM IMS-REPL-WDB501                                            
088303                                                                          
088304       PERFORM IMS-GHN-WDB501-NEXT                                        
088305     END-PERFORM                                                          
088306     .                                                                    
088310     EJECT                                                                
088400                                                                          
088500 HC-REPL-CUST-MARK SECTION.                                               
088600                                                                          
088700     PERFORM IMS-GU-WDB501-DEFAULT                                        
088800     IF SEGMENT-FINNS                                                     
088900       MOVE DEF-FK-BEGMRK TO GMTC-FK-BEGMRK                               
089000     END-IF                                                               
089100                                                                          
089200     MOVE '1'         TO GMTC-FK-KDFKTYP                                  
089300                                                                          
089400     PERFORM IMS-REPL-WDB501                                              
089500     PERFORM MFS-RENSA-FAELT-IN                                           
089600     .                                                                    
089700     EJECT                                                                
089800                                                                          
089900 HC-DELETE-CUSTOMER SECTION.                                              
090000                                                                          
090100     PERFORM IMS-DLET-WDB501                                              
090200     PERFORM MFS-RENSA-FAELT-IN                                           
090300     .                                                                    
090400     EJECT                                                                
090500                                                                          
090600 HD-COPY-CUSTOMER SECTION.                                                
090700                                                                          
090800     MOVE MFS-RENSA-FAELT       TO MOD-KUNDNR-IN                          
090900                                   MOD-KDCMD-IN                           
090910     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KUNDNR-IN-ATTR                     
090920     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                      
091000                                                                          
091100     MOVE COPY-FK-BEGMRK-RAD1   TO MOD-BEGMRK-RAD1-IN                     
091200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMRK-RAD1-IN-ATTR                
091300     MOVE COPY-FK-BEGMRK-RAD2   TO MOD-BEGMRK-RAD2-IN                     
091400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEGMRK-RAD2-IN-ATTR                
091500     .                                                                    
091600     EJECT                                                                
091700                                                                          
091701 S01-KOLLA-OM-GAELLANDE-KUND SECTION.                                     
091702                                                                          
091703     PERFORM IMS-GU-WDB201                                                
091704     IF SEGMENT-FINNS                                                     
091705*      IF GMTA-GMT-TISTADAT > ZERO                                        
091706*        IF GMTA-GMT-TISTODAT > ZERO                                      
091707*          MOVE GMTA-GMT-TISTODAT   TO TMP1-YYMMDD                        
091708*          MOVE DAGENS-DATUM        TO TMP2-YYMMDD                        
091709*          PERFORM WY2000P1                                               
091710*          IF TMP1-YYMMDD > TMP2-YYMMDD                                   
091711*            MOVE JA TO GAELLANDE-SW                                      
091712*          ELSE                                                           
091713*            MOVE NEJ TO GAELLANDE-SW                                     
091714*          END-IF                                                         
091715*        ELSE                                                             
091716*          MOVE JA TO GAELLANDE-SW                                        
091717*        END-IF                                                           
091719*      ELSE                                                               
091720*        MOVE NEJ TO GAELLANDE-SW                                         
091722*      END-IF                                                             
091723       MOVE JA TO GAELLANDE-SW                                            
091724     ELSE                                                                 
091725       MOVE NEJ   TO GAELLANDE-SW                                         
091726     END-IF                                                               
091727     .                                                                    
091728     EJECT                                                                
091729                                                                          
091730 S10-SAMMA-SIDA-EFTER-UPPDAT SECTION.                                     
091731                                                                          
091732************ FÖR ATT MAN SKA VISA SAMMA SIDA VID UPPDATERING              
091733                                                                          
091750     PERFORM MFS-RENSA-FAELT-UT                                           
091760     IF EGEN-MID OR HELP-MID                                              
091761       IF SPAR-IDTRANS = '4418'                                           
091762         MOVE SPAR-WDB501KY-ENTER                                         
091763                             TO NYCKLAR-TILL-BLAEDDRING                   
091790         MOVE DCS-IDDC         TO W-IDDC                                  
091791                                  W-IDDC-MIN                              
091792                                  W-IDDC-MAX                              
091793         MOVE W-MINKEY-KDFRAKT TO W-KDFRAKT                               
091794                                  W-KDFRAKT-MIN                           
091795                                  W-KDFRAKT-MAX                           
091796         MOVE W-MINKEY-IDDISTR TO W-IDDISTR                               
091797                                  W-IDDISTR-MIN                           
091798                                  W-IDDISTR-MAX                           
091799         MOVE W-MINKEY-IDKUNDNR TO W-IDKUNDNR                             
091800                                   W-IDKUNDNR-MIN                         
091803       END-IF                                                             
091804     END-IF                                                               
091805     .                                                                    
091806     EJECT                                                                
091807                                                                          
091810 MFS-RENSA-FAELT-UT SECTION.                                              
091900                                                                          
092000*    --- ALLA UTDATA-FÄLT                                                 
092100     MOVE MFS-RENSA-FAELT TO MOD-BEGMRK-RAD1-UT                           
092200                             MOD-BEGMRK-RAD2-UT                           
092300                                                                          
092400     MOVE +1 TO INDX                                                      
092500     PERFORM UNTIL INDX > MAX-INDX                                        
092600       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
092700       ADD +1 TO INDX                                                     
092800     END-PERFORM                                                          
092900     .                                                                    
093000     SKIP3                                                                
093100 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
093200                                                                          
093300     MOVE MFS-RENSA-FAELT  TO MOD-IDKUNDNR    (INDX)                      
093400                              MOD-BEGMRK-RAD1 (INDX)                      
093500                              MOD-BEGMRK-RAD2 (INDX)                      
093600     .                                                                    
093700     EJECT                                                                
093800                                                                          
093900 MFS-RENSA-FAELT-IN SECTION.                                              
094000                                                                          
094100*    --- ALLA INDATA-FÄLT                                                 
094200     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IN                                 
094300                             MOD-KUNDNR-IN                                
094400                             MOD-BEGMRK-RAD1-IN                           
094500                             MOD-BEGMRK-RAD2-IN                           
094600     .                                                                    
094700     EJECT                                                                
094800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
094900                                                                          
095000*    --- ALLA UTDATA-FÄLT                                                 
095100                                                                          
095200*    --- ALLA UTDATA-FÄLT                                                 
095300     MOVE MFS-ROER-EJ-FAELT TO MOD-BEGMRK-RAD1-UT                         
095400                               MOD-BEGMRK-RAD2-UT                         
095500                                                                          
095600     MOVE +1 TO INDX                                                      
095700     PERFORM UNTIL INDX > MAX-INDX                                        
095800       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
095900       ADD +1 TO INDX                                                     
096000     END-PERFORM                                                          
096100     .                                                                    
096200     SKIP3                                                                
096300 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
096400                                                                          
096500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR    (INDX)                     
096600                               MOD-BEGMRK-RAD1 (INDX)                     
096700                               MOD-BEGMRK-RAD2 (INDX)                     
096800     .                                                                    
096900     EJECT                                                                
097000                                                                          
097100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
097200                                                                          
097300*    --- ALLA INDATA-FÄLT                                                 
097400     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IN                               
097500                               MOD-KUNDNR-IN                              
097600                               MOD-BEGMRK-RAD1-IN                         
097700                               MOD-BEGMRK-RAD2-IN                         
097800     .                                                                    
097900     EJECT                                                                
098000 MFS-FORM-ATTR SECTION.                                                   
098100                                                                          
098200*    --- ALLA INDATA-FÄLT                                                 
098300     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-IN-ATTR                         
098400                                MOD-KUNDNR-IN-ATTR                        
098500                                MOD-BEGMRK-RAD1-IN-ATTR                   
098600                                MOD-BEGMRK-RAD2-IN-ATTR                   
098700     .                                                                    
099700     EJECT                                                                
099800* --- IMS SEKTIONER ---                                                   
099900     SKIP3                                                                
100000 IMS-GET-MSG SECTION.                                                     
100100                                                                          
100200     MOVE '  QC' TO GODK-STATUSKODER                                      
100300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
100400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100500     PERFORM IMS-STATUSKONTROLL                                           
100600     .                                                                    
100700     SKIP3                                                                
100800 IMS-INSERT-MSG SECTION.                                                  
100900                                                                          
101000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
101100       MOVE 'N' TO MFS-KDHUVOMR                                           
101200     END-IF                                                               
101300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
101400     MOVE SPACE TO GODK-STATUSKODER                                       
101500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
101600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101700     PERFORM IMS-STATUSKONTROLL                                           
101800     .                                                                    
101900     EJECT                                                                
102000 IMS-GU-WDB501-DEFAULT SECTION.                                           
102100                                                                          
102200     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-DEF-X ')'                    
102300          DELIMITED BY SIZE INTO SSA1                                     
102400     MOVE '  GE' TO GODK-STATUSKODER                                      
102500     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-2 SSA1                    
102600     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     .                                                                    
102900     SKIP3                                                                
103000 IMS-GU-WDB501-COPY-DEF SECTION.                                          
103100                                                                          
103200     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-COPY-X ')'                   
103300          DELIMITED BY SIZE INTO SSA1                                     
103400     MOVE '  GE' TO GODK-STATUSKODER                                      
103500     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-1 SSA1                    
103600     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     .                                                                    
103810     SKIP3                                                                
103820 IMS-GU-WDB501-COPY SECTION.                                              
103830                                                                          
103840*BS  STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-COPY-X ')'                   
103850     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-COPY-X                       
103860                    '&KDFKTYP  =' W-KDFKTYP-X                             
103870                    '+WDB501KY =' W-WDB501KY-COPY-X                       
103880                    '&KDFKTYP  =' W-KDFKTYP-BADA-X ')'                    
103890          DELIMITED BY SIZE INTO SSA1                                     
103891     MOVE '  GE' TO GODK-STATUSKODER                                      
103892     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-1 SSA1                    
103893     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
103894     PERFORM IMS-STATUSKONTROLL                                           
103895     .                                                                    
103900     SKIP3                                                                
104000 IMS-GHU-WDB501 SECTION.                                                  
104100                                                                          
104200     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X ')'                        
104300          DELIMITED BY SIZE INTO SSA1                                     
104400     MOVE '  GE' TO GODK-STATUSKODER                                      
104500     CALL CBLTDLI USING GHU GMTCU-PCB DLI-IO-AREA SSA1                    
104600     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     .                                                                    
104900     SKIP3                                                                
105000 IMS-GU-WDB501 SECTION.                                                   
105100                                                                          
105200     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
105300                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
105400                    '&KDFKTYP  =' W-KDFKTYP-X                             
105500                    '+WDB501KY>=' W-WDB501KY-MIN-X                        
105600                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
105700                    '&KDFKTYP  =' W-KDFKTYP-BADA-X ')'                    
105800          DELIMITED BY SIZE INTO SSA1                                     
105900     MOVE '  GE' TO GODK-STATUSKODER                                      
106000     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA SSA1                      
106100     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
106200     PERFORM IMS-STATUSKONTROLL                                           
106300     .                                                                    
106400     SKIP3                                                                
106500 IMS-GN-WDB501 SECTION.                                                   
106600                                                                          
106700     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
106800                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
106900                    '&KDFKTYP  =' W-KDFKTYP-X                             
107000                    '+WDB501KY>=' W-WDB501KY-MIN-X                        
107100                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
107200                    '&KDFKTYP  =' W-KDFKTYP-BADA-X ')'                    
107300          DELIMITED BY SIZE INTO SSA1                                     
107400     MOVE '  GE' TO GODK-STATUSKODER                                      
107500     CALL CBLTDLI USING GN GMTC-PCB DLI-IO-AREA SSA1                      
107600     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
107700     PERFORM IMS-STATUSKONTROLL                                           
107800     .                                                                    
107810     SKIP2                                                                
107820 IMS-GHN-WDB501-NEXT SECTION.                                             
107830                                                                          
107840     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
107850                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
107860                    '&KDFKTYP  =' W-KDFKTYP-1-X ')'                       
107870          DELIMITED BY SIZE INTO SSA1                                     
107880     MOVE '  GEGB' TO GODK-STATUSKODER                                    
107890     CALL CBLTDLI USING GHN GMTCU-PCB DLI-IO-AREA SSA1                    
107891     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
107892     PERFORM IMS-STATUSKONTROLL                                           
107893     .                                                                    
107894     SKIP2                                                                
107895 IMS-GHU-WDB501-FIRST SECTION.                                            
107896                                                                          
107897     STRING 'WLGMTC01(WDB501KY>=' W-WDB501KY-MIN-X                        
107898                    '&WDB501KY<=' W-WDB501KY-MAX-X                        
107899                    '&KDFKTYP  =' W-KDFKTYP-1-X ')'                       
107900          DELIMITED BY SIZE INTO SSA1                                     
107901     MOVE '  GE' TO GODK-STATUSKODER                                      
107902     CALL CBLTDLI USING GHU GMTCU-PCB DLI-IO-AREA SSA1                    
107903     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
107904     PERFORM IMS-STATUSKONTROLL                                           
107905     .                                                                    
107910     EJECT                                                                
108000 IMS-ISRT-WDB501 SECTION.                                                 
108100                                                                          
108200     MOVE 'WLGMTC01 ' TO SSA1                                             
108300     MOVE '  II' TO GODK-STATUSKODER                                      
108400     CALL CBLTDLI USING ISRT GMTCU-PCB DLI-IO-AREA SSA1                   
108500     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
108600     PERFORM IMS-STATUSKONTROLL                                           
108700     .                                                                    
108800     EJECT                                                                
108900 IMS-REPL-WDB501 SECTION.                                                 
109000                                                                          
109100     MOVE '  ' TO GODK-STATUSKODER                                        
109200     CALL CBLTDLI USING REPL GMTCU-PCB DLI-IO-AREA                        
109300     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
109400     PERFORM IMS-STATUSKONTROLL                                           
109500     .                                                                    
109600     SKIP2                                                                
109700 IMS-DLET-WDB501 SECTION.                                                 
109800                                                                          
109900     MOVE '  ' TO GODK-STATUSKODER                                        
110000     CALL CBLTDLI USING DLET GMTCU-PCB DLI-IO-AREA                        
110100     MOVE GMTCU-STATUS-CODE TO STATUS-WS                                  
110200     PERFORM IMS-STATUSKONTROLL                                           
110300     .                                                                    
110400     SKIP2                                                                
110410 IMS-GU-WDB201 SECTION.                                                   
110420                                                                          
110430     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
110440          DELIMITED BY SIZE INTO SSA1                                     
110450     MOVE '  GE' TO GODK-STATUSKODER                                      
110460     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-3 SSA1                    
110470     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
110480     PERFORM IMS-STATUSKONTROLL                                           
110490     .                                                                    
110491     SKIP3                                                                
110492 IMS-GU-WDB601    SECTION.                                                
110493     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
110494          DELIMITED BY SIZE INTO SSA1                                     
110495     MOVE '  GE' TO GODK-STATUSKODER                                      
110496     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
110497     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
110498     PERFORM IMS-STATUSKONTROLL                                           
110499     IF SEGMENT-SAKNAS                                                    
110500         MOVE SPACE TO DCS-KDDC                                           
110501     END-IF                                                               
110502     .                                                                    
110510 IMS-STATUSKONTROLL SECTION.                                              
110600                                                                          
110700     SET STATUS-IX TO 1                                                   
110800     SEARCH GODK-STATUS                                                   
110900       AT END                                                             
111000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
111100         DELIMITED BY SIZE INTO FELTEXT                                   
111200         CALL FELLOG                                                      
111300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
111400         CONTINUE                                                         
111500     END-SEARCH                                                           
111600     .                                                                    
111610     EJECT                                                                
