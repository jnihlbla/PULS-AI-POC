000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4053800.                                                
000400*AUTHOR.         LASSI OLGRENER.  /Lena Bromander                         
000500*DATE-WRITTEN.   93/02/10.        /April 2020                             
000600                                                                          
000700*                                                                         
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        VAL AV TULLFAKTUROR                                              
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDM7                                       
001400*        PROGRAMMET LÄSER      WDM7A                                      
001500*        PROGRAMMET UPPDATERAR WDM8                                       
001600*        PROGRAMMET UPPDATERAR WDR1 (WDGX4588) Tullid                     
001700*        PROGRAMMET UPPDATERAR WDE6                                       
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T538                                              
002100*                     W4T538U                                             
002200*        MID:         W4I53801                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W4O53801                                            
002600*                                                                         
002700* CHANGE LOG:                                                             
002800*                                                                         
002900*      Apr 2020 - L Bromander     - MOVED THE BUSINESS LOGIC TO           
003000*                                   NEW PROGRAM W4053810.                 
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W4053800'.            
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  YES                         PIC X       VALUE 'Y'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +9    COMP SYNC.        
004700 77  MAX-KVRADER                 PIC S9(4)  VALUE +9    COMP SYNC.        
004800*77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004900 77  KDTRPT-IX                   PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  ANT-SEGM                    PIC S9(3)  VALUE +0    COMP-3.           
005100 77  LAENGD-OMSTART              PIC S9(4)  VALUE +170  COMP SYNC.        
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +682  COMP SYNC.        
005300 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
005400 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
005500 77  WS-KDFRAKT-NUM              PIC 9(2)    VALUE ZERO.                  
005600 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005700 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005800 77  WS-KDFRAKT                  PIC X(2)    VALUE SPACE.                 
005900 77  WS-IDSKEPPN                 PIC X(7)    VALUE SPACE.                 
006000 77  WS-IDFAKT                   PIC X(7)    VALUE SPACE.                 
006100 77  WS-FLCONTAIN                PIC X(1)    VALUE SPACE.                 
006200 77  WS-KDTRPTYP                 PIC 9       VALUE ZERO.                  
006300 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
006400 77  WS-IDBOKN                   PIC X(15)   VALUE SPACE.                 
006500 77  WS-IDFORDREG                PIC X(30)   VALUE SPACE.                 
006600 77  FOREG-IDFAKT                PIC S9(7)   VALUE ZERO COMP-3.           
006700 77  AKT-FLCONTAIN               PIC X(1)    VALUE SPACE.                 
006800 77  AKT-KDTRPTYP                PIC 9       VALUE ZERO.                  
006900 01  FILLER.                                                              
007000     03  WS-DATUM-TID.                                                    
007100       05 WS-DATUM               PIC X(8)    VALUE SPACE.                 
007200       05 WS-KLOCKAN             PIC 9(10)   VALUE ZERO.                  
007300                                                                          
007400 01  WS-IDTULL.                                                           
007500   03  FILLER                    PIC X(2)    VALUE 'VP'.                  
007600   03  WS-IDTULLNR               PIC X(7)    VALUE ZERO.                  
007700   03  WS-RETULKS                PIC X(1)    VALUE ZERO.                  
007800     EJECT                                                                
007900                                                                          
008000 77  RAD-SW                      PIC X       VALUE 'N'.                   
008100     88  INGEN-RAD-VALD                      VALUE 'N'.                   
008200     88  RAD-VALD                            VALUE 'J'.                   
008300                                                                          
008400 77  SEND-SW                     PIC X       VALUE 'N'.                   
008500     88  NOT-SEND                            VALUE 'N'.                   
008600                                                                          
008700 77  UPPDATERA-SW                PIC X       VALUE 'N'.                   
008800     88  UPPDATERA                           VALUE 'J'.                   
008900                                                                          
009000 77  BORTTAG-SW                  PIC X       VALUE 'N'.                   
009100     88  BORTTAG                             VALUE 'J'.                   
009200                                                                          
009300 77  RELEASEA-SW                 PIC X       VALUE 'N'.                   
009400     88  RELEASEA                            VALUE 'J'.                   
009500                                                                          
009600 77  RESTART-SW                  PIC X       VALUE 'N'.                   
009700     88  RESTART                             VALUE 'J'.                   
009800                                                                          
009900 77  OMSTART-SW                  PIC X       VALUE 'N'.                   
010000     88  OMSTART                             VALUE 'J'.                   
010100                                                                          
010200 77  WDM7-BORTTAGEN-SW           PIC X       VALUE 'N'.                   
010300     88  WDM7-BORTTAGEN                      VALUE 'J'.                   
010400                                                                          
010500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010600     88  INDATA-OK                           VALUE 'J'.                   
010700     88  INDATA-FEL                          VALUE 'N'.                   
010800                                                                          
010900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011000     88  NYCKLAR-OK                          VALUE 'J'.                   
011100     88  NYCKLAR-FEL                         VALUE 'N'.                   
011200                                                                          
011300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011400     88  EGEN-MID                            VALUE '4538'.                
011500     88  GODK-MID                            VALUE '4531' '4538'.         
011600     88  HELP-MID                            VALUE '0551'.                
011700     EJECT                                                                
011800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011900 01  GENERELLA-SUBPROGRAM.                                                
012000*                                                                         
012100     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
012200     03  W4053810                PIC X(8)    VALUE 'W4053810'.            
012300*lb  03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
012700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013000*01 -COPY WMSGINIT                                                        
013100     EJECT                                                                
013200*    --- REQU AREA                                                        
013300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
013400 01  REQU-AREA.                                                           
013500*    03 -COPY WZ01REQU                                                    
013600*    03 -COPY W40538I1                                                    
013700*    --- RESP AREA                                                        
013800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
013900 01  RESP-AREA.                                                           
014000*    03 -COPY WZ01RESP                                                    
014100*    03 -COPY W40538O1                                                    
014200     EJECT                                                                
014300*lb  nedan nytt                                                           
014400 01  SPAR-AREA.                                                           
014500     03  SPAR-IDTRANS             PIC X(4)    VALUE '4538'.               
014600     03  FILLER           PIC X(16) VALUE 'ENTER-NYCKLAR'.                
014700     03  SPAR-WDM7-WDM7A-KEYS-ENTER.                                      
014800         05  W-IDFAKT-ENTER       PIC S9(7)   VALUE ZERO  COMP-3.         
014900         05  W-IDORDNR5-ENTER     PIC S9(7)   VALUE ZERO  COMP-3.         
015000         05  W-IDKOLLI-ENTER      PIC S9(5)   VALUE ZERO  COMP-3.         
015100         05  W-IDPRODNR-ENTER     PIC S9(7)   VALUE ZERO  COMP-3.         
015200         05  W-IDDISTR-ENTER      PIC X(4)    VALUE SPACE.                
015300         05  W-IDDC-ENTER         PIC X(2)    VALUE SPACE.                
015400     03  FILLER           PIC X(16) VALUE 'NEXT-NYCKLAR'.                 
015500     03  SPAR-WDM7-WDM7A-KEYS-NEXT.                                       
015600         05  W-IDFAKT-NEXT        PIC S9(7)   VALUE ZERO  COMP-3.         
015700         05  W-IDORDNR5-NEXT      PIC S9(7)   VALUE ZERO  COMP-3.         
015800         05  W-IDKOLLI-NEXT       PIC S9(5)   VALUE ZERO  COMP-3.         
015900         05  W-IDPRODNR-NEXT      PIC S9(7)   VALUE ZERO  COMP-3.         
016000         05  W-IDDISTR-NEXT       PIC X(4)    VALUE SPACE.                
016100         05  W-IDDC-NEXT          PIC X(2)    VALUE SPACE.                
016200*    --- PARAMETRAR TILL SUBPROGRAM CHECK                                 
016300     SKIP3                                                                
016400*lb  CHECK-PARM.                                                          
016500*lb- TULLID POS 1--4 ÄR FTGID ASCII-FORM V=86, P=80                       
016600*--- TULLID POS 5--11 FYLLS I MED IDTULLNR VID ANROP.                     
016700*    03  TULLID                  PIC X(11)   VALUE '8680+TULLNR'.         
016800*    03  LGD                     PIC S9(4) COMP SYNC VALUE +11.           
016900*    03  WEIGHT                  PIC 9(11)   VALUE 21212121212.           
017000*    03  FIGURES                 PIC S9(4) COMP SYNC VALUE +11.           
017100*    03  KSIFFR                  PIC X(1).                                
017200*    03  MODUL-10-11             PIC 9(2)    VALUE 10.                    
017300*lb  03  ALT-A-B                 PIC X(1)    VALUE 'B'.                   
017400     EJECT                                                                
017500*Lb  --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017600*Lb****PY WMEDAREA                                                        
017700     EJECT                                                                
017800*    --- PARAMETRAR TILL SUBPROGRAM WL01MCNV                              
017900*01 -COPY WL01MCNV                                                        
018000     SKIP3                                                                
018100 01  MESSAGE-CODES.                                                       
018200*LB  03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
018300*    03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
018400*    03  ERR-INVOICE-MISSING     PIC X(3)    VALUE '320'.                 
018500*    03  ERR-INFO-MISSING        PIC X(3)    VALUE '760'.                 
018600*    03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
018700*    03  ERR-TULL-ID-NOT-CLOSED  PIC X(3)    VALUE '022'.                 
018800*    03  ERR-TWO-FUNCTIONS       PIC X(3)    VALUE '097'.                 
018900*    03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
019000*    03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019100*    03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019200*    03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
019300*    03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
019400*    03  INF-NO-MORE-INFO        PIC X(3)    VALUE '106'.                 
019500*    03  INF-SEL-CONSUMED        PIC X(3)    VALUE '131'.                 
019600*Lb  03  INF-DELETE-NOT-OK       PIC X(3)    VALUE '365'.                 
019700     03  INF-TULL-ID-ENDED.                                               
019800       05  FILLER                PIC X(10)   VALUE                        
019900                                          'Tull-id VP'.                   
020000       05  INF-IDTULLNR          PIC X(7).                                
020100       05  INF-RETULKS           PIC X(1).                                
020200       05  FILLER                PIC X(6)    VALUE ' klart'.              
020300     03  INF-IDFAKT-DELETED.                                              
020400       05  FILLER                PIC X(7)    VALUE                        
020500                                          'Faktnr '.                      
020600       05  INF-IDFAKT            PIC X(7).                                
020700       05  FILLER                PIC X(27)   VALUE                        
020800           ' borttaget ur Tullregistret'.                                 
020900     EJECT                                                                
021000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021100*                                                                         
021200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021300                                                                          
021400*01  MID -COPY W4I53801                                                   
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021700     SKIP3                                                                
021800*01  -COPY WMSGAREA                                                       
021900     EJECT                                                                
022000*      05  -COPY W4I53801 -PRE MOD- -RED MSG-MID-OUT                      
022100     EJECT                                                                
022200     03  MOD REDEFINES MSG-AREA.                                          
022300*      05  -COPY W4O53801                                                 
022400     EJECT                                                                
022500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022600     SKIP3                                                                
022700*01  -COPY WMFSAREA                                                       
022800*LB  EJECT                                                                
022900*01  FILLER                      PIC X(16)   VALUE 'MSGSOP-AREA'.         
023000*01    MSG-SOP-AREA.                                                      
023100*LB********OPY WMSGSOP                                                    
023200*lb  EJECT                                                                
023300*01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
023400 01  STATUS-WS                   PIC XX.                                  
023500     88  SEGMENT-FINNS                       VALUE '  '.                  
023600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023800     88  BASEN-SLUT                          VALUE 'GB'.                  
023900     SKIP2                                                                
024000 01  GODK-STATUSKODER.                                                    
024100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024200     SKIP3                                                                
024300     EJECT                                                                
024400*    --- IMS FUNKTIONSKODER                                               
024500*01  -COPY W0003                                                          
024600     EJECT                                                                
024700 LINKAGE SECTION.                                                         
024800                                                                          
024900*01  -COPY W0009   -PRE MSG-                                              
025000*01  -COPY W0009   -PRE ALT-                                              
025100*01  -COPY W0008  -PRE WDP7-                                              
025200     05  FILLER                  PIC X.                                   
025300*01  -COPY W0008  -PRE WDM7-                                              
025400     05  FILLER                  PIC X.                                   
025500*01  -COPY W0008  -PRE WDM7A-                                             
025600     05  FILLER                  PIC X.                                   
025700*01  -COPY W0008  -PRE WDM8-                                              
025800     05  FILLER                  PIC X.                                   
025900*01  -COPY W0008  -PRE 4587-                                              
026000     05  FILLER                  PIC X.                                   
026100*01  -COPY W0008  -PRE WDE6-                                              
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB   WDP7-PCB                    
026500                          WDM7-PCB  WDM7A-PCB WDM8-PCB                    
026600                          4587-PCB  WDE6-PCB.                             
026700                                                                          
026800 MAIN SECTION.                                                            
026900     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB   WDP7-PCB                    
027000                          WDM7-PCB  WDM7A-PCB WDM8-PCB                    
027100                          4587-PCB  WDE6-PCB.                             
027200                                                                          
027300                                                                          
027400                                                                          
027500     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
027600     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
027700                                                                          
027800*--- DISPLAY 'START W4053800 ' WS-DATUM ' ' WS-KLOCKAN                    
027900                                                                          
028000     PERFORM IMS-GET-MSG                                                  
028100     IF SEGMENT-FINNS                                                     
028200       PERFORM A-INIT                                                     
028300       PERFORM B-INIT-KEYS                                                
028400       PERFORM C-INIT-REQU                                                
028500       IF MFS-UPDATE                                                      
028600         SET REQU-UPDATE         TO TRUE                                  
028700         PERFORM E-SAMMA-SIDA                                             
028800       ELSE                                                               
028900         IF MFS-FIRST                                                     
029000                                                                          
029100           SET REQU-FIRST        TO TRUE                                  
029200           PERFORM MFS-RENSA-FAELT-IN                                     
029300           MOVE MFS-RENSA-FAELT  TO MOD-FLCONTAIN-UT                      
029400                                    MOD-IDLBBET-UT                        
029500                                    MOD-KDTRPTYP-UT                       
029600                                    MOD-IDBOKN-UT                         
029700*                                   MOD-IDFORDREG-UT                      
029800         ELSE                                                             
029900           IF MFS-NEXT                                                    
030000             SET REQU-NEXT       TO TRUE                                  
030100             PERFORM D-NAESTA-SIDA                                        
030200           ELSE                                                           
030300             SET REQU-QUERY      TO TRUE                                  
030400             PERFORM E-SAMMA-SIDA                                         
030500           END-IF                                                         
030600         END-IF                                                           
030700       END-IF                                                             
030800       PERFORM F-CALL-BIZ-LOGIC-W4053810                                  
030900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O53801 + 4                      
031000       PERFORM IMS-INSERT-MSG                                             
031100     END-IF                                                               
031200                                                                          
031300     MOVE ZERO TO RETURN-CODE                                             
031400     GOBACK                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 A-INIT SECTION.                                                          
031800                                                                          
031900     IF MSG-DUBBLA-TRANSKODER                                             
032000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I53801                 
032100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
032200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032300     ELSE                                                                 
032400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I53801                  
032500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
032600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032700     END-IF                                                               
032800                                                                          
032900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
033000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
033100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033200                                                                          
033300     MOVE LOW-VALUE TO MSG-AREA                                           
033400     MOVE 'W4O538N1' TO MFS-IDMOD                                         
033500     MOVE '4538' TO MOD-IDTRANS                                           
033600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033700                                                                          
033800     IF EGEN-MID OR HELP-MID                                              
033900       CONTINUE                                                           
034000     ELSE                                                                 
034100       MOVE SPACE TO MFS-KDTRTYP                                          
034200       MOVE '7' TO MFS-IDPFK                                              
034300     END-IF                                                               
034400                                                                          
034500     .                                                                    
034600     EJECT                                                                
034700 B-INIT-KEYS     SECTION.                                                 
034800                                                                          
034900     MOVE ALL '+'            TO MSGI-WMSGINIT                             
035000     MOVE '001'              TO MSGI-KDCALL                               
035100     IF EGEN-MID                                                          
035200        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
035300        MOVE MID-IDFAKT-IN   TO MSGI-IDFAKT                               
035400     END-IF                                                               
035500     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
035600     MOVE '4538'             TO MSGI-IDTRANS                              
035700     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
035800     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
035900                                                                          
036000     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
036100                                                                          
036200     MOVE MSGI-IDSPRAK       TO MCNV-IDSPRAK                              
036300                                                                          
036400     MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR-IN                            
036500                                MOD-IDFAKT-IN                             
036600                                                                          
036700     IF MID-IDFAKT-IN = ALL '+'                                           
036800       IF MID-IDDISTR-IN = ALL '+'                                        
036900         MOVE MID-IDFAKT-UT    TO WS-IDFAKT                               
037000         IF WS-IDFAKT NOT = SPACE                                         
037100           INSPECT WS-IDFAKT REPLACING LEADING SPACE BY ZERO              
037200         END-IF                                                           
037300       ELSE                                                               
037400         MOVE SPACE            TO WS-IDFAKT                               
037500       END-IF                                                             
037600     ELSE                                                                 
037700       MOVE MID-IDFAKT-IN      TO WS-IDFAKT                               
037800       MOVE '7'                TO MFS-IDPFK                               
037900       MOVE SPACE              TO MFS-KDTRTYP                             
038000     END-IF                                                               
038100                                                                          
038200*   Om idfakt ifylld, är dist ej intressant                               
038300     IF WS-IDFAKT = SPACE                                                 
038400       IF MID-IDDISTR-IN = ALL '+'                                        
038500         MOVE MID-IDDISTR-UT   TO WS-IDDISTR                              
038600         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
038700       ELSE                                                               
038800         MOVE MID-IDDISTR-IN   TO WS-IDDISTR                              
038900         MOVE '7'              TO MFS-IDPFK                               
039000         MOVE SPACE            TO MFS-KDTRTYP                             
039100       END-IF                                                             
039200     END-IF                                                               
039300                                                                          
039400                                                                          
039500     MOVE WS-IDFAKT            TO REQU-IDFAKT-KEY                         
039600     MOVE WS-IDDISTR           TO REQU-IDDISTR-KEY                        
039700                                                                          
039800     MOVE MSGI-IDDC            TO REQU-IDDC-KEY                           
039900                                                                          
040000     IF GODK-MID                                                          
040100       MOVE WS-IDDISTR         TO MOD-IDDISTR-UT                          
040200       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
040300       MOVE WS-IDFAKT          TO MOD-IDFAKT-UT                           
040400       INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE              
040500     ELSE                                                                 
040600       MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR-UT                          
040700                                  MOD-IDFAKT-UT                           
040800     END-IF                                                               
040900     .                                                                    
041000     EJECT                                                                
041100 C-INIT-REQU SECTION.                                                     
041200                                                                          
041300*lb  MOVE MAX-KVRADER            TO REQU-KVRADER                          
041400                                                                          
041500     MOVE MID-FLCONTAIN-UT       TO REQU-FLCONTAIN-UT                     
041600     MOVE MID-IDLBBET-UT         TO REQU-IDLBBET-UT                       
041700     MOVE MID-IDBOKN-UT          TO REQU-IDBOKN-UT                        
041800*    MOVE MID-IDFORDREG-UT       TO REQU-IDFORDREG-UT                     
041900                                                                          
042000     MOVE MID-FLSLUT             TO REQU-FLSLUT                           
042100     MOVE MID-FLBORT             TO REQU-FLBORT                           
042200     MOVE MID-KDTRPTYP           TO REQU-KDTRPTYP                         
042300     MOVE MID-FLCONTAIN          TO REQU-FLCONTAIN                        
042400     MOVE MID-IDLBBET            TO REQU-IDLBBET                          
042500     MOVE MID-IDBOKN             TO REQU-IDBOKN                           
042600     MOVE MID-IDFORDREG          TO REQU-IDFORDREG                        
042700     MOVE MSGI-IDUSER            TO REQU-IDUSER                           
042800                                                                          
042900                                                                          
043000     .                                                                    
043100                                                                          
043200                                                                          
043300 D-NAESTA-SIDA SECTION.                                                   
043400                                                                          
043500                                                                          
043600     IF SPAR-IDTRANS = '4538'                                             
043700       MOVE W-IDFAKT-NEXT    TO REQU-IDFAKT-START                         
043800       MOVE W-IDORDNR5-NEXT  TO REQU-IDORDNR5-START                       
043900       MOVE W-IDKOLLI-NEXT   TO REQU-IDKOLLI-START                        
044000       MOVE W-IDPRODNR-NEXT  TO REQU-IDPRODNR-START                       
044100       MOVE W-IDDISTR-NEXT   TO REQU-IDDISTR-START                        
044200       MOVE W-IDDC-NEXT      TO REQU-IDDC-START                           
044300                                                                          
044400       PERFORM MFS-RENSA-FAELT-IN                                         
044500     ELSE                                                                 
044600       PERFORM MFS-RENSA-FAELT-IN                                         
044700     END-IF                                                               
044800     .                                                                    
044900                                                                          
045000 E-SAMMA-SIDA SECTION.                                                    
045100                                                                          
045200     IF SPAR-IDTRANS = '4538' OR '0551'                                   
045300       MOVE W-IDFAKT-ENTER   TO REQU-IDFAKT-START                         
045400       MOVE W-IDORDNR5-ENTER TO REQU-IDORDNR5-START                       
045500       MOVE W-IDKOLLI-ENTER  TO REQU-IDKOLLI-START                        
045600       MOVE W-IDPRODNR-ENTER TO REQU-IDPRODNR-START                       
045700       MOVE W-IDDISTR-ENTER  TO REQU-IDDISTR-START                        
045800       MOVE W-IDDC-ENTER     TO REQU-IDDC-START                           
045900     ELSE                                                                 
046000       PERFORM MFS-RENSA-FAELT-IN                                         
046100     END-IF                                                               
046200     .                                                                    
046300                                                                          
046400     EJECT                                                                
046500                                                                          
046600 F-CALL-BIZ-LOGIC-W4053810 SECTION.                                       
046700                                                                          
046800     CALL W4053810 USING REQU-AREA RESP-AREA MAX-KVRADER                  
046900                          ALT-PCB                                         
047000                          WDM7-PCB  WDM7A-PCB WDM8-PCB                    
047100                          4587-PCB  WDE6-PCB                              
047200                                                                          
047300     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
047400        RESP-IDMSG-INFO  NOT = SPACE                                      
047500       PERFORM FA-SET-MSG-AND-HILIGHT                                     
047600     END-IF                                                               
047700     PERFORM FB-MOVE-RESP-TO-MOD                                          
047800                                                                          
047900* SAVE START AND NEXT KEYS IN PROFILE DB                                  
048000                                                                          
048100     MOVE RESP-IDFAKT-START      TO W-IDFAKT-ENTER                        
048200     MOVE RESP-IDORDNR5-START    TO W-IDORDNR5-ENTER                      
048300     MOVE RESP-IDKOLLI-START     TO W-IDKOLLI-ENTER                       
048400     MOVE RESP-IDPRODNR-START    TO W-IDPRODNR-ENTER                      
048500     MOVE RESP-IDDISTR-START     TO W-IDDISTR-ENTER                       
048600     MOVE RESP-IDDC-START        TO W-IDDC-ENTER                          
048700                                                                          
048800     MOVE RESP-IDFAKT-NEXT       TO W-IDFAKT-NEXT                         
048900     MOVE RESP-IDORDNR5-NEXT     TO W-IDORDNR5-NEXT                       
049000     MOVE RESP-IDKOLLI-NEXT      TO W-IDKOLLI-NEXT                        
049100     MOVE RESP-IDPRODNR-NEXT     TO W-IDPRODNR-NEXT                       
049200     MOVE RESP-IDDISTR-NEXT      TO W-IDDISTR-NEXT                        
049300     MOVE RESP-IDDC-NEXT         TO W-IDDC-NEXT                           
049400                                                                          
049500     MOVE '002'                  TO MSGI-KDCALL                           
049600     MOVE '4538'                 TO SPAR-IDTRANS                          
049700     MOVE SPAR-AREA              TO MSGI-SPAR-AREA                        
049800     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
049900     .                                                                    
050000                                                                          
050100     EJECT                                                                
050200 FA-SET-MSG-AND-HILIGHT SECTION.                                          
050300                                                                          
050400*lb problem då error var 025 och info var 001i samma anrop                
050500*lb endast error-medd kom i retur.                                        
050600*lb Funkar för andra kombinationer ...hmm                                 
050700*lb                                                                       
050800*lb anropa 2 ggr, nu nr 1 error                                           
050900                                                                          
051000     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
051100     MOVE space                  TO MCNV-IDMSG-INFO                       
051200*--- MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
051300     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
051400                                                                          
051500     CALL WL01MCNV USING MCNV-AREA                                        
051600     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
051700*--- MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
051800                                                                          
051900*lb anropa 2 ggr, nu nr 2 info                                            
052000     MOVE space                  TO MCNV-IDMSG-ERROR                      
052100     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
052200     MOVE space                  TO MCNV-IDELMT-ERROR                     
052300                                                                          
052400     CALL WL01MCNV USING MCNV-AREA                                        
052500*--- MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
052600     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
052700                                                                          
052800     .                                                                    
052900                                                                          
053000     EJECT                                                                
053100 FB-MOVE-RESP-TO-MOD SECTION.                                             
053200                                                                          
053300     MOVE RESP-FLSLUT-ATTR    TO MOD-FLSLUT-ATTR                          
053400     IF RESP-FLSLUT = SPACE                                               
053500       MOVE MFS-RENSA-FAELT      TO MOD-FLSLUT                            
053600     ELSE                                                                 
053700       IF RESP-FLSLUT = ALL '+'                                           
053800         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLSLUT                            
053900       ELSE                                                               
054000         MOVE RESP-FLSLUT                                                 
054100                                 TO MOD-FLSLUT                            
054200       END-IF                                                             
054300     END-IF                                                               
054400                                                                          
054500     MOVE RESP-FLBORT-ATTR    TO MOD-FLBORT-ATTR                          
054600     IF RESP-FLBORT = SPACE                                               
054700       MOVE MFS-RENSA-FAELT      TO MOD-FLBORT                            
054800     ELSE                                                                 
054900       IF RESP-FLBORT = ALL '+'                                           
055000         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLBORT                            
055100       ELSE                                                               
055200         MOVE RESP-FLBORT                                                 
055300                                 TO MOD-FLBORT                            
055400       END-IF                                                             
055500     END-IF                                                               
055600                                                                          
055700     MOVE RESP-KDTRPTYP-ATTR  TO MOD-KDTRPTYP-ATTR                        
055800     IF RESP-KDTRPTYP = SPACE                                             
055900       MOVE MFS-RENSA-FAELT      TO MOD-KDTRPTYP                          
056000     ELSE                                                                 
056100       IF RESP-KDTRPTYP = ALL '+'                                         
056200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDTRPTYP                          
056300       ELSE                                                               
056400         MOVE RESP-KDTRPTYP                                               
056500                                 TO MOD-KDTRPTYP                          
056600       END-IF                                                             
056700     END-IF                                                               
056800                                                                          
056900     MOVE RESP-FLCONTAIN-ATTR  TO MOD-FLCONTAIN-ATTR                      
057000     IF RESP-FLCONTAIN = SPACE                                            
057100       MOVE MFS-RENSA-FAELT      TO MOD-FLCONTAIN                         
057200     ELSE                                                                 
057300       IF RESP-FLCONTAIN = ALL '+'                                        
057400         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLCONTAIN                         
057500       ELSE                                                               
057600         MOVE RESP-FLCONTAIN                                              
057700                                 TO MOD-FLCONTAIN                         
057800       END-IF                                                             
057900     END-IF                                                               
058000                                                                          
058100     MOVE RESP-IDLBBET-ATTR  TO MOD-IDLBBET-ATTR                          
058200     IF RESP-IDLBBET = SPACE                                              
058300       MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET                           
058400     ELSE                                                                 
058500       IF RESP-IDLBBET = ALL '+'                                          
058600         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLBBET                           
058700       ELSE                                                               
058800         MOVE RESP-IDLBBET                                                
058900                                 TO MOD-IDLBBET                           
059000       END-IF                                                             
059100     END-IF                                                               
059200                                                                          
059300     MOVE RESP-IDBOKN-ATTR  TO MOD-IDBOKN-ATTR                            
059400     IF RESP-IDBOKN = SPACE                                               
059500       MOVE MFS-RENSA-FAELT      TO MOD-IDBOKN                            
059600     ELSE                                                                 
059700       IF RESP-IDBOKN = ALL '+'                                           
059800         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDBOKN                            
059900       ELSE                                                               
060000         MOVE RESP-IDBOKN                                                 
060100                                 TO MOD-IDBOKN                            
060200       END-IF                                                             
060300     END-IF                                                               
060400                                                                          
060500     MOVE RESP-IDFORDREG-ATTR TO MOD-IDFORDREG-ATTR                       
060600     IF RESP-IDFORDREG = SPACE                                            
060700       MOVE MFS-RENSA-FAELT      TO MOD-IDFORDREG                         
060800     ELSE                                                                 
060900       IF RESP-IDFORDREG = ALL '+'                                        
061000         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFORDREG                         
061100       ELSE                                                               
061200         MOVE RESP-IDFORDREG                                              
061300                                 TO MOD-IDFORDREG                         
061400       END-IF                                                             
061500     END-IF                                                               
061600                                                                          
061700     IF RESP-KDTRPTYP-UT = SPACE                                          
061800       MOVE MFS-RENSA-FAELT      TO MOD-KDTRPTYP-UT                       
061900     ELSE                                                                 
062000       IF RESP-KDTRPTYP-UT = ALL '+'                                      
062100         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDTRPTYP-UT                       
062200       ELSE                                                               
062300         MOVE RESP-KDTRPTYP-UT                                            
062400                                 TO MOD-KDTRPTYP-UT                       
062500       END-IF                                                             
062600     END-IF                                                               
062700                                                                          
062800     IF RESP-FLCONTAIN-UT = SPACE                                         
062900       MOVE MFS-RENSA-FAELT      TO MOD-FLCONTAIN-UT                      
063000     ELSE                                                                 
063100       IF RESP-FLCONTAIN-UT = ALL '+'                                     
063200         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLCONTAIN-UT                      
063300       ELSE                                                               
063400         MOVE RESP-FLCONTAIN-UT                                           
063500                                 TO MOD-FLCONTAIN-UT                      
063600       END-IF                                                             
063700     END-IF                                                               
063800                                                                          
063900     IF RESP-IDLBBET-UT = SPACE                                           
064000       MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-UT                        
064100     ELSE                                                                 
064200       IF RESP-IDLBBET-UT = ALL '+'                                       
064300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLBBET-UT                        
064400       ELSE                                                               
064500         MOVE RESP-IDLBBET-UT                                             
064600                                 TO MOD-IDLBBET-UT                        
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     IF RESP-IDBOKN-UT = SPACE                                            
065100       MOVE MFS-RENSA-FAELT      TO MOD-IDBOKN-UT                         
065200     ELSE                                                                 
065300       IF RESP-IDBOKN-UT = ALL '+'                                        
065400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDBOKN-UT                         
065500       ELSE                                                               
065600         MOVE RESP-IDBOKN-UT                                              
065700                                 TO MOD-IDBOKN-UT                         
065800       END-IF                                                             
065900     END-IF                                                               
066000                                                                          
066100*    IF RESP-IDFORDREG-UT = SPACE                                         
066200*      MOVE MFS-RENSA-FAELT      TO MOD-IDFORDREG-UT                      
066300*    ELSE                                                                 
066400*      IF RESP-IDFORDREG-UT = ALL '+'                                     
066500*        MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFORDREG-UT                      
066600*      ELSE                                                               
066700*        MOVE RESP-IDFORDREG-UT                                           
066800*                                TO MOD-IDFORDREG-UT                      
066900*      END-IF                                                             
067000*    END-IF                                                               
067100                                                                          
067200*lb  ifyllda rader                                                        
067300     PERFORM                                                              
067400     VARYING INDX FROM +1 BY +1                                           
067500       UNTIL INDX > RESP-KVRADER                                          
067600                                                                          
067700       IF RESP-idfakt (INDX) = SPACE                                      
067800         MOVE MFS-RENSA-FAELT    TO MOD-idfakt (INDX)                     
067900       ELSE                                                               
068000         IF RESP-IDFAKT (INDX) = ALL '+'                                  
068100           MOVE MFS-ROER-EJ-FAELT                                         
068200                                 TO MOD-IDFAKT (INDX)                     
068300         ELSE                                                             
068400           MOVE RESP-IDFAKT (INDX)                                        
068500                                 TO MOD-IDFAKT (INDX)                     
068600         END-IF                                                           
068700       END-IF                                                             
068800                                                                          
068900       IF RESP-TIFAKT (INDX) = SPACE                                      
069000         MOVE MFS-RENSA-FAELT    TO MOD-TIFAKT (INDX)                     
069100       ELSE                                                               
069200         IF RESP-TIFAKT (INDX) = ALL '+'                                  
069300           MOVE MFS-ROER-EJ-FAELT                                         
069400                                 TO MOD-TIFAKT (INDX)                     
069500         ELSE                                                             
069600           MOVE RESP-TIFAKT (INDX)                                        
069700                                 TO MOD-TIFAKT (INDX)                     
069800         END-IF                                                           
069900       END-IF                                                             
070000                                                                          
070100       IF RESP-IDDISTR (INDX) = SPACE                                     
070200         MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR (INDX)                    
070300       ELSE                                                               
070400         IF RESP-IDDISTR (INDX) = ALL '+'                                 
070500           MOVE MFS-ROER-EJ-FAELT                                         
070600                                 TO MOD-IDDISTR (INDX)                    
070700         ELSE                                                             
070800           MOVE RESP-IDDISTR (INDX)                                       
070900                                 TO MOD-IDDISTR (INDX)                    
071000         END-IF                                                           
071100       END-IF                                                             
071200                                                                          
071300       IF RESP-IDKUNDNR (INDX) = SPACE                                    
071400         MOVE MFS-RENSA-FAELT    TO MOD-IDKUNDNR (INDX)                   
071500       ELSE                                                               
071600         IF RESP-IDKUNDNR (INDX) = ALL '+'                                
071700           MOVE MFS-ROER-EJ-FAELT                                         
071800                                 TO MOD-IDKUNDNR (INDX)                   
071900         ELSE                                                             
072000           MOVE RESP-IDKUNDNR (INDX)                                      
072100                                 TO MOD-IDKUNDNR (INDX)                   
072200         END-IF                                                           
072300       END-IF                                                             
072400                                                                          
072500       IF RESP-IDORDNR5 (INDX) = SPACE                                    
072600         MOVE MFS-RENSA-FAELT    TO MOD-IDORDNR5 (INDX)                   
072700       ELSE                                                               
072800         IF RESP-IDORDNR5 (INDX) = ALL '+'                                
072900           MOVE MFS-ROER-EJ-FAELT                                         
073000                                 TO MOD-IDORDNR5 (INDX)                   
073100         ELSE                                                             
073200           MOVE RESP-IDORDNR5 (INDX)                                      
073300                                 TO MOD-IDORDNR5 (INDX)                   
073400         END-IF                                                           
073500       END-IF                                                             
073600                                                                          
073700       IF RESP-IDKOLLI  (INDX) = SPACE                                    
073800         MOVE MFS-RENSA-FAELT    TO MOD-IDKOLLI  (INDX)                   
073900       ELSE                                                               
074000         IF RESP-IDKOLLI  (INDX) = ALL '+'                                
074100           MOVE MFS-ROER-EJ-FAELT                                         
074200                                 TO MOD-IDKOLLI  (INDX)                   
074300         ELSE                                                             
074400           MOVE RESP-IDKOLLI  (INDX)                                      
074500                                 TO MOD-IDKOLLI  (INDX)                   
074600         END-IF                                                           
074700       END-IF                                                             
074800                                                                          
074900       IF RESP-KDORDKL  (INDX) = SPACE                                    
075000         MOVE MFS-RENSA-FAELT    TO MOD-KDORDKL  (INDX)                   
075100       ELSE                                                               
075200         IF RESP-KDORDKL  (INDX) = ALL '+'                                
075300           MOVE MFS-ROER-EJ-FAELT                                         
075400                                 TO MOD-KDORDKL  (INDX)                   
075500         ELSE                                                             
075600           MOVE RESP-KDORDKL  (INDX)                                      
075700                                 TO MOD-KDORDKL  (INDX)                   
075800         END-IF                                                           
075900       END-IF                                                             
076000                                                                          
076100       IF RESP-FLORDSPE (INDX) = SPACE                                    
076200         MOVE MFS-RENSA-FAELT    TO MOD-FLORDSPE (INDX)                   
076300       ELSE                                                               
076400         IF RESP-FLORDSPE (INDX) = ALL '+'                                
076500           MOVE MFS-ROER-EJ-FAELT                                         
076600                                 TO MOD-FLORDSPE (INDX)                   
076700         ELSE                                                             
076800           MOVE RESP-FLORDSPE (INDX)                                      
076900                                 TO MOD-FLORDSPE (INDX)                   
077000         END-IF                                                           
077100       END-IF                                                             
077200                                                                          
077300       IF RESP-IDPRODNR (INDX) = SPACE                                    
077400         MOVE MFS-RENSA-FAELT    TO MOD-IDPRODNR (INDX)                   
077500       ELSE                                                               
077600         IF RESP-IDPRODNR (INDX) = ALL '+'                                
077700           MOVE MFS-ROER-EJ-FAELT                                         
077800                                 TO MOD-IDPRODNR (INDX)                   
077900         ELSE                                                             
078000           MOVE RESP-IDPRODNR (INDX)                                      
078100                                 TO MOD-IDPRODNR (INDX)                   
078200         END-IF                                                           
078300       END-IF                                                             
078400                                                                          
078500       IF RESP-TIREGDAT (INDX) = SPACE                                    
078600         MOVE MFS-RENSA-FAELT    TO MOD-TIREGDAT (INDX)                   
078700       ELSE                                                               
078800         IF RESP-TIREGDAT (INDX) = ALL '+'                                
078900           MOVE MFS-ROER-EJ-FAELT                                         
079000                                 TO MOD-TIREGDAT (INDX)                   
079100         ELSE                                                             
079200           MOVE RESP-TIREGDAT (INDX)                                      
079300                                 TO MOD-TIREGDAT (INDX)                   
079400         END-IF                                                           
079500       END-IF                                                             
079600                                                                          
079700       IF RESP-TIREGTID (INDX) = SPACE                                    
079800         MOVE MFS-RENSA-FAELT    TO MOD-TIREGTID (INDX)                   
079900       ELSE                                                               
080000         IF RESP-TIREGTID (INDX) = ALL '+'                                
080100           MOVE MFS-ROER-EJ-FAELT                                         
080200                                 TO MOD-TIREGTID (INDX)                   
080300         ELSE                                                             
080400           MOVE RESP-TIREGTID (INDX)                                      
080500                                 TO MOD-TIREGTID (INDX)                   
080600         END-IF                                                           
080700       END-IF                                                             
080800                                                                          
080900       IF RESP-FLKLAR (INDX) = SPACE                                      
081000         MOVE MFS-RENSA-FAELT    TO MOD-FLKLAR (INDX)                     
081100       ELSE                                                               
081200         IF RESP-FLKLAR (INDX) = ALL '+'                                  
081300           MOVE MFS-ROER-EJ-FAELT                                         
081400                                 TO MOD-FLKLAR (INDX)                     
081500         ELSE                                                             
081600           MOVE RESP-FLKLAR (INDX)                                        
081700                                 TO MOD-FLKLAR (INDX)                     
081800         END-IF                                                           
081900       END-IF                                                             
082000                                                                          
082100                                                                          
082200     END-PERFORM                                                          
082300                                                                          
082400*lb  tomma rader upp till 9                                               
082500     PERFORM                                                              
082600     VARYING INDX FROM INDX BY +1                                         
082700       UNTIL INDX > MAX-INDX                                              
082800       MOVE MFS-RENSA-FAELT      TO                                       
082900                                                                          
083000                                    MOD-IDFAKT  (INDX)                    
083100                                    MOD-TIFAKT  (INDX)                    
083200                                    MOD-IDDISTR (INDX)                    
083300                                    MOD-IDKUNDNR (INDX)                   
083400                                    MOD-IDORDNR5 (INDX)                   
083500                                    MOD-IDKOLLI (INDX)                    
083600                                    MOD-KDORDKL (INDX)                    
083700                                    MOD-FLORDSPE (INDX)                   
083800                                    MOD-IDPRODNR (INDX)                   
083900                                    MOD-TIREGDAT (INDX)                   
084000                                    MOD-TIREGTID (INDX)                   
084100                                    MOD-FLKLAR   (INDX)                   
084200     END-PERFORM                                                          
084300     .                                                                    
084400                                                                          
084500                                                                          
084600 MFS-RENSA-FAELT-IN SECTION.                                              
084700                                                                          
084800*    --- ALLA INDATA-FÄLT                                                 
084900     MOVE MFS-RENSA-FAELT      TO MOD-FLBORT                              
085000                                  MOD-FLSLUT                              
085100                                  MOD-KDTRPTYP                            
085200                                  MOD-FLCONTAIN                           
085300                                  MOD-IDLBBET                             
085400                                  MOD-IDBOKN                              
085500                                  MOD-IDFORDREG                           
085600     .                                                                    
085700                                                                          
085800                                                                          
085900     EJECT                                                                
086000     EJECT                                                                
086100                                                                          
086200*----IMS SEKTIONER----                                                    
086300                                                                          
086400     SKIP3                                                                
086500 IMS-GET-MSG SECTION.                                                     
086600                                                                          
086700     MOVE '  QC' TO GODK-STATUSKODER                                      
086800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
086900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087000     PERFORM IMS-STATUSKONTROLL                                           
087100     .                                                                    
087200     SKIP3                                                                
087300 IMS-INSERT-MSG SECTION.                                                  
087400                                                                          
087500     IF  MSGI-IDLAND-SPR NOT = 'GB'                                       
087600         MOVE '0' TO MFS-KDHUVOMR                                         
087700     END-IF                                                               
087800                                                                          
087900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
088000     MOVE SPACE TO GODK-STATUSKODER                                       
088100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
088200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088300     PERFORM IMS-STATUSKONTROLL                                           
088400     .                                                                    
088500     SKIP3                                                                
088600 IMS-STATUSKONTROLL SECTION.                                              
088700                                                                          
088800     SET STATUS-IX TO 1                                                   
088900     SEARCH GODK-STATUS                                                   
089000       AT END                                                             
089100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
089200         DELIMITED BY SIZE INTO FELTEXT                                   
089300         CALL FELLOG                                                      
089400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
089500         CONTINUE                                                         
089600     END-SEARCH                                                           
089700     .                                                                    
