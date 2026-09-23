000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6018210.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN / ARCHANA BHAT.                     
000400 DATE-WRITTEN.   99/08/24 / JULY 2012.                                    
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        REGISTRERING AV FÖRPACKNINGS EMBALLAGE                           
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        REQU:        W60182I1                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        RESP:        W60182O1                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W6018210'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003400                                                                          
003500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003600     88  INDATA-OK                           VALUE 'J'.                   
003700     88  INDATA-FEL                          VALUE 'N'.                   
003800                                                                          
003900                                                                          
004000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004100     88  NYCKLAR-OK                          VALUE 'J'.                   
004200     88  NYCKLAR-FEL                         VALUE 'N'.                   
004300                                                                          
004400 77  EMBX-SW                     PIC X       VALUE 'J'.                   
004500     88  EMBX-OK                             VALUE 'J'.                   
004600     88  EMBX-FEL                            VALUE 'N'.                   
004700                                                                          
004800 01  NYTT-13-SEGMENT-TABLE.                                               
004900     03  NYTT-13-SEGMENT OCCURS 14  PIC X.                                
005000                                                                          
005100 01  WQ-TAB.                                                              
005200     03  FILLER OCCURS 3.                                                 
005300         05  WQ-IDARTNR          PIC 9(9).                                
005400         05  WQ-KDEMBKOD         PIC 9(3).                                
005500     03  FILLER OCCURS 5.                                                 
005600         05  WQ-KVQPACK          PIC 9(5).                                
005700                                                                          
005800 01  WX-TAB.                                                              
005900     03  FILLER OCCURS 10.                                                
006000         05  WX-IDARTNR          PIC 9(9).                                
006100         05  WX-KVQPACK          PIC 9(5).                                
006200                                                                          
006300 01  W-IDARTNR-SPAR              PIC S9(9) COMP-3 VALUE ZERO.             
006400 01  KW-IDARTNR-SPAR             PIC S9(9) COMP-3 VALUE ZERO.             
006500 01  IX-X                        PIC S9(3) COMP-3 VALUE ZERO.             
006600 01  INDX                        PIC S9(3) COMP-3 VALUE ZERO.             
006700 01  MAX-KVRADER-WEB             PIC S9(3) COMP-3 VALUE ZERO.             
006800 01  ARBETSAREOR.                                                         
006900     03  DAGENS-DATUM            PIC 9(6)  VALUE ZERO.                    
007000     03  WS-TIUPPDAT-EMB         PIC 9(6)  VALUE ZERO.                    
007100     03  WS-BEFT                 PIC 9(2)  VALUE ZERO.                    
007200     03  WS-KDEMBKEY             PIC X(3)  VALUE SPACE.                   
007300     03  FILLER  REDEFINES WS-KDEMBKEY.                                   
007400         05  FILLER              PIC X(1).                                
007500         05  WS-KDEMBKEY-IX      PIC 9(2).                                
007600     03  WS-KDEMBKEY-1           PIC X(3)  VALUE SPACE.                   
007700     03  FILLER  REDEFINES WS-KDEMBKEY-1.                                 
007800         05  FILLER              PIC X(1).                                
007900         05  WS-KDEMBKEY-1-IX    PIC 9(2).                                
008000 01  ALL-SPACE.                                                           
008100     03 FILLER                   PIC X(50)   VALUE SPACE.                 
008200*                                                                         
008300 01  ALL-PLUS.                                                            
008400     03 FILLER                   PIC X(50)   VALUE ALL '+'.               
008500     EJECT                                                                
008600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008700 01  GENERELLA-SUBPROGRAM.                                                
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
009100     EJECT                                                                
009200     SKIP3                                                                
009300 01  MESSAGE-CODES.                                                       
009400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
009500     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
009600     03  INF-PRESS-PF23          PIC X(3)    VALUE '013'.                 
009700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
009800     03  ERR-PART-MISSING        PIC X(3)    VALUE '025'.                 
009900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
010000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010100     03  ERR-REFILL-PART         PIC X(3)    VALUE '386'.                 
010200     EJECT                                                                
010300 01  FELTEXTER.                                                           
010400     03  FELTEXT1 PIC X(36)                                               
010500                 VALUE 'INGEN BEHÖRIGHET FÖR PF23 UPDATERING'.            
010600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010900     SKIP3                                                                
011000*01 -COPY WMSGINIT                                                        
011100     EJECT                                                                
011200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011300*                                                                         
011400 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
011500*01  -COPY WMSGAREA                                                       
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011800*01  -COPY WMFSAREA                                                       
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'WWDC99'.              
012100*01  -COPY WWDC99                                                         
012200     EJECT                                                                
012300 01  FILLER                      PIC X(16)   VALUE 'WWLNDKON'.            
012400*01  -COPY WWLNDKON                                                       
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL W005WDK7                                         
012700 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
012800*   -COPY W005WDK7                                                        
012900     EJECT                                                                
013000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013100*                                                                         
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013400     SKIP3                                                                
013500 01  NYCKLAR-TILL-DLI.                                                    
013600     03  W-IDARTNR-X.                                                     
013700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013800     03  KW-IDARTNR-X.                                                    
013900         05  KW-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
014000     03  W-KDSEGKEY-X.                                                    
014100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
014200     03  W-KDEMBAL-X.                                                     
014300         05  W-KDEMBAL           PIC X(3)    VALUE SPACE.                 
014400     03  KW-KDEMBAL-X.                                                    
014500         05  KW-KDEMBAL          PIC X(3)    VALUE SPACE.                 
014600     03  W-IDLAND-X.                                                      
014700         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
014800     03  W-IDDC-X.                                                        
014900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015000     SKIP2                                                                
015100*    --- STATUS-KOD FRÅN IMS                                              
015200 01  STATUS-WS                   PIC XX.                                  
015300     88  SEGMENT-FINNS                       VALUE '  '.                  
015400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015600     SKIP2                                                                
015700 01  GODK-STATUSKODER.                                                    
015800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015900     SKIP3                                                                
016000 01  SSA1                        PIC X(64).                               
016100 01  SSA2                        PIC X(64).                               
016200 01  SSA3                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800                                                                          
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
017000 01  DLI-IO-WDK601.                                                       
017100*    03  -COPY WDK601                                                     
017200     EJECT                                                                
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
017400 01  DLI-IO-WDK611.                                                       
017500*    03  -COPY WDK611                                                     
017600     EJECT                                                                
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
017800 01  DLI-IO-WDK613.                                                       
017900*    03  -COPY WDK613                                                     
018000     EJECT                                                                
018100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
018200 01  DLI-IO-WDK701.                                                       
018300*    03  -COPY WDK701                                                     
018400     EJECT                                                                
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018600 01  DLI-IO-WDK711.                                                       
018700*    03  -COPY WDK711                                                     
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
019000 01  DLI-IO-WDK712.                                                       
019100*    03  -COPY WDK712                                                     
019200     EJECT                                                                
019300*    ---  DLI INPUT-OUTPUT AREA   FÖR KOPIERING                           
019400                                                                          
019500 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDK726'.                     
019600 01  DLI-IO-WDK726.                                                       
019700*  03   -COPY WDK726                                                      
019800                                                                          
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-KWDK601'.                     
020000 01  DLI-IO-KWDK601.                                                      
020100*    03  -COPY WDK601 -PRE K                                              
020200     EJECT                                                                
020300 01  FILLER         PIC X(16) VALUE 'DLI-IO-KWDK611'.                     
020400 01  DLI-IO-KWDK611.                                                      
020500*    03  -COPY WDK611  -PRE K                                             
020600     EJECT                                                                
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-KWDK613'.                     
020800 01  DLI-IO-KWDK613.                                                      
020900*    03  -COPY WDK613  -PRE K                                             
021000     EJECT                                                                
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311'.                      
021200 01  DLI-IO-WDT311.                                                       
021300*    03  -COPY WDT311                                                     
021400     EJECT                                                                
021500 LINKAGE SECTION.                                                         
021600 01  REQU-AREA.                                                           
021700*    03 -COPY WZ01REQU                                                    
021800*    03 -COPY W60182I1                                                    
021900     EJECT                                                                
022000 01  RESP-AREA.                                                           
022100*    03 -COPY WZ01RESP                                                    
022200*    03 -COPY W60182O1                                                    
022300     EJECT                                                                
022400 01  MAX-KVRADER                 PIC S9(4) COMP.                          
022500*                                                                         
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE WDK6-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000*01  -COPY W0008  -PRE KWDK6-                                             
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008  -PRE WDT3-                                              
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008  -PRE WDK7-                                              
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023900*01  -COPY W0008  -PRE WDB6-                                              
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
024300                           WDK6-PCB KWDK6-PCB WDT3-PCB WDK7-PCB           
024400                           WDB6-PCB.                                      
024500                                                                          
024600 MAIN SECTION.                                                            
024700                                                                          
024800     PERFORM A-INIT                                                       
024900     PERFORM B-KOLLA-NYCKLAR                                              
025000     IF NYCKLAR-OK                                                        
025100       IF REQU-UPDATE OR REQU-UPD-V                                       
025200         PERFORM G-KOLLA-INPUT                                            
025300         IF INDATA-OK                                                     
025400           PERFORM H-UPPDATERA                                            
025500         END-IF                                                           
025600       ELSE                                                               
025700         IF REQU-FIRST                                                    
025800           PERFORM C-FOERSTA-SIDA                                         
025900         ELSE                                                             
026000           PERFORM E-SAMMA-SIDA                                           
026100         END-IF                                                           
026200       END-IF                                                             
026300       PERFORM F-LAES-VISA-INFO                                           
026400     END-IF                                                               
026500                                                                          
026600     GOBACK                                                               
026700     .                                                                    
026800     EJECT                                                                
026900 A-INIT SECTION.                                                          
027000                                                                          
027100     MOVE ALL-PLUS     TO RESP-W60182O1                                   
027200     PERFORM MFS-FORM-ATTR                                                
027300                                                                          
027400     MOVE 001          TO RESP-IDMSGVER                                   
027500     MOVE SPACE        TO RESP-IDMSG-ERROR                                
027600                          RESP-IDMSG-INFO                                 
027700                          RESP-IDELMT-ERROR                               
027800     ACCEPT DAGENS-DATUM FROM DATE                                        
027900     .                                                                    
028000     EJECT                                                                
028100 B-KOLLA-NYCKLAR SECTION.                                                 
028200                                                                          
028300     MOVE JA  TO NYCKLAR-SW                                               
028400     MOVE NEJ TO EMBX-SW                                                  
028500*    -- KONTROLL AV IDARTNR                                               
028600     IF REQU-IDARTNR-KEY NUMERIC                                          
028700       MOVE REQU-IDARTNR-KEY TO W-IDARTNR W-IDARTNR-SPAR                  
028800     ELSE                                                                 
028900       MOVE NEJ TO NYCKLAR-SW                                             
028901     END-IF                                                               
028902     IF REQU-IDDC-KEY NUMERIC                                             
028903        MOVE REQU-IDDC-KEY      TO WS-IDDC                                
028904                                   W-IDDC                                 
028905     ELSE                                                                 
028906        MOVE REQU-IDDC          TO WS-IDDC                                
028907                                   W-IDDC                                 
028908     END-IF                                                               
028909                                                                          
028910     IF NYCKLAR-FEL                                                       
028920       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
028930       PERFORM MFS-RENSA-FAELT-IN                                         
028940       PERFORM MFS-RENSA-FAELT-UT                                         
028950     END-IF                                                               
028960                                                                          
028970     .                                                                    
028980     EJECT                                                                
028990 C-FOERSTA-SIDA SECTION.                                                  
029000                                                                          
029100     PERFORM MFS-RENSA-FAELT-IN                                           
029200     .                                                                    
029300     EJECT                                                                
029400 E-SAMMA-SIDA SECTION.                                                    
029500                                                                          
029600     MOVE +1 TO IX-X                                                      
029700     MOVE +0 TO INDX                                                      
029800     PERFORM UNTIL IX-X > MAX-KVRADER                                     
029900       IF REQU-EMB-X (IX-X) = ALL '+'                                     
030000          ADD +1   TO INDX                                                
030100       END-IF                                                             
030200       ADD +1      TO IX-X                                                
030300     END-PERFORM                                                          
030400                                                                          
030500     IF REQU-IDARTNR-KOPI = ALL '+' AND                                   
030600        REQU-EMB-Q       = ALL '+'  AND                                   
030700        INDX = MAX-KVRADER                                                
030800         PERFORM MFS-RENSA-FAELT-IN                                       
030900     ELSE                                                                 
031000         MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                           
031100         PERFORM EA-REQU-INDATA-TILL-MOD                                  
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 EA-REQU-INDATA-TILL-MOD SECTION.                                         
031600                                                                          
031700     IF REQU-IDARTNR-KOPI           NOT = ALL '+'                         
031800        MOVE REQU-IDARTNR-KOPI      TO RESP-IDARTNR-KOPI                  
031900        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-IDARTNR-KOPI-ATTR             
032000     ELSE                                                                 
032100        MOVE ALL-SPACE              TO RESP-IDARTNR-KOPI                  
032200     END-IF                                                               
032300                                                                          
032400     IF REQU-IDARTNR-EMBQ0          NOT = ALL '+'                         
032500        MOVE REQU-IDARTNR-EMBQ0     TO RESP-IDARTNR-EMBQ0                 
032600        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-IDARTNR-EMBQ0-ATTR            
032700     ELSE                                                                 
032800        MOVE ALL-SPACE              TO RESP-IDARTNR-EMBQ0                 
032900     END-IF                                                               
033000                                                                          
033100     IF REQU-KVQPACK-EMBQ0          NOT = ALL '+'                         
033200        MOVE REQU-KVQPACK-EMBQ0     TO RESP-KVQPACK-EMBQ0                 
033300        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KVQPACK-EMBQ0-ATTR            
033400     ELSE                                                                 
033500        MOVE ALL-SPACE              TO RESP-KVQPACK-EMBQ0                 
033600     END-IF                                                               
033700                                                                          
033800     IF REQU-KDEMBKOD-EMBQ0         NOT = ALL '+'                         
033900        MOVE REQU-KDEMBKOD-EMBQ0    TO RESP-KDEMBKOD-EMBQ0                
034000        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KDEMBKOD-EMBQ0-ATTR           
034100     ELSE                                                                 
034200        MOVE ALL-SPACE              TO RESP-KDEMBKOD-EMBQ0                
034300     END-IF                                                               
034400                                                                          
034500     IF REQU-IDARTNR-EMBQ1          NOT = ALL '+'                         
034600        MOVE REQU-IDARTNR-EMBQ1     TO RESP-IDARTNR-EMBQ1                 
034700        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-IDARTNR-EMBQ1-ATTR            
034800     ELSE                                                                 
034900        MOVE ALL-SPACE              TO RESP-IDARTNR-EMBQ1                 
035000     END-IF                                                               
035100                                                                          
035200     IF REQU-KVQPACK-EMBQ1          NOT = ALL '+'                         
035300        MOVE REQU-KVQPACK-EMBQ1     TO RESP-KVQPACK-EMBQ1                 
035400        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KVQPACK-EMBQ1-ATTR            
035500     ELSE                                                                 
035600        MOVE ALL-SPACE              TO RESP-KVQPACK-EMBQ1                 
035700     END-IF                                                               
035800                                                                          
035900     IF REQU-KDEMBKOD-EMBQ1         NOT = ALL '+'                         
036000        MOVE REQU-KDEMBKOD-EMBQ1    TO RESP-KDEMBKOD-EMBQ1                
036100        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KDEMBKOD-EMBQ1-ATTR           
036200     ELSE                                                                 
036300        MOVE ALL-SPACE              TO RESP-KDEMBKOD-EMBQ1                
036400     END-IF                                                               
036500                                                                          
036600     IF REQU-IDARTNR-EMBQ2          NOT = ALL '+'                         
036700        MOVE REQU-IDARTNR-EMBQ2     TO RESP-IDARTNR-EMBQ2                 
036800        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-IDARTNR-EMBQ2-ATTR            
036900     ELSE                                                                 
037000        MOVE ALL-SPACE              TO RESP-IDARTNR-EMBQ2                 
037100     END-IF                                                               
037200                                                                          
037300     IF REQU-KVQPACK-EMBQ2          NOT = ALL '+'                         
037400        MOVE REQU-KVQPACK-EMBQ2     TO RESP-KVQPACK-EMBQ2                 
037500        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KVQPACK-EMBQ2-ATTR            
037600     ELSE                                                                 
037700        MOVE ALL-SPACE              TO RESP-KVQPACK-EMBQ2                 
037800     END-IF                                                               
037900                                                                          
038000     IF REQU-KDEMBKOD-EMBQ2         NOT = ALL '+'                         
038100        MOVE REQU-KDEMBKOD-EMBQ2    TO RESP-KDEMBKOD-EMBQ2                
038200        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KDEMBKOD-EMBQ2-ATTR           
038300     ELSE                                                                 
038400        MOVE ALL-SPACE              TO RESP-KDEMBKOD-EMBQ2                
038500     END-IF                                                               
038600                                                                          
038700     IF REQU-KVQPACK-EMBQ3          NOT = ALL '+'                         
038800        MOVE REQU-KVQPACK-EMBQ3     TO RESP-KVQPACK-EMBQ3                 
038900        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KVQPACK-EMBQ3-ATTR            
039000     ELSE                                                                 
039100        MOVE ALL-SPACE              TO RESP-KVQPACK-EMBQ3                 
039200     END-IF                                                               
039300                                                                          
039400     IF REQU-KVQPACK-EMBQ4          NOT = ALL '+'                         
039500        MOVE REQU-KVQPACK-EMBQ4     TO RESP-KVQPACK-EMBQ4                 
039600        MOVE MFS-ADD-LAES-IN-FAELT  TO RESP-KVQPACK-EMBQ4-ATTR            
039700     ELSE                                                                 
039800        MOVE ALL-SPACE              TO RESP-KVQPACK-EMBQ4                 
039900     END-IF                                                               
040000                                                                          
040100     MOVE +1 TO IX-X                                                      
040200     PERFORM UNTIL IX-X > MAX-KVRADER                                     
040300        IF REQU-IDARTNR-EMBX    (IX-X) NOT = ALL '+'                      
040400           MOVE REQU-IDARTNR-EMBX (IX-X)                                  
040500                               TO RESP-IDARTNR-EMBX (IX-X)                
040600           MOVE MFS-ADD-LAES-IN-FAELT                                     
040700                               TO RESP-IDARTNR-EMBX-ATTR (IX-X)           
040800        ELSE                                                              
040900           MOVE ALL-SPACE                                                 
041000                               TO RESP-IDARTNR-EMBX (IX-X)                
041100        END-IF                                                            
041200                                                                          
041300        IF REQU-KVQPACK-EMBX (IX-X) NOT = ALL '+'                         
041400           MOVE REQU-KVQPACK-EMBX (IX-X)                                  
041500                               TO RESP-KVQPACK-EMBX (IX-X)                
041600           MOVE MFS-ADD-LAES-IN-FAELT                                     
041700                               TO RESP-KVQPACK-EMBX-ATTR (IX-X)           
041800        ELSE                                                              
041900           MOVE ALL-SPACE                                                 
042000                               TO RESP-KVQPACK-EMBX (IX-X)                
042100        END-IF                                                            
042200                                                                          
042300        ADD +1 TO IX-X                                                    
042400     END-PERFORM                                                          
042500     .                                                                    
042600     EJECT                                                                
042700 F-LAES-VISA-INFO SECTION.                                                
042800                                                                          
042900     PERFORM FA-LAES-GRUNDDATA                                            
043000                                                                          
043100     IF SEGMENT-SAKNAS                                                    
043200        MOVE ERR-PART-MISSING    TO RESP-IDMSG-ERROR                      
043300        MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                     
043400        PERFORM MFS-RENSA-FAELT-UT                                        
043500     ELSE                                                                 
043600        PERFORM MFS-RENSA-FAELT-UT                                        
043700        IF NDC-CN                                                         
043800          MOVE WC-LAND-CN        TO W-IDLAND                              
043900        ELSE                                                              
044000          IF NDC-US                                                       
044100            MOVE WC-LAND-US      TO W-IDLAND                              
044200          ELSE                                                            
044300            MOVE WC-LAND-SE      TO W-IDLAND                              
044400          END-IF                                                          
044500        END-IF                                                            
044600        PERFORM  IMS-GET-WDT3-FPCK                                        
044700        IF SEGMENT-FINNS                                                  
044800          MOVE FPCK-BEFT         TO WS-BEFT                               
044900          MOVE WS-BEFT           TO RESP-BEFT                             
045000          MOVE FPCK-KDFORP       TO RESP-KDFORP                           
045100        ELSE                                                              
045200          IF W-IDLAND = WC-LAND-CN OR WC-LAND-US                          
045300            IF WS-BEFT = 0                                                
045400              MOVE WC-LAND-SE    TO W-IDLAND                              
045500              PERFORM  IMS-GET-WDT3-FPCK                                  
045600              IF SEGMENT-FINNS                                            
045700                MOVE FPCK-BEFT   TO WS-BEFT                               
045800                MOVE WS-BEFT     TO RESP-BEFT                             
045900              END-IF                                                      
046000            END-IF                                                        
046100          END-IF                                                          
046200        END-IF                                                            
046210        IF REQU-IDDC-KEY NUMERIC                                          
046220          MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                             
046230        ELSE                                                              
046240          MOVE SPACE         TO RESP-IDDC-KEY                             
046250        END-IF                                                            
046300        MOVE CLAG-TIUPPDAT-EMB   TO WS-TIUPPDAT-EMB                       
046400        MOVE WS-TIUPPDAT-EMB     TO RESP-TIUPPDAT                         
046500        MOVE CLAG-IDUSER-EMB     TO RESP-IDUSER                           
046600        MOVE CLAG-KVQPACK-4      TO RESP-KVQPACK-EMBQ4-UT                 
046700        INSPECT RESP-KVQPACK-EMBQ4-UT                                     
046800                                 REPLACING LEADING ZERO BY SPACE          
046900        PERFORM FB-LAES-EMBALLAGE                                         
047000          PERFORM S01-GET-EMQ-0-1-2-VALUES                                
047100        IF REQU-IDMSGVER = 001                                            
047200          PERFORM MFS-LOCK-FIELDS-CHINA                                   
047300        ELSE IF W-IDLAND = WC-LAND-CN OR WC-LAND-US                       
047400          PERFORM MFS-LOCK-FIELDS-US-CN                                   
047500        END-IF                                                            
047600        END-IF                                                            
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000 FA-LAES-GRUNDDATA SECTION.                                               
048100     MOVE W-IDARTNR-SPAR TO W-IDARTNR                                     
048200                                                                          
048300     PERFORM IMS-GU-WDK6-ROT                                              
048400     IF SEGMENT-FINNS                                                     
048500        PERFORM  IMS-GET-WDK6-CLAG                                        
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 FB-LAES-EMBALLAGE SECTION.                                               
049000                                                                          
049100     PERFORM IMS-GET-WDK6-EMB                                             
049200                                                                          
049300     PERFORM UNTIL SEGMENT-SAKNAS                                         
049400       IF EMB-KDEMBKEY (1:1) = 'Q'                                        
049500        IF EMB-KDEMBKEY = 'Q0'                                            
049600           MOVE EMB-IDARTNR-EMB    TO RESP-IDARTNR-EMBQ0-UT               
049700           MOVE EMB-KVQPACK-EMB    TO RESP-KVQPACK-EMBQ0-UT               
049800           MOVE EMB-KDEMBKOD       TO RESP-KDEMBKOD-EMBQ0-UT              
049900           INSPECT RESP-IDARTNR-EMBQ0-UT                                  
050000                                   REPLACING LEADING ZERO BY SPACE        
050100           INSPECT RESP-KVQPACK-EMBQ0-UT                                  
050200                                   REPLACING LEADING ZERO BY SPACE        
050300           INSPECT RESP-KDEMBKOD-EMBQ0-UT                                 
050400                                   REPLACING LEADING ZERO BY SPACE        
050500        END-IF                                                            
050600        IF EMB-KDEMBKEY = 'Q1'                                            
050700           MOVE EMB-IDARTNR-EMB    TO RESP-IDARTNR-EMBQ1-UT               
050800           MOVE EMB-KVQPACK-EMB    TO RESP-KVQPACK-EMBQ1-UT               
050900           MOVE EMB-KDEMBKOD       TO RESP-KDEMBKOD-EMBQ1-UT              
051000           INSPECT RESP-IDARTNR-EMBQ1-UT                                  
051100                                   REPLACING LEADING ZERO BY SPACE        
051200           INSPECT RESP-KVQPACK-EMBQ1-UT                                  
051300                                   REPLACING LEADING ZERO BY SPACE        
051400           INSPECT RESP-KDEMBKOD-EMBQ1-UT                                 
051500                                   REPLACING LEADING ZERO BY SPACE        
051600        END-IF                                                            
051700        IF EMB-KDEMBKEY = 'Q2'                                            
051800           MOVE EMB-IDARTNR-EMB    TO RESP-IDARTNR-EMBQ2-UT               
051900           MOVE EMB-KVQPACK-EMB    TO RESP-KVQPACK-EMBQ2-UT               
052000           MOVE EMB-KDEMBKOD       TO RESP-KDEMBKOD-EMBQ2-UT              
052100           INSPECT RESP-IDARTNR-EMBQ2-UT                                  
052200                                   REPLACING LEADING ZERO BY SPACE        
052300           INSPECT RESP-KVQPACK-EMBQ2-UT                                  
052400                                   REPLACING LEADING ZERO BY SPACE        
052500           INSPECT RESP-KDEMBKOD-EMBQ2-UT                                 
052600                                   REPLACING LEADING ZERO BY SPACE        
052700        END-IF                                                            
052800        IF EMB-KDEMBKEY = 'Q3'                                            
052900           MOVE EMB-KVQPACK-EMB    TO RESP-KVQPACK-EMBQ3-UT               
053000           INSPECT RESP-KVQPACK-EMBQ3-UT                                  
053100                                   REPLACING LEADING ZERO BY SPACE        
053200        END-IF                                                            
053300       END-IF                                                             
053400                                                                          
053500       IF EMB-KDEMBKEY (1:1) = 'X'                                        
053600          MOVE EMB-KDEMBKEY        TO WS-KDEMBKEY                         
053700          MOVE WS-KDEMBKEY-IX      TO IX-X                                
053800          MOVE EMB-IDARTNR-EMB     TO RESP-IDARTNR-EMBX-UT (IX-X)         
053900          MOVE EMB-KVQPACK-EMB     TO RESP-KVQPACK-EMBX-UT (IX-X)         
054000          INSPECT RESP-IDARTNR-EMBX-UT (IX-X)                             
054100                                   REPLACING LEADING ZERO BY SPACE        
054200          INSPECT RESP-KVQPACK-EMBX-UT (IX-X)                             
054300                                   REPLACING LEADING ZERO BY SPACE        
054400       END-IF                                                             
054500                                                                          
054600       PERFORM IMS-GET-WDK6-EMB                                           
054700     END-PERFORM                                                          
054800     .                                                                    
054900     EJECT                                                                
055000 G-KOLLA-INPUT SECTION.                                                   
055100                                                                          
055200     MOVE JA  TO INDATA-SW                                                
055210     IF REQU-IDDC-KEY NUMERIC AND REQU-IDDC NOT = REQU-IDDC-KEY           
055220       MOVE NEJ TO INDATA-SW                                              
055221       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
055240     END-IF                                                               
055600     IF (REQU-KVQPACK-EMBQ1 NOT = ALL '+'                                 
055700        AND REQU-UPDATE)                                                  
055800        OR (REQU-KVQPACK-EMBQ1 NOT = ALL '+'                              
055900        AND REQU-UPDATE)                                                  
056000        MOVE INF-PRESS-PF23        TO RESP-IDMSG-ERROR                    
056100        MOVE 'PF23'                TO RESP-IDELMT-ERROR                   
056200        PERFORM MFS-ROER-EJ-FAELT-UT                                      
056300        PERFORM MFS-ROER-EJ-FAELT-IN                                      
056400        PERFORM MFS-LAES-IN-IGEN                                          
056500        MOVE NEJ                   TO INDATA-SW                           
056600     ELSE                                                                 
056700       MOVE +1 TO IX-X                                                    
056800       MOVE +0 TO INDX                                                    
056900       PERFORM UNTIL IX-X > MAX-KVRADER                                   
057000         IF REQU-EMB-X (IX-X) = ALL '+'                                   
057100            ADD   +1 TO INDX                                              
057200         END-IF                                                           
057300         ADD +1      TO IX-X                                              
057400       END-PERFORM                                                        
057500       IF REQU-IDARTNR-KOPI = ALL '+' AND                                 
057600          REQU-EMB-Q     = ALL '+' AND                                    
057700          INDX = MAX-KVRADER                                              
057800         MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                    
057900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
058000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
058100         MOVE NEJ TO INDATA-SW                                            
058200       ELSE                                                               
058300         MOVE W-IDARTNR-SPAR TO W-IDARTNR                                 
058400         PERFORM IMS-GU-WDK6-ROT                                          
058500         IF SEGMENT-FINNS                                                 
058600            IF NDC-CN OR NDC-US                                           
058700              PERFORM IMS-GU-WDK711                                       
058800              IF SEGMENT-SAKNAS                                           
058900                MOVE ERR-PART-MISSING    TO RESP-IDMSG-ERROR              
059000                MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR             
059100                MOVE NEJ TO INDATA-SW                                     
059200                PERFORM MFS-ROER-EJ-FAELT-UT                              
059300                PERFORM MFS-ROER-EJ-FAELT-IN                              
059400              ELSE                                                        
059500*               IF SLAG-IDDC-REF NOT = SPACE                              
059600*                 MOVE ERR-REFILL-PART     TO RESP-IDMSG-ERROR            
059700*                 MOVE NEJ TO INDATA-SW                                   
059800*                 PERFORM MFS-ROER-EJ-FAELT-UT                            
059900*                 PERFORM MFS-ROER-EJ-FAELT-IN                            
060000*               END-IF                                                    
060100                CONTINUE                                                  
060200              END-IF                                                      
060300            END-IF                                                        
060400            IF INDATA-OK                                                  
060500              PERFORM GA-KOLLA-KOPIERING                                  
060600              PERFORM GB-KOLLA-Q0-EMBALLAGE                               
060700              PERFORM GC-KOLLA-Q1-EMBALLAGE                               
060800              PERFORM GD-KOLLA-Q2-EMBALLAGE                               
060900              PERFORM S02-CHECK-USER-ID-WEB                               
061000              PERFORM GE-KOLLA-Q3-EMBALLAGE                               
061100              PERFORM GF-KOLLA-Q4-EMBALLAGE                               
061200              PERFORM GG-KOLLA-X-EMBALLAGE                                
061300              IF INDATA-FEL                                               
061400                MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR             
061500                PERFORM MFS-ROER-EJ-FAELT-UT                              
061600                PERFORM MFS-ROER-EJ-FAELT-IN                              
061700              END-IF                                                      
061800            END-IF                                                        
061900         ELSE                                                             
062000            MOVE NEJ TO INDATA-SW                                         
062100         END-IF                                                           
062200       END-IF                                                             
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 GA-KOLLA-KOPIERING SECTION.                                              
062700                                                                          
062800       IF REQU-IDARTNR-KOPI      NOT = ALL '+'                            
062900         IF REQU-IDARTNR-KOPI NOT NUMERIC                                 
063000           MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-KOPI-ATTR               
063100           MOVE NEJ TO INDATA-SW                                          
063200         ELSE                                                             
063300            MOVE REQU-IDARTNR-KOPI TO KW-IDARTNR KW-IDARTNR-SPAR          
063400            PERFORM IMS-GU-KWDK6-CLAG                                     
063500            IF SEGMENT-SAKNAS                                             
063600              MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-KOPI-ATTR            
063700              MOVE NEJ TO INDATA-SW                                       
063800            ELSE                                                          
063900              MOVE MFS-NUM-FAELT-RAETT TO RESP-IDARTNR-KOPI-ATTR          
064000            END-IF                                                        
064100         END-IF                                                           
064200       END-IF                                                             
064300     .                                                                    
064400     EJECT                                                                
064500 GB-KOLLA-Q0-EMBALLAGE SECTION.                                           
064600                                                                          
064700*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
064800       IF REQU-IDARTNR-EMBQ0  NOT = ALL '+'  OR                           
064900          REQU-KVQPACK-EMBQ0  NOT = ALL '+'  OR                           
065000          REQU-KDEMBKOD-EMBQ0 NOT = ALL '+'                               
065100         MOVE 'Q0' TO W-KDEMBAL                                           
065200         PERFORM IMS-GET-WDK6-EMB-KVAL                                    
065300         MOVE NEJ TO NYTT-13-SEGMENT (11)                                 
065400         IF SEGMENT-SAKNAS                                                
065500            MOVE JA  TO NYTT-13-SEGMENT (11)                              
065600            MOVE ZERO TO WQ-IDARTNR (1)                                   
065700            MOVE ZERO TO WQ-KVQPACK (1)                                   
065800            MOVE ZERO TO WQ-KDEMBKOD (1)                                  
065900         END-IF                                                           
066000       END-IF                                                             
066100                                                                          
066200       IF REQU-IDARTNR-EMBQ0 NOT = ALL '+'                                
066300         IF REQU-IDARTNR-EMBQ0 NOT NUMERIC                                
066400           MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-EMBQ0-ATTR              
066500           MOVE NEJ TO INDATA-SW                                          
066600         ELSE                                                             
066700            MOVE REQU-IDARTNR-EMBQ0    TO KW-IDARTNR                      
066800            PERFORM IMS-GU-KWDK6-CLAG                                     
066900            IF SEGMENT-SAKNAS                                             
067000             IF REQU-IDARTNR-EMBQ0    = ZERO                              
067100              MOVE MFS-NUM-FAELT-RAETT     TO                             
067200                                 RESP-IDARTNR-EMBQ0-ATTR                  
067300              MOVE REQU-IDARTNR-EMBQ0      TO                             
067400                                 WQ-IDARTNR (1)                           
067500             ELSE                                                         
067600              MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-EMBQ0-ATTR           
067700              MOVE NEJ TO INDATA-SW                                       
067800             END-IF                                                       
067900            ELSE                                                          
068000              MOVE MFS-NUM-FAELT-RAETT     TO                             
068100                                 RESP-IDARTNR-EMBQ0-ATTR                  
068200              MOVE REQU-IDARTNR-EMBQ0      TO                             
068300                                 WQ-IDARTNR (1)                           
068400            END-IF                                                        
068500         END-IF                                                           
068600       END-IF                                                             
068700       IF REQU-KVQPACK-EMBQ0    NOT = ALL '+'                             
068800         IF REQU-KVQPACK-EMBQ0    NOT NUMERIC                             
068900           MOVE MFS-NUM-FAELT-FEL   TO RESP-KVQPACK-EMBQ0-ATTR            
069000           MOVE NEJ                 TO INDATA-SW                          
069100         ELSE                                                             
069200           MOVE MFS-NUM-FAELT-RAETT     TO                                
069300                                 RESP-KVQPACK-EMBQ0-ATTR                  
069400           MOVE REQU-KVQPACK-EMBQ0      TO                                
069500                                 WQ-KVQPACK (1)                           
069600         END-IF                                                           
069700       END-IF                                                             
069800       IF REQU-KDEMBKOD-EMBQ0 NOT = ALL '+'                               
069900         IF REQU-KDEMBKOD-EMBQ0 NOT NUMERIC                               
070000           MOVE MFS-NUM-FAELT-FEL   TO RESP-KDEMBKOD-EMBQ0-ATTR           
070100           MOVE NEJ                 TO INDATA-SW                          
070200         ELSE                                                             
070300           MOVE MFS-NUM-FAELT-RAETT     TO                                
070400                                 RESP-KDEMBKOD-EMBQ0-ATTR                 
070500           MOVE REQU-KDEMBKOD-EMBQ0     TO                                
070600                                 WQ-KDEMBKOD (1)                          
070700         END-IF                                                           
070800       END-IF                                                             
070900     .                                                                    
071000     EJECT                                                                
071100 GC-KOLLA-Q1-EMBALLAGE SECTION.                                           
071200                                                                          
071300*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
071400       IF REQU-IDARTNR-EMBQ1     NOT = ALL '+'  OR                        
071500          REQU-KVQPACK-EMBQ1     NOT = ALL '+'  OR                        
071600          REQU-KDEMBKOD-EMBQ1    NOT = ALL '+'                            
071700              MOVE 'Q1' TO W-KDEMBAL                                      
071800              PERFORM IMS-GET-WDK6-EMB-KVAL                               
071900              MOVE NEJ  TO NYTT-13-SEGMENT (12)                           
072000              IF SEGMENT-SAKNAS                                           
072100                 MOVE JA  TO NYTT-13-SEGMENT (12)                         
072200                 MOVE ZERO TO WQ-IDARTNR (2)                              
072300                 MOVE ZERO TO WQ-KVQPACK (2)                              
072400                 MOVE ZERO TO WQ-KDEMBKOD (2)                             
072500              END-IF                                                      
072600       END-IF                                                             
072700                                                                          
072800       IF REQU-IDARTNR-EMBQ1    NOT = ALL '+'                             
072900         IF REQU-IDARTNR-EMBQ1    NOT NUMERIC                             
073000           MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-EMBQ1-ATTR              
073100           MOVE NEJ TO INDATA-SW                                          
073200         ELSE                                                             
073300            MOVE REQU-IDARTNR-EMBQ1    TO KW-IDARTNR                      
073400            PERFORM IMS-GU-KWDK6-CLAG                                     
073500            IF SEGMENT-SAKNAS                                             
073600             IF REQU-IDARTNR-EMBQ1    = ZERO                              
073700              MOVE MFS-NUM-FAELT-RAETT     TO                             
073800                                 RESP-IDARTNR-EMBQ1-ATTR                  
073900              MOVE REQU-IDARTNR-EMBQ1      TO                             
074000                                 WQ-IDARTNR (2)                           
074100             ELSE                                                         
074200              MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-EMBQ1-ATTR           
074300              MOVE NEJ TO INDATA-SW                                       
074400             END-IF                                                       
074500            ELSE                                                          
074600              MOVE MFS-NUM-FAELT-RAETT     TO                             
074700                                 RESP-IDARTNR-EMBQ1-ATTR                  
074800              MOVE REQU-IDARTNR-EMBQ1      TO                             
074900                                 WQ-IDARTNR (2)                           
075000            END-IF                                                        
075100         END-IF                                                           
075200       END-IF                                                             
075300       IF REQU-KVQPACK-EMBQ1    NOT = ALL '+'                             
075400         IF REQU-KVQPACK-EMBQ1    NOT NUMERIC                             
075500           MOVE MFS-NUM-FAELT-FEL   TO RESP-KVQPACK-EMBQ1-ATTR            
075600           MOVE NEJ                 TO INDATA-SW                          
075700         ELSE                                                             
075800           MOVE MFS-NUM-FAELT-RAETT     TO                                
075900                                 RESP-KVQPACK-EMBQ1-ATTR                  
076000           MOVE REQU-KVQPACK-EMBQ1      TO                                
076100                                 WQ-KVQPACK (2)                           
076200         END-IF                                                           
076300       END-IF                                                             
076400       IF REQU-KDEMBKOD-EMBQ1    NOT = ALL '+'                            
076500         IF REQU-KDEMBKOD-EMBQ1    NOT NUMERIC                            
076600           MOVE MFS-NUM-FAELT-FEL   TO RESP-KDEMBKOD-EMBQ1-ATTR           
076700           MOVE NEJ                 TO INDATA-SW                          
076800         ELSE                                                             
076900           MOVE MFS-NUM-FAELT-RAETT     TO                                
077000                                 RESP-KDEMBKOD-EMBQ1-ATTR                 
077100           MOVE REQU-KDEMBKOD-EMBQ1     TO                                
077200                                 WQ-KDEMBKOD (2)                          
077300         END-IF                                                           
077400       END-IF                                                             
077500     .                                                                    
077600     EJECT                                                                
077700 GD-KOLLA-Q2-EMBALLAGE SECTION.                                           
077800                                                                          
077900*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
078000       IF REQU-IDARTNR-EMBQ2     NOT = ALL '+'  OR                        
078100          REQU-KVQPACK-EMBQ2     NOT = ALL '+'  OR                        
078200          REQU-KDEMBKOD-EMBQ2    NOT = ALL '+'                            
078300              MOVE 'Q2' TO W-KDEMBAL                                      
078400              PERFORM IMS-GET-WDK6-EMB-KVAL                               
078500              MOVE NEJ TO NYTT-13-SEGMENT (13)                            
078600              IF SEGMENT-SAKNAS                                           
078700                 MOVE JA  TO NYTT-13-SEGMENT (13)                         
078800                 MOVE ZERO TO WQ-IDARTNR (3)                              
078900                 MOVE ZERO TO WQ-KVQPACK (3)                              
079000                 MOVE ZERO TO WQ-KDEMBKOD (3)                             
079100              END-IF                                                      
079200       END-IF                                                             
079300                                                                          
079400       IF REQU-IDARTNR-EMBQ2    NOT = ALL '+'                             
079500         IF REQU-IDARTNR-EMBQ2    NOT NUMERIC                             
079600           MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-EMBQ2-ATTR              
079700           MOVE NEJ TO INDATA-SW                                          
079800         ELSE                                                             
079900            MOVE REQU-IDARTNR-EMBQ2    TO KW-IDARTNR                      
080000            PERFORM IMS-GU-KWDK6-CLAG                                     
080100            IF SEGMENT-SAKNAS                                             
080200             IF REQU-IDARTNR-EMBQ2    = ZERO                              
080300              MOVE MFS-NUM-FAELT-RAETT     TO                             
080400                                 RESP-IDARTNR-EMBQ2-ATTR                  
080500              MOVE REQU-IDARTNR-EMBQ2      TO                             
080600                                 WQ-IDARTNR (3)                           
080700             ELSE                                                         
080800              MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-EMBQ2-ATTR           
080900              MOVE NEJ TO INDATA-SW                                       
081000             END-IF                                                       
081100            ELSE                                                          
081200              MOVE MFS-NUM-FAELT-RAETT     TO                             
081300                                 RESP-IDARTNR-EMBQ2-ATTR                  
081400              MOVE REQU-IDARTNR-EMBQ2      TO                             
081500                                 WQ-IDARTNR (3)                           
081600            END-IF                                                        
081700         END-IF                                                           
081800       END-IF                                                             
081900       IF REQU-KVQPACK-EMBQ2   NOT = ALL '+'                              
082000         IF REQU-KVQPACK-EMBQ2    NOT NUMERIC                             
082100           MOVE MFS-NUM-FAELT-FEL   TO RESP-KVQPACK-EMBQ2-ATTR            
082200           MOVE NEJ                 TO INDATA-SW                          
082300         ELSE                                                             
082400           MOVE MFS-NUM-FAELT-RAETT     TO                                
082500                                 RESP-KVQPACK-EMBQ2-ATTR                  
082600           MOVE REQU-KVQPACK-EMBQ2      TO                                
082700                                 WQ-KVQPACK (3)                           
082800         END-IF                                                           
082900       END-IF                                                             
083000       IF REQU-KDEMBKOD-EMBQ2    NOT = ALL '+'                            
083100         IF REQU-KDEMBKOD-EMBQ2    NOT NUMERIC                            
083200           MOVE MFS-NUM-FAELT-FEL   TO RESP-KDEMBKOD-EMBQ2-ATTR           
083300           MOVE NEJ                 TO INDATA-SW                          
083400         ELSE                                                             
083500           MOVE MFS-NUM-FAELT-RAETT     TO                                
083600                                 RESP-KDEMBKOD-EMBQ2-ATTR                 
083700           MOVE REQU-KDEMBKOD-EMBQ2     TO                                
083800                                 WQ-KDEMBKOD (3)                          
083900         END-IF                                                           
084000       END-IF                                                             
084100     .                                                                    
084200     EJECT                                                                
084300 GE-KOLLA-Q3-EMBALLAGE SECTION.                                           
084400                                                                          
084500*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
084600       IF REQU-KVQPACK-EMBQ3     NOT = ALL '+'                            
084700              MOVE 'Q3' TO W-KDEMBAL                                      
084800              PERFORM IMS-GET-WDK6-EMB-KVAL                               
084900              MOVE NEJ TO NYTT-13-SEGMENT (14)                            
085000              IF SEGMENT-SAKNAS                                           
085100                 MOVE JA  TO NYTT-13-SEGMENT (14)                         
085200                 MOVE ZERO TO WQ-KVQPACK (4)                              
085300              END-IF                                                      
085400       END-IF                                                             
085500                                                                          
085600       IF REQU-KVQPACK-EMBQ3    NOT = ALL '+'                             
085700         IF REQU-KVQPACK-EMBQ3    NOT NUMERIC                             
085800           MOVE MFS-NUM-FAELT-FEL   TO RESP-KVQPACK-EMBQ3-ATTR            
085900           MOVE NEJ                 TO INDATA-SW                          
086000         ELSE                                                             
086100           MOVE MFS-NUM-FAELT-RAETT     TO                                
086200                                 RESP-KVQPACK-EMBQ3-ATTR                  
086300           MOVE REQU-KVQPACK-EMBQ3      TO                                
086400                                 WQ-KVQPACK (4)                           
086500         END-IF                                                           
086600       END-IF                                                             
086700     .                                                                    
086800     EJECT                                                                
086900 GF-KOLLA-Q4-EMBALLAGE SECTION.                                           
087000                                                                          
087100     IF REQU-KVQPACK-EMBQ4    NOT = ALL '+'                               
087200       IF REQU-KVQPACK-EMBQ4    NOT NUMERIC                               
087300         MOVE MFS-NUM-FAELT-FEL    TO RESP-KVQPACK-EMBQ4-ATTR             
087400         MOVE NEJ                  TO INDATA-SW                           
087500       ELSE                                                               
087600         MOVE MFS-NUM-FAELT-RAETT  TO RESP-KVQPACK-EMBQ4-ATTR             
087700         MOVE REQU-KVQPACK-EMBQ4    TO WQ-KVQPACK(5)                      
087800       END-IF                                                             
087900     END-IF                                                               
088000     .                                                                    
088100     EJECT                                                                
088200 GG-KOLLA-X-EMBALLAGE SECTION.                                            
088300                                                                          
088400       MOVE +1 TO IX-X                                                    
088500       PERFORM UNTIL IX-X > MAX-KVRADER                                   
088600                                                                          
088700        IF REQU-IDARTNR-EMBX    (IX-X)   NOT = ALL '+' OR                 
088800           REQU-KVQPACK-EMBX    (IX-X)   NOT = ALL '+'                    
088900              MOVE 'X' TO WS-KDEMBKEY                                     
089000              MOVE IX-X TO WS-KDEMBKEY-IX                                 
089100              MOVE WS-KDEMBKEY TO W-KDEMBAL                               
089200              PERFORM IMS-GET-WDK6-EMB-KVAL                               
089300              MOVE NEJ  TO NYTT-13-SEGMENT (IX-X)                         
089400              IF SEGMENT-SAKNAS                                           
089500                 MOVE JA   TO NYTT-13-SEGMENT (IX-X)                      
089600                 MOVE ZERO TO WX-IDARTNR (IX-X)                           
089700                 MOVE ZERO TO WX-KVQPACK (IX-X)                           
089800              END-IF                                                      
089900        END-IF                                                            
090000                                                                          
090100          IF REQU-IDARTNR-EMBX    (IX-X) NOT = ALL '+'                    
090200            IF REQU-IDARTNR-EMBX    (IX-X) NOT NUMERIC                    
090300               MOVE MFS-NUM-FAELT-FEL            TO                       
090400                                 RESP-IDARTNR-EMBX-ATTR (IX-X)            
090500               MOVE NEJ TO INDATA-SW                                      
090600            ELSE                                                          
090700               MOVE REQU-IDARTNR-EMBX  (IX-X)  TO KW-IDARTNR              
090800               PERFORM IMS-GU-KWDK6-CLAG                                  
090900               IF SEGMENT-SAKNAS                                          
091000                IF REQU-IDARTNR-EMBX    (IX-X) = ZERO                     
091100                  MOVE MFS-NUM-FAELT-RAETT        TO                      
091200                                 RESP-IDARTNR-EMBX-ATTR (IX-X)            
091300                  MOVE REQU-IDARTNR-EMBX    (IX-X) TO                     
091400                                 WX-IDARTNR (IX-X)                        
091500                ELSE                                                      
091600                  MOVE MFS-NUM-FAELT-FEL          TO                      
091700                                 RESP-IDARTNR-EMBX-ATTR (IX-X)            
091800                  MOVE NEJ TO INDATA-SW                                   
091900                END-IF                                                    
092000               ELSE                                                       
092100                  MOVE MFS-NUM-FAELT-RAETT        TO                      
092200                                 RESP-IDARTNR-EMBX-ATTR (IX-X)            
092300                  MOVE REQU-IDARTNR-EMBX    (IX-X) TO                     
092400                                 WX-IDARTNR (IX-X)                        
092500               END-IF                                                     
092600            END-IF                                                        
092700          END-IF                                                          
092800          IF REQU-KVQPACK-EMBX    (IX-X) NOT = ALL '+'                    
092900            IF REQU-KVQPACK-EMBX    (IX-X) NOT NUMERIC                    
093000              MOVE MFS-NUM-FAELT-FEL   TO                                 
093100                                 RESP-KVQPACK-EMBX-ATTR (IX-X)            
093200              MOVE NEJ                 TO INDATA-SW                       
093300            ELSE                                                          
093400              MOVE MFS-NUM-FAELT-RAETT TO                                 
093500                                 RESP-KVQPACK-EMBX-ATTR (IX-X)            
093600              MOVE REQU-KVQPACK-EMBX (IX-X) TO                            
093700                                 WX-KVQPACK (IX-X)                        
093800            END-IF                                                        
093900          END-IF                                                          
094000          ADD +1 TO IX-X                                                  
094100       END-PERFORM                                                        
094200     .                                                                    
094300     EJECT                                                                
094400 H-UPPDATERA SECTION.                                                     
094500                                                                          
094600     MOVE W-IDARTNR-SPAR TO W-IDARTNR                                     
094700     PERFORM IMS-GU-WDK6-ROT                                              
094800     PERFORM IMS-GET-WDK6-CLAG                                            
094900     IF SEGMENT-FINNS                                                     
095000        IF REQU-IDARTNR-KOPI NOT = ALL '+'                                
095100           PERFORM HA-KOPIERA                                             
095200        ELSE                                                              
095300          IF NDC-CN OR NDC-US                                             
095400            PERFORM HB-UPPD-WDK712-CN-US                                  
095500          ELSE                                                            
095600            PERFORM HC-UPPD-WDK611                                        
095700            PERFORM HD-UPPD-WDK613                                        
095800          END-IF                                                          
095900        END-IF                                                            
096000                                                                          
096100          MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                         
096200          PERFORM MFS-FORM-ATTR                                           
096300          PERFORM MFS-RENSA-FAELT-IN                                      
096400* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
096500     END-IF                                                               
096600     .                                                                    
096700     EJECT                                                                
096800 HA-KOPIERA  SECTION.                                                     
096900                                                                          
097000*    KOPIERING FRÅN KCLAG TILL CLAG                                       
097100                                                                          
097200     MOVE KW-IDARTNR-SPAR             TO KW-IDARTNR                       
097300     PERFORM IMS-GU-KWDK6-ROT                                             
097400     PERFORM IMS-GET-KWDK6-CLAG                                           
097500     IF SEGMENT-FINNS                                                     
097600        IF CLAG-IDARTNR-EMBQ0   NOT = KCLAG-IDARTNR-EMBQ0 OR              
097700           CLAG-IDARTNR-EMBQ1   NOT = KCLAG-IDARTNR-EMBQ1 OR              
097800           CLAG-IDARTNR-EMBQ2   NOT = KCLAG-IDARTNR-EMBQ2 OR              
097900           CLAG-KVQPACK-0       NOT = KCLAG-KVQPACK-0     OR              
098000           CLAG-KVQPACK-1       NOT = KCLAG-KVQPACK-1     OR              
098100           CLAG-KVQPACK-2       NOT = KCLAG-KVQPACK-2     OR              
098200           CLAG-KVQPACK-3       NOT = KCLAG-KVQPACK-3     OR              
098300           CLAG-KVQPACK-4       NOT = KCLAG-KVQPACK-4     OR              
098400           CLAG-KDEMBKOD-0      NOT = KCLAG-KDEMBKOD-0    OR              
098500           CLAG-KDEMBKOD-1      NOT = KCLAG-KDEMBKOD-1    OR              
098600           CLAG-KDEMBKOD-2      NOT = KCLAG-KDEMBKOD-2                    
098700           MOVE KCLAG-IDARTNR-EMBQ0   TO CLAG-IDARTNR-EMBQ0               
098800           MOVE KCLAG-IDARTNR-EMBQ1   TO CLAG-IDARTNR-EMBQ1               
098900           MOVE KCLAG-IDARTNR-EMBQ2   TO CLAG-IDARTNR-EMBQ2               
099000           MOVE KCLAG-KVQPACK-0       TO CLAG-KVQPACK-0                   
099100           MOVE KCLAG-KVQPACK-1       TO CLAG-KVQPACK-1                   
099200           MOVE KCLAG-KVQPACK-2       TO CLAG-KVQPACK-2                   
099300           MOVE KCLAG-KVQPACK-3       TO CLAG-KVQPACK-3                   
099400           MOVE KCLAG-KVQPACK-4       TO CLAG-KVQPACK-4                   
099500           MOVE KCLAG-KDEMBKOD-0      TO CLAG-KDEMBKOD-0                  
099600           MOVE KCLAG-KDEMBKOD-1      TO CLAG-KDEMBKOD-1                  
099700           MOVE KCLAG-KDEMBKOD-2      TO CLAG-KDEMBKOD-2                  
099800           MOVE REQU-IDUSER           TO CLAG-IDUSER-EMB                  
099900           MOVE DAGENS-DATUM          TO CLAG-TIUPPDAT-EMB                
100000           PERFORM IMS-REPL-WDK6-CLAG                                     
100100* OBS ÄVEN WDK613 SKALL UPPDATERAS MED DESSA DATA                         
100200*          BORDE RÄCKA MED NEDANSTÅENDE (GAMLA VÄRDEN PÅ K611 ?)          
100300*                                       (KONVERTERAS BAS ?     )          
100400        END-IF                                                            
100500                                                                          
100600        PERFORM IMS-GET-KWDK6-EMB                                         
100700        PERFORM UNTIL SEGMENT-SAKNAS                                      
100800           MOVE KEMB-KDEMBKEY   TO W-KDEMBAL                              
100900           PERFORM IMS-GET-WDK6-EMB-KVAL                                  
101000           MOVE KEMB-WDK613     TO EMB-WDK613                             
101100           IF SEGMENT-FINNS                                               
101200              PERFORM IMS-REPL-WDK6-EMB                                   
101300           ELSE                                                           
101400              PERFORM IMS-ISRT-WDK6-EMB                                   
101500           END-IF                                                         
101600           PERFORM IMS-GET-KWDK6-EMB                                      
101700        END-PERFORM                                                       
101800* OBS  MÖJLIGT FEL:  ATT GAMLA SEGMENT EJ TAS BORT !                      
101900* OBS  TILLS VIDARE GÖR VI DETTA MEDVETET                                 
102000     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102300 HB-UPPD-WDK712-CN-US SECTION.                                            
102400                                                                          
102500     IF NDC-US                                                            
102600       MOVE WC-LAND-US            TO W-IDLAND                             
102700     ELSE                                                                 
102800       MOVE WC-LAND-CN            TO W-IDLAND                             
102900     END-IF                                                               
103000     PERFORM IMS-GHU-WDK712                                               
103100     PERFORM HBA-CHECK-MOVE-EMQ-VALUES                                    
103200     PERFORM IMS-REPL-WDK712                                              
103300     PERFORM HBA-CHECK-MOVE-EMQ-X-VALUES                                  
103400     .                                                                    
103500     EJECT                                                                
103600 HBA-CHECK-MOVE-EMQ-VALUES SECTION.                                       
103700     IF REQU-IDARTNR-EMBQ0     NOT = ALL '+'                              
103800        MOVE WQ-IDARTNR (1)       TO LART-IDARTNR-EMBQ0                   
103900     END-IF                                                               
104000     IF REQU-IDARTNR-EMBQ1     NOT = ALL '+'                              
104100        MOVE WQ-IDARTNR (2)       TO LART-IDARTNR-EMBQ1                   
104200     END-IF                                                               
104300     IF REQU-IDARTNR-EMBQ2     NOT = ALL '+'                              
104400        MOVE WQ-IDARTNR (3)       TO LART-IDARTNR-EMBQ2                   
104500     END-IF                                                               
104600     IF REQU-KVQPACK-EMBQ3     NOT = ALL '+'                              
104700        MOVE WQ-KVQPACK (4)       TO LART-KVQPACK-3                       
104800     END-IF                                                               
104900     IF WQ-IDARTNR (1) > 0 OR WQ-IDARTNR (2) > 0 OR                       
105000        WQ-IDARTNR (3) > 0                                                
105100       IF REQU-IDUSER-IN > SPACE                                          
105200         MOVE REQU-IDUSER-IN        TO LART-IDUSER-EMB                    
105300       ELSE                                                               
105400         MOVE REQU-IDUSER           TO LART-IDUSER-EMB                    
105500       END-IF                                                             
105600       MOVE DAGENS-DATUM          TO LART-TIUPPDAT-EMB                    
105700     END-IF                                                               
105800     .                                                                    
105900     EJECT                                                                
106000 HBA-CHECK-MOVE-EMQ-X-VALUES SECTION.                                     
106100                                                                          
106200       MOVE +1 TO IX-X                                                    
106300       MOVE +5 TO MAX-KVRADER-WEB                                         
106400       PERFORM UNTIL IX-X > MAX-KVRADER-WEB                               
106500                                                                          
106600        IF REQU-IDARTNR-EMBX    (IX-X)   NOT = ALL '+'                    
106700         IF REQU-IDARTNR-EMBX (IX-X) NOT NUMERIC                          
106800           MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-EMBX-ATTR (IX-X)        
106900           MOVE NEJ TO INDATA-SW                                          
107000         ELSE                                                             
107100                                                                          
107200              MOVE 'X' TO WS-KDEMBKEY-1(1:1)                              
107300              MOVE IX-X TO WS-KDEMBKEY-1-IX                               
107400                                                                          
107500           PERFORM IMS-GET-WDK726                                         
107600           MOVE REQU-IDARTNR-EMBX (IX-X) TO LEMB-IDARTNR-EMB              
107700           IF SEGMENT-FINNS                                               
107800              PERFORM IMS-REPL-WDK726                                     
107900           ELSE                                                           
108000             MOVE ALL '+'        TO WDK7-W005WDK7                         
108100             MOVE 'WDK726'       TO WDK7-IDSEGM                           
108200             MOVE W-IDARTNR      TO WDK7-IDARTNR-KFB                      
108300             MOVE W-IDLAND       TO WDK7-IDLANDX2-KFB                     
108400                                  WDK7-IDLANDX2                           
108500             MOVE WS-KDEMBKEY-1  TO WDK7-KDEMBKEY                         
108600             MOVE REQU-IDARTNR-EMBX (IX-X) TO WDK7-IDARTNR-EMB            
108700                                                                          
108800                                                                          
108900             CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB          
109000                                               WDK7-PCB                   
109100                                                                          
109200           END-IF                                                         
109300         END-IF                                                           
109400        END-IF                                                            
109500       ADD +1 TO IX-X                                                     
109600       END-PERFORM                                                        
109700                                                                          
109800     .                                                                    
109900     EJECT                                                                
110000 HC-UPPD-WDK611  SECTION.                                                 
110100                                                                          
110200     MOVE REQU-IDUSER             TO CLAG-IDUSER-EMB                      
110300     MOVE DAGENS-DATUM            TO CLAG-TIUPPDAT-EMB                    
110400     IF REQU-IDARTNR-EMBQ0     NOT = ALL '+'                              
110500        MOVE WQ-IDARTNR (1)       TO CLAG-IDARTNR-EMBQ0                   
110600     END-IF                                                               
110700     IF REQU-IDARTNR-EMBQ1     NOT = ALL '+'                              
110800        MOVE WQ-IDARTNR (2)       TO CLAG-IDARTNR-EMBQ1                   
110900     END-IF                                                               
111000     IF REQU-IDARTNR-EMBQ2     NOT = ALL '+'                              
111100        MOVE WQ-IDARTNR (3)       TO CLAG-IDARTNR-EMBQ2                   
111200     END-IF                                                               
111300     IF REQU-KVQPACK-EMBQ0     NOT = ALL '+'                              
111400        MOVE WQ-KVQPACK (1)       TO CLAG-KVQPACK-0                       
111500     END-IF                                                               
111600     IF REQU-KVQPACK-EMBQ1     NOT = ALL '+'                              
111700        MOVE WQ-KVQPACK (2)       TO CLAG-KVQPACK-1                       
111800     END-IF                                                               
111900     IF REQU-KVQPACK-EMBQ2     NOT = ALL '+'                              
112000        MOVE WQ-KVQPACK (3)       TO CLAG-KVQPACK-2                       
112100     END-IF                                                               
112200     IF REQU-KVQPACK-EMBQ3     NOT = ALL '+'                              
112300        MOVE WQ-KVQPACK (4)       TO CLAG-KVQPACK-3                       
112400     END-IF                                                               
112500     IF REQU-KVQPACK-EMBQ4     NOT = ALL '+'                              
112600        MOVE WQ-KVQPACK (5)       TO CLAG-KVQPACK-4                       
112700     END-IF                                                               
112800     IF REQU-KDEMBKOD-EMBQ0    NOT = ALL '+'                              
112900        MOVE WQ-KDEMBKOD(1)       TO CLAG-KDEMBKOD-0                      
113000     END-IF                                                               
113100     IF REQU-KDEMBKOD-EMBQ1    NOT = ALL '+'                              
113200        MOVE WQ-KDEMBKOD(2)       TO CLAG-KDEMBKOD-1                      
113300     END-IF                                                               
113400     IF REQU-KDEMBKOD-EMBQ2    NOT = ALL '+'                              
113500        MOVE WQ-KDEMBKOD(3)       TO CLAG-KDEMBKOD-2                      
113600     END-IF                                                               
113700                                                                          
113800     PERFORM IMS-REPL-WDK6-CLAG                                           
113900     .                                                                    
114000     EJECT                                                                
114100 HD-UPPD-WDK613  SECTION.                                                 
114200                                                                          
114300     IF REQU-EMB-Q NOT = ALL '+'                                          
114400        PERFORM HCA-UPPD-WDK613-Q                                         
114500     END-IF                                                               
114600                                                                          
114700     MOVE +1 TO IX-X                                                      
114800     PERFORM UNTIL IX-X > MAX-KVRADER OR EMBX-OK                          
114900       IF REQU-EMB-X (IX-X) NOT = ALL '+'                                 
115000          MOVE JA  TO EMBX-SW                                             
115100       END-IF                                                             
115200       ADD +1      TO IX-X                                                
115300     END-PERFORM                                                          
115400                                                                          
115500     IF EMBX-OK                                                           
115600                                                                          
115700        MOVE +1 TO IX-X                                                   
115800        PERFORM UNTIL IX-X > MAX-KVRADER                                  
115900           IF REQU-IDARTNR-EMBX    (IX-X) NOT = ALL '+' OR                
116000              REQU-KVQPACK-EMBX    (IX-X) NOT = ALL '+'                   
116100              MOVE 'X'  TO WS-KDEMBKEY                                    
116200              MOVE IX-X TO WS-KDEMBKEY-IX                                 
116300              MOVE WS-KDEMBKEY TO W-KDEMBAL                               
116400              PERFORM IMS-GET-WDK6-EMB-KVAL                               
116500              IF REQU-IDARTNR-EMBX    (IX-X) NOT = ALL '+' OR             
116600                 NYTT-13-SEGMENT (IX-X) = JA                              
116700                 MOVE WX-IDARTNR (IX-X) TO EMB-IDARTNR-EMB                
116800              END-IF                                                      
116900              IF REQU-KVQPACK-EMBX    (IX-X) NOT = ALL '+' OR             
117000                 NYTT-13-SEGMENT (IX-X) = JA                              
117100                 MOVE WX-KVQPACK (IX-X) TO EMB-KVQPACK-EMB                
117200              END-IF                                                      
117300              MOVE ZERO              TO EMB-KDEMBKOD                      
117400              IF SEGMENT-FINNS                                            
117500                 IF EMB-IDARTNR-EMB = ZERO                                
117600                    PERFORM IMS-DLET-WDK6-EMB                             
117700                 ELSE                                                     
117800                    PERFORM IMS-REPL-WDK6-EMB                             
117900                 END-IF                                                   
118000              ELSE                                                        
118100                 MOVE WS-KDEMBKEY    TO EMB-KDEMBKEY                      
118200                 PERFORM IMS-ISRT-WDK6-EMB                                
118300              END-IF                                                      
118400           END-IF                                                         
118500           ADD +1 TO IX-X                                                 
118600        END-PERFORM                                                       
118700                                                                          
118800     END-IF                                                               
118900     .                                                                    
119000     EJECT                                                                
119100 HCA-UPPD-WDK613-Q  SECTION.                                              
119200                                                                          
119300        IF REQU-IDARTNR-EMBQ0    NOT = ALL '+' OR                         
119400           REQU-KVQPACK-EMBQ0    NOT = ALL '+' OR                         
119500           REQU-KDEMBKOD-EMBQ0    NOT = ALL '+'                           
119600           MOVE 'Q0' TO W-KDEMBAL                                         
119700           PERFORM IMS-GET-WDK6-EMB-KVAL                                  
119800           IF REQU-IDARTNR-EMBQ0    NOT = ALL '+' OR                      
119900              NYTT-13-SEGMENT (11) = JA                                   
120000              MOVE WQ-IDARTNR  (1)   TO EMB-IDARTNR-EMB                   
120100           END-IF                                                         
120200           IF REQU-KVQPACK-EMBQ0    NOT = ALL '+' OR                      
120300              NYTT-13-SEGMENT (11) = JA                                   
120400              MOVE WQ-KVQPACK  (1)   TO EMB-KVQPACK-EMB                   
120500           END-IF                                                         
120600           IF REQU-KDEMBKOD-EMBQ0    NOT = ALL '+' OR                     
120700              NYTT-13-SEGMENT (11) = JA                                   
120800              MOVE WQ-KDEMBKOD (1)   TO EMB-KDEMBKOD                      
120900           END-IF                                                         
121000           IF SEGMENT-FINNS                                               
121100              PERFORM IMS-REPL-WDK6-EMB                                   
121200           ELSE                                                           
121300              MOVE 'Q0'           TO EMB-KDEMBKEY                         
121400              IF REQU-IDARTNR-EMBQ0 = ALL '+'                             
121500                 MOVE 0           TO EMB-IDARTNR-EMB                      
121600              END-IF                                                      
121700                                                                          
121800              IF REQU-KVQPACK-EMBQ0 = ALL '+'                             
121900                 MOVE 0           TO EMB-KVQPACK-EMB                      
122000              END-IF                                                      
122100                                                                          
122200              IF REQU-KDEMBKOD-EMBQ0 = ALL '+'                            
122300                 MOVE 0           TO EMB-KDEMBKOD                         
122400              END-IF                                                      
122500              PERFORM IMS-ISRT-WDK6-EMB                                   
122600           END-IF                                                         
122700        END-IF                                                            
122800                                                                          
122900        IF REQU-IDARTNR-EMBQ1    NOT = ALL '+' OR                         
123000           REQU-KVQPACK-EMBQ1    NOT = ALL '+' OR                         
123100           REQU-KDEMBKOD-EMBQ1    NOT = ALL '+'                           
123200           MOVE 'Q1' TO W-KDEMBAL                                         
123300           PERFORM IMS-GET-WDK6-EMB-KVAL                                  
123400           IF REQU-IDARTNR-EMBQ1    NOT = ALL '+' OR                      
123500              NYTT-13-SEGMENT (12) = JA                                   
123600              MOVE WQ-IDARTNR  (2)   TO EMB-IDARTNR-EMB                   
123700           END-IF                                                         
123800           IF REQU-KVQPACK-EMBQ1    NOT = ALL '+' OR                      
123900              NYTT-13-SEGMENT (12) = JA                                   
124000              MOVE WQ-KVQPACK  (2)   TO EMB-KVQPACK-EMB                   
124100           END-IF                                                         
124200           IF REQU-KDEMBKOD-EMBQ1    NOT = ALL '+' OR                     
124300              NYTT-13-SEGMENT (12) = JA                                   
124400              MOVE WQ-KDEMBKOD (2)   TO EMB-KDEMBKOD                      
124500           END-IF                                                         
124600           IF SEGMENT-FINNS                                               
124700              PERFORM IMS-REPL-WDK6-EMB                                   
124800           ELSE                                                           
124900              MOVE 'Q1'           TO EMB-KDEMBKEY                         
125000              IF REQU-IDARTNR-EMBQ1 = ALL '+'                             
125100                 MOVE 0           TO EMB-IDARTNR-EMB                      
125200              END-IF                                                      
125300                                                                          
125400              IF REQU-KVQPACK-EMBQ1 = ALL '+'                             
125500                 MOVE 0           TO EMB-KVQPACK-EMB                      
125600              END-IF                                                      
125700                                                                          
125800              IF REQU-KDEMBKOD-EMBQ1 = ALL '+'                            
125900                 MOVE 0           TO EMB-KDEMBKOD                         
126000              END-IF                                                      
126100              PERFORM IMS-ISRT-WDK6-EMB                                   
126200           END-IF                                                         
126300        END-IF                                                            
126400                                                                          
126500        IF REQU-IDARTNR-EMBQ2    NOT = ALL '+' OR                         
126600           REQU-KVQPACK-EMBQ2    NOT = ALL '+' OR                         
126700           REQU-KDEMBKOD-EMBQ2    NOT = ALL '+'                           
126800           MOVE 'Q2' TO W-KDEMBAL                                         
126900           PERFORM IMS-GET-WDK6-EMB-KVAL                                  
127000           IF REQU-IDARTNR-EMBQ2    NOT = ALL '+' OR                      
127100              NYTT-13-SEGMENT (13) = JA                                   
127200              MOVE WQ-IDARTNR  (3)   TO EMB-IDARTNR-EMB                   
127300           END-IF                                                         
127400           IF REQU-KVQPACK-EMBQ2    NOT = ALL '+' OR                      
127500              NYTT-13-SEGMENT (13) = JA                                   
127600              MOVE WQ-KVQPACK  (3)   TO EMB-KVQPACK-EMB                   
127700           END-IF                                                         
127800           IF REQU-KDEMBKOD-EMBQ2    NOT = ALL '+' OR                     
127900              NYTT-13-SEGMENT (13) = JA                                   
128000              MOVE WQ-KDEMBKOD (3)   TO EMB-KDEMBKOD                      
128100           END-IF                                                         
128200           IF SEGMENT-FINNS                                               
128300              PERFORM IMS-REPL-WDK6-EMB                                   
128400           ELSE                                                           
128500              MOVE 'Q2'           TO EMB-KDEMBKEY                         
128600              IF REQU-IDARTNR-EMBQ2 = ALL '+'                             
128700                 MOVE 0           TO EMB-IDARTNR-EMB                      
128800              END-IF                                                      
128900                                                                          
129000              IF REQU-KVQPACK-EMBQ2 = ALL '+'                             
129100                 MOVE 0           TO EMB-KVQPACK-EMB                      
129200              END-IF                                                      
129300                                                                          
129400              IF REQU-KDEMBKOD-EMBQ2 = ALL '+'                            
129500                 MOVE 0           TO EMB-KDEMBKOD                         
129600              END-IF                                                      
129700              PERFORM IMS-ISRT-WDK6-EMB                                   
129800           END-IF                                                         
129900        END-IF                                                            
130000        IF REQU-KVQPACK-EMBQ3    NOT = ALL '+'                            
130100           MOVE 'Q3' TO W-KDEMBAL                                         
130200           PERFORM IMS-GET-WDK6-EMB-KVAL                                  
130300           IF REQU-KVQPACK-EMBQ3    NOT = ALL '+' OR                      
130400              NYTT-13-SEGMENT (14) = JA                                   
130500              MOVE WQ-KVQPACK  (4)   TO EMB-KVQPACK-EMB                   
130600           END-IF                                                         
130700           IF SEGMENT-FINNS                                               
130800              PERFORM IMS-REPL-WDK6-EMB                                   
130900           ELSE                                                           
131000              MOVE 'Q3'           TO EMB-KDEMBKEY                         
131100                                                                          
131200              IF REQU-KVQPACK-EMBQ3 = ALL '+'                             
131300                 MOVE 0           TO EMB-KVQPACK-EMB                      
131400              END-IF                                                      
131500                                                                          
131600** MOVE ZEROS TO IDARTNR & KDEMBKOD TO AVOID LOW-VALUES                   
131700              MOVE 0           TO EMB-IDARTNR-EMB                         
131800              MOVE 0           TO EMB-KDEMBKOD                            
131900              PERFORM IMS-ISRT-WDK6-EMB                                   
132000           END-IF                                                         
132100        END-IF                                                            
132200     .                                                                    
132300     EJECT                                                                
132400 S01-GET-EMQ-0-1-2-VALUES SECTION.                                        
132500** TO CONSIDER ONLY CHINA & US                                            
132600     IF NDC-CN OR NDC-US                                                  
132700     PERFORM IMS-GU-WDK711                                                
132800     IF SEGMENT-FINNS                                                     
132900       IF SLAG-IDDC-REF > SPACE                                           
133000         MOVE CLAG-KDARTURS        TO RESP-KDARTURS                       
133100       ELSE                                                               
133200         MOVE SPACE                TO RESP-KDARTURS                       
133300       END-IF                                                             
133400     ELSE                                                                 
133500       MOVE SPACE                  TO RESP-KDARTURS                       
133600     END-IF                                                               
133700*                                                                         
133800     IF NDC-US                                                            
133900       MOVE WC-LAND-US             TO W-IDLAND                            
134000     ELSE                                                                 
134100       MOVE WC-LAND-CN             TO W-IDLAND                            
134200     END-IF                                                               
134300     PERFORM IMS-GU-WDK712                                                
134400     IF SEGMENT-FINNS                                                     
134500       MOVE LART-KDARTURS            TO RESP-KDARTURS                     
134600       IF LART-IDARTNR-EMBQ0 > 0 OR                                       
134700          LART-IDARTNR-EMBQ1 > 0 OR                                       
134800          LART-IDARTNR-EMBQ2 > 0                                          
134900         MOVE LART-TIUPPDAT-EMB      TO WS-TIUPPDAT-EMB                   
135000         MOVE WS-TIUPPDAT-EMB        TO RESP-TIUPPDAT                     
135100         MOVE LART-IDUSER-EMB        TO RESP-IDUSER                       
135200       ELSE                                                               
135300         MOVE ZERO                   TO RESP-TIUPPDAT                     
135400         MOVE SPACE                  TO RESP-IDUSER                       
135500       END-IF                                                             
135600* *                                                                       
135700       IF LART-IDARTNR-EMBQ0 > 0                                          
135800         MOVE LART-IDARTNR-EMBQ0 TO RESP-IDARTNR-EMBQ0-UT                 
135900         INSPECT RESP-IDARTNR-EMBQ0-UT                                    
136000                 REPLACING LEADING ZERO BY SPACE                          
136100       END-IF                                                             
136200* *                                                                       
136300       IF LART-IDARTNR-EMBQ1 > 0                                          
136400         MOVE LART-IDARTNR-EMBQ1 TO RESP-IDARTNR-EMBQ1-UT                 
136500         INSPECT RESP-IDARTNR-EMBQ1-UT                                    
136600                 REPLACING LEADING ZERO BY SPACE                          
136700       END-IF                                                             
136800* *                                                                       
136900       IF LART-IDARTNR-EMBQ2 > 0                                          
137000         MOVE LART-IDARTNR-EMBQ2 TO RESP-IDARTNR-EMBQ2-UT                 
137100         INSPECT RESP-IDARTNR-EMBQ2-UT                                    
137200                 REPLACING LEADING ZERO BY SPACE                          
137300       END-IF                                                             
137400                                                                          
137500       IF LART-KVQPACK-3 > 0                                              
137600         MOVE LART-KVQPACK-3 TO RESP-KVQPACK-EMBQ3-UT                     
137700         INSPECT RESP-KVQPACK-EMBQ3-UT                                    
137800                 REPLACING LEADING ZERO BY SPACE                          
137900       END-IF                                                             
138000                                                                          
138100                                                                          
138200     PERFORM IMS-GU-WDK712                                                
138300     PERFORM IMS-GNP-WDK726                                               
138400       PERFORM UNTIL SEGMENT-SAKNAS                                       
138500           IF SEGMENT-FINNS                                               
138600       IF LEMB-KDEMBKEY (1:1) = 'X'                                       
138700          MOVE LEMB-KDEMBKEY       TO WS-KDEMBKEY-1                       
138800          MOVE WS-KDEMBKEY-1-IX    TO IX-X                                
138900          MOVE LEMB-IDARTNR-EMB    TO RESP-IDARTNR-EMBX-UT (IX-X)         
139000**        MOVE EMB-KVQPACK-EMB     TO RESP-KVQPACK-EMBX-UT (IX-X)         
139100          INSPECT RESP-IDARTNR-EMBX-UT (IX-X)                             
139200                                   REPLACING LEADING ZERO BY SPACE        
139300          MOVE ZEROS               TO RESP-KVQPACK-EMBX-UT (IX-X)         
139400          INSPECT RESP-KVQPACK-EMBX-UT (IX-X)                             
139500                                   REPLACING LEADING ZERO BY SPACE        
139600                                                                          
139700       END-IF                                                             
139800                                                                          
139900       END-IF                                                             
140000       PERFORM IMS-GNP-WDK726                                             
140100     END-PERFORM                                                          
140200* *                                                                       
140300     END-IF                                                               
140400** TO CONSIDER ONLY CHINA & US                                            
140500     END-IF                                                               
140600     .                                                                    
140700     EJECT                                                                
140800 S02-CHECK-USER-ID-WEB SECTION.                                           
140900     IF REQU-IDMSGVER = 1                                                 
141000       IF REQU-IDARTNR-EMBQ0 > 0 OR REQU-IDARTNR-EMBQ1 > 0                
141100          OR REQU-IDARTNR-EMBQ2 > 0                                       
141200         IF REQU-IDUSER-IN = ALL '+'                                      
141300           MOVE MFS-ALFA-FAELT-FEL TO RESP-IDUSER-ATTR                    
141400           MOVE NEJ TO INDATA-SW                                          
141500         END-IF                                                           
141600       END-IF                                                             
141700*      When it's an API call, REQU-IDUSER may not have value.             
141800*      So, we check if REQU-IDUSER-IN is filled in and use that           
141900*      instead.                                                           
142000       IF REQU-IDUSER = SPACES OR LOW-VALUES OR ALL '+'                   
142100         IF REQU-IDUSER-IN = SPACES OR LOW-VALUES OR ALL '+'              
142200           MOVE MFS-ALFA-FAELT-FEL TO RESP-IDUSER-ATTR                    
142300           MOVE NEJ TO INDATA-SW                                          
142400         ELSE                                                             
142500           MOVE REQU-IDUSER-IN     TO REQU-IDUSER                         
142600         END-IF                                                           
142700       END-IF                                                             
142800     END-IF                                                               
142900     .                                                                    
143000     EJECT                                                                
143100                                                                          
143200 MFS-RENSA-FAELT-UT SECTION.                                              
143300                                                                          
143400*    --- ALLA UTDATA-FÄLT                                                 
143500     MOVE ALL-SPACE       TO RESP-BEFT                                    
143600                             RESP-KDFORP                                  
143700                             RESP-FLFPINST-UPP                            
143800                             RESP-TIUPPDAT                                
143900                             RESP-IDUSER                                  
144000                             RESP-IDARTNR-EMBQ0-UT                        
144100                             RESP-IDARTNR-EMBQ1-UT                        
144200                             RESP-IDARTNR-EMBQ2-UT                        
144300                             RESP-KVQPACK-EMBQ0-UT                        
144400                             RESP-KVQPACK-EMBQ1-UT                        
144500                             RESP-KVQPACK-EMBQ2-UT                        
144600                             RESP-KVQPACK-EMBQ3-UT                        
144700                             RESP-KVQPACK-EMBQ4-UT                        
144800                             RESP-KDEMBKOD-EMBQ0-UT                       
144900                             RESP-KDEMBKOD-EMBQ1-UT                       
145000                             RESP-KDEMBKOD-EMBQ2-UT                       
145100     MOVE +1 TO IX-X                                                      
145200     PERFORM UNTIL IX-X > MAX-KVRADER                                     
145300        MOVE ALL-SPACE    TO RESP-IDARTNR-EMBX-UT (IX-X)                  
145400                             RESP-KVQPACK-EMBX-UT (IX-X)                  
145500        ADD +1 TO IX-X                                                    
145600     END-PERFORM                                                          
145700                                                                          
145800     .                                                                    
145900     EJECT                                                                
146000 MFS-RENSA-FAELT-IN SECTION.                                              
146100                                                                          
146200*    --- ALLA INDATA-FÄLT                                                 
146300     MOVE ALL-SPACE     TO   RESP-IDARTNR-KOPI                            
146400     MOVE ALL-SPACE     TO   RESP-IDARTNR-EMBQ0                           
146500                             RESP-IDARTNR-EMBQ1                           
146600                             RESP-IDARTNR-EMBQ2                           
146700                             RESP-KVQPACK-EMBQ0                           
146800                             RESP-KVQPACK-EMBQ1                           
146900                             RESP-KVQPACK-EMBQ2                           
147000                             RESP-KVQPACK-EMBQ3                           
147100                             RESP-KVQPACK-EMBQ4                           
147200                             RESP-KDEMBKOD-EMBQ0                          
147300                             RESP-KDEMBKOD-EMBQ1                          
147400                             RESP-KDEMBKOD-EMBQ2                          
147500     MOVE +1 TO IX-X                                                      
147600     PERFORM UNTIL IX-X > MAX-KVRADER                                     
147700        MOVE ALL-SPACE  TO   RESP-IDARTNR-EMBX (IX-X)                     
147800                             RESP-KVQPACK-EMBX (IX-X)                     
147900        ADD +1 TO IX-X                                                    
148000     END-PERFORM                                                          
148100     .                                                                    
148200     EJECT                                                                
148300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
148400                                                                          
148500*    --- ALLA UTDATA-FÄLT                                                 
148600     MOVE ALL-PLUS          TO RESP-BEFT                                  
148700                               RESP-KDFORP                                
148800                               RESP-FLFPINST-UPP                          
148900                               RESP-TIUPPDAT                              
149000                               RESP-IDUSER                                
149100                               RESP-IDARTNR-EMBQ0-UT                      
149200                               RESP-IDARTNR-EMBQ1-UT                      
149300                               RESP-IDARTNR-EMBQ2-UT                      
149400                               RESP-KVQPACK-EMBQ0-UT                      
149500                               RESP-KVQPACK-EMBQ1-UT                      
149600                               RESP-KVQPACK-EMBQ2-UT                      
149700                               RESP-KVQPACK-EMBQ3-UT                      
149800                               RESP-KVQPACK-EMBQ4-UT                      
149900                               RESP-KDEMBKOD-EMBQ0-UT                     
150000                               RESP-KDEMBKOD-EMBQ1-UT                     
150100                               RESP-KDEMBKOD-EMBQ2-UT                     
150200     MOVE +1 TO IX-X                                                      
150300     PERFORM UNTIL IX-X > MAX-KVRADER                                     
150400        MOVE ALL-PLUS          TO RESP-IDARTNR-EMBX-UT (IX-X)             
150500                                  RESP-KVQPACK-EMBX-UT (IX-X)             
150600        ADD +1 TO IX-X                                                    
150700     END-PERFORM                                                          
150800     .                                                                    
150900     SKIP3                                                                
151000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
151100                                                                          
151200*    --- ALLA INDATA-FÄLT                                                 
151300     MOVE ALL-PLUS          TO RESP-IDARTNR-KOPI                          
151400     MOVE ALL-PLUS          TO RESP-IDARTNR-EMBQ0                         
151500                               RESP-IDARTNR-EMBQ1                         
151600                               RESP-IDARTNR-EMBQ2                         
151700                               RESP-KVQPACK-EMBQ0                         
151800                               RESP-KVQPACK-EMBQ1                         
151900                               RESP-KVQPACK-EMBQ2                         
152000                               RESP-KVQPACK-EMBQ3                         
152100                               RESP-KVQPACK-EMBQ4                         
152200                               RESP-KDEMBKOD-EMBQ0                        
152300                               RESP-KDEMBKOD-EMBQ1                        
152400                               RESP-KDEMBKOD-EMBQ2                        
152500     MOVE +1 TO IX-X                                                      
152600     PERFORM UNTIL IX-X > MAX-KVRADER                                     
152700        MOVE ALL-PLUS          TO RESP-IDARTNR-EMBX (IX-X)                
152800                                  RESP-KVQPACK-EMBX (IX-X)                
152900        ADD +1 TO IX-X                                                    
153000     END-PERFORM                                                          
153100     .                                                                    
153200     EJECT                                                                
153300 MFS-FORM-ATTR SECTION.                                                   
153400                                                                          
153500*    --- ALLA INDATA-FÄLT                                                 
153600     MOVE MFS-FORMATETS-ATTR TO RESP-IDARTNR-KOPI-ATTR                    
153700     MOVE MFS-FORMATETS-ATTR TO RESP-IDARTNR-EMBQ0-ATTR                   
153800                                RESP-IDARTNR-EMBQ1-ATTR                   
153900                                RESP-IDARTNR-EMBQ2-ATTR                   
154000                                RESP-KVQPACK-EMBQ0-ATTR                   
154100                                RESP-KVQPACK-EMBQ1-ATTR                   
154200                                RESP-KVQPACK-EMBQ2-ATTR                   
154300                                RESP-KVQPACK-EMBQ3-ATTR                   
154400                                RESP-KVQPACK-EMBQ4-ATTR                   
154500                                RESP-KDEMBKOD-EMBQ0-ATTR                  
154600                                RESP-KDEMBKOD-EMBQ1-ATTR                  
154700                                RESP-KDEMBKOD-EMBQ2-ATTR                  
154800                                RESP-IDUSER-ATTR                          
154900     MOVE +1 TO IX-X                                                      
155000     PERFORM UNTIL IX-X > MAX-KVRADER                                     
155100        MOVE MFS-FORMATETS-ATTR TO                                        
155200                                RESP-IDARTNR-EMBX-ATTR (IX-X)             
155300                                RESP-KVQPACK-EMBX-ATTR (IX-X)             
155400        ADD +1 TO IX-X                                                    
155500     END-PERFORM                                                          
155600     .                                                                    
155700     SKIP2                                                                
155800 MFS-LAES-IN-IGEN SECTION.                                                
155900                                                                          
156000*    --- ALLA INDATA-FÄLT                                                 
156100     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDARTNR-KOPI-ATTR                 
156200     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDARTNR-EMBQ0-ATTR                
156300                                   RESP-IDARTNR-EMBQ1-ATTR                
156400                                   RESP-IDARTNR-EMBQ2-ATTR                
156500                                   RESP-KVQPACK-EMBQ0-ATTR                
156600                                   RESP-KVQPACK-EMBQ1-ATTR                
156700                                   RESP-KVQPACK-EMBQ2-ATTR                
156800                                   RESP-KVQPACK-EMBQ3-ATTR                
156900                                   RESP-KVQPACK-EMBQ4-ATTR                
157000                                   RESP-KDEMBKOD-EMBQ0-ATTR               
157100                                   RESP-KDEMBKOD-EMBQ1-ATTR               
157200                                   RESP-KDEMBKOD-EMBQ2-ATTR               
157300     MOVE +1 TO IX-X                                                      
157400     PERFORM UNTIL IX-X > MAX-KVRADER                                     
157500        MOVE MFS-ADD-LAES-IN-FAELT                                        
157600                                TO RESP-IDARTNR-EMBX-ATTR(IX-X)           
157700                                   RESP-KVQPACK-EMBX-ATTR(IX-X)           
157800        ADD +1 TO IX-X                                                    
157900     END-PERFORM                                                          
158000     .                                                                    
158100     EJECT                                                                
158200 MFS-LOCK-FIELDS-US-CN SECTION.                                           
158300     MOVE +1 TO IX-X                                                      
158400     PERFORM UNTIL IX-X > 10                                              
158500        MOVE MFS-CLOSE-FIELD    TO RESP-KVQPACK-EMBX-ATTR(IX-X)           
158600                                                                          
158700        ADD +1 TO IX-X                                                    
158800     END-PERFORM                                                          
158900     .                                                                    
159000     EJECT                                                                
159100                                                                          
159200 MFS-LOCK-FIELDS-CHINA SECTION.                                           
159300     MOVE MFS-CLOSE-FIELD    TO RESP-IDARTNR-KOPI-ATTR                    
159400                                RESP-KVQPACK-EMBQ0-ATTR                   
159500                                RESP-KDEMBKOD-EMBQ0-ATTR                  
159600                                RESP-KVQPACK-EMBQ1-ATTR                   
159700                                RESP-KDEMBKOD-EMBQ1-ATTR                  
159800                                RESP-KVQPACK-EMBQ2-ATTR                   
159900                                RESP-KDEMBKOD-EMBQ2-ATTR                  
160000**                              RESP-KVQPACK-EMBQ3-ATTR                   
160100                                RESP-KVQPACK-EMBQ4-ATTR                   
160200     MOVE +1 TO IX-X                                                      
160300     PERFORM UNTIL IX-X > 10                                              
160400**      MOVE MFS-CLOSE-FIELD    TO RESP-IDARTNR-EMBX-ATTR(IX-X)           
160500        MOVE MFS-CLOSE-FIELD    TO RESP-KVQPACK-EMBX-ATTR(IX-X)           
160600                                                                          
160700                                                                          
160800        ADD +1 TO IX-X                                                    
160900     END-PERFORM                                                          
161000     .                                                                    
161100     EJECT                                                                
161200* --- IMS SEKTIONER ---                                                   
161300     SKIP3                                                                
161400 IMS-GU-WDK711 SECTION.                                                   
161500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
161600          DELIMITED BY SIZE INTO SSA1                                     
161700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
161800          DELIMITED BY SIZE INTO SSA2                                     
161900     MOVE 'GE  ' TO GODK-STATUSKODER                                      
162000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
162100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
162200     PERFORM IMS-STATUSKONTROLL                                           
162300     .                                                                    
162400 IMS-GU-WDK712 SECTION.                                                   
162500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
162600          DELIMITED BY SIZE INTO SSA1                                     
162700     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
162800          DELIMITED BY SIZE INTO SSA2                                     
162900     MOVE 'GE  ' TO GODK-STATUSKODER                                      
163000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
163100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
163200     PERFORM IMS-STATUSKONTROLL                                           
163300     .                                                                    
163400 IMS-GNP-WDK726 SECTION.                                                  
163500                                                                          
163600     MOVE '  GE' TO GODK-STATUSKODER                                      
163700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK726                        
163800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
163900     PERFORM IMS-STATUSKONTROLL                                           
164000     .                                                                    
164100*IMS-GU-WDK726 SECTION.                                                   
164200*    STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
164300*         DELIMITED BY SIZE INTO SSA1                                     
164400*    STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
164500*         DELIMITED BY SIZE INTO SSA2                                     
164600*    MOVE 'WDK726'          TO SSA3                                       
164700*    MOVE 'GE  ' TO GODK-STATUSKODER                                      
164800*    CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2 SSA3          
164900*    MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
165000*    PERFORM IMS-STATUSKONTROLL                                           
165100*    .                                                                    
165200*IMS-GN-WDK726 SECTION.                                                   
165300*    STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
165400*         DELIMITED BY SIZE INTO SSA1                                     
165500*    STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
165600*         DELIMITED BY SIZE INTO SSA2                                     
165700*    MOVE 'WDK726'          TO SSA3                                       
165800*    MOVE 'GE  ' TO GODK-STATUSKODER                                      
165900*    CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2 SSA3          
166000*    MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
166100*    PERFORM IMS-STATUSKONTROLL                                           
166200*    .                                                                    
166300 IMS-GHU-WDK712 SECTION.                                                  
166400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
166500          DELIMITED BY SIZE INTO SSA1                                     
166600     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
166700          DELIMITED BY SIZE INTO SSA2                                     
166800     MOVE '  ' TO GODK-STATUSKODER                                        
166900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
167000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
167100     PERFORM IMS-STATUSKONTROLL                                           
167200     .                                                                    
167300 IMS-REPL-WDK712 SECTION.                                                 
167400     MOVE '  ' TO GODK-STATUSKODER                                        
167500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
167600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
167700     PERFORM IMS-STATUSKONTROLL                                           
167800     .                                                                    
167900     EJECT                                                                
168000 IMS-GET-WDK726 SECTION.                                                  
168100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
168200          DELIMITED BY SIZE INTO SSA1                                     
168300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
168400          DELIMITED BY SIZE INTO SSA2                                     
168500     STRING 'WDK726  (KDEMBKEY =' WS-KDEMBKEY-1 ')'                       
168600          DELIMITED BY SIZE INTO SSA3                                     
168700     MOVE '  GE' TO GODK-STATUSKODER                                      
168800     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK726 SSA1 SSA2 SSA3         
168900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
169000     PERFORM IMS-STATUSKONTROLL                                           
169100     .                                                                    
169200 IMS-REPL-WDK726 SECTION.                                                 
169300     MOVE '  ' TO GODK-STATUSKODER                                        
169400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK726                       
169500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
169600     PERFORM IMS-STATUSKONTROLL                                           
169700     .                                                                    
169800     EJECT                                                                
169900 IMS-GU-WDK6-ROT SECTION.                                                 
170000                                                                          
170100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
170200          DELIMITED BY SIZE INTO SSA1                                     
170300     MOVE '  GE' TO GODK-STATUSKODER                                      
170400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
170500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
170600     PERFORM IMS-STATUSKONTROLL                                           
170700     .                                                                    
170800     EJECT                                                                
170900 IMS-GET-WDK6-CLAG SECTION.                                               
171000                                                                          
171100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
171200          DELIMITED BY SIZE INTO SSA1                                     
171300     MOVE '  GE' TO GODK-STATUSKODER                                      
171400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
171500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
171600     PERFORM IMS-STATUSKONTROLL                                           
171700     .                                                                    
171800     SKIP3                                                                
171900 IMS-REPL-WDK6-CLAG SECTION.                                              
172000                                                                          
172100     MOVE '  ' TO GODK-STATUSKODER                                        
172200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
172300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
172400     PERFORM IMS-STATUSKONTROLL                                           
172500     .                                                                    
172600     EJECT                                                                
172700 IMS-GET-WDT3-FPCK SECTION.                                               
172800                                                                          
172900     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
173000          DELIMITED BY SIZE INTO SSA1                                     
173100     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
173200          DELIMITED BY SIZE INTO SSA2                                     
173300     MOVE '  GE' TO GODK-STATUSKODER                                      
173400     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT311 SSA1 SSA2               
173500     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
173600     PERFORM IMS-STATUSKONTROLL                                           
173700     .                                                                    
173800     EJECT                                                                
173900 IMS-GET-WDK6-EMB SECTION.                                                
174000                                                                          
174100     STRING 'WDK613    '                                                  
174200          DELIMITED BY SIZE INTO SSA1                                     
174300     MOVE '  GE' TO GODK-STATUSKODER                                      
174400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK613 SSA1                  
174500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
174600     PERFORM IMS-STATUSKONTROLL                                           
174700     .                                                                    
174800     SKIP3                                                                
174900 IMS-GET-WDK6-EMB-KVAL SECTION.                                           
175000                                                                          
175100     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
175200          DELIMITED BY SIZE INTO SSA1                                     
175300     MOVE '  GE' TO GODK-STATUSKODER                                      
175400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK613 SSA1                  
175500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
175600     PERFORM IMS-STATUSKONTROLL                                           
175700     .                                                                    
175800     SKIP3                                                                
175900 IMS-ISRT-WDK6-EMB SECTION.                                               
176000                                                                          
176100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
176200          DELIMITED BY SIZE INTO SSA1                                     
176300     MOVE 'WDK613   ' TO SSA2                                             
176400     MOVE '  II' TO GODK-STATUSKODER                                      
176500     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
176600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
176700     PERFORM IMS-STATUSKONTROLL                                           
176800     .                                                                    
176900     SKIP3                                                                
177000 IMS-REPL-WDK6-EMB SECTION.                                               
177100                                                                          
177200     MOVE '  ' TO GODK-STATUSKODER                                        
177300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK613                       
177400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
177500     PERFORM IMS-STATUSKONTROLL                                           
177600     .                                                                    
177700     SKIP3                                                                
177800 IMS-DLET-WDK6-EMB SECTION.                                               
177900                                                                          
178000     MOVE '  ' TO GODK-STATUSKODER                                        
178100     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK613                       
178200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
178300     PERFORM IMS-STATUSKONTROLL                                           
178400     .                                                                    
178500     EJECT                                                                
178600*    ANROP FÖR KOPIERING                                                  
178700                                                                          
178800 IMS-GU-KWDK6-ROT SECTION.                                                
178900                                                                          
179000     STRING 'WDK601  (IDARTNR  =' KW-IDARTNR-X ')'                        
179100          DELIMITED BY SIZE INTO SSA1                                     
179200     MOVE '  GE' TO GODK-STATUSKODER                                      
179300     CALL CBLTDLI USING GU KWDK6-PCB DLI-IO-KWDK601 SSA1                  
179400     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
179500     PERFORM IMS-STATUSKONTROLL                                           
179600     .                                                                    
179700     SKIP3                                                                
179800 IMS-GU-KWDK6-CLAG SECTION.                                               
179900                                                                          
180000     STRING 'WDK601  (IDARTNR  =' KW-IDARTNR-X ')'                        
180100          DELIMITED BY SIZE INTO SSA1                                     
180200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
180300          DELIMITED BY SIZE INTO SSA2                                     
180400     MOVE '  GE' TO GODK-STATUSKODER                                      
180500     CALL CBLTDLI USING GU KWDK6-PCB DLI-IO-KWDK611 SSA1 SSA2             
180600     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
180700     PERFORM IMS-STATUSKONTROLL                                           
180800     .                                                                    
180900     EJECT                                                                
181000 IMS-GET-KWDK6-CLAG SECTION.                                              
181100                                                                          
181200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
181300          DELIMITED BY SIZE INTO SSA1                                     
181400     MOVE '  GE' TO GODK-STATUSKODER                                      
181500     CALL CBLTDLI USING GHNP KWDK6-PCB DLI-IO-KWDK611 SSA1                
181600     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
181700     PERFORM IMS-STATUSKONTROLL                                           
181800     .                                                                    
181900     EJECT                                                                
182000 IMS-GET-KWDK6-EMB SECTION.                                               
182100                                                                          
182200     STRING 'WDK613    '                                                  
182300          DELIMITED BY SIZE INTO SSA1                                     
182400     MOVE '  GE' TO GODK-STATUSKODER                                      
182500     CALL CBLTDLI USING GHNP KWDK6-PCB DLI-IO-KWDK613 SSA1                
182600     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
182700     PERFORM IMS-STATUSKONTROLL                                           
182800     .                                                                    
182900     EJECT                                                                
183000 IMS-STATUSKONTROLL SECTION.                                              
183100                                                                          
183200     SET STATUS-IX TO 1                                                   
183300     SEARCH GODK-STATUS                                                   
183400       AT END                                                             
183500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
183600         DELIMITED BY SIZE INTO FELTEXT                                   
183700         CALL FELLOG                                                      
183800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
183900         CONTINUE                                                         
184000     END-SEARCH                                                           
184100     .                                                                    
