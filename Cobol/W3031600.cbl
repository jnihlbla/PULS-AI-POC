000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3031600.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   93/12/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR KAMPANJ RABATT INFORMATION.                                
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLPRIB (WDC2)                              
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W3T316                                              
001500*        MID:         W3I31601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W3O31601                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W3031600'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003500*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003700 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
003800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003900                                                                          
004000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004100 77  WS-KDARTKAM                 PIC X(5)    VALUE SPACE.                 
004200                                                                          
004300 01  WS-IDDISTR                  PIC X(5)    VALUE SPACE.                 
004400 01  FILLER REDEFINES WS-IDDISTR.                                         
004500     03 FILLER                   PIC X(1).                                
004600     03 WS-IDDISTR-IN            PIC X(4).                                
004700                                                                          
004800 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
004900 01  FILLER REDEFINES WS-IDPROMR.                                         
005000     03  WS-MARKBOLAG            PIC X(1).                                
005100     03  FILLER                  PIC X(2).                                
005200                                                                          
005300 01  DAGENS-DATUM-Y2K            PIC 9(8)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM-Y2K.                                   
005500     03  DAGENS-AAAA-Y2K         PIC 9(4).                                
005600     03  DAGENS-MM-Y2K           PIC 9(2).                                
005700     03  DAGENS-DD-Y2K           PIC 9(2).                                
005800                                                                          
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES DAGENS-DATUM.                                       
006100     03  DAGENS-AA               PIC 9(2).                                
006200     03  DAGENS-MM               PIC 9(2).                                
006300     03  DAGENS-DD               PIC 9(2).                                
006400                                                                          
006500 77  KAMPANJ-SW                  PIC X       VALUE 'J'.                   
006600     88  ALLA-KAMPANJER                      VALUE 'J'.                   
006700     88  UNIK-KAMPANJ                        VALUE 'N'.                   
006800                                                                          
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300 77  PRISOMR-SW                  PIC X       VALUE 'J'.                   
007400     88  PRISOMR-FINNS                       VALUE 'J'.                   
007500     88  PRISOMR-SAKNAS                      VALUE 'N'.                   
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007800     88  EGEN-MID                            VALUE '3316'.                
007900     88  GODK-MID                            VALUE '3316'.                
008000     88  HELP-MID                            VALUE '0551'.                
008100     EJECT                                                                
008200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008300 01  GENERELLA-SUBPROGRAM.                                                
008400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900*01 -COPY WMEDAREA                                                        
009000     SKIP3                                                                
009100 01  MESSAGE-CODES.                                                       
009200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009500     03  ERR-PRICEAREA-MISSING   PIC X(3)    VALUE '236'.                 
009600     03  ERR-TRANSFER-MISSING    PIC X(3)    VALUE '237'.                 
009700     EJECT                                                                
009800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010100     SKIP3                                                                
010200*01  MID -COPY W3I31601                                                   
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010500     SKIP3                                                                
010600*01  -COPY WMSGAREA                                                       
010700     EJECT                                                                
010800     03  MOD REDEFINES MSG-AREA.                                          
010900*      05  -COPY W3O31601                                                 
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011200     SKIP3                                                                
011300*01  -COPY WMFSAREA                                                       
011400     EJECT                                                                
011500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011600*                                                                         
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  NYCKLAR-TILL-DLI.                                                    
012100     03  W-IDPROMR-X.                                                     
012200         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
012300     SKIP2                                                                
012400     03  W-WDC212KY-X.                                                    
012500         05  W-KDARTKAM          PIC  9(5)   VALUE ZERO.                  
012600         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
012700     03  W-WDC212KY-MIN-X.                                                
012800         05  W-KDARTKAM-MIN      PIC  9(5)   VALUE ZERO.                  
012900         05  W-DASTADAT-MIN      PIC 9(8)    VALUE ZERO.                  
013000     03  W-WDC212KY-MAX-X.                                                
013100         05  W-KDARTKAM-MAX      PIC  9(5)   VALUE ZERO.                  
013200         05  W-DASTADAT-MAX      PIC 9(8)    VALUE ZERO.                  
013300     SKIP2                                                                
013400*   NYCKLAR TILL KUNDREG             ***********                          
013500                                                                          
013600     03  W-IDGMT-MIN-X.                                                   
013700         05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.           
013800         05  W-IDKUNDNR-B1       PIC S9(7)   VALUE ZERO COMP-3.           
013900                                                                          
014000     03  W-IDGMT-MAX-X.                                                   
014100         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
014200         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE 9999999                
014300                                                        COMP-3.           
014400                                                                          
014500     03  W-IDDISTR-X.                                                     
014600         05  WA-IDDISTR  PIC S9(5)           COMP-3.                      
014700*   NYCKLAR TILL BETALNINGSREGISTRET ***********                          
014800     03  W-WDB101KY-X.                                                    
014900         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
015000         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
015100                                                                          
015200*    --- STATUS-KOD FRÅN IMS                                              
015300 01  STATUS-WS                   PIC XX.                                  
015400     88  SEGMENT-FINNS                       VALUE '  '.                  
015500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016900     SKIP3                                                                
017000 01  DLI-IO-AREA.                                                         
017100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
017200     SKIP3                                                                
017300     03  WLPRIB01 REDEFINES IO-AREA.                                      
017400*        05  -COPY WDC201  -PRE PRIB-                                     
017500     SKIP3                                                                
017600     03  WLPRIB12 REDEFINES IO-AREA.                                      
017700*        05  -COPY WDC212  -PRE PRIB-                                     
017800     EJECT                                                                
017900**   KUNDREGISTER                                                         
018000 01  DLI-IO-AREA2.                                                        
018100*    03  WLGMTA01  -COPY WDB201 -PRE GMTA-                                
018200     EJECT                                                                
018300**   BETALNINGSREGISTER                                                   
018400 01  DLI-IO-AREA3.                                                        
018500     03  WDB1011.                                                         
018600*        05  -COPY WDB101  -PRE WDB1-                                     
018700     EJECT                                                                
018800 LINKAGE SECTION.                                                         
018900                                                                          
019000*01  -COPY W0009   -PRE MSG-                                              
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE PRIB-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008  -PRE GMTA-                                              
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800*01  -COPY W0008  -PRE WDB1-                                              
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING MSG-PCB PRIB-PCB GMTA-PCB WDB1-PCB.            
020200 MAIN SECTION.                                                            
020300     ENTRY 'DLITCBL' USING MSG-PCB PRIB-PCB GMTA-PCB WDB1-PCB.            
020400                                                                          
020500     PERFORM IMS-GET-MSG                                                  
020600     IF SEGMENT-FINNS                                                     
020700       PERFORM A-INIT                                                     
020800       PERFORM B-KOLLA-NYCKLAR                                            
020900       IF NYCKLAR-OK                                                      
021000           IF MFS-FIRST                                                   
021100             PERFORM C-FOERSTA-SIDA                                       
021200           ELSE                                                           
021300             IF MFS-NEXT                                                  
021400               PERFORM D-NAESTA-SIDA                                      
021500             ELSE                                                         
021600               PERFORM E-SAMMA-SIDA                                       
021700             END-IF                                                       
021800           END-IF                                                         
021900         PERFORM F-LAES-VISA-INFO                                         
022000       END-IF                                                             
022100       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O31601 + 4                      
022200       PERFORM IMS-INSERT-MSG                                             
022300     END-IF                                                               
022400                                                                          
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 A-INIT SECTION.                                                          
023000                                                                          
023100     IF MSG-DUBBLA-TRANSKODER                                             
023200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31601                 
023300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
023400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023500     ELSE                                                                 
023600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31601                  
023700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
023800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023900     END-IF                                                               
024000                                                                          
024100      MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                     
024200      MOVE MSG-IDPFK   TO MFS-IDPFK                                       
024300      MOVE MFS-IDTRANS TO W-IDTRANS                                       
024400                                                                          
024500      MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                     
024600      MOVE MSG-IDPFK   TO MFS-IDPFK                                       
024700      MOVE MFS-IDTRANS TO W-IDTRANS                                       
024800                                                                          
024900      MOVE LOW-VALUE TO MSG-AREA                                          
025000      MOVE 'W3O316N1' TO MFS-IDMOD                                        
025100      MOVE '3316' TO MOD-IDTRANS                                          
025200      MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                
025300      MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                                
025400                                                                          
025500     IF EGEN-MID OR HELP-MID                                              
025600       CONTINUE                                                           
025700     ELSE                                                                 
025800       MOVE SPACE TO MFS-KDTRTYP                                          
025900       MOVE '7' TO MFS-IDPFK                                              
026000     END-IF                                                               
026100                                                                          
026200     IF ENGLISH-TEXT                                                      
026300       MOVE +2 TO SPRAK-IX                                                
026400       MOVE 'GB ' TO MED-IDSKYLT                                          
026500     ELSE                                                                 
026600       MOVE +1 TO SPRAK-IX                                                
026700       MOVE 'S  ' TO MED-IDSKYLT                                          
026800     END-IF                                                               
026900     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-Y2K                  
027000     MOVE DAGENS-DATUM-Y2K TO W-DASTADAT                                  
027100                              W-DASTADAT-MAX                              
027200     .                                                                    
027300     EJECT                                                                
027400 B-KOLLA-NYCKLAR SECTION.                                                 
027500                                                                          
027600     MOVE JA TO NYCKLAR-SW                                                
027700     MOVE JA TO PRISOMR-SW                                                
027800                                                                          
027900*    -- KONTROLL AV IDPROMR                                               
028000     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                               
028100                                                                          
028200     IF MID-IDPROMR-IN = ALL '+'                                          
028300       MOVE MID-IDPROMR-UT TO WS-IDPROMR                                  
028400     ELSE                                                                 
028500       MOVE MID-IDPROMR-IN TO WS-IDPROMR                                  
028600       MOVE SPACE       TO MID-IDDISTR-UT                                 
028700       MOVE '7'         TO MFS-IDPFK                                      
028800       MOVE SPACE       TO MFS-KDTRTYP                                    
028900     END-IF                                                               
029000                                                                          
029100     IF WS-IDPROMR NOT = SPACE                                            
029200       MOVE WS-IDPROMR TO W-IDPROMR                                       
029300     END-IF                                                               
029400                                                                          
029500*    -- KONTROLL AV KDARTKAM                                              
029600                                                                          
029700     IF MID-KDARTKAM-IN = ALL '+'                                         
029800       MOVE MID-KDARTKAM-UT TO WS-KDARTKAM                                
029900       INSPECT WS-KDARTKAM REPLACING LEADING SPACE BY ZERO                
030000     ELSE                                                                 
030100       MOVE MID-KDARTKAM-IN TO WS-KDARTKAM                                
030200       MOVE '7'         TO MFS-IDPFK                                      
030300       MOVE SPACE       TO MFS-KDTRTYP                                    
030400     END-IF                                                               
030500                                                                          
030600     IF EGEN-MID                                                          
030700       CONTINUE                                                           
030800     ELSE                                                                 
030900       IF GODK-MID                                                        
031000         IF WS-KDARTKAM NUMERIC                                           
031100           CONTINUE                                                       
031200         ELSE                                                             
031300           MOVE ZERO TO WS-KDARTKAM                                       
031400         END-IF                                                           
031500       END-IF                                                             
031600     END-IF                                                               
031700                                                                          
031800     IF WS-KDARTKAM NUMERIC                                               
031900       IF WS-KDARTKAM = ZERO                                              
032000         MOVE JA TO KAMPANJ-SW                                            
032100       ELSE                                                               
032200         MOVE NEJ TO KAMPANJ-SW                                           
032300       END-IF                                                             
032400     ELSE                                                                 
032500       MOVE NEJ TO NYCKLAR-SW                                             
032600     END-IF                                                               
032700                                                                          
032800     MOVE WS-KDARTKAM TO W-KDARTKAM                                       
032900                         W-KDARTKAM-MIN                                   
033000                         W-KDARTKAM-MAX                                   
033100     IF WS-KDARTKAM = ZERO                                                
033200       MOVE 99999 TO W-KDARTKAM-MAX                                       
033300     END-IF                                                               
033400                                                                          
033500*    -- KONTROLL AV IDDISTR                                               
033600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
033700                                                                          
033800     IF MID-IDDISTR-IN = ALL '+'                                          
033900       MOVE MID-IDDISTR-UT TO WS-IDDISTR-IN                               
034000     ELSE                                                                 
034100       MOVE MID-IDDISTR-IN TO WS-IDDISTR-IN                               
034200       MOVE '7'         TO MFS-IDPFK                                      
034300       MOVE SPACE       TO MFS-KDTRTYP                                    
034400     END-IF                                                               
034500                                                                          
034600     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
034700                                                                          
034800     IF WS-IDDISTR NUMERIC                                                
034900       IF WS-IDDISTR > ZERO                                               
035000         MOVE WS-IDDISTR TO WA-IDDISTR                                    
035100                                                                          
035200                            W-IDDISTR-B1                                  
035300                            W-IDDISTR-B2                                  
035400        END-IF                                                            
035500     END-IF                                                               
035600                                                                          
035700     IF WS-IDPROMR = SPACE                                                
035800        IF WS-IDDISTR NUMERIC                                             
035900           IF WS-IDDISTR > ZERO                                           
036000              MOVE NEJ TO PRISOMR-SW                                      
036100           ELSE                                                           
036200              MOVE NEJ TO NYCKLAR-SW                                      
036300           END-IF                                                         
036400        ELSE                                                              
036500           MOVE NEJ TO NYCKLAR-SW                                         
036600        END-IF                                                            
036700     ELSE                                                                 
036800        IF WS-IDDISTR NUMERIC                                             
036900           IF WS-IDDISTR > ZERO                                           
037000              MOVE NEJ TO PRISOMR-SW                                      
037100           END-IF                                                         
037200        END-IF                                                            
037300     END-IF                                                               
037400                                                                          
037500     IF EGEN-MID OR GODK-MID                                              
037600        CONTINUE                                                          
037700     ELSE                                                                 
037800        MOVE NEJ TO NYCKLAR-SW                                            
037900     END-IF                                                               
038000                                                                          
038100     IF NYCKLAR-OK                                                        
038200       IF EGEN-MID OR GODK-MID                                            
038300         IF PRISOMR-SAKNAS                                                
038400            MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                        
038500            MOVE SPACE           TO W-IDPROMR                             
038600         ELSE                                                             
038700            MOVE WS-IDPROMR TO MOD-IDPROMR-UT                             
038800         END-IF                                                           
038900         MOVE WS-KDARTKAM       TO MOD-KDARTKAM-UT                        
039000         INSPECT MOD-KDARTKAM-UT REPLACING LEADING ZERO BY SPACE          
039100         MOVE WS-IDDISTR-IN TO MOD-IDDISTR-UT                             
039200         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
039300       ELSE                                                               
039400         MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                           
039500         MOVE MFS-RENSA-FAELT TO MOD-KDARTKAM-UT                          
039600         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
039700       END-IF                                                             
039800     ELSE                                                                 
039900       IF EGEN-MID OR GODK-MID                                            
040000          MOVE WS-IDPROMR TO MOD-IDPROMR-UT                               
040100          MOVE WS-KDARTKAM         TO MOD-KDARTKAM-UT                     
040200          MOVE WS-IDDISTR-IN       TO MOD-IDDISTR-UT                      
040300          INSPECT MOD-KDARTKAM-UT REPLACING LEADING ZERO BY SPACE         
040400          INSPECT MOD-IDDISTR-UT                                          
040500                REPLACING LEADING ZERO BY SPACE                           
040600       ELSE                                                               
040700          MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                          
040800          MOVE MFS-RENSA-FAELT TO MOD-KDARTKAM-UT                         
040900          MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                          
041000       END-IF                                                             
041100     END-IF                                                               
041200                                                                          
041300     IF NYCKLAR-FEL                                                       
041400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
041500       CALL WMEDKONV USING MED-WMEDAREA                                   
041600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041700       PERFORM MFS-RENSA-FAELT-UT                                         
041800     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100 C-FOERSTA-SIDA SECTION.                                                  
042200                                                                          
042300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
042400     CALL WMEDKONV USING MED-WMEDAREA                                     
042500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
042600                                                                          
042700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
042800*    MOVE ZERO  TO W-KDARTKAM                                             
042900*    MOVE ZERO  TO W-DASTADAT                                             
043000     .                                                                    
043100     EJECT                                                                
043200 D-NAESTA-SIDA SECTION.                                                   
043300                                                                          
043400     MOVE MID-IDPROMR-SPAR  TO W-IDPROMR                                  
043500     MOVE MID-KDARTKAM-NEXT TO W-KDARTKAM-MIN                             
043600     MOVE MID-TISTADAT-NEXT TO W-DASTADAT-MIN                             
043700*---Y2K-FIX*******                                                        
043800     IF MID-TISTADAT-NEXT NOT = ZERO                                      
043900       IF MID-TISTADAT-NEXT < 500000                                      
044000         MOVE 20          TO W-DASTADAT-MIN(1:2)                          
044100       ELSE                                                               
044200         IF MID-TISTADAT-NEXT < 999999                                    
044300           MOVE 19        TO W-DASTADAT-MIN(1:2)                          
044400         ELSE                                                             
044500           MOVE 99999999  TO W-DASTADAT-MIN                               
044600         END-IF                                                           
044700       END-IF                                                             
044800     END-IF                                                               
044900     .                                                                    
045000     EJECT                                                                
045100 E-SAMMA-SIDA SECTION.                                                    
045200                                                                          
045300     IF EGEN-MID OR HELP-MID                                              
045400       MOVE MID-KDARTKAM-ENTER TO W-KDARTKAM                              
045500       MOVE MID-TISTADAT-ENTER TO W-DASTADAT                              
045600*---Y2K-FIX*******                                                        
045700       IF MID-TISTADAT-ENTER NOT = ZERO                                   
045800         IF MID-TISTADAT-ENTER < 500000                                   
045900           MOVE 20          TO W-DASTADAT(1:2)                            
046000         ELSE                                                             
046100           IF MID-TISTADAT-ENTER < 999999                                 
046200             MOVE 19        TO W-DASTADAT(1:2)                            
046300           ELSE                                                           
046400             MOVE 99999999  TO W-DASTADAT                                 
046500           END-IF                                                         
046600         END-IF                                                           
046700       END-IF                                                             
046800     END-IF                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 F-LAES-VISA-INFO SECTION.                                                
047200                                                                          
047300     IF PRISOMR-SAKNAS                                                    
047400        PERFORM FC-HT-PROM-VIA-DISTRIKT                                   
047500     END-IF                                                               
047600     PERFORM IMS-GU-WDC201                                                
047700                                                                          
047800     IF SEGMENT-SAKNAS                                                    
047900       MOVE ERR-PRICEAREA-MISSING TO MED-IDMFSFEL                         
048000       CALL WMEDKONV USING MED-WMEDAREA                                   
048100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
048200       PERFORM MFS-RENSA-FAELT-UT                                         
048300     ELSE                                                                 
048400       MOVE PRIB-PRO-IDPROMR TO MOD-IDPROMR-SPAR                          
048500       MOVE +1 TO INDX                                                    
048600       IF ALLA-KAMPANJER                                                  
048700         PERFORM IMS-GNP-WDC212                                           
048800         IF SEGMENT-FINNS                                                 
048900           MOVE PRIB-KAM-KDARTKAM      TO MOD-KDARTKAM-ENTER              
049000           MOVE PRIB-KAM-DASTADAT(3:6) TO MOD-TISTADAT-ENTER              
049100         ELSE                                                             
049200           MOVE ZERO           TO MOD-KDARTKAM-ENTER                      
049300           MOVE ZERO           TO MOD-TISTADAT-ENTER                      
049400           MOVE ERR-TRANSFER-MISSING TO MED-IDMFSFEL                      
049500           CALL WMEDKONV USING MED-WMEDAREA                               
049600           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
049700           PERFORM MFS-RENSA-FAELT-UT                                     
049800         END-IF                                                           
049900                                                                          
050000         PERFORM UNTIL INDX > MAX-INDX                                    
050100           IF SEGMENT-FINNS                                               
050200             MOVE PRIB-KAM-KDARTKAM TO MOD-KDARTKAM         (INDX)        
050300             MOVE PRIB-KAM-REARTRAB-BULK TO                               
050400                  MOD-REARTRAB-BULK(INDX)                                 
050500             MOVE PRIB-KAM-REARTRAB-DO   TO MOD-REARTRAB-DO (INDX)        
050600             MOVE PRIB-KAM-DASTADAT(3:6) TO MOD-TISTADAT    (INDX)        
050700             MOVE PRIB-KAM-TISTODAT      TO MOD-TISTODAT   (INDX)         
050800             PERFORM IMS-GNP-WDC212                                       
050900           ELSE                                                           
051000             MOVE MFS-RENSA-FAELT TO MOD-KDARTKAM    (INDX)               
051100                                     MOD-REARTRAB-BULK (INDX)             
051200                                     MOD-REARTRAB-DO (INDX)               
051300                                     MOD-TISTADAT    (INDX)               
051400                                     MOD-TISTODAT    (INDX)               
051500           END-IF                                                         
051600           ADD 1 TO INDX                                                  
051700         END-PERFORM                                                      
051800                                                                          
051900         IF SEGMENT-FINNS                                                 
052000           MOVE PRIB-KAM-KDARTKAM      TO MOD-KDARTKAM-NEXT               
052100           MOVE PRIB-KAM-DASTADAT(3:6) TO MOD-TISTADAT-NEXT               
052200           MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                    
052300           CALL WMEDKONV USING MED-WMEDAREA                               
052400           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
052500         ELSE                                                             
052600           MOVE ZERO           TO MOD-KDARTKAM-NEXT                       
052700           MOVE ZERO           TO MOD-TISTADAT-NEXT                       
052800         END-IF                                                           
052900       ELSE                                                               
053000         PERFORM FB-LAES-UNIK                                             
053100       END-IF                                                             
053200                                                                          
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600 FB-LAES-UNIK SECTION.                                                    
053700     SKIP2                                                                
053800                                                                          
053900     PERFORM IMS-GNP-WDC212                                               
054000     IF SEGMENT-FINNS                                                     
054100       MOVE PRIB-KAM-KDARTKAM      TO MOD-KDARTKAM-ENTER                  
054200       MOVE PRIB-KAM-DASTADAT(3:6) TO MOD-TISTADAT-ENTER                  
054300       MOVE PRIB-KAM-KDARTKAM      TO MOD-KDARTKAM        (INDX)          
054400       MOVE PRIB-KAM-REARTRAB-DO   TO MOD-REARTRAB-DO     (INDX)          
054500       MOVE PRIB-KAM-REARTRAB-BULK TO MOD-REARTRAB-BULK   (INDX)          
054600       MOVE PRIB-KAM-DASTADAT(3:6) TO MOD-TISTADAT        (INDX)          
054700       MOVE PRIB-KAM-TISTODAT      TO MOD-TISTODAT        (INDX)          
054800     ELSE                                                                 
054900       MOVE ZERO               TO MOD-KDARTKAM-ENTER                      
055000       MOVE ZERO               TO MOD-TISTADAT-ENTER                      
055100       MOVE ERR-TRANSFER-MISSING TO MED-IDMFSFEL                          
055200       CALL WMEDKONV USING MED-WMEDAREA                                   
055300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
055400       PERFORM MFS-RENSA-FAELT-UT                                         
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 FC-HT-PROM-VIA-DISTRIKT SECTION.                                         
055900     SKIP2                                                                
056000     PERFORM IMS-GET-WLGMTA01                                             
056100     IF SEGMENT-FINNS                                                     
056200        MOVE GMTA-GMT-IDPARTNR    TO W-WDB1-IDPARTNR                      
056300        MOVE GMTA-GMT-IDFTG       TO W-WDB1-IDFTG                         
056400        PERFORM IMS-GHU-WDB101                                            
056500        IF SEGMENT-FINNS                                                  
056600           MOVE WDB1-BET-IDPROMR  TO WS-IDPROMR                           
056700           MOVE WS-IDPROMR        TO W-IDPROMR                            
056800        END-IF                                                            
056900     END-IF                                                               
057000     .                                                                    
057100     EJECT                                                                
057200                                                                          
057300 MFS-RENSA-FAELT-UT SECTION.                                              
057400                                                                          
057500*    --- ALLA UTDATA-FÄLT                                                 
057600*    --- INKL. BLÄDDRINGSNYCKLAR                                          
057700     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-SPAR                             
057800                             MOD-KDARTKAM-ENTER                           
057900                             MOD-KDARTKAM-NEXT                            
058000                             MOD-TISTADAT-ENTER                           
058100                             MOD-TISTADAT-NEXT                            
058200     MOVE +1 TO INDX                                                      
058300     PERFORM UNTIL INDX > MAX-INDX                                        
058400       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
058500       ADD +1 TO INDX                                                     
058600     END-PERFORM                                                          
058700     .                                                                    
058800     SKIP2                                                                
058900 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
059000                                                                          
059100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
059200     MOVE MFS-RENSA-FAELT TO MOD-KDARTKAM      (INDX)                     
059300                             MOD-REARTRAB-DO   (INDX)                     
059400                             MOD-REARTRAB-BULK (INDX)                     
059500                             MOD-TISTADAT      (INDX)                     
059600                             MOD-TISTODAT      (INDX)                     
059700     .                                                                    
059800     EJECT                                                                
059900* --- IMS SEKTIONER ---                                                   
060000     SKIP3                                                                
060100 IMS-GET-MSG SECTION.                                                     
060200                                                                          
060300     MOVE '  QC' TO GODK-STATUSKODER                                      
060400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
060500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     SKIP3                                                                
060900 IMS-INSERT-MSG SECTION.                                                  
061000                                                                          
061100     IF NOT ENGLISH-TEXT                                                  
061200       MOVE '0' TO MFS-KDHUVOMR                                           
061300     END-IF                                                               
061400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
061500     MOVE SPACE TO GODK-STATUSKODER                                       
061600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
061700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000     EJECT                                                                
062100*                                                                         
062200******** PRIS-RABATT REGISTER PRISOMRÅDESINFORMATION.                     
062300*                                                                         
062400 IMS-GU-WDC201 SECTION.                                                   
062500     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
062600          DELIMITED BY SIZE INTO SSA1                                     
062700     MOVE '  GE' TO GODK-STATUSKODER                                      
062800     CALL CBLTDLI USING GU PRIB-PCB DLI-IO-AREA SSA1                      
062900     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
063000     PERFORM IMS-STATUSKONTROLL                                           
063100     .                                                                    
063200     EJECT                                                                
063300*                                                                         
063400******** PRIS-RABATT REGISTER KAMPANJRABATTINFORMATION.                   
063500*                                                                         
063600 IMS-GNP-WDC212 SECTION.                                                  
063700     STRING 'WLPRIB12(WDC212KY>=' W-WDC212KY-MIN-X                        
063800                    '&WDC212KY<=' W-WDC212KY-MAX-X ')'                    
063900          DELIMITED BY SIZE INTO SSA1                                     
064000     MOVE '  GE' TO GODK-STATUSKODER                                      
064100     CALL CBLTDLI USING GNP PRIB-PCB DLI-IO-AREA SSA1                     
064200     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500     EJECT                                                                
064600 IMS-GET-WLGMTA01  SECTION.                                               
064700                                                                          
064800     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
064900                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
065000             DELIMITED BY SIZE INTO SSA1                                  
065100     MOVE '  GE' TO GODK-STATUSKODER                                      
065200     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA2 SSA1                     
065300     MOVE GMTA-STATUS-CODE  TO STATUS-WS                                  
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600     SKIP2                                                                
065700 IMS-GHU-WDB101 SECTION.                                                  
065800                                                                          
065900     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
066000          DELIMITED BY SIZE INTO SSA1                                     
066100     MOVE '  GE' TO GODK-STATUSKODER                                      
066200     CALL CBLTDLI USING GU  WDB1-PCB DLI-IO-AREA3 SSA1                    
066300     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
066400     PERFORM IMS-STATUSKONTROLL                                           
066500     .                                                                    
066600     SKIP3                                                                
066700 IMS-STATUSKONTROLL SECTION.                                              
066800                                                                          
066900     SET STATUS-IX TO 1                                                   
067000     SEARCH GODK-STATUS                                                   
067100       AT END                                                             
067200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
067300         DELIMITED BY SIZE INTO FELTEXT                                   
067400         CALL FELLOG                                                      
067500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
067600         CONTINUE                                                         
067700     END-SEARCH                                                           
067800     .                                                                    
