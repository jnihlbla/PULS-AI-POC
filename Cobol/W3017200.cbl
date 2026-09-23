000100 PROCESS                                                                  
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W3017200.                                                
000500*AUTHOR.         THOMAS LARSSON.                                          
000600*DATE-WRITTEN.   92/05/19.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        VISA VILKA OBJEKT SOM FINNS PÅ ETT RAPPORTNUMMER                 
001200*        SAMT GODKÄNNA OBJEKTEN ELLER JUSTERA PÅ BILDEN                   
001300*        SÅ ATT OBJEKTEN MOTSVARAR DET SOM FINNS I PALLEN.                
001400*        ETT RAPPORTNUMMER MOTSVARAR ETT KOLLI (PALL).                    
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLBYTF (WDM6)                              
001700*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001800*        PROGRAMMET LÄSER      WDK6                                       
001900*        PROGRAMMET LÄSER      WDK7                                       
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W3T172                                              
002300*        MID:         W3I17201                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W3O17201                                            
002700*                                                                         
002800*    CHANGE LOG:                                                          
002900*                                                                         
003000*      DIGAMBAR/20020715                                                  
003100*      PUT IDBYTREP-9KOMPL IN WDM611                                      
003200*                                                                         
003300*      ETRACKER 1072007 060815/EÖ                                         
003400*      ADD SCRAP COMMAND                                                  
003500*                                                                         
003600*      ETRACKER 2816396 061004/EÖ                                         
003700*      AUTOMATIC UPDATING OF NEW CORES TO DC21, ET                        
003800*                                                                         
003900*      ETRACKER 2045675 061113/EÖ                                         
004000*      ADD INFO IF LOCATION IS MISSING                                    
004100*                                                                         
004200*      PULS NDC WEB FOR AUSTRALIA.                                        
004300*      ETRACKER 101181787. OCT/NOV 2012  /KA                              
004400*      PROGRAM "WEBBFIED" AND SPLIT INTO A MAIN AND SUBPROGRAM            
004500*                                                                         
004600*      ETRACKER 10206694. FEB 2015       /RAHUL REDDY                     
004700*      PRINT CORE LABELS IN MAASTRICHT                                    
004800*                                                                         
004900*      ETRACKER 10251639  OCT 2015       /RAHUL REDDY                     
005000*      FIX ERROR MESSAGES                                                 
005100*                                                                         
005200*                                                                         
005300     SKIP3                                                                
005400 ENVIRONMENT DIVISION.                                                    
005500     EJECT                                                                
005600 DATA DIVISION.                                                           
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(08)   VALUE 'W3017200'.            
006100                                                                          
006200 77  WS-SECTION                  PIC X(20)   VALUE SPACE.                 
006300                                                                          
006400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006600                                                                          
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
007100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
007200 77  MAX-KVRADER                 PIC S9(4)  VALUE +9    COMP SYNC.        
007300                                                                          
007310 01  MESSAGE-CODES.                                                       
007320     03  ERROR-CODES.                                                     
007330         05  INF-FIRST-PAGE          PIC X(3)   VALUE '010'.              
007340         05  INF-LAST-PAGE           PIC X(3)   VALUE '012'.              
007350         05  INF-MORE-LINES          PIC X(3)   VALUE '011'.              
007400                                                                          
007410*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
007500*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 16                   
007600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +815  COMP SYNC.        
007700                                                                          
007800 01 ALL-PLUS.                                                             
007900    03 FILLER  PIC X(20) VALUE '++++++++++++++++++++'.                    
008000 01 ALL-SPACE.                                                            
008100    03 FILLER  PIC X(20) VALUE SPACE.                                     
008200                                                                          
008300 01 ERR-WRONG-KEY                PIC X(3)    VALUE '043'.                 
008400                                                                          
008500     EJECT                                                                
008600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008700                                                                          
008800 01  WS-SDC-91                   PIC X(2)    VALUE '91'.                  
008900                                                                          
009000 01  WS-SPAR-OBJNR               PIC S9(9)   VALUE ZERO COMP-3.           
009100 01  WS-IDBYTRAD                 PIC 9(5)    VALUE ZERO.                  
009200                                                                          
009300*    -- NUMERIC FORMAT INSTEAD OF NUMERIC-EDITED IN RESP-                 
009400 01  WS-RESP-KVRADER             PIC S9(5)   COMP-3.                      
009500                                                                          
009600 77  KOLL-SW                     PIC X       VALUE 'J'.                   
009700     88  INMATNING-OK                        VALUE 'J'.                   
009800     88  INMATNING-EJ-OK                     VALUE 'N'.                   
009900                                                                          
010000 01  BYT-SW                      PIC X       VALUE 'J'.                   
010100     88  BYT-BILD                            VALUE 'J'.                   
010200     88  BYT-EJ-BILD                         VALUE 'N'.                   
010300                                                                          
010400 01  NDC-BEHORIGHET-SW           PIC X       VALUE 'J'.                   
010500     88  BEHORIGHET-FINNS                    VALUE 'J'.                   
010600     88  BEHORIGHET-SAKNAS                   VALUE 'N'.                   
010700                                                                          
010800 01  STATUS-SW                   PIC X       VALUE 'J'.                   
010900     88  STATUS-OK                           VALUE 'J'.                   
011000     88  STATUS-EJ-OK                        VALUE 'N'.                   
011100                                                                          
011200 01  ALLT-SW                     PIC X       VALUE 'J'.                   
011300     88  ALLT-OK                             VALUE 'J'.                   
011400                                                                          
011500 01  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011600     88  EGEN-MID                            VALUE '3172'.                
011700     88  GODK-MID                            VALUE '3171' '3177'          
011800                                                   '3173'.                
011900     88  HELP-MID                            VALUE '0551'.                
012000                                                                          
012100     EJECT                                                                
012200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012300 01  GENERELLA-SUBPROGRAM.                                                
012400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012700     03  W3017210                PIC X(8)    VALUE 'W3017210'.            
012800     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
012900                                                                          
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013200*                                                                         
013300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013400*01 -COPY WMSGINIT                                                        
013500                                                                          
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL BIZ-LOGIC PROGRAM                                
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014000 01  REQU-AREA.                                                           
014100*    03 -COPY WZ01REQU                                                    
014200*    03 -COPY W30172I1                                                    
014300                                                                          
014400     EJECT                                                                
014500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
014600 01  RESP-AREA.                                                           
014700*    03 -COPY WZ01RESP                                                    
014800*    03 -COPY W30172O1                                                    
014900                                                                          
014910 01  SAVE-AREA.                                                           
014920     03  SAVE-IDTRANS           PIC X(4)    VALUE '3172'.                 
014930     03  SAVE-PGNO              PIC 9(2)    VALUE 01.                     
014950     03  SAVE-AREA-PREV OCCURS 80 TIMES.                                  
014970         05  SAVE-IDBYTRAD-PREV     PIC 9(5).                             
014993     03  SAVE-AREA-NEXT.                                                  
014994         05  SAVE-IDBYTRAD-NEXT     PIC 9(5).                             
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'MCNV-AREA'.           
015200*01  -COPY WL01MCNV                                                       
015300                                                                          
015400     EJECT                                                                
015500*    --- VALID IDDC CODES                                                 
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'IDDC CODES'.          
015800*01 -COPY WWDC99                                                          
015900*01 -COPY WWDCKONS                                                        
016000     EJECT                                                                
016100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016200*                                                                         
016300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016400     SKIP3                                                                
016500*01  MID -COPY W3I17201                                                   
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016800                                                                          
016900*01  -COPY WMSGAREA                                                       
017000                                                                          
017100     EJECT                                                                
017200     03  MOD REDEFINES MSG-AREA.                                          
017300*      05  -COPY W3O17201                                                 
017400                                                                          
017500     EJECT                                                                
017600 01  W-PROG-TO-PROG-SW-1.                                                 
017700     03  M-SW-LL-1               PIC S9(4)   VALUE +159 COMP SYNC.        
017800     03  M-SW-Z1-Z2-1            PIC X(2)    VALUE LOW-VALUE.             
017900     03  M-SW-KDTRANS-1          PIC X(8)    VALUE 'W3T173  '.            
018000     03  M-SW-IDTRANS-1          PIC X(4)    VALUE '3172'.                
018100     03  M-SW-KDMFSTYP-1         PIC X(1)    VALUE '1'.                   
018200                                                                          
018300     03 MID -COPY W3I17301 -PRE 3173-                                     
018400                                                                          
020200                                                                          
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020500                                                                          
020600*01  -COPY WMFSAREA                                                       
020700                                                                          
020800     EJECT                                                                
020900*                                                                         
021000 01  STATUS-WS                   PIC XX.                                  
021100     88  SEGMENT-FINNS                       VALUE '  '.                  
021200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021400     SKIP2                                                                
021500 01  GODK-STATUSKODER.                                                    
021600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021700     EJECT                                                                
021800*    --- IMS FUNKTIONSKODER                                               
021900*01  -COPY W0003                                                          
022000     EJECT                                                                
022100 LINKAGE SECTION.                                                         
022200                                                                          
022300*01  -COPY W0009   -PRE MSG-                                              
022400     EJECT                                                                
022500*01  -COPY W0009   -PRE ALT1-                                             
022600     EJECT                                                                
023100*01  -COPY W0009   -PRE DISTRDOC-                                         
023200     EJECT                                                                
023300*01  -COPY W0008   -PRE USEA-                                             
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023600*01  -COPY W0008  -PRE BYTF-                                              
023700     05  FILLER                  PIC X.                                   
023800     EJECT                                                                
023900*01  -COPY W0008  -PRE WDR2-                                              
024000     05  FILLER                  PIC X.                                   
024100     EJECT                                                                
024200*01  -COPY W0008  -PRE BENA-                                              
024300     05  FILLER                  PIC X.                                   
024400*01  -COPY W0008  -PRE WDK6-                                              
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE BYTF2-                                             
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000*01  -COPY W0008  -PRE WDK7-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300*01  -COPY W0008  -PRE WDB6-                                              
025400     05  FILLER                  PIC X.                                   
025500                                                                          
025600     EJECT                                                                
025700 PROCEDURE DIVISION  USING MSG-PCB                                        
025800                  ALT1-PCB DISTRDOC-PCB                                   
025900                  USEA-PCB BYTF-PCB  WDR2-PCB BENA-PCB                    
026000                  WDK6-PCB BYTF2-PCB WDK7-PCB WDB6-PCB.                   
026100     ENTRY 'DLITCBL' USING MSG-PCB                                        
026200                  ALT1-PCB DISTRDOC-PCB                                   
026300                  USEA-PCB BYTF-PCB  WDR2-PCB BENA-PCB                    
026400                  WDK6-PCB BYTF2-PCB WDK7-PCB WDB6-PCB.                   
026500                                                                          
026600     PERFORM IMS-GET-MSG                                                  
026700     IF SEGMENT-FINNS                                                     
026800       PERFORM A-INIT                                                     
026900       PERFORM B-INIT-KEYS                                                
027000       PERFORM Q-KOLL-OM-BYT-INMATAT                                      
027100       IF BYT-EJ-BILD                                                     
027200         IF MFS-UPDATE                                                    
027300           SET REQU-UPDATE TO TRUE                                        
027400           PERFORM MFS-FORM-ATTR                                          
027500         ELSE                                                             
027600           IF MFS-FIRST                                                   
027700             PERFORM C-FOERSTA-SIDA                                       
027800           ELSE                                                           
027900             IF MFS-NEXT                                                  
028000               PERFORM D-NAESTA-SIDA                                      
028010             ELSE                                                         
028020              IF MFS-PREVIOUS                                             
028040                PERFORM I-PREV-PAGE                                       
028100              ELSE                                                        
028200               PERFORM E-SAMMA-SIDA                                       
028300              END-IF                                                      
028310             END-IF                                                       
028400           END-IF                                                         
028500         END-IF                                                           
028600         IF ALLT-OK                                                       
028700           PERFORM F-CALL-BIZ-LOGIC                                       
028800           IF MFS-IDTRANS = '3173'                                        
028900*            -- JUMP FROM 3173. WHY ARE WE DOING THIS?                    
029000             PERFORM R-3173-FIX-SOME-FIELDS                               
029100           END-IF                                                         
029200         END-IF                                                           
029300                                                                          
029400         COMPUTE MSG-KVLL = LENGTH OF MOD-W3O17201 + 17                   
029500         PERFORM IMS-INSERT-MSG                                           
029600       END-IF                                                             
029700     END-IF                                                               
029800                                                                          
029900     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
030100     .                                                                    
030200                                                                          
030300     EJECT                                                                
030400 A-INIT SECTION.                                                          
030500                                                                          
030600     MOVE 'A-INIT '    TO WS-SECTION                                      
030700     IF MSG-DUBBLA-TRANSKODER                                             
030800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I17201                 
030900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
031000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
031100     ELSE                                                                 
031200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I17201                  
031300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031500     END-IF                                                               
031600                                                                          
031700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
032000                                                                          
032100     MOVE LOW-VALUE TO MSG-AREA                                           
032200     MOVE 'W3O172N1' TO MFS-IDMOD                                         
032300     MOVE '3172' TO MOD-IDTRANS                                           
032400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
032500                                                                          
032600     IF NOT (EGEN-MID OR HELP-MID)                                        
032700     OR MID-IDDISTR-IN NOT = ALL '+'                                      
032800     OR MID-IDBYTRAP-IN NOT = ALL '+'                                     
032900       MOVE '7'   TO MFS-IDPFK                                            
033000       MOVE SPACE TO MFS-KDTRTYP                                          
033100     END-IF                                                               
033200     .                                                                    
033300                                                                          
033400     EJECT                                                                
033500 B-INIT-KEYS    SECTION.                                                  
033600     MOVE 'B-INIT-KEYS'     TO WS-SECTION                                 
033700                                                                          
033800                                                                          
033900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034000     MOVE '001'             TO MSGI-KDCALL                                
034100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034200     MOVE '3172'            TO MSGI-IDTRANS                               
034300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
034400     IF EGEN-MID OR GODK-MID                                              
034500*      -- FROM 3172, 3171 OR 3177                                         
034600       MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                               
034700       MOVE MID-IDBYTRAP-IN TO MSGI-IDBYTRAP                              
034800     END-IF                                                               
034900     IF EGEN-MID                                                          
035000*      -- FROM 3172                                                       
035100       MOVE MID-KDPRT       TO MSGI-KDPRT                                 
035200     END-IF                                                               
035300                                                                          
035400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035410                                                                          
035420     MOVE MSGI-SPAR-AREA  TO SAVE-AREA                                    
035500                                                                          
035600     IF MSGI-IDUSER = 'MWGB112 '                                          
035700     OR MSGI-IDUSER = 'MWGB216 '                                          
035800     OR MSGI-IDUSER = 'MWGB219 '                                          
036100        MOVE WC-SDC-NL-ET TO MSGI-IDDC                                    
036200     END-IF                                                               
036300                                                                          
036400     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
036500                                    REQU-IDSPRAK                          
036600                                                                          
036700     MOVE MSGI-KDPRT             TO REQU-KDPRT                            
036800                                                                          
036900*    -- NO DC IN MID, TAKE IT FROM PROFILE                                
037000     MOVE MSGI-IDDC     TO REQU-IDDC-KEY                                  
037100                                                                          
037200*    -- INIT IDDISTR --                                                   
037300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
037400     MOVE MSGI-IDDISTR    TO REQU-IDDISTR-KEY,                            
037500                             MOD-IDDISTR-UT                               
037600     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
037700                                                                          
037800*    -- INIT IDBYTRAP --                                                  
037900     MOVE MFS-RENSA-FAELT TO MOD-IDBYTRAP-IN                              
038000*    INSPECT MSGI-IDBYTRAP REPLACING LEADING SPACE BY ZERO                
038100     MOVE MSGI-IDBYTRAP   TO REQU-IDBYTRAP-KEY,                           
038200                             MOD-IDBYTRAP-UT                              
038300     INSPECT MOD-IDBYTRAP-UT REPLACING LEADING ZERO BY SPACE              
038400                                                                          
038500     IF NOT (EGEN-MID OR GODK-MID)                                        
038600*      -- NOT FROM 3172, 3171 OR 3177                                     
038700       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
038800                               MOD-IDBYTRAP-UT                            
038900     END-IF                                                               
039000     .                                                                    
039100                                                                          
039200     EJECT                                                                
039300 C-FOERSTA-SIDA SECTION.                                                  
039400     MOVE 'C-FOERSTA-SIDA'   TO WS-SECTION                                
039500                                                                          
039600     SET REQU-FIRST          TO TRUE                                      
039710     MOVE ZERO               TO REQU-IDBYTRAD-START                       
039800                                                                          
039910     PERFORM MFS-RENSA-FAELT-IN                                           
040000     .                                                                    
040100     EJECT                                                                
040200 D-NAESTA-SIDA SECTION.                                                   
040300     MOVE 'D-NAESTA-SIDA'    TO WS-SECTION                                
040400                                                                          
040500     SET REQU-NEXT           TO TRUE                                      
040810     IF SAVE-IDTRANS = '3172'                                             
040820       IF (SAVE-PGNO < 80) AND (SAVE-PGNO >= 1)                           
040830         MOVE SAVE-IDBYTRAD-NEXT    TO REQU-IDBYTRAD-START                
040880       ELSE                                                               
040890         IF SAVE-PGNO >= 80                                               
040895           MOVE SAVE-IDBYTRAD-PREV (80) TO REQU-IDBYTRAD-START            
040896         END-IF                                                           
040897       END-IF                                                             
040898     END-IF                                                               
040899     PERFORM MFS-RENSA-FAELT-IN                                           
040900     .                                                                    
041000                                                                          
041100     EJECT                                                                
041200 E-SAMMA-SIDA SECTION.                                                    
041300     MOVE 'E-SAMMA-SIDA'     TO WS-SECTION                                
041400                                                                          
041600     IF EGEN-MID OR HELP-MID                                              
041700       SET REQU-QUERY        TO TRUE                                      
041800     ELSE                                                                 
041900       SET REQU-FIRST        TO TRUE                                      
042000     END-IF                                                               
042100                                                                          
042110     IF SAVE-IDTRANS = '3172'                                             
042120      IF SAVE-PGNO < 1                                                    
042130         MOVE 1   TO SAVE-PGNO                                            
042140      ELSE                                                                
042150        IF SAVE-PGNO >= 80                                                
042160          MOVE 80 TO SAVE-PGNO                                            
042170        END-IF                                                            
042180      END-IF                                                              
042190      MOVE SAVE-IDBYTRAD-PREV (SAVE-PGNO) TO REQU-IDBYTRAD-START          
042195     ELSE                                                                 
042196      MOVE 1      TO SAVE-PGNO                                            
042197     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400                                                                          
042410 I-PREV-PAGE SECTION.                                                     
042420                                                                          
042430     MOVE 'I-PREV-PAGE' TO WS-SECTION                                     
042431     SET REQU-PREVIOUS       TO TRUE                                      
042490     IF SAVE-IDTRANS = '3172'                                             
042491      IF SAVE-PGNO > 1 AND SAVE-PGNO <= 80                                
042493       MOVE SAVE-IDBYTRAD-PREV (SAVE-PGNO - 1)                            
042500                                            TO REQU-IDBYTRAD-START        
042501      ELSE                                                                
042502       IF SAVE-PGNO <= 1                                                  
042503        MOVE 1 TO SAVE-PGNO                                               
042505        MOVE SAVE-IDBYTRAD-PREV (SAVE-PGNO) TO REQU-IDBYTRAD-START        
042509       END-IF                                                             
042510      END-IF                                                              
042511     END-IF                                                               
042512     PERFORM MFS-RENSA-FAELT-IN                                           
042513     .                                                                    
042514     EJECT                                                                
042520 F-CALL-BIZ-LOGIC SECTION.                                                
042600     MOVE 'F-CALL-BIZ-LOGIC'   TO WS-SECTION                              
042700                                                                          
042800     PERFORM FA-INIT-REQU                                                 
042810     CALL W3017210 USING REQU-AREA RESP-AREA MAX-KVRADER                  
042820                         MSG-PCB  DISTRDOC-PCB                            
042830                         USEA-PCB BYTF-PCB  WDR2-PCB BENA-PCB             
042840                         WDK6-PCB BYTF2-PCB WDK7-PCB WDB6-PCB.            
042900                                                                          
043000     IF MFS-FIRST                                                         
043100       MOVE 1 TO SAVE-PGNO                                                
043200       PERFORM FD-MOVE-KEYS-TO-PROFILE-DB                                 
043300       IF RESP-IDMSG-ERROR = SPACES                                       
043310          MOVE INF-FIRST-PAGE  TO RESP-IDMSG-ERROR                        
043320       END-IF                                                             
043330     END-IF                                                               
043340     IF MFS-NEXT                                                          
043350       IF (SAVE-IDBYTRAD-PREV(SAVE-PGNO) = RESP-IDBYTRAD-START)           
043352             MOVE INF-LAST-PAGE  TO RESP-IDMSG-ERROR                      
043370       ELSE                                                               
043380        IF SAVE-PGNO < 80                                                 
043390          COMPUTE SAVE-PGNO = SAVE-PGNO + 1                               
043392          PERFORM FD-MOVE-KEYS-TO-PROFILE-DB                              
043393        ELSE                                                              
043394         IF (SAVE-PGNO = 80 AND                                           
043395             RESP-IDMSG-INFO NOT = INF-LAST-PAGE)                         
043400             MOVE 80 TO SAVE-PGNO                                         
043402             MOVE INF-MORE-LINES    TO RESP-IDMSG-INFO                    
043403         END-IF                                                           
043404        END-IF                                                            
043405       END-IF                                                             
043406     END-IF                                                               
043407     IF MFS-PREVIOUS                                                      
043409       IF SAVE-PGNO = 1 AND RESP-IDMSG-ERROR = SPACES                     
043411          MOVE INF-FIRST-PAGE  TO RESP-IDMSG-ERROR                        
043412       ELSE                                                               
043413        IF SAVE-PGNO > 1 AND SAVE-PGNO <= 80                              
043415          COMPUTE SAVE-PGNO = SAVE-PGNO - 1                               
043417          MOVE RESP-IDBYTRAD-NEXT  TO SAVE-IDBYTRAD-NEXT                  
043418        END-IF                                                            
043419       END-IF                                                             
043420     END-IF                                                               
043421     IF MFS-ENTER                                                         
043430          PERFORM FD-MOVE-KEYS-TO-PROFILE-DB                              
043440      END-IF                                                              
043500      IF RESP-IDMSG-ERROR NOT = SPACE OR                                  
043510         RESP-IDMSG-INFO  NOT = SPACE                                     
043520        PERFORM FB-SET-MSG-AND-HILIGHT                                    
043600      END-IF                                                              
043710        PERFORM FC-MOVE-RESP-TO-MOD                                       
043810       MOVE '002'               TO MSGI-KDCALL                            
043820       MOVE '3172'              TO MSGI-IDTRANS                           
043830       MOVE '3172'              TO SAVE-IDTRANS                           
043840       MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                      
043850       MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                            
043860       MOVE SAVE-AREA           TO MSGI-SPAR-AREA                         
043870       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
043900     .                                                                    
044000     EJECT                                                                
044100                                                                          
044200 FA-INIT-REQU SECTION.                                                    
044300                                                                          
044400     MOVE '101'             TO REQU-IDMSGVER                              
044500     MOVE MSGI-IDUSER       TO REQU-IDUSER                                
044600     MOVE MAX-KVRADER       TO REQU-KVRADER                               
044700*    -- SUBPROGRAM NEEDS TO KNOW WHERE WE COME FROM                       
044800     MOVE W-IDTRANS         TO REQU-IDTRANS-FROM                          
044960                                                                          
045000     MOVE MID-FLAGGA        TO REQU-FLAGGA                                
045100     MOVE MID-IDKUNDNR      TO REQU-IDKUNDNR                              
045200     MOVE MID-IDFAKT        TO REQU-IDFAKT                                
045300     MOVE MID-OBJNR-SPAERR  TO REQU-IDARTNR-OBJ-SPAERR                    
045400     MOVE MID-IDBYTRAD-3173 TO REQU-IDBYTRAD-3173                         
045500                                                                          
045600*    -- MID FIELD IS 9(3) REQU FIELD IS 9(5)                              
045700     IF MID-ANTAL-IN NUMERIC                                              
045800       MOVE MID-ANTAL-IN      TO REQU-KVRETUR-IN                          
045900     ELSE                                                                 
046000       IF MID-ANTAL-IN = ALL '+'                                          
046100         MOVE ALL-PLUS        TO REQU-KVRETUR-IN                          
046200       ELSE                                                               
046300         IF MID-ANTAL-IN = ALL-SPACE                                      
046400           MOVE ALL-SPACE     TO REQU-KVRETUR-IN                          
046500         ELSE                                                             
046600*          -- WELL, IT'S WRONG ANYWAY                                     
046700           MOVE MID-ANTAL-IN    TO REQU-KVRETUR-IN                        
046800         END-IF                                                           
046900       END-IF                                                             
047000     END-IF                                                               
047100                                                                          
047200     MOVE MID-ANMARK-IN     TO REQU-KDBYTREF-IN                           
047300     MOVE MID-FLSKROT-IN    TO REQU-FLSKROT-IN                            
047310     MOVE MID-GODK-IN       TO REQU-FLGODK-IN                             
047400                                                                          
047500     MOVE 1 TO INDX                                                       
047600     PERFORM UNTIL INDX > MAX-KVRADER                                     
047700       MOVE MID-SELECT-URVAL(INDX)  TO REQU-KDCMD-LINE(INDX)              
047800       MOVE MID-OBJNR(INDX)         TO REQU-IDARTNR-OBJ-LINE(INDX)        
047900       MOVE MID-KVPOINT(INDX)       TO REQU-KVPOINT-LINE(INDX)            
048000       MOVE MID-IDTABNR(INDX)       TO REQU-IDTABNR-LINE(INDX)            
048100                                                                          
048200*      -- MID FIELD IS 9(3) REQU FIELD IS 9(5)                            
048300       IF MID-KVRETUR-GODK(INDX)     NUMERIC                              
048400         MOVE MID-KVRETUR-GODK(INDX)                                      
048500                             TO REQU-KVRETUR-GODK-LINE(INDX)              
048600       ELSE                                                               
048700         IF MID-KVRETUR-GODK(INDX)   = ALL '+'                            
048800           MOVE ALL-PLUS     TO REQU-KVRETUR-GODK-LINE(INDX)              
048900         ELSE                                                             
049000           IF MID-KVRETUR-GODK(INDX) = ALL-SPACE                          
049100             MOVE ALL-SPACE  TO REQU-KVRETUR-GODK-LINE(INDX)              
049200           ELSE                                                           
049300*            -- WELL, IT'S WRONG ANYWAY                                   
049400             MOVE MID-KVRETUR-GODK(INDX)                                  
049500                             TO REQU-KVRETUR-GODK-LINE(INDX)              
049600           END-IF                                                         
049700         END-IF                                                           
049800       END-IF                                                             
049900                                                                          
050010       MOVE MID-KDBYTSTA-IN-UT-RAD(INDX)                                  
050100                                    TO REQU-KDBYTSTA-LINE(INDX)           
050200       MOVE MID-ANMARK-IN-UT(INDX)  TO REQU-KDBYTREF-LINE(INDX)           
050300       MOVE MID-FLSKROT-IN-UT(INDX) TO REQU-FLSKROT-LINE(INDX)            
050400       MOVE MID-IDBYTRAD(INDX)      TO REQU-IDBYTRAD-LINE(INDX)           
050500                                                                          
050600       ADD 1 TO INDX                                                      
050700     END-PERFORM                                                          
050800     .                                                                    
050900     EJECT                                                                
050901                                                                          
051000                                                                          
051100 FB-SET-MSG-AND-HILIGHT SECTION.                                          
051200     MOVE 'FB-SET-MSG-AND-HILIGHT' TO WS-SECTION                          
051300                                                                          
051400     MOVE RESP-IDMSG-ERROR TO MCNV-IDMSG-ERROR                            
051500     MOVE RESP-IDELMT-ERROR TO MCNV-IDELMT-ERROR                          
051600     MOVE RESP-IDMSG-INFO  TO MCNV-IDMSG-INFO                             
051700     MOVE MSGI-IDSPRAK     TO MCNV-IDSPRAK                                
051800                                                                          
051900     CALL WL01MCNV USING MCNV-AREA                                        
052000     MOVE MCNV-MFSINF      TO MOD-TEMFSINF                                
052100     MOVE MCNV-MFSFEL      TO MOD-TEMFSFEL                                
052200                                                                          
052300*    -- MEDDELANDE OM GAMMAL VIPS-ANVÄNDARE                               
052400     MOVE MFS-RENSA-FAELT  TO MOD-TEXTRAD                                 
052500     IF RESP-IDMSG-VIPS NOT = SPACE                                       
052600       MOVE SPACE            TO MCNV-IDMSG-ERROR                          
052700       MOVE SPACE            TO MCNV-IDELMT-ERROR                         
052800       MOVE RESP-IDMSG-VIPS  TO MCNV-IDMSG-INFO                           
052900                                                                          
053000       CALL WL01MCNV USING MCNV-AREA                                      
053100       MOVE MCNV-TEMFSINF    TO MOD-TEXTRAD                               
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500                                                                          
053600 FC-MOVE-RESP-TO-MOD SECTION.                                             
053700     MOVE 'FC-MOVE-RESP-TO-MOD' TO WS-SECTION                             
053800                                                                          
053900*    -- IBLAND OM DET BLIR FEL                                            
054000*    -- ENTER OCH SAMTIDIGT RAD-INMATNING?                                
054100*    PERFORM MFS-LAES-IN-IGEN ???                                         
054200                                                                          
054300     MOVE RESP-KDPRT-ATTR            TO MOD-KDPRT-ATTR                    
054400     IF RESP-KDPRT = SPACE                                                
054500       MOVE MFS-ERASE-FIELD          TO MOD-KDPRT                         
054600     ELSE                                                                 
054700       IF RESP-KDPRT = ALL '+'                                            
054800         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRT                         
054900       ELSE                                                               
055000         MOVE RESP-KDPRT             TO MOD-KDPRT                         
055100       END-IF                                                             
055200     END-IF                                                               
055300                                                                          
057300                                                                          
057400     IF RESP-FLAGGA              = SPACE                                  
057500       MOVE SPACE           TO MOD-FLAGGA                                 
057600     ELSE                                                                 
057700       IF RESP-FLAGGA            = ALL '+'                                
057800         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA                        
057900      ELSE                                                                
058000         MOVE RESP-FLAGGA            TO MOD-FLAGGA                        
058100      END-IF                                                              
058200     END-IF                                                               
058300                                                                          
058400     IF RESP-IDKUNDNR            = SPACE                                  
058500       MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR                               
058600     ELSE                                                                 
058700       IF RESP-IDKUNDNR          = ALL '+'                                
058800         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDKUNDNR                      
058900      ELSE                                                                
059000         MOVE RESP-IDKUNDNR          TO MOD-IDKUNDNR                      
059100      END-IF                                                              
059200     END-IF                                                               
059300                                                                          
059400     IF RESP-IDFAKT              = SPACE                                  
059500       MOVE MFS-ERASE-FIELD TO MOD-IDFAKTNR                               
059600     ELSE                                                                 
059700       IF RESP-IDFAKT            = ALL '+'                                
059800         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDFAKTNR                      
059900      ELSE                                                                
060000         MOVE RESP-IDFAKT            TO MOD-IDFAKTNR                      
060100      END-IF                                                              
060200     END-IF                                                               
060300                                                                          
060400     IF RESP-KDBYTSTA-RAPP     = ALL '+'                                  
060500       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDBYTSTA-RAPP                   
060600     ELSE                                                                 
060700       MOVE RESP-KDBYTSTA-RAPP     TO MOD-KDBYTSTA-RAPP                   
060800     END-IF                                                               
060900                                                                          
061000     IF RESP-IDUSER-GODK       = ALL '+'                                  
061100       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDUSER-GODK                     
061200     ELSE                                                                 
061300       MOVE RESP-IDUSER-GODK       TO MOD-IDUSER-GODK                     
061400     END-IF                                                               
061500                                                                          
061600     IF RESP-KVRETUR-TOTU        = SPACE                                  
061700       MOVE MFS-ERASE-FIELD TO MOD-KVRETUR-TOTU                           
061800     ELSE                                                                 
061900       IF RESP-KVRETUR-TOTU      = ALL '+'                                
062000         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVRETUR-TOTU                  
062100      ELSE                                                                
062200         MOVE RESP-KVRETUR-TOTU      TO MOD-KVRETUR-TOTU                  
062300       END-IF                                                             
062400     END-IF                                                               
062500                                                                          
062600     IF RESP-KVRETUR-TOTG        = SPACE                                  
062700       MOVE MFS-ERASE-FIELD TO MOD-KVRETUR-TOTG                           
062800     ELSE                                                                 
062900       IF RESP-KVRETUR-TOTG      = ALL '+'                                
063000         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVRETUR-TOTG                  
063100      ELSE                                                                
063200         MOVE RESP-KVRETUR-TOTG      TO MOD-KVRETUR-TOTG                  
063300      END-IF                                                              
063400     END-IF                                                               
063500                                                                          
063600*    -- IDMSG-VIPS IS PROCESSED IN SECTION FB-                            
063700                                                                          
063800     MOVE RESP-IDARTNR-OBJ-SPAERR-ATTR TO MOD-OBJNR-SPAERR-ATTR           
063900                                                                          
064000     IF RESP-IDARTNR-OBJ-SPAERR  = SPACE                                  
064100       MOVE MFS-ERASE-FIELD TO MOD-OBJNR-SPAERR                           
064200     ELSE                                                                 
064300       IF RESP-IDARTNR-OBJ-SPAERR = ALL '+'                               
064400         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-OBJNR-SPAERR                  
064500      ELSE                                                                
064600         IF WS-IDBYTRAD = ZERO                                            
064700            MOVE MFS-ERASE-FIELD TO MOD-OBJNR-SPAERR                      
064800         ELSE                                                             
064900            MOVE RESP-IDARTNR-OBJ-SPAERR TO MOD-OBJNR-SPAERR              
065000         END-IF                                                           
065100      END-IF                                                              
065200     END-IF                                                               
065300                                                                          
065400     IF RESP-IDBYTRAD-3173       = SPACE                                  
065500       MOVE MFS-ERASE-FIELD TO MOD-IDBYTRAD-3173                          
065600     ELSE                                                                 
065700       IF RESP-IDBYTRAD-3173     = ALL '+'                                
065800         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDBYTRAD-3173                 
065900      ELSE                                                                
066000         MOVE RESP-IDBYTRAD-3173     TO MOD-IDBYTRAD-3173                 
066100      END-IF                                                              
066200     END-IF                                                               
066300                                                                          
066400     MOVE RESP-KVRETUR-IN-ATTR TO MOD-ANTAL-IN-ATTR                       
066500                                                                          
066600     IF RESP-KVRETUR-IN          = SPACE                                  
066700       MOVE MFS-ERASE-FIELD TO MOD-ANTAL-IN                               
066800     ELSE                                                                 
066900       IF RESP-KVRETUR-IN        = ALL '+'                                
067000         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ANTAL-IN                      
067100      ELSE                                                                
067200         MOVE RESP-KVRETUR-IN        TO MOD-ANTAL-IN                      
067300      END-IF                                                              
067400     END-IF                                                               
067500                                                                          
067600     MOVE RESP-KDBYTREF-IN-ATTR TO MOD-ANMARK-IN-ATTR                     
067700                                                                          
067800     IF RESP-KDBYTREF-IN         = SPACE                                  
067900       MOVE MFS-ERASE-FIELD TO MOD-ANMARK-IN                              
068000     ELSE                                                                 
068100       IF RESP-KDBYTREF-IN       = ALL '+'                                
068200         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ANMARK-IN                     
068300      ELSE                                                                
068400         MOVE RESP-KDBYTREF-IN       TO MOD-ANMARK-IN                     
068500      END-IF                                                              
068600     END-IF                                                               
068700                                                                          
068800     MOVE RESP-FLSKROT-IN-ATTR TO MOD-FLSKROT-IN-ATTR                     
068900                                                                          
069000     IF RESP-FLSKROT-IN         = SPACE                                   
069100       MOVE MFS-ERASE-FIELD          TO MOD-FLSKROT-IN                    
069200     ELSE                                                                 
069300       IF RESP-FLSKROT-IN         = ALL '+'                               
069400         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLSKROT-IN                    
069500       ELSE                                                               
069600         MOVE RESP-FLSKROT-IN         TO MOD-FLSKROT-IN                   
069700       END-IF                                                             
069800     END-IF                                                               
069810                                                                          
069820     MOVE RESP-FLGODK-IN-ATTR TO MOD-GODK-IN-ATTR                         
069830                                                                          
069840     IF RESP-FLGODK-IN         = SPACE                                    
069850       MOVE MFS-ERASE-FIELD          TO MOD-GODK-IN                       
069860     ELSE                                                                 
069870       IF RESP-FLGODK-IN         = ALL '+'                                
069880         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-GODK-IN                       
069890       ELSE                                                               
069891         MOVE RESP-FLGODK-IN         TO MOD-GODK-IN                       
069892       END-IF                                                             
069893     END-IF                                                               
069900                                                                          
070000     MOVE +1 TO INDX                                                      
070100     MOVE RESP-KVRADER TO WS-RESP-KVRADER                                 
070200     PERFORM UNTIL INDX > WS-RESP-KVRADER                                 
070300                                                                          
070400       MOVE RESP-KDCMD-LINE-ATTR(INDX)  TO                                
070500            MOD-SELECT-URVAL-ATTR(INDX)                                   
070600                                                                          
070700       IF RESP-KDCMD-LINE(INDX) = SPACE                                   
070800         MOVE MFS-ERASE-FIELD          TO MOD-SELECT-URVAL(INDX)          
070900       ELSE                                                               
071000         IF RESP-KDCMD-LINE(INDX)  = ALL '+'                              
071100           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-SELECT-URVAL(INDX)          
071200         ELSE                                                             
071300           MOVE RESP-KDCMD-LINE(INDX)  TO MOD-SELECT-URVAL(INDX)          
071400         END-IF                                                           
071500       END-IF                                                             
071600                                                                          
071700       IF RESP-IDARTNR-OBJ-LINE(INDX) = SPACE                             
071800         MOVE MFS-ERASE-FIELD          TO MOD-IDARTNR-OBJ(INDX)           
071900       ELSE                                                               
072000         IF RESP-IDARTNR-OBJ-LINE(INDX)  = ALL '+'                        
072100           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-OBJ(INDX)           
072200        ELSE                                                              
072300           MOVE RESP-IDARTNR-OBJ-LINE(INDX)                               
072400                                       TO MOD-IDARTNR-OBJ(INDX)           
072500        END-IF                                                            
072600       END-IF                                                             
072700                                                                          
072800       IF RESP-KVPOINT-LINE(INDX)  = SPACE                                
072900         MOVE MFS-ERASE-FIELD           TO MOD-KVPOINT(INDX)              
073000       ELSE                                                               
073100         IF RESP-KVPOINT-LINE(INDX)= ALL '+'                              
073200           MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVPOINT(INDX)              
073300        ELSE                                                              
073400           MOVE RESP-KVPOINT-LINE(INDX) TO MOD-KVPOINT(INDX)              
073500        END-IF                                                            
073600       END-IF                                                             
073700                                                                          
073800       IF RESP-IDTABNR-LINE(INDX)  = SPACE                                
073900         MOVE MFS-ERASE-FIELD           TO MOD-IDTABNR(INDX)              
074000       ELSE                                                               
074100         IF RESP-IDTABNR-LINE(INDX)= ALL '+'                              
074200           MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-IDTABNR(INDX)              
074300        ELSE                                                              
074400           MOVE RESP-IDTABNR-LINE(INDX) TO MOD-IDTABNR(INDX)              
074500        END-IF                                                            
074600       END-IF                                                             
074700                                                                          
074800       IF RESP-KVRETUR-URSP-LINE(INDX) = SPACE                            
074900         MOVE MFS-ERASE-FIELD          TO MOD-KVRETUR-URSP(INDX)          
075000       ELSE                                                               
075100         IF RESP-KVRETUR-URSP-LINE(INDX)  = ALL '+'                       
075200           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVRETUR-URSP(INDX)          
075300        ELSE                                                              
075400           MOVE RESP-KVRETUR-URSP-LINE(INDX)                              
075500                                       TO MOD-KVRETUR-URSP(INDX)          
075600        END-IF                                                            
075700       END-IF                                                             
075800                                                                          
075900       MOVE RESP-KVRETUR-GODK-LINE-ATTR(INDX)  TO                         
076000            MOD-KVRETUR-GODK-ATTR(INDX)                                   
076100                                                                          
076200       IF RESP-KVRETUR-GODK-LINE(INDX) = SPACE                            
076300         MOVE MFS-ERASE-FIELD          TO MOD-KVRETUR-GODK(INDX)          
076400       ELSE                                                               
076500         IF RESP-KVRETUR-GODK-LINE(INDX) = ALL '+'                        
076600           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVRETUR-GODK(INDX)          
076700        ELSE                                                              
076800           MOVE RESP-KVRETUR-GODK-LINE(INDX)                              
076900                                       TO MOD-KVRETUR-GODK(INDX)          
077000        END-IF                                                            
077100       END-IF                                                             
077200                                                                          
077300       IF RESP-KDBYTSTA-LINE(INDX) = SPACE                                
077400         MOVE MFS-ERASE-FIELD          TO MOD-KDBYTSTA-RAD(INDX)          
077500       ELSE                                                               
077600         IF RESP-KDBYTSTA-LINE(INDX) = ALL '+'                            
077700           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDBYTSTA-RAD(INDX)          
077800        ELSE                                                              
077900           MOVE RESP-KDBYTSTA-LINE(INDX)                                  
078000                                       TO MOD-KDBYTSTA-RAD(INDX)          
078100        END-IF                                                            
078200       END-IF                                                             
078300                                                                          
078400       MOVE RESP-KDBYTREF-LINE-ATTR(INDX)  TO                             
078500            MOD-ANMARK-IN-UT-ATTR(INDX)                                   
078600                                                                          
078700       IF RESP-KDBYTREF-LINE(INDX) = SPACE                                
078800         MOVE MFS-ERASE-FIELD          TO MOD-ANMARK-IN-UT(INDX)          
078900       ELSE                                                               
079000         IF RESP-KDBYTREF-LINE(INDX) = ALL '+'                            
079100           MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-ANMARK-IN-UT(INDX)        
079200         ELSE                                                             
079300           MOVE RESP-KDBYTREF-LINE(INDX) TO MOD-ANMARK-IN-UT(INDX)        
079400         END-IF                                                           
079500       END-IF                                                             
079600                                                                          
079700       IF RESP-FLSKROT-LINE (INDX) = SPACE                                
079800         MOVE MFS-ERASE-FIELD         TO MOD-FLSKROT-IN-UT(INDX)          
079900       ELSE                                                               
080000         IF RESP-FLSKROT-LINE (INDX) = ALL '+'                            
080100           MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-FLSKROT-IN-UT(INDX)        
080200         ELSE                                                             
080300           MOVE RESP-FLSKROT-LINE(INDX) TO MOD-FLSKROT-IN-UT(INDX)        
080400         END-IF                                                           
080500       END-IF                                                             
080600                                                                          
080700       IF RESP-BEART-LINE   (INDX) = SPACE                                
080800         MOVE MFS-ERASE-FIELD          TO MOD-BEART(INDX)                 
080900       ELSE                                                               
081000         IF RESP-BEART-LINE   (INDX) = ALL '+'                            
081100           MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BEART(INDX)                 
081200         ELSE                                                             
081300           MOVE RESP-BEART-LINE (INDX) TO MOD-BEART(INDX)                 
081400         END-IF                                                           
081500       END-IF                                                             
081600                                                                          
081700       IF RESP-IDBYTRAD-LINE(INDX) = SPACE                                
081800         MOVE MFS-ERASE-FIELD            TO MOD-IDBYTRAD(INDX)            
081900       ELSE                                                               
082000         IF RESP-IDBYTRAD-LINE(INDX) = ALL '+'                            
082100           MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDBYTRAD(INDX)            
082200        ELSE                                                              
082300           MOVE RESP-IDBYTRAD-LINE(INDX) TO MOD-IDBYTRAD(INDX)            
082400        END-IF                                                            
082500       END-IF                                                             
082600                                                                          
082700       IF RESP-BERADREF-LINE(INDX) = SPACE                                
082800         MOVE MFS-ERASE-FIELD            TO MOD-BERADREF(INDX)            
082900       ELSE                                                               
083000         IF RESP-BERADREF-LINE(INDX) = ALL '+'                            
083100           MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-BERADREF(INDX)            
083200         ELSE                                                             
083300           MOVE RESP-BERADREF-LINE(INDX) TO MOD-BERADREF(INDX)            
083400         END-IF                                                           
083500       END-IF                                                             
083600                                                                          
083700       IF RESP-FLAGGA-LOC-LINE(INDX) = SPACE                              
083800         MOVE MFS-ERASE-FIELD              TO MOD-FLAGGA-LOC(INDX)        
083900       ELSE                                                               
084000         IF RESP-FLAGGA-LOC-LINE(INDX) = ALL '+'                          
084100           MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLAGGA-LOC(INDX)        
084200         ELSE                                                             
084300           MOVE RESP-FLAGGA-LOC-LINE(INDX) TO MOD-FLAGGA-LOC(INDX)        
084400         END-IF                                                           
084500       END-IF                                                             
084600                                                                          
084700       ADD 1 TO INDX                                                      
084800     END-PERFORM                                                          
084900                                                                          
085000*    -- CLEAR ANY REMAINING LINES                                         
085100     PERFORM UNTIL INDX > MAX-KVRADER                                     
085200       MOVE MFS-CLOSE-FIELD    TO MOD-SELECT-URVAL-ATTR(INDX)             
085300       MOVE MFS-ERASE-FIELD    TO MOD-SELECT-URVAL(INDX)                  
085400       MOVE MFS-ERASE-FIELD    TO MOD-IDARTNR-OBJ(INDX)                   
085500       MOVE MFS-ERASE-FIELD    TO MOD-KVPOINT(INDX)                       
085600       MOVE MFS-ERASE-FIELD    TO MOD-IDTABNR(INDX)                       
085700       MOVE MFS-ERASE-FIELD    TO MOD-KVRETUR-URSP(INDX)                  
085800       MOVE MFS-CLOSE-FIELD    TO MOD-KVRETUR-GODK-ATTR(INDX)             
085900       MOVE MFS-ERASE-FIELD    TO MOD-KVRETUR-GODK(INDX)                  
086000       MOVE MFS-ERASE-FIELD    TO MOD-KDBYTSTA-RAD(INDX)                  
086100       MOVE MFS-CLOSE-FIELD    TO MOD-ANMARK-IN-UT-ATTR(INDX)             
086200       MOVE MFS-ERASE-FIELD    TO MOD-ANMARK-IN-UT(INDX)                  
086300       MOVE MFS-ERASE-FIELD    TO MOD-FLSKROT-IN-UT(INDX)                 
086400       MOVE MFS-ERASE-FIELD    TO MOD-BEART(INDX)                         
086500       MOVE MFS-ERASE-FIELD    TO MOD-IDBYTRAD(INDX)                      
086600       MOVE MFS-ERASE-FIELD    TO MOD-BERADREF(INDX)                      
086700                                                                          
086800       ADD 1 TO INDX                                                      
086900     END-PERFORM                                                          
087000     .                                                                    
087010 FD-MOVE-KEYS-TO-PROFILE-DB SECTION.                                      
087020     MOVE RESP-IDBYTRAD-START TO SAVE-IDBYTRAD-PREV(SAVE-PGNO)            
087030     MOVE RESP-IDBYTRAD-NEXT  TO SAVE-IDBYTRAD-NEXT                       
087100     .                                                                    
087200     EJECT                                                                
087300 Q-KOLL-OM-BYT-INMATAT SECTION.                                           
087400     MOVE 'Q-KOLL-OM-BYT-INMATAT' TO WS-SECTION                           
087500                                                                          
087600     MOVE NEJ TO BYT-SW                                                   
087700     MOVE SPACE TO NDC-BEHORIGHET-SW                                      
087800     IF EGEN-MID                                                          
087900       MOVE +1 TO INDX                                                    
088000       PERFORM UNTIL INDX > MAX-KVRADER                                   
088100         IF MID-SELECT-URVAL (INDX) NOT = '+'                             
088200         AND MID-SELECT-URVAL (INDX) NOT = SPACE                          
088300                                                                          
088400           EVALUATE MID-SELECT-URVAL (INDX)                               
088500           WHEN  'I'                                                      
088600             MOVE NEJ TO ALLT-SW                                          
088700             MOVE JA  TO BYT-SW                                           
088800             MOVE MID-OBJNR (INDX) TO WS-SPAR-OBJNR                       
088900             IF WS-SPAR-OBJNR NOT = ZERO                                  
089000               PERFORM QA-BYT-BILD                                        
089300             END-IF                                                       
089400             MOVE +9999 TO INDX                                           
089500                                                                          
089600           WHEN 'R'                                                       
089700             IF NDC-NA                                                    
089800                MOVE +9999 TO INDX                                        
089900                MOVE NEJ TO NDC-BEHORIGHET-SW                             
090600             END-IF                                                       
090500             MOVE +9999 TO INDX                                           
090700                                                                          
090800           WHEN 'D'                                                       
090900             IF MFS-UPDATE                                                
091000               MOVE +9999 TO INDX                                         
091100             END-IF                                                       
091200                                                                          
091300           WHEN 'G'                                                       
091400           WHEN 'A'                                                       
091500               MOVE +9999 TO INDX                                         
091600                                                                          
091700           WHEN 'S'                                                       
091800             IF MFS-UPDATE                                                
091900                MOVE +9999 TO INDX                                        
092000             END-IF                                                       
092100                                                                          
092200           WHEN OTHER                                                     
092300             IF ENGLISH-TEXT                                              
092400               IF NDC-NA                                                  
092500                  MOVE 'WRONG ONLY I D S OR A'   TO MOD-TEMFSINF          
092600               ELSE                                                       
092700                  MOVE 'WRONG ONLY I R D S OR A' TO MOD-TEMFSINF          
092800               END-IF                                                     
092900             ELSE                                                         
093000               MOVE 'FEL VAL VÄLJ I D R S ELLER G' TO MOD-TEMFSINF        
093100             END-IF                                                       
093200                                                                          
093300           END-EVALUATE                                                   
093400         END-IF                                                           
093500                                                                          
093600         ADD +1 TO INDX                                                   
093700       END-PERFORM                                                        
093800     END-IF                                                               
093900     .                                                                    
094000                                                                          
094100     EJECT                                                                
094200 QA-BYT-BILD SECTION.                                                     
094300     MOVE 'QA-BYT-BILD' TO WS-SECTION                                     
094400                                                                          
094500     MOVE MFS-KDMFSFOR  TO M-SW-KDMFSTYP-1                                
094600                                                                          
094700     INSPECT MID-OBJNR (INDX) REPLACING LEADING SPACE BY ZERO             
094800     MOVE LOW-VALUE TO 3173-MID-W3I17301                                  
094900     MOVE MID-OBJNR (INDX) TO 3173-MID-IDARTNR-IN                         
095000     MOVE MSGI-IDDISTR     TO 3173-MID-IDDISTR-IN                         
095100     MOVE MSGI-IDBYTRAP    TO 3173-MID-IDBYTRAP-IN                        
095200     MOVE MID-IDBYTRAD(INDX) TO 3173-MID-IDBYTRAD-IN                      
095300     PERFORM IMS-INSERT-ALT1-MSG                                          
095400     .                                                                    
095500                                                                          
095600     EJECT                                                                
098900 R-3173-FIX-SOME-FIELDS SECTION.                                          
099000     MOVE 'R-3173-FIX-SOME-FIELDS'  TO WS-SECTION                         
099100                                                                          
099200*    IF MOD-OBJNR-SPAERR NOT = MFS-RENSA-FAELT                            
099300     IF RESP-IDARTNR-OBJ-SPAERR NOT = SPACE                               
099400       MOVE +1 TO INDX                                                    
099500       MOVE RESP-KVRADER TO WS-RESP-KVRADER                               
099600       PERFORM UNTIL INDX > WS-RESP-KVRADER                               
099700         IF RESP-KDCMD-LINE-ATTR (INDX)                                   
099800         NOT = MFS-STAENG-FAELT                                           
099900            MOVE MFS-FORMATETS-ATTR TO                                    
100000                 MOD-SELECT-URVAL-ATTR (INDX)                             
100100         END-IF                                                           
100200         ADD +1 TO INDX                                                   
100300       END-PERFORM                                                        
100400       MOVE MFS-ADD-SAETT-CURSOR TO MOD-ANTAL-IN-ATTR                     
100500     END-IF                                                               
100600     MOVE MFS-STAENG-FAELT TO MOD-OBJNR-SPAERR-ATTR                       
100700     .                                                                    
100800     EJECT                                                                
100900 MFS-RENSA-FAELT-IN SECTION.                                              
101000                                                                          
101100*    --- ALLA INDATA-FÄLT                                                 
101200     IF MFS-IDTRANS = '3173'                                              
101300       MOVE MFS-RENSA-FAELT TO MOD-OBJNR-SPAERR                           
101400                               MOD-ANTAL-IN                               
101500                               MOD-GODK-IN                                
101600                               MOD-ANMARK-IN                              
101700       IF MID-ANMARK-IN = 'XXX'                                           
101800          MOVE MID-ANMARK-IN TO MOD-ANMARK-IN                             
101900       END-IF                                                             
102000       MOVE MFS-ADD-LAES-IN-FAELT TO                                      
102100                               MOD-ANMARK-IN-ATTR                         
102200       MOVE MID-OBJNR-SPAERR TO MOD-OBJNR-SPAERR                          
102300       IF MID-IDBYTRAD(1)  NUMERIC                                        
102400         MOVE MID-IDBYTRAD(1)  TO WS-IDBYTRAD                             
102500       END-IF                                                             
102600       MOVE MID-IDBYTRAD-3173 TO MOD-IDBYTRAD-3173                        
102700                                                                          
102800       IF WS-IDBYTRAD = ZERO                                              
102900         MOVE MFS-RENSA-FAELT TO MOD-OBJNR-SPAERR                         
103000       ELSE                                                               
103100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-OBJNR-SPAERR-ATTR              
103200       END-IF                                                             
103300       MOVE MFS-STAENG-FAELT  TO MOD-OBJNR-SPAERR-ATTR                    
103400     ELSE                                                                 
103500       MOVE MFS-RENSA-FAELT TO MOD-OBJNR-SPAERR                           
103600                               MOD-ANTAL-IN                               
103700                               MOD-ANMARK-IN                              
103800                               MOD-GODK-IN                                
103900     END-IF                                                               
104000     MOVE +1 TO INDX                                                      
104100     PERFORM UNTIL INDX > MAX-KVRADER                                     
104200       MOVE MFS-RENSA-FAELT TO MOD-SELECT-URVAL       (INDX)              
104300                               MOD-KVRETUR-GODK       (INDX)              
104400                               MOD-ANMARK-IN-UT       (INDX)              
104500                               MOD-FLSKROT-IN-UT      (INDX)              
104600                               ADD +1 TO INDX                             
104700     END-PERFORM                                                          
104800     .                                                                    
104900                                                                          
105000     EJECT                                                                
105100 MFS-FORM-ATTR SECTION.                                                   
105200                                                                          
105300*    --- ALLA INDATA-FÄLT                                                 
105400     MOVE +1 TO INDX                                                      
105500     PERFORM UNTIL INDX > MAX-KVRADER                                     
105600       MOVE MFS-FORMATETS-ATTR TO MOD-SELECT-URVAL-ATTR (INDX)            
105700                                MOD-KVRETUR-GODK-ATTR (INDX)              
105800                                MOD-ANMARK-IN-UT-ATTR (INDX)              
105900       ADD +1 TO INDX                                                     
106000     END-PERFORM                                                          
106100                                                                          
106200     MOVE MFS-FORMATETS-ATTR TO MOD-KDPRT-ATTR                            
106300     MOVE MFS-FORMATETS-ATTR TO MOD-OBJNR-SPAERR-ATTR                     
106400     MOVE MFS-FORMATETS-ATTR TO MOD-ANTAL-IN-ATTR                         
106500     MOVE MFS-FORMATETS-ATTR TO MOD-ANMARK-IN-ATTR                        
106600     MOVE MFS-FORMATETS-ATTR TO MOD-GODK-IN-ATTR                          
106700     .                                                                    
106800     SKIP2                                                                
106900 MFS-LAES-IN-IGEN SECTION.                                                
107000                                                                          
107100*    --- ALLA INDATA-FÄLT                                                 
107200     MOVE +1 TO INDX                                                      
107300     PERFORM UNTIL INDX > MAX-KVRADER                                     
107400       IF MID-SELECT-URVAL (INDX) NOT = ALL '+'                           
107500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SELECT-URVAL-ATTR(INDX)        
107600       END-IF                                                             
107700       IF MID-KVRETUR-GODK (INDX) NOT = ALL '+'                           
107800         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVRETUR-GODK-ATTR(INDX)        
107900       END-IF                                                             
108000       IF MID-ANMARK-IN-UT (INDX) NOT = ALL '+'                           
108100         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ANMARK-IN-UT-ATTR(INDX)        
108200       END-IF                                                             
108300*      IF MID-FLSKROT-IN-UT (INDX) NOT = ALL '+'                          
108400*       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSKROT-IN-UT-ATTR(INDX)        
108500*      END-IF                                                             
108600       ADD +1 TO INDX                                                     
108700     END-PERFORM                                                          
108800                                                                          
108900     IF MID-KDPRT NOT = ALL '+'                                           
109000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRT-ATTR                       
109100     END-IF                                                               
109200     IF MID-OBJNR-SPAERR NOT = ALL '+'                                    
109300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-OBJNR-SPAERR-ATTR                
109400     END-IF                                                               
109500     IF MID-ANTAL-IN NOT = ALL '+'                                        
109600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ANTAL-IN-ATTR                    
109700     END-IF                                                               
109800     IF MID-ANMARK-IN NOT = ALL '+'                                       
109900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ANMARK-IN-ATTR                   
110000     END-IF                                                               
110100     IF MID-GODK-IN NOT = ALL '+'                                         
110200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-GODK-IN-ATTR                     
110300     END-IF                                                               
110400     .                                                                    
110500     EJECT                                                                
110600* --- IMS SEKTIONER ---                                                   
110700     SKIP3                                                                
110800 IMS-GET-MSG SECTION.                                                     
110900     MOVE 'IMS-GET-MSG' TO WS-SECTION                                     
111000                                                                          
111100     MOVE '  QC' TO GODK-STATUSKODER                                      
111200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
111300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111400     PERFORM IMS-STATUSKONTROLL                                           
111500     .                                                                    
111600     SKIP3                                                                
111700 IMS-INSERT-MSG SECTION.                                                  
111800     MOVE 'IMS-INSERT-MSG' TO WS-SECTION                                  
111900                                                                          
112000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
112100       MOVE '0' TO MFS-KDHUVOMR                                           
112200     END-IF                                                               
112300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
112400     MOVE SPACE TO GODK-STATUSKODER                                       
112500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
112600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
112700     PERFORM IMS-STATUSKONTROLL                                           
112800     .                                                                    
112900     EJECT                                                                
113000 IMS-INSERT-ALT1-MSG SECTION.                                             
113100     MOVE 'IMS-INSERT-ALT1-MSG' TO WS-SECTION                             
113200                                                                          
113300     MOVE SPACE TO GODK-STATUSKODER                                       
113400     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW-1                 
113500     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
113600     PERFORM IMS-STATUSKONTROLL                                           
113700     .                                                                    
113800     SKIP3                                                                
115700 IMS-STATUSKONTROLL SECTION.                                              
115800                                                                          
115900     SET STATUS-IX TO 1                                                   
116000     SEARCH GODK-STATUS                                                   
116100       AT END                                                             
116200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
116300         DELIMITED BY SIZE INTO FELTEXT                                   
116400         CALL FELLOG                                                      
116500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
116600         CONTINUE                                                         
116700     END-SEARCH                                                           
116800     .                                                                    
