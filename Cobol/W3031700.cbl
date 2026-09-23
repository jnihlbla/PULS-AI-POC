000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3031700.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   94/02/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR ETT PRISOMRÅDE OCH DE ARTIKLAR SOM HAR ARTIKEL-            
000900*        RABATT.                                                          
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDC2                                       
001200*                              WDB1                                       
001300*                              WDB2                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W3T317                                              
001700*        MID:         W3I31701                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W3O31701                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W3031700'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004100                                                                          
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400 01  WS-IDDISTR                  PIC X(5)    VALUE SPACE.                 
004500 01  FILLER REDEFINES WS-IDDISTR.                                         
004600     03 FILLER                   PIC X(1).                                
004700     03 WS-IDDISTR-IN            PIC X(4).                                
004800                                                                          
004900                                                                          
005000 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
005100 01  FILLER REDEFINES WS-IDPROMR.                                         
005200     03  WS-MARKBOLAG            PIC X(1).                                
005300     03  FILLER                  PIC X(2).                                
005400                                                                          
005500 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005600                                                                          
005700 01  DAGENS-DATUM-Y2K            PIC 9(8)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM-Y2K.                                   
005900     03  DAGENS-AAAA-Y2K         PIC 9(2).                                
006000     03  DAGENS-MM-Y2K           PIC 9(2).                                
006100     03  DAGENS-DD-Y2K           PIC 9(2).                                
006200                                                                          
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-AA               PIC 9(2).                                
006600     03  DAGENS-MM               PIC 9(2).                                
006700     03  DAGENS-DD               PIC 9(2).                                
006800                                                                          
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300 77  PRISOMR-SW                  PIC X       VALUE 'J'.                   
007400     88  PRISOMR-FINNS                       VALUE 'J'.                   
007500     88  PRISOMR-SAKNAS                      VALUE 'N'.                   
007600                                                                          
007700 77  ARTIKEL-SW                  PIC X       VALUE 'J'.                   
007800     88  ALLA-ARTIKLAR                       VALUE 'J'.                   
007900     88  UNIK-ARTIKEL                        VALUE 'N'.                   
008000                                                                          
008100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008200     88  EGEN-MID                            VALUE '3317'.                
008300     88  GODK-MID                            VALUE '3317'.                
008400     88  HELP-MID                            VALUE '0551'.                
008500     EJECT                                                                
008600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008700 01  GENERELLA-SUBPROGRAM.                                                
008800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009400*01 -COPY WMSGINIT                                                        
009500     EJECT                                                                
009600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009700*01 -COPY WMEDAREA                                                        
009800     SKIP3                                                                
009900 01  MESSAGE-CODES.                                                       
010000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010300     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
010400     03  ERR-PRICEAREA-MISSING   PIC X(3)    VALUE '236'.                 
010500     EJECT                                                                
010600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010900     SKIP3                                                                
011000*01  MID -COPY W3I31701                                                   
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011300     SKIP3                                                                
011400*01  -COPY WMSGAREA                                                       
011500     EJECT                                                                
011600     03  MOD REDEFINES MSG-AREA.                                          
011700*      05  -COPY W3O31701                                                 
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012000     SKIP3                                                                
012100*01  -COPY WMFSAREA                                                       
012200     EJECT                                                                
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800 01  NYCKLAR-TILL-DLI.                                                    
012900     03  W-IDPROMR-X.                                                     
013000         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
013100     03  W-WDC211KY-X.                                                    
013200         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
013300         05  W-DASTADAT          PIC 9(8)     VALUE ZERO.                 
013400     SKIP2                                                                
013500     03  W-WDC211KY-MIN-X.                                                
013600         05  W-IDARTNR-MIN       PIC S9(9)    VALUE ZERO COMP-3.          
013700         05  W-DASTADAT-MIN      PIC 9(8)     VALUE ZERO.                 
013800     SKIP2                                                                
013900     03  W-WDC211KY-MAX-X.                                                
014000         05  W-IDARTNR-MAX       PIC S9(9)    VALUE ZERO COMP-3.          
014100         05  W-DASTADAT-MAX      PIC 9(8)     VALUE ZERO.                 
014200     SKIP2                                                                
014300*   NYCKLAR TILL KUNDREG             ***********                          
014400     03  W-IDGMT-MIN-X.                                                   
014500         05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.           
014600         05  W-IDKUNDNR-B1       PIC S9(7)   VALUE ZERO COMP-3.           
014700                                                                          
014800     03  W-IDGMT-MAX-X.                                                   
014900         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
015000         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE 9999999                
015100                                             COMP-3.                      
015200                                                                          
015300     03  W-IDDISTR-X.                                                     
015400         05  WA-IDDISTR  PIC S9(5)           COMP-3.                      
015500*   NYCKLAR TILL BETALNINGSREGISTRET ***********                          
015600     03  W-WDB101KY-X.                                                    
015700         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
015800         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
015900                                                                          
016000*    --- STATUS-KOD FRÅN IMS                                              
016100 01  STATUS-WS                   PIC XX.                                  
016200     88  SEGMENT-FINNS                       VALUE '  '.                  
016300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016500     SKIP2                                                                
016600 01  GODK-STATUSKODER.                                                    
016700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016800     SKIP3                                                                
016900 01  SSA1                        PIC X(64).                               
017000 01  SSA2                        PIC X(64).                               
017100     EJECT                                                                
017200*    --- IMS FUNKTIONSKODER                                               
017300*01  -COPY W0003                                                          
017400     EJECT                                                                
017500*    ---  DLI INPUT-OUTPUT AREA                                           
017600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017700     SKIP3                                                                
017800 01  DLI-IO-AREA.                                                         
017900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
018000     SKIP3                                                                
018100     03  WDC201 REDEFINES IO-AREA.                                        
018200*        05  -COPY WDC201  -PRE WDC2-                                     
018300     SKIP3                                                                
018400     03  WDC211 REDEFINES IO-AREA.                                        
018500*        05  -COPY WDC211  -PRE WDC2-                                     
018600**   KUNDREGISTER                                                         
018700 01  DLI-IO-AREA2.                                                        
018800*    03  WDB201    -COPY WDB201 -PRE WDB2-                                
018900     EJECT                                                                
019000**   BETALNINGSREGISTER                                                   
019100 01  DLI-IO-AREA3.                                                        
019200     03  WDB101.                                                          
019300*        05  -COPY WDB101  -PRE WDB1-                                     
019400     EJECT                                                                
019500 LINKAGE SECTION.                                                         
019600                                                                          
019700*01  -COPY W0009   -PRE MSG-                                              
019800     EJECT                                                                
019900*01  -COPY W0008  -PRE USEA-                                              
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200*01  -COPY W0008  -PRE WDC2-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500*01  -COPY W0008  -PRE WDB2-                                              
020600     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
020800*01  -COPY W0008  -PRE WDB1-                                              
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
021200                                   WDC2-PCB WDB2-PCB WDB1-PCB.            
021300 MAIN SECTION.                                                            
021400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
021500                                   WDC2-PCB WDB2-PCB WDB1-PCB.            
021600                                                                          
021700     PERFORM IMS-GET-MSG                                                  
021800     IF SEGMENT-FINNS                                                     
021900       PERFORM A-INIT                                                     
022000       PERFORM B-KOLLA-NYCKLAR                                            
022100       IF NYCKLAR-OK                                                      
022200           IF MFS-FIRST                                                   
022300             PERFORM C-FOERSTA-SIDA                                       
022400           ELSE                                                           
022500             IF MFS-NEXT                                                  
022600               PERFORM D-NAESTA-SIDA                                      
022700             ELSE                                                         
022800               PERFORM E-SAMMA-SIDA                                       
022900             END-IF                                                       
023000           END-IF                                                         
023100         PERFORM F-LAES-VISA-INFO                                         
023200       END-IF                                                             
023300       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O31701 + 4                      
023400       PERFORM IMS-INSERT-MSG                                             
023500     END-IF                                                               
023600                                                                          
023700     MOVE ZERO TO RETURN-CODE                                             
023800     GOBACK                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 A-INIT SECTION.                                                          
024200                                                                          
024300     IF MSG-DUBBLA-TRANSKODER                                             
024400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31701                 
024500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024700     ELSE                                                                 
024800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31701                  
024900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
025000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025100     END-IF                                                               
025200                                                                          
025300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
025400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025600                                                                          
025700     MOVE LOW-VALUE TO MSG-AREA                                           
025800     MOVE 'W3O317N1' TO MFS-IDMOD                                         
025900     MOVE '3317' TO MOD-IDTRANS                                           
026000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
026100                                                                          
026200     IF EGEN-MID OR HELP-MID                                              
026300       CONTINUE                                                           
026400     ELSE                                                                 
026500       MOVE SPACE TO MFS-KDTRTYP                                          
026600       MOVE '7' TO MFS-IDPFK                                              
026700     END-IF                                                               
026800                                                                          
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
027900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
028000     MOVE '001'             TO MSGI-KDCALL                                
028100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
028200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
028300     MOVE '3317'            TO MSGI-IDTRANS                               
028400                                                                          
028500     IF MFS-IDTRANS = '3317'                                              
028600       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
028700       MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                                
028800     ELSE                                                                 
028900       MOVE SPACE          TO MID-IDPROMR-IN                              
029000                              MID-IDDISTR-IN                              
029100     END-IF                                                               
029200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029300                                                                          
029400     IF MSGI-IDLAND-SPR = 'GB'                                            
029500       MOVE +2 TO SPRAK-IX                                                
029600       MOVE 'GB ' TO MED-IDSKYLT                                          
029700     ELSE                                                                 
029800       MOVE +1 TO SPRAK-IX                                                
029900       MOVE 'S  ' TO MED-IDSKYLT                                          
030000     END-IF                                                               
030100                                                                          
030200*    -- KONTROLL AV IDPROMR                                               
030300     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                               
030400                                                                          
030500     IF MID-IDPROMR-IN = ALL '+'                                          
030600       MOVE MID-IDPROMR-UT TO WS-IDPROMR                                  
030700     ELSE                                                                 
030800       MOVE MID-IDPROMR-IN TO WS-IDPROMR                                  
030900       MOVE '7'         TO MFS-IDPFK                                      
031000       MOVE SPACE       TO MFS-KDTRTYP                                    
031100       MOVE SPACE       TO MID-IDDISTR-UT                                 
031200     END-IF                                                               
031300                                                                          
031400     IF WS-IDPROMR NOT = SPACE                                            
031500        MOVE WS-IDPROMR TO W-IDPROMR                                      
031600     END-IF                                                               
031700                                                                          
031800*    -- KONTROLL AV IDARTNR                                               
031900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
032000                                                                          
032100     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
032200     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
032300     MOVE WS-IDARTNR          TO MOD-IDARTNR-UT                           
032400     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
032500                                                                          
032600     IF MID-IDARTNR-IN = ALL '+'                                          
032700       CONTINUE                                                           
032800     ELSE                                                                 
032900       MOVE '7'         TO MFS-IDPFK                                      
033000       MOVE SPACE       TO MFS-KDTRTYP                                    
033100     END-IF                                                               
033200                                                                          
033300     IF EGEN-MID                                                          
033400      CONTINUE                                                            
033500     ELSE                                                                 
033600       IF GODK-MID                                                        
033700         IF WS-IDARTNR NUMERIC                                            
033800           CONTINUE                                                       
033900         ELSE                                                             
034000           MOVE ZERO TO WS-IDARTNR                                        
034100         END-IF                                                           
034200       END-IF                                                             
034300     END-IF                                                               
034400                                                                          
034500     IF WS-IDARTNR NUMERIC                                                
034600       IF WS-IDARTNR = ZERO                                               
034700         MOVE JA TO ARTIKEL-SW                                            
034800       ELSE                                                               
034900         MOVE NEJ TO ARTIKEL-SW                                           
035000       END-IF                                                             
035100       MOVE WS-IDARTNR TO W-IDARTNR                                       
035200                          W-IDARTNR-MIN                                   
035300                          W-IDARTNR-MAX                                   
035400       IF WS-IDARTNR = ZERO                                               
035500         MOVE 999999999 TO W-IDARTNR-MAX                                  
035600       END-IF                                                             
035700     ELSE                                                                 
035800       MOVE NEJ TO NYCKLAR-SW                                             
035900     END-IF                                                               
036000                                                                          
036100*    -- KONTROLL AV IDDISTR                                               
036200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
036300                                                                          
036400     IF MID-IDDISTR-IN = ALL '+'                                          
036500       MOVE MID-IDDISTR-UT TO WS-IDDISTR-IN                               
036600     ELSE                                                                 
036700       MOVE MID-IDDISTR-IN TO WS-IDDISTR-IN                               
036800       MOVE '7'         TO MFS-IDPFK                                      
036900       MOVE SPACE       TO MFS-KDTRTYP                                    
037000     END-IF                                                               
037100                                                                          
037200     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
037300                                                                          
037400     IF WS-IDDISTR NUMERIC                                                
037500       IF WS-IDDISTR > ZERO                                               
037600         MOVE WS-IDDISTR TO WA-IDDISTR                                    
037700                                                                          
037800                            W-IDDISTR-B1                                  
037900                            W-IDDISTR-B2                                  
038000        END-IF                                                            
038100     END-IF                                                               
038200                                                                          
038300     IF WS-IDPROMR = SPACE                                                
038400        IF WS-IDDISTR NUMERIC                                             
038500           IF WS-IDDISTR > ZERO                                           
038600              MOVE NEJ TO PRISOMR-SW                                      
038700           ELSE                                                           
038800              MOVE NEJ TO NYCKLAR-SW                                      
038900           END-IF                                                         
039000        ELSE                                                              
039100           MOVE NEJ TO NYCKLAR-SW                                         
039200        END-IF                                                            
039300     ELSE                                                                 
039400        IF WS-IDDISTR NUMERIC                                             
039500           IF WS-IDDISTR > ZERO                                           
039600              MOVE NEJ TO PRISOMR-SW                                      
039700           END-IF                                                         
039800        END-IF                                                            
039900     END-IF                                                               
040000                                                                          
040100     IF EGEN-MID OR GODK-MID                                              
040200        CONTINUE                                                          
040300     ELSE                                                                 
040400        MOVE NEJ TO NYCKLAR-SW                                            
040500     END-IF                                                               
040600                                                                          
040700     IF NYCKLAR-OK                                                        
040800       IF EGEN-MID OR GODK-MID                                            
040900         IF PRISOMR-SAKNAS                                                
041000            MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                        
041100            MOVE SPACE           TO W-IDPROMR                             
041200         ELSE                                                             
041300            MOVE WS-IDPROMR      TO MOD-IDPROMR-UT                        
041400         END-IF                                                           
041500         MOVE WS-IDDISTR-IN TO MOD-IDDISTR-UT                             
041600         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
041700       ELSE                                                               
041800         MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                           
041900         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
042000       END-IF                                                             
042100     ELSE                                                                 
042200       IF EGEN-MID OR GODK-MID                                            
042300          MOVE WS-IDPROMR        TO MOD-IDPROMR-UT                        
042400          MOVE WS-IDDISTR-IN     TO MOD-IDDISTR-UT                        
042500          INSPECT MOD-IDDISTR-UT                                          
042600                REPLACING LEADING ZERO BY SPACE                           
042700       ELSE                                                               
042800          MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                          
042900          MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                          
043000       END-IF                                                             
043100     END-IF                                                               
043200                                                                          
043300     IF NYCKLAR-FEL                                                       
043400       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
043500       CALL WMEDKONV USING MED-WMEDAREA                                   
043600       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
043700       PERFORM MFS-RENSA-FAELT-UT                                         
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 C-FOERSTA-SIDA SECTION.                                                  
044200                                                                          
044300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
044400     CALL WMEDKONV USING MED-WMEDAREA                                     
044500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
044600                                                                          
044700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
044800     MOVE ZERO  TO W-IDARTNR                                              
044900                   W-DASTADAT                                             
045000                   W-DASTADAT-MIN                                         
045100     .                                                                    
045200     EJECT                                                                
045300 D-NAESTA-SIDA SECTION.                                                   
045400                                                                          
045500     MOVE MID-IDPROMR-SPAR  TO W-IDPROMR                                  
045600     MOVE MID-IDARTNR-NEXT  TO W-IDARTNR-MIN                              
045700     MOVE MID-TISTADAT-NEXT TO W-DASTADAT-MIN                             
045800*---Y2K-FIX*******                                                        
045900     IF MID-TISTADAT-NEXT NOT = ZERO                                      
046000       IF MID-TISTADAT-NEXT < 50000                                       
046100         MOVE 20       TO W-DASTADAT-MIN(1:2)                             
046200       ELSE                                                               
046300         IF MID-TISTADAT-NEXT < 999999                                    
046400           MOVE 19       TO W-DASTADAT-MIN(1:2)                           
046500         ELSE                                                             
046600           MOVE 99999999  TO W-DASTADAT-MIN                               
046700         END-IF                                                           
046800       END-IF                                                             
046900     END-IF                                                               
047000     MOVE 99999999          TO W-DASTADAT-MAX                             
047100     .                                                                    
047200     EJECT                                                                
047300 E-SAMMA-SIDA SECTION.                                                    
047400                                                                          
047500     IF EGEN-MID OR HELP-MID                                              
047600       MOVE MID-IDARTNR-ENTER  TO W-IDARTNR                               
047700                                  W-IDARTNR-MIN                           
047800                                  W-IDARTNR-MAX                           
047900       MOVE MID-TISTADAT-ENTER TO W-DASTADAT                              
048000                                  W-DASTADAT-MIN                          
048100                                  W-DASTADAT-MAX                          
048200*---Y2K-FIX*******                                                        
048300       IF MID-TISTADAT-ENTER NOT = ZERO                                   
048400         IF MID-TISTADAT-ENTER < 50000                                    
048500           MOVE 20          TO W-DASTADAT(1:2)                            
048600           MOVE 20          TO W-DASTADAT-MIN(1:2)                        
048700           MOVE 20          TO W-DASTADAT-MAX(1:2)                        
048800         ELSE                                                             
048900           IF MID-TISTADAT-ENTER < 999999                                 
049000             MOVE 19        TO W-DASTADAT(1:2)                            
049100             MOVE 19        TO W-DASTADAT-MIN(1:2)                        
049200             MOVE 19        TO W-DASTADAT-MAX(1:2)                        
049300           ELSE                                                           
049400             MOVE 99999999  TO W-DASTADAT                                 
049500             MOVE 99999999  TO W-DASTADAT-MIN                             
049600             MOVE 99999999  TO W-DASTADAT-MAX                             
049700           END-IF                                                         
049800         END-IF                                                           
049900       END-IF                                                             
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 F-LAES-VISA-INFO SECTION.                                                
050400                                                                          
050500     IF PRISOMR-SAKNAS                                                    
050600        PERFORM FB-HT-PROM-VIA-DISTRIKT                                   
050700     END-IF                                                               
050800                                                                          
050900     PERFORM IMS-GU-WDC201                                                
051000                                                                          
051100     IF SEGMENT-SAKNAS                                                    
051200       MOVE ERR-PRICEAREA-MISSING TO MED-IDMFSFEL                         
051300       CALL WMEDKONV USING MED-WMEDAREA                                   
051400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
051500       PERFORM MFS-RENSA-FAELT-UT                                         
051600     ELSE                                                                 
051700       MOVE WDC2-PRO-IDPROMR TO MOD-IDPROMR-SPAR                          
051800       MOVE +1 TO INDX                                                    
051900       IF ALLA-ARTIKLAR                                                   
052000         PERFORM IMS-GN-WDC211                                            
052100         IF SEGMENT-FINNS                                                 
052200           MOVE WDC2-ART-IDARTNR TO MOD-IDARTNR-ENTER                     
052300           MOVE WDC2-ART-DASTADAT(3:6) TO MOD-TISTADAT-ENTER              
052400         ELSE                                                             
052500           MOVE ZERO           TO MOD-IDARTNR-ENTER                       
052600           MOVE SPACE          TO MOD-IDPROMR-SPAR                        
052700           MOVE ZERO           TO MOD-TISTADAT-ENTER                      
052800           MOVE PART-MISSING TO MED-IDMFSFEL                              
052900           CALL WMEDKONV USING MED-WMEDAREA                               
053000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
053100           PERFORM MFS-RENSA-FAELT-UT                                     
053200         END-IF                                                           
053300                                                                          
053400         PERFORM UNTIL INDX > MAX-INDX                                    
053500           IF SEGMENT-FINNS                                               
053600             MOVE WDC2-ART-IDARTNR     TO MOD-IDARTNR       (INDX)        
053700             MOVE WDC2-ART-REARTRAB-BULK TO                               
053800                  MOD-REARTRAB-BULK (INDX)                                
053900             MOVE WDC2-ART-REARTRAB-DO   TO MOD-REARTRAB-DO (INDX)        
054000             MOVE WDC2-ART-DASTADAT(3:6) TO MOD-TISTADAT    (INDX)        
054100             MOVE WDC2-ART-TISTODAT      TO MOD-TISTODAT    (INDX)        
054200             PERFORM IMS-GN-WDC211                                        
054300           ELSE                                                           
054400             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR            (INDX)        
054500                                     MOD-REARTRAB-BULK      (INDX)        
054600                                     MOD-REARTRAB-DO        (INDX)        
054700                                     MOD-TISTADAT           (INDX)        
054800                                     MOD-TISTODAT           (INDX)        
054900           END-IF                                                         
055000           ADD 1 TO INDX                                                  
055100         END-PERFORM                                                      
055200                                                                          
055300         IF SEGMENT-FINNS                                                 
055400           MOVE WDC2-ART-IDARTNR       TO MOD-IDARTNR-NEXT                
055500           MOVE WDC2-ART-DASTADAT(3:6) TO MOD-TISTADAT-NEXT               
055600           MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                    
055700           CALL WMEDKONV USING MED-WMEDAREA                               
055800           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
055900         ELSE                                                             
056000           MOVE ZERO           TO MOD-IDARTNR-NEXT                        
056100           MOVE ZERO           TO MOD-TISTADAT-NEXT                       
056200         END-IF                                                           
056300       ELSE                                                               
056400         PERFORM FA-LAES-UNIK-ARTIKEL                                     
056500       END-IF                                                             
056600                                                                          
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000 FA-LAES-UNIK-ARTIKEL SECTION.                                            
057100                                                                          
057200     PERFORM IMS-GNP-WDC211                                               
057300     IF SEGMENT-FINNS                                                     
057400       MOVE WDC2-ART-IDARTNR       TO MOD-IDARTNR-ENTER                   
057500       MOVE WDC2-ART-DASTADAT(3:6) TO MOD-TISTADAT-ENTER                  
057600       MOVE WDC2-ART-IDARTNR       TO MOD-IDARTNR        (INDX)           
057700       MOVE WDC2-ART-REARTRAB-BULK TO MOD-REARTRAB-BULK  (INDX)           
057800       MOVE WDC2-ART-REARTRAB-DO   TO MOD-REARTRAB-DO    (INDX)           
057900       MOVE WDC2-ART-DASTADAT(3:6) TO MOD-TISTADAT       (INDX)           
058000       MOVE WDC2-ART-TISTODAT      TO MOD-TISTODAT       (INDX)           
058100     ELSE                                                                 
058200       MOVE ZERO                   TO MOD-IDARTNR-ENTER                   
058300       MOVE ZERO                   TO MOD-TISTADAT-ENTER                  
058400       MOVE PART-MISSING   TO MED-IDMFSFEL                                
058500       CALL WMEDKONV USING MED-WMEDAREA                                   
058600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
058700       PERFORM MFS-RENSA-FAELT-UT                                         
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100 FB-HT-PROM-VIA-DISTRIKT SECTION.                                         
059200                                                                          
059300     PERFORM IMS-GET-WDB201                                               
059400     IF SEGMENT-FINNS                                                     
059500        MOVE WDB2-GMT-IDPARTNR  TO W-WDB1-IDPARTNR                        
059600        MOVE WDB2-GMT-IDFTG     TO W-WDB1-IDFTG                           
059700        PERFORM IMS-GU-WDB101                                             
059800        IF SEGMENT-FINNS                                                  
059900           MOVE WDB1-BET-IDPROMR TO WS-IDPROMR                            
060000           MOVE WS-IDPROMR  TO W-IDPROMR                                  
060100        END-IF                                                            
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500                                                                          
060600 MFS-RENSA-FAELT-UT SECTION.                                              
060700                                                                          
060800*    --- ALLA UTDATA-FÄLT                                                 
060900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
061000     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-SPAR                             
061100                             MOD-IDARTNR-ENTER                            
061200                             MOD-IDARTNR-NEXT                             
061300                             MOD-TISTADAT-ENTER                           
061400                             MOD-TISTADAT-NEXT                            
061500     MOVE +1 TO INDX                                                      
061600     PERFORM UNTIL INDX > MAX-INDX                                        
061700       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
061800       ADD +1 TO INDX                                                     
061900     END-PERFORM                                                          
062000     .                                                                    
062100     SKIP2                                                                
062200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
062300                                                                          
062400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
062500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR       (INDX)                     
062600                             MOD-REARTRAB-DO   (INDX)                     
062700                             MOD-REARTRAB-BULK (INDX)                     
062800                             MOD-TISTADAT      (INDX)                     
062900                             MOD-TISTODAT      (INDX)                     
063000     .                                                                    
063100     EJECT                                                                
063200* --- IMS SEKTIONER ---                                                   
063300     SKIP3                                                                
063400 IMS-GET-MSG SECTION.                                                     
063500                                                                          
063600     MOVE '  QC' TO GODK-STATUSKODER                                      
063700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
063800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     SKIP3                                                                
064200 IMS-INSERT-MSG SECTION.                                                  
064300                                                                          
064400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
064500       MOVE '0' TO MFS-KDHUVOMR                                           
064600     END-IF                                                               
064700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
064800     MOVE SPACE TO GODK-STATUSKODER                                       
064900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
065000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065100     PERFORM IMS-STATUSKONTROLL                                           
065200     .                                                                    
065300     EJECT                                                                
065400 IMS-GU-WDC201 SECTION.                                                   
065500     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
065600          DELIMITED BY SIZE INTO SSA1                                     
065700     MOVE '  GE' TO GODK-STATUSKODER                                      
065800     CALL CBLTDLI USING GU WDC2-PCB DLI-IO-AREA SSA1                      
065900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     SKIP2                                                                
066300 IMS-GNP-WDC211 SECTION.                                                  
066400     STRING 'WDC211  (WDC211KY>=' W-WDC211KY-MIN-X                        
066500                    '&WDC211KY<=' W-WDC211KY-MAX-X ')'                    
066600          DELIMITED BY SIZE INTO SSA1                                     
066700     MOVE '  GE' TO GODK-STATUSKODER                                      
066800     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA SSA1                     
066900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
067000     PERFORM IMS-STATUSKONTROLL                                           
067100     .                                                                    
067200     SKIP2                                                                
067300 IMS-GN-WDC211 SECTION.                                                   
067400     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
067500          DELIMITED BY SIZE INTO SSA1                                     
067600     STRING 'WDC211  (WDC211KY>=' W-WDC211KY-MIN-X                        
067700                    '&WDC211KY<=' W-WDC211KY-MAX-X ')'                    
067800          DELIMITED BY SIZE INTO SSA2                                     
067900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
068000     CALL CBLTDLI USING GN WDC2-PCB DLI-IO-AREA SSA1 SSA2                 
068100     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
068200     PERFORM IMS-STATUSKONTROLL                                           
068300     .                                                                    
068400     EJECT                                                                
068500 IMS-GET-WDB201    SECTION.                                               
068600                                                                          
068700     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
068800                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
068900             DELIMITED BY SIZE INTO SSA1                                  
069000     MOVE '  GE' TO GODK-STATUSKODER                                      
069100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA2 SSA1                     
069200     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
069300     PERFORM IMS-STATUSKONTROLL                                           
069400     .                                                                    
069500     SKIP2                                                                
069600 IMS-GU-WDB101 SECTION.                                                   
069700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
069800          DELIMITED BY SIZE INTO SSA1                                     
069900     MOVE '  GE' TO GODK-STATUSKODER                                      
070000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA3 SSA1                     
070100     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     .                                                                    
070400     SKIP3                                                                
070500 IMS-STATUSKONTROLL SECTION.                                              
070600                                                                          
070700     SET STATUS-IX TO 1                                                   
070800     SEARCH GODK-STATUS                                                   
070900       AT END                                                             
071000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
071100         DELIMITED BY SIZE INTO FELTEXT                                   
071200         CALL FELLOG                                                      
071300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
071400         CONTINUE                                                         
071500     END-SEARCH                                                           
071600     .                                                                    
