000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3031400.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   93/12/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR ARTIKELNS RABATTER OCH PRIS.                               
000900*                                                                         
001000*        PROGRAMMET LÄSER WDD3 BENÄMNINGSREGISTRET                        
001100*        PROGRAMMET LÄSER WDC1 PRISREGISTRET                              
001200*        PROGRAMMET LÄSER WDC1A PRISREGISTER                              
001300*        PROGRAMMET LÄSER WDB1 BETALARREGISTRET                           
001400*        PROGRAMMET LÄSER WDK6 ARTIKELREGISTRET                           
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W3T314                                              
001800*        MID:         W3I31401                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W3O31401                                            
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W3031400'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004200                                                                          
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004500 77  WS-IDARTNR-FEL              PIC X(9)    VALUE SPACE.                 
004600 77  WS-IDMARKBO                 PIC X(1)    VALUE SPACE.                 
004700 77  WS-KDARTKAM                 PIC X(5)    VALUE SPACE.                 
005100 77  WS-KDVALISO-MC              PIC X(3)    VALUE 'SEK'.                 
005600                                                                          
005700 01  WS-IDDISTR                  PIC X(5)    VALUE SPACE.                 
005800 01  FILLER REDEFINES WS-IDDISTR.                                         
005900     03 FILLER                   PIC X(1).                                
006000     03 WS-IDDISTR-IN            PIC X(4).                                
006100                                                                          
006200                                                                          
006300 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
006400 01  FILLER REDEFINES WS-IDPROMR.                                         
006500     03  WS-MARKBOLAG            PIC X.                                   
006600     03  FILLER                  PIC X(2).                                
006700                                                                          
006800 77  LAES-SW                     PIC X       VALUE 'J'.                   
006900     88  LAES-UNIK                           VALUE 'J'.                   
007000     88  LAES-SEKUND                         VALUE 'N'.                   
007100                                                                          
007200 77  PRISOMR-SW                  PIC X       VALUE 'J'.                   
007300     88  PRISOMR-FINNS                       VALUE 'J'.                   
007400     88  PRISOMR-SAKNAS                      VALUE 'N'.                   
007500                                                                          
007600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007700     88  NYCKLAR-OK                          VALUE 'J'.                   
007800     88  NYCKLAR-FEL                         VALUE 'N'.                   
007900                                                                          
008000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008100     88  EGEN-MID                            VALUE '3314'.                
008200     88  GODK-MID                            VALUE '3314'.                
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008600 01  GENERELLA-SUBPROGRAM.                                                
008700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009300*01 -COPY WMSGINIT                                                        
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009600*01 -COPY WMEDAREA                                                        
009700     SKIP3                                                                
009800 01  MESSAGE-CODES.                                                       
009900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010200     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
010300     03  ERR-TRANSFER-MISSING    PIC X(3)    VALUE '237'.                 
010400     EJECT                                                                
010500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010800     SKIP3                                                                
010900*01  MID -COPY W3I31401                                                   
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011200     SKIP3                                                                
011300*01  -COPY WMSGAREA                                                       
011400     EJECT                                                                
011500     03  MOD REDEFINES MSG-AREA.                                          
011600*      05  -COPY W3O31401                                                 
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011900     SKIP3                                                                
012000*01  -COPY WMFSAREA                                                       
012100     EJECT                                                                
012200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012300*                                                                         
012400     SKIP2                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600     SKIP3                                                                
012700 01  NYCKLAR-TILL-DLI.                                                    
012800     03  W-WDC101KY-X.                                                    
012900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013000         05  W-IDMARKBO          PIC X       VALUE SPACE.                 
013100     03  W-WDC1A1KY-MIN-X.                                                
013200         05  W-KDARTKAM-MIN      PIC  9(5)   VALUE ZERO.                  
013300         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
013400         05  W-IDMARKBO-MIN      PIC X       VALUE SPACE.                 
013500     03  W-WDC1A1KY-MAX-X.                                                
013600         05  W-KDARTKAM-MAX      PIC  9(5)   VALUE  99999.                
013700         05  W-IDARTNR-MAX       PIC S9(9)                                
013800                                 VALUE +999999999 COMP-3.                 
013900         05  W-IDMARKBO-MAX      PIC X       VALUE SPACE.                 
014000                                                                          
014100     03  W-IDARTNR-X.                                                     
014200         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
014300     03  W-KDARTKAM-X.                                                    
014400         05  W-KDARTKAM          PIC  9(5)   VALUE ZERO.                  
014500     03  W-IDSKYLT-X.                                                     
014600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
014700     03  W-IDARTNR-WDK6-X.                                                
014800         05  W-IDARTNR-WDK6      PIC S9(9)   VALUE ZERO COMP-3.           
014900     SKIP2                                                                
015000                                                                          
015100*   NYCKLAR TILL KUNDREG             ***********                          
015200                                                                          
015300     03  W-IDGMT-MIN-X.                                                   
015400         05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.           
015500         05  W-IDKUNDNR-B1       PIC S9(7)   VALUE ZERO COMP-3.           
015600                                                                          
015700     03  W-IDGMT-MAX-X.                                                   
015800         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
015900         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE 9999999                
016000                                                        COMP-3.           
016100                                                                          
016200     03  W-IDDISTR-X.                                                     
016300         05  WA-IDDISTR  PIC S9(5)           COMP-3.                      
016400                                                                          
016500*   NYCKLAR TILL BETALNINGSREGISTRET ***********                          
016600     03  W-WDB101KY-X.                                                    
016700         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
016800         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
016900                                                                          
017000     SKIP2                                                                
017100*    --- STATUS-KOD FRÅN IMS                                              
017200 01  STATUS-WS                   PIC XX.                                  
017300     88  SEGMENT-FINNS                       VALUE '  '.                  
017400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(96).                               
018100 01  SSA2                        PIC X(64).                               
018200     EJECT                                                                
018300*    --- IMS FUNKTIONSKODER                                               
018400*01  -COPY W0003                                                          
018500     EJECT                                                                
018600*    ---  DLI INPUT-OUTPUT AREA                                           
018700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018800     SKIP3                                                                
018900 01  DLI-IO-AREA.                                                         
019000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
019100     SKIP3                                                                
019200     03  WDC101 REDEFINES IO-AREA.                                        
019300*        05  -COPY WDC101  -PRE WDC1-                                     
019400     EJECT                                                                
019500     03  WDC1A01 REDEFINES IO-AREA.                                       
019600*        05  -COPY WDC1A1  -PRE WDC1A-                                    
019700     EJECT                                                                
019800     03  WDD311 REDEFINES IO-AREA.                                        
019900*        05  -COPY WDD311  -PRE WDD3-                                     
020000     EJECT                                                                
020100     03  WDK601 REDEFINES IO-AREA.                                        
020200*        05  -COPY WDK601  -PRE WDK601-                                   
020300     EJECT                                                                
020400**   KUNDREGISTER                                                         
020500 01  DLI-IO-AREA2.                                                        
020600*    03  WDB201    -COPY WDB201 -PRE WDB2-                                
020700     EJECT                                                                
020800**   BETALNINGSREGISTER                                                   
020900 01  DLI-IO-AREA3.                                                        
021000     03  WDB101.                                                          
021100*        05  -COPY WDB101  -PRE WDB1-                                     
021200     EJECT                                                                
021300 LINKAGE SECTION.                                                         
021400                                                                          
021500*01  -COPY W0009   -PRE MSG-                                              
021600     EJECT                                                                
021700*01  -COPY W0008  -PRE USEA-                                              
021800     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000*01  -COPY W0008  -PRE WDC1-                                              
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008  -PRE WDD3-                                              
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008  -PRE WDC1A-                                             
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE WDK6-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE WDB2-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008  -PRE WDB1-                                              
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
023900                                   WDC1-PCB WDD3-PCB WDC1A-PCB            
024000                           WDK6-PCB WDB2-PCB WDB1-PCB.                    
024100 MAIN SECTION.                                                            
024200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
024300                                   WDC1-PCB WDD3-PCB WDC1A-PCB            
024400                           WDK6-PCB WDB2-PCB WDB1-PCB.                    
024500                                                                          
024600     PERFORM IMS-GET-MSG                                                  
024700     IF SEGMENT-FINNS                                                     
024800       PERFORM A-INIT                                                     
024900       PERFORM B-KOLLA-NYCKLAR                                            
025000       IF NYCKLAR-OK                                                      
025100           IF MFS-FIRST                                                   
025200             PERFORM C-FOERSTA-SIDA                                       
025300           ELSE                                                           
025400             IF MFS-NEXT                                                  
025500               PERFORM D-NAESTA-SIDA                                      
025600             ELSE                                                         
025700               PERFORM E-SAMMA-SIDA                                       
025800             END-IF                                                       
025900           END-IF                                                         
026000         PERFORM F-LAES-VISA-INFO                                         
026100       END-IF                                                             
026200       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O31401 + 4                      
026300       PERFORM IMS-INSERT-MSG                                             
026400     END-IF                                                               
026500                                                                          
026600     MOVE ZERO TO RETURN-CODE                                             
026700     GOBACK                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 A-INIT SECTION.                                                          
027100                                                                          
027200     IF MSG-DUBBLA-TRANSKODER                                             
027300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31401                 
027400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027600     ELSE                                                                 
027700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31401                  
027800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
028000     END-IF                                                               
028100                                                                          
028200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028500                                                                          
028600     MOVE LOW-VALUE TO MSG-AREA                                           
028700     MOVE 'W3O314N1' TO MFS-IDMOD                                         
028800     MOVE '3314' TO MOD-IDTRANS                                           
028900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029000                                                                          
029100     IF EGEN-MID OR HELP-MID                                              
029200       CONTINUE                                                           
029300     ELSE                                                                 
029400       MOVE SPACE TO MFS-KDTRTYP                                          
029500       MOVE '7' TO MFS-IDPFK                                              
029600     END-IF                                                               
029700                                                                          
029800     IF ENGLISH-TEXT                                                      
029900       MOVE +2 TO SPRAK-IX                                                
030000       MOVE 'GB ' TO MED-IDSKYLT                                          
030100       MOVE 'GB ' TO W-IDSKYLT                                            
030200     ELSE                                                                 
030300       MOVE +1 TO SPRAK-IX                                                
030400       MOVE 'S  ' TO MED-IDSKYLT                                          
030500       MOVE 'S  ' TO W-IDSKYLT                                            
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 B-KOLLA-NYCKLAR SECTION.                                                 
031000                                                                          
031100     MOVE JA TO NYCKLAR-SW                                                
031200     MOVE JA TO PRISOMR-SW                                                
031300                                                                          
031400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
031500     MOVE '001'             TO MSGI-KDCALL                                
031600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
031800     MOVE '3314'            TO MSGI-IDTRANS                               
031900                                                                          
032000     IF MFS-IDTRANS = '3314'                                              
032100       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
032200       MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                                
032300     ELSE                                                                 
032400       MOVE SPACE          TO MID-IDPROMR-IN                              
032500                              MID-IDDISTR-IN                              
032600                              MID-KDARTKAM-IN                             
032700     END-IF                                                               
032800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032900                                                                          
033000*    -- KONTROLL AV PRISOMRÅDE                                            
033100     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                               
033200                                                                          
033300     IF MID-IDPROMR-IN = ALL '+'                                          
033400       MOVE MID-IDPROMR-UT TO WS-IDPROMR                                  
033500     ELSE                                                                 
033600       MOVE SPACE       TO MID-IDDISTR-UT                                 
033700       MOVE MID-IDPROMR-IN TO WS-IDPROMR                                  
033800       MOVE '7'         TO MFS-IDPFK                                      
033900       MOVE SPACE       TO MFS-KDTRTYP                                    
034000     END-IF                                                               
034100                                                                          
034200     IF WS-IDPROMR NOT = SPACE                                            
034300       MOVE WS-MARKBOLAG  TO  W-IDMARKBO                                  
034400     END-IF                                                               
034500                                                                          
034600*    -- KONTROLL AV IDARTNR                                               
034700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
034800                                                                          
034900     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
035000                          WS-IDARTNR-FEL                                  
035100     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
035200     MOVE WS-IDARTNR          TO MOD-IDARTNR-UT                           
035300     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
035400                                                                          
035500     IF WS-IDARTNR NOT NUMERIC                                            
035600         MOVE ZERO TO WS-IDARTNR                                          
035700     END-IF                                                               
035800                                                                          
035900     IF MID-IDARTNR-IN = ALL '+'                                          
036000       CONTINUE                                                           
036100     ELSE                                                                 
036200                                                                          
036300* KDARTKAM NOLLAS FÖR ATT KUNNA SÖKA BARA PÅ ARTNR OM DET ÄR SÅ           
036400* ATT FÖREGÅENDE SÖKNING VAR MED KDARTKAM.                                
036500                                                                          
036600       MOVE ZERO  TO MID-KDARTKAM-UT                                      
036700                                                                          
036800*                                                                         
036900*                                                                         
037000       MOVE '7'         TO MFS-IDPFK                                      
037100       MOVE SPACE       TO MFS-KDTRTYP                                    
037200     END-IF                                                               
037300                                                                          
037400     IF WS-IDARTNR NUMERIC                                                
037500       IF GODK-MID                                                        
037600         IF WS-IDARTNR = ZERO                                             
037700           MOVE ZERO TO W-IDARTNR                                         
037800         ELSE                                                             
037900           MOVE WS-IDARTNR TO W-IDARTNR                                   
038000                              W-IDARTNR-WDD3                              
038100                              W-IDARTNR-WDK6                              
038200         END-IF                                                           
038300       ELSE                                                               
038400         MOVE NEJ TO NYCKLAR-SW                                           
038500       END-IF                                                             
038600     ELSE                                                                 
038700       MOVE NEJ TO NYCKLAR-SW                                             
038800     END-IF                                                               
038900                                                                          
039000*    -- KONTROLL AV KAMPANJRABATT/TRANSFERKOD                             
039100     MOVE MFS-RENSA-FAELT TO MOD-KDARTKAM-IN                              
039200                                                                          
039300     IF MID-KDARTKAM-IN = ALL '+'                                         
039400       MOVE MID-KDARTKAM-UT TO WS-KDARTKAM                                
039500       INSPECT WS-KDARTKAM REPLACING LEADING SPACE BY ZERO                
039600     ELSE                                                                 
039700       MOVE MID-KDARTKAM-IN TO WS-KDARTKAM                                
039800       IF WS-KDARTKAM NUMERIC                                             
039900         MOVE ZERO            TO WS-IDARTNR                               
040000       END-IF                                                             
040100       MOVE '7'         TO MFS-IDPFK                                      
040200       MOVE SPACE       TO MFS-KDTRTYP                                    
040300     END-IF                                                               
040400                                                                          
040500     IF NOT EGEN-MID                                                      
040600       MOVE ZERO TO WS-KDARTKAM                                           
040700     END-IF                                                               
040800                                                                          
040900     IF WS-KDARTKAM NUMERIC                                               
041000       IF WS-KDARTKAM = ZERO                                              
041100         MOVE JA TO LAES-SW                                               
041200         MOVE ZERO TO W-KDARTKAM                                          
041300       ELSE                                                               
041400         MOVE NEJ TO LAES-SW                                              
041500         MOVE WS-KDARTKAM TO W-KDARTKAM                                   
041600                             W-KDARTKAM-MIN                               
041700                             W-KDARTKAM-MAX                               
041800       END-IF                                                             
041900     ELSE                                                                 
042000       IF WS-IDARTNR NOT = ZERO                                           
042100         MOVE JA TO LAES-SW                                               
042200       END-IF                                                             
042300     END-IF                                                               
042400                                                                          
042500*    -- KONTROLL AV IDDISTR                                               
042600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
042700*    MOVE MSGI-IDDISTR TO WS-IDDISTR-IN                                   
042800                                                                          
042900     IF MID-IDDISTR-IN = ALL '+'                                          
043000       MOVE MID-IDDISTR-UT TO WS-IDDISTR-IN                               
043100     ELSE                                                                 
043200       MOVE MID-IDDISTR-IN TO WS-IDDISTR-IN                               
043300       MOVE '7'         TO MFS-IDPFK                                      
043400       MOVE SPACE       TO MFS-KDTRTYP                                    
043500     END-IF                                                               
043600                                                                          
043700     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
043800                                                                          
043900     IF WS-IDDISTR NUMERIC                                                
044000       IF WS-IDDISTR > ZERO                                               
044100         MOVE WS-IDDISTR TO WA-IDDISTR                                    
044200                                                                          
044300                            W-IDDISTR-B1                                  
044400                            W-IDDISTR-B2                                  
044500       END-IF                                                             
044600     ELSE                                                                 
044700       MOVE NEJ TO NYCKLAR-SW                                             
044800     END-IF                                                               
044900                                                                          
045000                                                                          
045100     IF WS-IDARTNR = ZERO AND                                             
045200        WS-KDARTKAM = ZERO                                                
045300        MOVE NEJ TO NYCKLAR-SW                                            
045400     END-IF                                                               
045500                                                                          
045600     IF WS-IDPROMR NOT = SPACE                                            
045700        IF WS-IDDISTR NUMERIC                                             
045800           IF WS-IDDISTR > ZERO                                           
045900              MOVE NEJ TO PRISOMR-SW                                      
046000           END-IF                                                         
046100        END-IF                                                            
046200     ELSE                                                                 
046300        IF WS-IDDISTR NUMERIC                                             
046400           IF WS-IDDISTR  > ZERO                                          
046500              MOVE NEJ TO PRISOMR-SW                                      
046600           ELSE                                                           
046700              MOVE NEJ TO NYCKLAR-SW                                      
046800           END-IF                                                         
046900        ELSE                                                              
047000           MOVE NEJ TO NYCKLAR-SW                                         
047100        END-IF                                                            
047200     END-IF                                                               
047300                                                                          
047400     IF EGEN-MID OR GODK-MID                                              
047500        CONTINUE                                                          
047600     ELSE                                                                 
047700        MOVE NEJ TO NYCKLAR-SW                                            
047800     END-IF                                                               
047900                                                                          
048000     IF NYCKLAR-OK                                                        
048100       IF EGEN-MID OR GODK-MID                                            
048200         IF PRISOMR-SAKNAS                                                
048300            MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                        
048400            MOVE SPACE           TO  W-IDMARKBO                           
048500         ELSE                                                             
048600            MOVE WS-IDPROMR      TO MOD-IDPROMR-UT                        
048700         END-IF                                                           
048800         MOVE WS-KDARTKAM     TO MOD-KDARTKAM-UT                          
048900         INSPECT MOD-KDARTKAM-UT REPLACING LEADING ZERO BY SPACE          
049000         MOVE WS-IDDISTR-IN TO MOD-IDDISTR-UT                             
049100         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
049200       ELSE                                                               
049300         MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                           
049400         MOVE MFS-RENSA-FAELT TO MOD-KDARTKAM-UT                          
049500         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
049600       END-IF                                                             
049700     ELSE                                                                 
049800       IF EGEN-MID OR GODK-MID                                            
049900          MOVE WS-IDPROMR    TO MOD-IDPROMR-UT                            
050000          MOVE WS-KDARTKAM     TO MOD-KDARTKAM-UT                         
050100          INSPECT MOD-KDARTKAM-UT REPLACING LEADING ZERO BY SPACE         
050200          MOVE WS-IDDISTR-IN TO MOD-IDDISTR-UT                            
050300          INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
050400       ELSE                                                               
050500          MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                          
050600          MOVE MFS-RENSA-FAELT TO MOD-KDARTKAM-UT                         
050700          MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                          
050800       END-IF                                                             
050900     END-IF                                                               
051000                                                                          
051100     IF NYCKLAR-FEL                                                       
051200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
051300       CALL WMEDKONV USING MED-WMEDAREA                                   
051400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
051500       PERFORM MFS-RENSA-FAELT-UT                                         
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 C-FOERSTA-SIDA SECTION.                                                  
052000                                                                          
052100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
052200     CALL WMEDKONV USING MED-WMEDAREA                                     
052300     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
052400     .                                                                    
052500     EJECT                                                                
052600 D-NAESTA-SIDA SECTION.                                                   
052700                                                                          
052800     MOVE LOW-VALUE         TO W-WDC1A1KY-MIN-X                           
052900     MOVE HIGH-VALUE        TO W-WDC1A1KY-MAX-X                           
053000     MOVE WS-KDARTKAM       TO W-KDARTKAM                                 
053100                               W-KDARTKAM-MIN                             
053200                               W-KDARTKAM-MAX                             
053300     MOVE MID-IDARTNR-NEXT  TO W-IDARTNR                                  
053400                               W-IDARTNR-MIN                              
053500     MOVE MID-IDMARKBO-NEXT TO W-IDMARKBO                                 
053600     .                                                                    
053700     EJECT                                                                
053800 E-SAMMA-SIDA SECTION.                                                    
053900                                                                          
054000     IF EGEN-MID OR HELP-MID                                              
054100       MOVE LOW-VALUE         TO W-WDC1A1KY-MIN-X                         
054200       MOVE HIGH-VALUE        TO W-WDC1A1KY-MAX-X                         
054300       MOVE WS-KDARTKAM        TO W-KDARTKAM                              
054400                                  W-KDARTKAM-MIN                          
054500                                  W-KDARTKAM-MAX                          
054600       MOVE MID-IDARTNR-ENTER  TO W-IDARTNR                               
054700                                  W-IDARTNR-MIN                           
054800       MOVE MID-IDMARKBO-ENTER TO W-IDMARKBO                              
054900     END-IF                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 F-LAES-VISA-INFO SECTION.                                                
055300                                                                          
055400     IF PRISOMR-SAKNAS                                                    
055500        PERFORM FB-HT-PROM-VIA-DISTRIKT                                   
055600     END-IF                                                               
055700                                                                          
055800     IF LAES-UNIK                                                         
055900       PERFORM FA-LAES-UNIK                                               
056000     ELSE                                                                 
056100       PERFORM IMS-GN-WDC1                                                
056200       IF SEGMENT-SAKNAS                                                  
056300         MOVE ERR-TRANSFER-MISSING TO MED-IDMFSFEL                        
056400         CALL WMEDKONV USING MED-WMEDAREA                                 
056500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
056600         PERFORM MFS-RENSA-FAELT-UT                                       
056700       ELSE                                                               
056800         MOVE +1 TO INDX                                                  
056900         IF SEGMENT-FINNS                                                 
057000           MOVE WDC1A-SEQA-IDARTNR TO MOD-IDARTNR-ENTER                   
057100                                     W-IDARTNR                            
057200           MOVE WDC1A-SEQA-IDMARKBO TO MOD-IDMARKBO-ENTER                 
057300                                      W-IDMARKBO                          
057400           PERFORM IMS-GU-WDC101                                          
057500         ELSE                                                             
057600           MOVE ZERO           TO MOD-IDARTNR-ENTER                       
057700           MOVE SPACE          TO MOD-IDMARKBO-ENTER                      
057800         END-IF                                                           
057900                                                                          
058000         PERFORM UNTIL INDX > MAX-INDX                                    
058100           IF SEGMENT-FINNS                                               
058200              MOVE WDC1-ART-IDARTNR     TO MOD-IDARTNR     (INDX)         
058300                                           W-IDARTNR-WDD3                 
058400                                           W-IDARTNR-WDK6                 
                    IF WDB1-BET-FLARTRAB = 'J'                                  
                      MOVE WDC1-ART-KDARTRAB-ALT                                
                                              TO MOD-NORMAL-RAB  (INDX)         
                    ELSE                                                        
                      MOVE WDC1-ART-KDARTRAB  TO MOD-NORMAL-RAB  (INDX)         
                    END-IF                                                      
058600              MOVE WDC1-ART-KDARTKAM    TO MOD-KAMPANJ-RAB (INDX)         
058700              MOVE WDC1-ART-PRARTBTO-MARK TO                              
058800                   MOD-RETAIL-PRIS (INDX)                                 
058900              PERFORM S06-HAMTA-MARKNADSVALUTA                            
059000              MOVE WS-KDVALISO-MC       TO MOD-KDVALISO    (INDX)         
059100              PERFORM IMS-GU-BENAEMNING                                   
059200              IF SEGMENT-FINNS                                            
059300                MOVE WDD3-TEXT-BEART TO MOD-BENAEMNING     (INDX)         
059400              ELSE                                                        
059500                MOVE MFS-RENSA-FAELT TO MOD-BENAEMNING     (INDX)         
059600              END-IF                                                      
059700              PERFORM IMS-GU-WDK601                                       
059800              IF SEGMENT-FINNS                                            
059900                MOVE WDK601-ART-IDFKNGRP TO MOD-IDFKNGRP   (INDX)         
060000                MOVE WDK601-ART-KDPRODSL TO MOD-KDPRODSL   (INDX)         
060100              ELSE                                                        
060200                MOVE ZERO            TO MOD-IDFKNGRP       (INDX)         
060300                MOVE ZERO            TO MOD-KDPRODSL       (INDX)         
060400              END-IF                                                      
060500                                                                          
060600              PERFORM IMS-GN-WDC1                                         
060700              IF SEGMENT-FINNS                                            
060800                MOVE WDC1A-SEQA-IDARTNR TO W-IDARTNR                      
060900                MOVE WDC1A-SEQA-IDMARKBO TO W-IDMARKBO                    
061000                PERFORM IMS-GU-WDC101                                     
061100              END-IF                                                      
061200              ADD +1 TO INDX                                              
061300           ELSE                                                           
061400             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR            (INDX)        
061500                                     MOD-IDFKNGRP           (INDX)        
061600                                     MOD-KDPRODSL           (INDX)        
061700                                     MOD-NORMAL-RAB         (INDX)        
061800                                     MOD-KAMPANJ-RAB        (INDX)        
061900                                     MOD-RETAIL-PRIS        (INDX)        
062000                                     MOD-KDVALISO           (INDX)        
062100                                     MOD-BENAEMNING         (INDX)        
062200             ADD 1 TO INDX                                                
062300           END-IF                                                         
062400         END-PERFORM                                                      
062500                                                                          
062600         IF SEGMENT-FINNS                                                 
062700           MOVE WDC1-ART-IDARTNR TO MOD-IDARTNR-NEXT                      
062800           MOVE WDC1-ART-IDMARKBO TO MOD-IDMARKBO-NEXT                    
062900           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
063000           CALL WMEDKONV USING MED-WMEDAREA                               
063100           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
063200         ELSE                                                             
063300           MOVE ZERO           TO MOD-IDARTNR-NEXT                        
063400           MOVE SPACE          TO MOD-IDMARKBO-NEXT                       
063500         END-IF                                                           
063600       END-IF                                                             
063700                                                                          
063800     END-IF                                                               
063900     .                                                                    
064000     EJECT                                                                
064100                                                                          
064200 FA-LAES-UNIK SECTION.                                                    
064300                                                                          
064400     PERFORM IMS-GU-WDC101                                                
064500     IF SEGMENT-SAKNAS                                                    
064600       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
064700       CALL WMEDKONV USING MED-WMEDAREA                                   
064800       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
064900       PERFORM MFS-RENSA-FAELT-UT                                         
065000     ELSE                                                                 
065100       MOVE +1 TO INDX                                                    
065200       IF SEGMENT-FINNS                                                   
065300         MOVE WDC1-ART-IDARTNR    TO MOD-IDARTNR-ENTER                    
065400         MOVE WDC1-ART-IDMARKBO   TO MOD-IDMARKBO-ENTER                   
065500       ELSE                                                               
065600         MOVE ZERO                TO MOD-IDARTNR-ENTER                    
065700         MOVE SPACE               TO MOD-IDMARKBO-ENTER                   
065800       END-IF                                                             
065900                                                                          
066000       IF SEGMENT-FINNS                                                   
066100         MOVE WDC1-ART-IDARTNR         TO MOD-IDARTNR     (INDX)          
               IF WDB1-BET-FLARTRAB = 'J'                                       
066200           MOVE WDC1-ART-KDARTRAB-ALT  TO MOD-NORMAL-RAB  (INDX)          
               ELSE                                                             
066200           MOVE WDC1-ART-KDARTRAB      TO MOD-NORMAL-RAB  (INDX)          
               END-IF                                                           
066300         MOVE WDC1-ART-KDARTKAM        TO MOD-KAMPANJ-RAB (INDX)          
066400         MOVE WDC1-ART-PRARTBTO-MARK   TO MOD-RETAIL-PRIS (INDX)          
066500         PERFORM S06-HAMTA-MARKNADSVALUTA                                 
066600         MOVE WS-KDVALISO-MC           TO MOD-KDVALISO    (INDX)          
066700         PERFORM IMS-GU-BENAEMNING                                        
066800         IF SEGMENT-FINNS                                                 
066900           MOVE WDD3-TEXT-BEART        TO MOD-BENAEMNING  (INDX)          
067000         ELSE                                                             
067100           MOVE MFS-RENSA-FAELT        TO MOD-BENAEMNING  (INDX)          
067200         END-IF                                                           
067300         PERFORM IMS-GU-WDK601                                            
067400         IF SEGMENT-FINNS                                                 
067500           MOVE WDK601-ART-IDFKNGRP    TO MOD-IDFKNGRP    (INDX)          
067600           MOVE WDK601-ART-KDPRODSL    TO MOD-KDPRODSL    (INDX)          
067700         ELSE                                                             
067800           MOVE MFS-RENSA-FAELT        TO MOD-IDFKNGRP    (INDX)          
067900           MOVE MFS-RENSA-FAELT        TO MOD-KDPRODSL    (INDX)          
068000         END-IF                                                           
068100       END-IF                                                             
068200       ADD +1 TO INDX                                                     
068300       PERFORM UNTIL INDX > MAX-INDX                                      
068400         MOVE MFS-RENSA-FAELT          TO MOD-IDARTNR     (INDX)          
068500                                          MOD-IDFKNGRP    (INDX)          
068600                                          MOD-KDPRODSL    (INDX)          
068700                                        MOD-NORMAL-RAB    (INDX)          
068800                                        MOD-KAMPANJ-RAB   (INDX)          
068900                                        MOD-RETAIL-PRIS   (INDX)          
069000                                        MOD-KDVALISO      (INDX)          
069100                                        MOD-BENAEMNING    (INDX)          
069200         ADD 1 TO INDX                                                    
069300       END-PERFORM                                                        
069400     END-IF                                                               
069500     .                                                                    
069600     EJECT                                                                
069700                                                                          
069800 FB-HT-PROM-VIA-DISTRIKT SECTION.                                         
069900                                                                          
070000     PERFORM IMS-GET-WDB201                                               
070100     IF SEGMENT-FINNS                                                     
070200       MOVE WDB2-GMT-IDPARTNR    TO W-WDB1-IDPARTNR                       
070300       MOVE WDB2-GMT-IDFTG       TO W-WDB1-IDFTG                          
070400       PERFORM IMS-GU-WDB101                                              
070500       IF SEGMENT-FINNS                                                   
070600         MOVE WDB1-BET-IDPROMR   TO WS-IDPROMR                            
070700         MOVE WS-MARKBOLAG       TO W-IDMARKBO                            
070800       END-IF                                                             
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200 S06-HAMTA-MARKNADSVALUTA  SECTION.                                       
071300                                                                          
071400     IF W-IDMARKBO = SPACE                                                
073400       MOVE SPACE                  TO  WS-KDVALISO-MC                     
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900 MFS-RENSA-FAELT-UT SECTION.                                              
074000                                                                          
074100*    --- ALLA UTDATA-FÄLT                                                 
074200*    --- INKL. BLÄDDRINGSNYCKLAR                                          
074300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-ENTER                            
074400                             MOD-IDARTNR-NEXT                             
074500                             MOD-IDMARKBO-ENTER                           
074600                             MOD-IDMARKBO-NEXT                            
074700     MOVE +1 TO INDX                                                      
074800     PERFORM UNTIL INDX > MAX-INDX                                        
074900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
075000       ADD +1 TO INDX                                                     
075100     END-PERFORM                                                          
075200     .                                                                    
075300     SKIP2                                                                
075400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
075500                                                                          
075600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
075700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR     (INDX)                       
075800                             MOD-IDFKNGRP    (INDX)                       
075900                             MOD-KDPRODSL    (INDX)                       
076000                             MOD-NORMAL-RAB  (INDX)                       
076100                             MOD-KAMPANJ-RAB (INDX)                       
076200                             MOD-RETAIL-PRIS (INDX)                       
076300                             MOD-KDVALISO    (INDX)                       
076400                             MOD-BENAEMNING  (INDX)                       
076500     .                                                                    
076600     EJECT                                                                
076700* --- IMS SEKTIONER ---                                                   
076800     SKIP3                                                                
076900 IMS-GET-MSG SECTION.                                                     
077000                                                                          
077100     MOVE '  QC' TO GODK-STATUSKODER                                      
077200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
077300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077400     PERFORM IMS-STATUSKONTROLL                                           
077500     .                                                                    
077600     SKIP3                                                                
077700 IMS-INSERT-MSG SECTION.                                                  
077800                                                                          
077900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
078000       MOVE '0' TO MFS-KDHUVOMR                                           
078100     END-IF                                                               
078200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
078300     MOVE SPACE TO GODK-STATUSKODER                                       
078400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
078500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
078600     PERFORM IMS-STATUSKONTROLL                                           
078700     .                                                                    
078800     EJECT                                                                
078900 IMS-GU-WDC101 SECTION.                                                   
079000     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
079100          DELIMITED BY SIZE INTO SSA1                                     
079200     MOVE '  GE' TO GODK-STATUSKODER                                      
079300     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-AREA SSA1                      
079400     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
079500     PERFORM IMS-STATUSKONTROLL                                           
079600     .                                                                    
079700     SKIP2                                                                
079800 IMS-GN-WDC1 SECTION.                                                     
079900     STRING 'WDC1A1  (WDC1A1KY>=' W-WDC1A1KY-MIN-X                        
080000                    '&WDC1A1KY<=' W-WDC1A1KY-MAX-X                        
080100                    '&IDMARKBO =' W-IDMARKBO ')'                          
080200          DELIMITED BY SIZE INTO SSA1                                     
080300     MOVE '  GE' TO GODK-STATUSKODER                                      
080400     CALL CBLTDLI USING GN WDC1A-PCB DLI-IO-AREA SSA1                     
080500     MOVE WDC1A-STATUS-CODE TO STATUS-WS                                  
080600     PERFORM IMS-STATUSKONTROLL                                           
080700     .                                                                    
080800     EJECT                                                                
080900 IMS-GU-BENAEMNING SECTION.                                               
081000     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
081100          DELIMITED BY SIZE INTO SSA1                                     
081200     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
081300          DELIMITED BY SIZE INTO SSA2                                     
081400     MOVE '  GE' TO GODK-STATUSKODER                                      
081500     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA SSA1 SSA2                 
081600     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
081700     PERFORM IMS-STATUSKONTROLL                                           
081800     .                                                                    
081900     EJECT                                                                
082000 IMS-GU-WDK601 SECTION.                                                   
082100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
082200          DELIMITED BY SIZE INTO SSA1                                     
082300     MOVE '  GE' TO GODK-STATUSKODER                                      
082400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1                      
082500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
082600     PERFORM IMS-STATUSKONTROLL                                           
082700     .                                                                    
082800     SKIP2                                                                
082900 IMS-GET-WDB201    SECTION.                                               
083000                                                                          
083100     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
083200                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
083300             DELIMITED BY SIZE INTO SSA1                                  
083400     MOVE '  GE' TO GODK-STATUSKODER                                      
083500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA2 SSA1                     
083600     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
083700     PERFORM IMS-STATUSKONTROLL                                           
083800     .                                                                    
083900     SKIP2                                                                
084000 IMS-GU-WDB101 SECTION.                                                   
084100     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
084200          DELIMITED BY SIZE INTO SSA1                                     
084300     MOVE '  GE' TO GODK-STATUSKODER                                      
084400     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA3 SSA1                     
084500     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     .                                                                    
084800     SKIP3                                                                
084900 IMS-STATUSKONTROLL SECTION.                                              
085000                                                                          
085100     SET STATUS-IX TO 1                                                   
085200     SEARCH GODK-STATUS                                                   
085300       AT END                                                             
085400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
085500         DELIMITED BY SIZE INTO FELTEXT                                   
085600         CALL FELLOG                                                      
085700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
085800         CONTINUE                                                         
085900     END-SEARCH                                                           
086000     .                                                                    
