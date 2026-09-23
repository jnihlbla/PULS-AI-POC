000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4032100.                                                
000400 AUTHOR.         M LUNDBERG.                                              
000500 DATE-WRITTEN.   JAN  1986.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        EJ RAPP RADER PER ORDER.                                         
001100*        NOT REPORTED LINES PER ORDER                                     
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T321                                              
001500*        MID:         W4I32101                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O32101                                            
001900*    SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77   PROGRAM-NAMN           VALUE 'W4032100'                             
002800                                 PIC X(8).                                
002900 77    JA                        PIC X       VALUE 'J'.                   
003000 77    NEJ                       PIC X       VALUE 'N'.                   
003100 77    BORTTAG                   PIC X       VALUE 'B'.                   
003200 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
003300 77    SPRAK-INDX                PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77    WS-RAD-IND                PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77    MOD-RAD-IND               PIC S9(9)   VALUE +0   COMP SYNC.        
003600 77    MAX-LINE                  PIC S9(9)   VALUE +13  COMP SYNC.        
003700 77    WS-SPAR-KDORDSTA          PIC S9(1)              COMP-3.           
003800 77    WS-JFR-IDPRODNR           PIC S9(7)              COMP-3.           
003900 77    WS-IDKUNDRF               PIC X(10).                               
004000 77    WS-KVORAPP                PIC S9(7) VALUE ZERO COMP-3.             
004100 77    WS-SPAR-IDPURAD           PIC S9(5) VALUE ZERO COMP-3.             
004200 77    WS-START-IDPURAD          PIC S9(5) VALUE ZERO COMP-3.             
004300 77    WS-STOPP-IDPURAD          PIC S9(5) VALUE ZERO COMP-3.             
004400 77    WS-SPAR-KVORDRAD-LEVPLC1  PIC S9(5) VALUE ZERO COMP-3.             
004500 77    WS-SPAR-KVORDRAD-LEVPLC2  PIC S9(5) VALUE ZERO COMP-3.             
004600 01  W-SPAR-IDKUNDRF.                                                     
004700     03  FILLER                  PIC X(2)    VALUE '00'.                  
004800     03  W-SPAR-IDORDNR5         PIC X(5)    VALUE '+++++'.               
004900     03  FILLER                  PIC X(3)    VALUE '+++'.                 
005000                                                                          
005100*77    WS-SPAR-KVORDRAD-LEVPL    PIC S9(5) VALUE ZERO COMP-3.             
005200*LINE DELETED                                                             
005300     SKIP2                                                                
005400*      - - - - - - - - - - - - - *****                                    
005500 77    WS-KDMFSFOR               PIC 9       VALUE ZERO.                  
005600 77    WS-KDFEL                  PIC 9(2)    VALUE ZERO COMP-3.           
005700     SKIP2                                                                
005800 01    WS-IDPRODNR                        PIC X(7).                       
005900 01    IDPRODNR-WS REDEFINES WS-IDPRODNR  PIC 9(7).                       
006000     SKIP2                                                                
006100 01    WS-IDDISTR                         PIC X(4).                       
006200 01    IDDISTR-WS REDEFINES WS-IDDISTR    PIC 9(4).                       
006300     SKIP2                                                                
006400 01    WS-IDKUNDNR                        PIC X(6).                       
006500 01    IDKUNDNR-WS REDEFINES WS-IDKUNDNR  PIC 9(6).                       
006600     SKIP2                                                                
006700 01    WS-IDORDNR                         PIC X(5).                       
006800 01    IDORDNR-WS REDEFINES WS-IDORDNR    PIC 9(5).                       
006900     SKIP2                                                                
007000 01    WS-IDKOLLI                         PIC X(5).                       
007100 01    IDKOLLI-WS REDEFINES WS-IDKOLLI    PIC 9(5).                       
007200     SKIP2                                                                
007300*      --- VALID IDDD CODES                                               
007400*                                                                         
007500*01    -COPY WWDC99                                                       
007600       EJECT                                                              
007700 01    WS-JFR-IDANSTNR.                                                   
007800   03  FILLER                    PIC X(3).                                
007900   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
008000     EJECT                                                                
008100 77    WS-IDRADNR-ORD-TOM        PIC 9(4).                                
008200 77    WS-ADPACOMR               PIC X(2).                                
008300     EJECT                                                                
008400 01    NYCKEL-TYP                PIC S9(2)   COMP-3.                      
008500   88  GAMMAL-NYCKEL             VALUE +1.                                
008600   88  NY-NYCKEL                 VALUE +2.                                
008700     SKIP3                                                                
008800 01    FRAN-BILD                 PIC 9(4).                                
008900   88  FRAN-BILD-OK              VALUE 4312                               
009000                                       4321 4322 4323 4324 4325.          
009100   88  FRAN-BILD-EGEN            VALUE 4321.                              
009200     SKIP3                                                                
009300 01    WS-SLINGA-KLAR            PIC X(1).                                
009400   88  SLINGA-KLAR               VALUE 'J'.                               
009500     SKIP3                                                                
009600 01    WS-FIRST-TIME             PIC X(1).                                
009700   88  FIRST-TIME                VALUE 'J'.                               
009800     SKIP3                                                                
009900 01    WS-NYCKEL-NAESTA.                                                  
010000   03  WS-IDPRODNR-NYCKEL        PIC 9(7).                                
010100   03  WS-IDANSTNR-NYCKEL        PIC 9(5).                                
010200   03  WS-IDRADNR-ORD-TOM-NYCKEL PIC 9(4).                                
010300     SKIP2                                                                
010400 01    DYNAMISKA-SUBPROGRAM.                                              
010500   03  CBLTDLI                   PIC X(8) VALUE 'CBLTDLI '.               
010600   03  FELLOG                    PIC X(8) VALUE 'FELLOG  '.               
010700   03  W005INIT                  PIC X(8) VALUE 'W005INIT'.               
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011000*01 -COPY WMSGINIT                                                        
011100     EJECT                                                                
011200 77    SW-IDPRODNR               PIC X.                                   
011300 77    SW-ORDERID                PIC X.                                   
011400     EJECT                                                                
011500 01    NYCKLAR-TILL-DLI.                                                  
011600   03    W-IDPRODNR-X.                                                    
011700     05    W-IDPRODNR            PIC S9(7)    COMP-3.                     
011800     SKIP2                                                                
011900   03    W-IDRADNR-ORD-X.                                                 
012000     05    W-IDRADNR-ORD         PIC S9(5)    COMP-3.                     
012100     SKIP2                                                                
012200   03    W-IDRADNR-ORAPP-X.                                               
012300     05    W-IDRADNR-ORAPP       PIC S9(5)    COMP-3.                     
012400     SKIP2                                                                
012500   03    W-WDE4A1-KUNDORDER-X.                                            
012600     05    W-4A1-IDDISTR             PIC S9(5)    COMP-3.                 
012700     05    W-4A1-IDKUNDNR            PIC S9(7)    COMP-3.                 
012800     05    W-4A1-IDKUNDRF.                                                
012900       07  W-4A1-IDORDNR             PIC X(5).                            
013000       07  FILLER                    PIC X(5)     VALUE SPACE.            
013100     SKIP2                                                                
013200   03    W-WDE401-KUNDORDER-X.                                            
013300     05    W-401-IDDISTR             PIC S9(5)    COMP-3.                 
013400     05    W-401-IDKUNDNR            PIC S9(7)    COMP-3.                 
013500     05    W-401-IDKUNDRF.                                                
013600       07  W-401-IDORDNR             PIC X(5).                            
013700       07  FILLER                    PIC X(5)     VALUE SPACE.            
013800     05    W-401-IDPRODNR            PIC S9(7)    COMP-3.                 
013900     05    W-401-IDPLKLST            PIC S9(3)    COMP-3.                 
014000     SKIP2                                                                
014100   03    W-OLD-KUNDORDER-X.                                               
014200     05    W-OLD-IDDISTR             PIC S9(5)    COMP-3.                 
014300     05    W-OLD-IDKUNDNR            PIC S9(7)    COMP-3.                 
014400     05    W-OLD-IDKUNDRF.                                                
014500       07  W-OLD-IDORDNR             PIC X(5).                            
014600       07  FILLER                    PIC X(5).                            
014700     05    W-OLD-IDPRODNR            PIC S9(7)    COMP-3.                 
014800     05    W-OLD-IDPLKLST            PIC S9(3)    COMP-3.                 
014900     SKIP2                                                                
015000   03    W-WDE420-KEYSEQ-X.                                               
015100     05    W-420-IDPRODNR            PIC S9(7)    COMP-3.                 
015200     05    W-420-IDPURAD             PIC S9(5)    COMP-3.                 
015300     SKIP2                                                                
015400   03    W-WDE420-KEYSEQ-MIN-X.                                           
015500     05    W-420-IDPRODNR-MIN        PIC S9(7)    COMP-3.                 
015600     05    W-420-IDPURAD-MIN         PIC S9(5)    COMP-3.                 
015700     SKIP2                                                                
015800   03    W-WDE420-KEYSEQ-MAX-X.                                           
015900     05    W-420-IDPRODNR-MAX        PIC S9(7)    COMP-3.                 
016000     05    W-420-IDPURAD-MAX         PIC S9(5)    COMP-3.                 
016100   03    W-WDE601-IDPRODNR-X.                                             
016200     05    W-601-IDPRODNR            PIC S9(7)    COMP-3.                 
016300     EJECT                                                                
016400 01    MEDDELANDE.                                                        
016500   03    FEL1.                                                            
016600     05    FILLER                PIC X(40)   VALUE                        
016700             '701. ORDERN SAKNAS                     '.                   
016800     05    FILLER                PIC X(40)   VALUE                        
016900             '701. ORDER MISSING                     '.                   
017000   03    FILLER REDEFINES FEL1.                                           
017100     05    FEL-1 OCCURS 2        PIC X(40).                               
017200     SKIP2                                                                
017300   03    FEL2.                                                            
017400     05    FILLER                PIC X(40)   VALUE                        
017500             '783. ORDERN EJ DELAD                   '.                   
017600     05    FILLER                PIC X(40)   VALUE                        
017700             '783. ORDER HAS NOT BEEN SPLIT          '.                   
017800   03    FILLER REDEFINES FEL2.                                           
017900     05    FEL-2 OCCURS 2        PIC X(40).                               
018000     SKIP2                                                                
018100   03    FEL3.                                                            
018200     05    FILLER                PIC X(40)   VALUE                        
018300             '710. ORDERN FÄRDIGRAPPORTERAD          '.                   
018400     05    FILLER                PIC X(40)   VALUE                        
018500             '710 ORDER TOTALLY REPORTED             '.                   
018600   03    FILLER REDEFINES FEL3.                                           
018700     05    FEL-3 OCCURS 2        PIC X(40).                               
018800     SKIP2                                                                
018900   03    FEL4.                                                            
019000     05    FILLER                PIC X(40)   VALUE                        
019100             '784. BLÄDDRING EJ TILLÅTEN             '.                   
019200     05    FILLER                PIC X(40)   VALUE                        
019300             '784. SCROLLING NOT ALLOWED             '.                   
019400   03    FILLER REDEFINES FEL4.                                           
019500     05    FEL-4 OCCURS 2        PIC X(40).                               
019600     EJECT                                                                
019700   03    FEL5.                                                            
019800     05    FILLER                PIC X(40)   VALUE                        
019900             '749. FEL NYCKEL                        '.                   
020000     05    FILLER                PIC X(40)   VALUE                        
020100             '749. WRONG KEY                         '.                   
020200   03    FILLER REDEFINES FEL5.                                           
020300     05    FEL-5 OCCURS 2        PIC X(40).                               
020400     SKIP2                                                                
020500   03    MED1.                                                            
020600     05    FILLER                PIC X(40)   VALUE                        
020700             '778. FLER RADER FINNS                  '.                   
020800     05    FILLER                PIC X(40)   VALUE                        
020900             '778. MORE LINES                        '.                   
021000   03    FILLER REDEFINES MED1.                                           
021100     05    MED-1 OCCURS 2        PIC X(40).                               
021200     EJECT                                                                
021300******************************************************************        
021400*                                                                         
021500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
021600*                                                                         
021700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
021800     SKIP3                                                                
021900*01    MID -COPY W4I32101.                                                
022000     EJECT                                                                
022100*01    -COPY WMSGAREA                                                     
022200     EJECT                                                                
022300*  03    MOD -COPY W4O32101  -RED MSG-AREA.                               
022400     EJECT                                                                
022500*01    -COPY WMFSAREA                                                     
022600     EJECT                                                                
022700******************************************************************        
022800*                                                                         
022900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023000*                                                                         
023100 01    IMS-WS.                                                            
023200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
023300     SKIP3                                                                
023400*                        **** STATUS-KOD FRÅN IMS                         
023500   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
023600     88    KUNDORDER-SEK-FINNS              VALUE '  '.                   
023700     88    KUNDORDER-SEK-SAKNAS             VALUE 'GE' 'GB'.              
023800   03    STATUS-WS               PIC XX.                                  
023900     88    SEGMENT-FINNS                    VALUE '  '.                   
024000     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
024100     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
024200     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
024300     SKIP3                                                                
024400   03    GODK-STATUSKODER.                                                
024500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
024600     SKIP3                                                                
024700 01    SSA1                      PIC X(64).                               
024800 01    SSA2                      PIC X(64).                               
024900 01    SSA3                      PIC X(64).                               
025000 01    SSA4                      PIC X(64).                               
025100     EJECT                                                                
025200*                            IMS FUNKTIONSKODER                           
025300*01    -COPY W0003                                                        
025400     EJECT                                                                
025500*                            DLI INPUT-OUTPUT AREA                        
025600 01    DLI-IO-AREA.                                                       
025700   03    IO-AREA                 PIC X(510)  VALUE SPACE.                 
025800     SKIP3                                                                
025900*  03    WDE401   -COPY WDE401              -RED IO-AREA.                 
026000     EJECT                                                                
026100*  03    WDE411   -COPY WDE411              -RED IO-AREA.                 
026200     EJECT                                                                
026300*  03    WDE601   -COPY WDE601              -RED IO-AREA.                 
026400     EJECT                                                                
026500 LINKAGE SECTION.                                                         
026600*01    -COPY W0009     -PRE MSG-                                          
026700     EJECT                                                                
026800*01    -COPY W0008     -PRE USEA-                                         
026900     05  FILLER                  PIC X.                                   
027000     EJECT                                                                
027100*01    -COPY W0008     -PRE WDE4-                                         
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400*01    -COPY W0008     -PRE WDE42-                                        
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700*01    -COPY W0008     -PRE WDE43-                                        
027800     05  FILLER                  PIC X.                                   
027900     EJECT                                                                
028000*01    -COPY W0008     -PRE WDE6-                                         
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB                               
028400                          WDE4-PCB WDE42-PCB WDE43-PCB WDE6-PCB.          
028500     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
028600                           WDE4-PCB WDE42-PCB WDE43-PCB WDE6-PCB.         
028700     SKIP2                                                                
028800 STYR SECTION.                                                            
028900     PERFORM IMS-GET-MSG                                                  
029000     IF SEGMENT-FINNS                                                     
029100        PERFORM A-INIT-SPARA-INPUT                                        
029200        PERFORM B-KOLLA-INPUT                                             
029300        IF WS-KDFEL = ZERO                                                
029400           PERFORM C-HAEMTA-STARTNYCKEL                                   
029500        END-IF                                                            
029600        IF WS-KDFEL = ZERO                                                
029700           PERFORM N-BLANKA-RADER                                         
029800           PERFORM D-BEHANDLA-RADER                                       
029900        END-IF                                                            
030000        IF WS-KDFEL > ZERO                                                
030100           PERFORM J-HAMTA-MEDDELANDE                                     
030200           IF WS-KDFEL < 15                                               
030300              PERFORM N-BLANKA-RADER                                      
030400           END-IF                                                         
030500        END-IF                                                            
030600     ELSE                                                                 
030700        PERFORM L-TOM-SKAERM                                              
030800     END-IF                                                               
030900     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O32101 + 4                        
031000     PERFORM IMS-INSERT-MSG                                               
031100     MOVE ZERO TO RETURN-CODE                                             
031200     GOBACK                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 A-INIT-SPARA-INPUT SECTION.                                              
031600                                                                          
031700     IF MSG-DUBBLA-TRANSKODER                                             
031800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I32101                 
031900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
032000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
032100       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
032200       MOVE MSG-IDPFK                     TO MFS-IDPFK                    
032300     ELSE                                                                 
032400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I32101                 
032500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
032600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
032700       MOVE ' '                           TO MFS-KDTRTYP                  
032800     END-IF                                                               
032900                                                                          
033000     MOVE ZERO                 TO WS-KDFEL                                
033100     MOVE ZERO                 TO W-IDPRODNR                              
033200     MOVE LOW-VALUE            TO MSG-AREA                                
033300     MOVE MFS-IDTRANS          TO FRAN-BILD                               
033400                                                                          
033500     PERFORM AB-INIT-NYCKLAR                                              
033600                                                                          
033700     MOVE 'W4O321N1'           TO MFS-IDMOD                               
033800     MOVE '4321'               TO MOD-IDTRANS                             
033900                                                                          
034000     IF  FRAN-BILD-EGEN                                                   
034100         MOVE MID-IDPRODNR-NYCKEL         TO WS-IDPRODNR-NYCKEL           
034200         MOVE MID-IDRADNR-ORD-TOM-NYCKEL  TO                              
034300                                      WS-IDRADNR-ORD-TOM-NYCKEL           
034400     ELSE                                                                 
034500         MOVE ZERO                 TO WS-IDPRODNR-NYCKEL                  
034600                                      WS-IDRADNR-ORD-TOM-NYCKEL           
034700     END-IF                                                               
034800                                                                          
034900     MOVE MFS-RENSA-FAELT      TO MOD-IDANSTNR-IN                         
035000                                  MOD-IDDISTR-IN                          
035100                                  MOD-IDKUNDNR-IN                         
035200                                  MOD-IDORDNR-IN                          
035300                                  MOD-IDKOLLI-IN                          
035400                                  MOD-IDPRODNR-IN                         
035500                                  MOD-IDDC-IN                             
035600                                  MOD-TEMFSFEL                            
035700                                  MOD-TEMFSINF                            
035800                                                                          
035900     MOVE 1                    TO MOD-RAD-IND                             
036000     PERFORM UNTIL MOD-RAD-IND > 26                                       
036100       MOVE MFS-ROER-EJ-FAELT  TO MOD-ADPACOMR    (MOD-RAD-IND)           
036200                                  MOD-IDANSTNR    (MOD-RAD-IND)           
036300                                  MOD-KVORAPP     (MOD-RAD-IND)           
036400                             MOD-IDRADNR-ORD-FROM   (MOD-RAD-IND)         
036500                             MOD-IDRADNR-ORD-TOM    (MOD-RAD-IND)         
036600       ADD 1 TO MOD-RAD-IND                                               
036700     END-PERFORM                                                          
036800     .                                                                    
036900     EJECT                                                                
037000 AB-INIT-NYCKLAR SECTION.                                                 
037100                                                                          
037200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037300     MOVE '001'             TO MSGI-KDCALL                                
037400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037500     MOVE '4321'            TO MSGI-IDTRANS                               
037600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037700     IF FRAN-BILD-EGEN                                                    
037800        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
037900        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
038000        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
038100        IF MID-IDORDNR-IN       NOT = ALL '+'                             
038200           MOVE MID-IDORDNR-IN  TO W-SPAR-IDORDNR5                        
038300           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
038400        END-IF                                                            
038500     END-IF                                                               
038600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038700                                                                          
038800     IF MSGI-IDLAND-SPR = 'GB'                                            
038900       MOVE +2 TO SPRAK-INDX                                              
039000     ELSE                                                                 
039100       MOVE +1 TO SPRAK-INDX                                              
039200     END-IF                                                               
039300                                                                          
039400     MOVE NEJ              TO SW-IDPRODNR                                 
039500                              SW-ORDERID                                  
039600     MOVE 1                TO NYCKEL-TYP                                  
039700     MOVE ZERO             TO WS-IDKOLLI                                  
039800*                                                                         
039900     IF FRAN-BILD-OK                                                      
040000        IF MID-IDPRODNR-IN NOT = ALL '+'                                  
040100            MOVE 2                   TO NYCKEL-TYP                        
040200            MOVE JA                  TO SW-IDPRODNR                       
040300            MOVE MID-IDPRODNR-IN     TO WS-IDPRODNR                       
040400            INSPECT WS-IDPRODNR  REPLACING ALL '+' BY ZERO                
040500        ELSE                                                              
040600            MOVE MID-IDPRODNR-UT     TO WS-IDPRODNR                       
040700            INSPECT WS-IDPRODNR REPLACING ALL SPACE BY ZERO               
040800        END-IF                                                            
040900     ELSE                                                                 
041000        IF MSGI-IDPRODNR NUMERIC                                          
041100           MOVE MSGI-IDPRODNR           TO WS-IDPRODNR                    
041110        ELSE                                                              
041120           MOVE ZERO                    TO WS-IDPRODNR                    
041130        END-IF                                                            
041200     END-IF                                                               
041300*                                                                         
041400     IF FRAN-BILD-OK                                                      
041500        IF MID-IDDISTR-IN NOT = ALL '+'                                   
041600            MOVE MID-IDDISTR-IN      TO WS-IDDISTR                        
041700            MOVE 2                   TO NYCKEL-TYP                        
041800            MOVE JA                  TO SW-ORDERID                        
041900        ELSE                                                              
042000            MOVE MID-IDDISTR-UT      TO WS-IDDISTR                        
042100            INSPECT WS-IDDISTR REPLACING ALL SPACE BY ZERO                
042200        END-IF                                                            
042300*                                                                         
042400        IF MID-IDORDNR-IN NOT = ALL '+'                                   
042500            MOVE MID-IDORDNR-IN      TO WS-IDORDNR                        
042600            MOVE WS-IDORDNR          TO WS-IDKUNDRF                       
042700            MOVE 2                   TO NYCKEL-TYP                        
042800            MOVE JA                  TO SW-ORDERID                        
042900        ELSE                                                              
043000            MOVE MID-IDORDNR-UT      TO WS-IDORDNR                        
043100            MOVE WS-IDORDNR          TO WS-IDKUNDRF                       
043200            INSPECT WS-IDORDNR REPLACING ALL SPACE BY ZERO                
043300        END-IF                                                            
043400*                                                                         
043500        IF MID-IDKUNDNR-IN NOT = ALL '+'                                  
043600            MOVE MID-IDKUNDNR-IN     TO WS-IDKUNDNR                       
043700            MOVE 2                   TO NYCKEL-TYP                        
043800            MOVE JA                  TO SW-ORDERID                        
043900        ELSE                                                              
044000            MOVE MID-IDKUNDNR-UT     TO WS-IDKUNDNR                       
044100            INSPECT WS-IDKUNDNR REPLACING ALL SPACE BY ZERO               
044200        END-IF                                                            
044300*                                                                         
044400*       --- CHECK INPUT WAREHOUSE IDENTIFIER                              
044500*                                                                         
044600        IF MID-IDDC-IN NOT = ALL '+'                                      
044700            MOVE MID-IDDC-IN         TO WS-IDDC                           
044800        ELSE                                                              
044900            MOVE MID-IDDC-UT         TO WS-IDDC                           
045000        END-IF                                                            
045100     ELSE                                                                 
045200        MOVE MSGI-IDDC               TO WS-IDDC                           
045300     END-IF                                                               
045400                                                                          
045500     .                                                                    
045600     EJECT                                                                
045700 B-KOLLA-INPUT SECTION.                                                   
045800     IF WS-IDPRODNR NUMERIC                                               
045900       IF  WS-IDPRODNR > ZERO                                             
046000       AND SW-ORDERID NOT = JA                                            
046100           MOVE JA                   TO SW-IDPRODNR                       
046200       END-IF                                                             
046300     ELSE                                                                 
046400       MOVE 5                        TO WS-KDFEL                          
046500     END-IF                                                               
046600*                                                                         
046700     IF FRAN-BILD-OK                                                      
046800        IF WS-IDKUNDNR NUMERIC                                            
046900*         IF WS-IDKUNDNR > ZERO                                           
047000            CONTINUE                                                      
047100*         END-IF                                                          
047200        ELSE                                                              
047300          MOVE 5                     TO WS-KDFEL                          
047400        END-IF                                                            
047500*                                                                         
047600        IF WS-IDDISTR NUMERIC                                             
047700*         IF WS-IDDISTR > ZERO                                            
047800            CONTINUE                                                      
047900*         END-IF                                                          
048000        ELSE                                                              
048100          MOVE 5                     TO WS-KDFEL                          
048200        END-IF                                                            
048300*                                                                         
048400        IF WS-IDORDNR NUMERIC                                             
048500*         IF WS-IDORDNR > ZERO                                            
048600            CONTINUE                                                      
048700*         END-IF                                                          
048800        ELSE                                                              
048900          MOVE 5                     TO WS-KDFEL                          
049000        END-IF                                                            
049100     END-IF                                                               
049200*                                                                         
049300     IF WS-IDDC IS > SPACE                                                
049400         CONTINUE                                                         
049500     ELSE                                                                 
049600       MOVE MSGI-IDDC                TO WS-IDDC                           
049700     END-IF                                                               
049800     EJECT                                                                
049900     IF NY-NYCKEL                                                         
050000     OR NOT FRAN-BILD-EGEN                                                
050100       IF SW-IDPRODNR = JA                                                
050200         MOVE IDPRODNR-WS           TO W-IDPRODNR                         
050300         MOVE NEJ                   TO SW-ORDERID                         
050400         MOVE ZERO                  TO WS-IDDISTR                         
050500                                       WS-IDKUNDNR                        
050600                                       WS-IDORDNR                         
050700       ELSE                                                               
050800         MOVE JA                    TO SW-ORDERID                         
050900*        IF WS-IDDISTR = ZERO                                             
051000*          INSPECT MID-IDDISTR-UT REPLACING LEADING                       
051100*          SPACE BY ZERO                                                  
051200*          MOVE MID-IDDISTR-UT TO WS-IDDISTR                              
051300*        END-IF                                                           
051400*        IF WS-IDKUNDNR = ZERO                                            
051500*          INSPECT MID-IDKUNDNR-UT REPLACING LEADING                      
051600*          SPACE BY ZERO                                                  
051700*          MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                            
051800*        END-IF                                                           
051900*        IF WS-IDORDNR = ZERO                                             
052000*          INSPECT MID-IDORDNR-UT REPLACING LEADING                       
052100*          SPACE BY ZERO                                                  
052200*          MOVE MID-IDORDNR-UT TO WS-IDORDNR                              
052300*        END-IF                                                           
052400       END-IF                                                             
052500       MOVE ZERO                    TO W-IDRADNR-ORD                      
052600                                       W-IDRADNR-ORAPP                    
052700     ELSE                                                                 
052800       IF GAMMAL-NYCKEL                                                   
052900         PERFORM BA-HAEMTA-NYCKEL-NAESTA                                  
053000         MOVE IDPRODNR-WS           TO W-IDPRODNR                         
053100         MOVE JA                    TO SW-IDPRODNR                        
053200       ELSE                                                               
053300         MOVE 4                     TO WS-KDFEL                           
053400       END-IF                                                             
053500     END-IF                                                               
053600     EJECT                                                                
053700     MOVE WS-IDPRODNR   TO MOD-IDPRODNR-UT                                
053800     INSPECT MOD-IDPRODNR-UT REPLACING                                    
053900                             LEADING ZEROES BY SPACE                      
054000     MOVE WS-IDKUNDNR   TO MOD-IDKUNDNR-UT                                
054100     INSPECT MOD-IDKUNDNR-UT REPLACING                                    
054200                             LEADING ZEROES BY SPACE                      
054300     MOVE WS-IDDISTR   TO MOD-IDDISTR-UT                                  
054400     INSPECT MOD-IDDISTR-UT  REPLACING                                    
054500                             LEADING ZEROES BY SPACE                      
054600     MOVE WS-IDORDNR   TO MOD-IDORDNR-UT                                  
054700     INSPECT MOD-IDORDNR-UT  REPLACING                                    
054800                             LEADING ZEROES BY SPACE                      
054900     MOVE WS-IDKOLLI   TO MOD-IDKOLLI-UT                                  
055000     INSPECT MOD-IDKOLLI-UT  REPLACING                                    
055100                             LEADING ZEROES BY SPACE                      
055200     MOVE WS-IDDC      TO MOD-IDDC-UT                                     
055300     .                                                                    
055400     EJECT                                                                
055500 BA-HAEMTA-NYCKEL-NAESTA SECTION.                                         
055600     IF WS-IDPRODNR-NYCKEL > ZERO                                         
055700       MOVE WS-IDPRODNR-NYCKEL      TO W-IDPRODNR                         
055800                                       WS-IDPRODNR                        
055900     END-IF                                                               
056000     IF MFS-IDPFK = '8'                                                   
056100       MOVE WS-IDRADNR-ORD-TOM-NYCKEL TO W-IDRADNR-ORD                    
056200                                         W-IDRADNR-ORAPP                  
056300     ELSE                                                                 
056400       MOVE ZERO                      TO W-IDRADNR-ORD                    
056500                                         W-IDRADNR-ORAPP                  
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 C-HAEMTA-STARTNYCKEL SECTION.                                            
057000     SKIP2                                                                
057100*                                                                         
057200     IF MFS-IDPFK = '8'                                                   
057300        IF NOT GAMMAL-NYCKEL                                              
057400           MOVE 4 TO WS-KDFEL                                             
057500        ELSE                                                              
057600           MOVE WS-IDPRODNR-NYCKEL        TO W-420-IDPRODNR               
057700                                             W-IDPRODNR                   
057800                                             WS-IDPRODNR                  
057900           MOVE WS-IDRADNR-ORD-TOM-NYCKEL TO W-420-IDPURAD                
058000           PERFORM IMS-GU-KUNDORDER-SEK-INV                               
058100*                                                                         
058200           IF SEGMENT-FINNS                                               
058300              MOVE KORD-IDDISTR      TO W-OLD-IDDISTR                     
058400                                        IDDISTR-WS                        
058500              MOVE KORD-IDKUNDNR     TO W-OLD-IDKUNDNR                    
058600                                        IDKUNDNR-WS                       
058700              MOVE KORD-IDKUNDRF     TO W-OLD-IDKUNDRF                    
058800                                        WS-IDKUNDRF                       
058900              MOVE KORD-IDPRODNR     TO W-OLD-IDPRODNR                    
059000              MOVE KORD-IDPLKLST     TO W-OLD-IDPLKLST                    
059100           ELSE                                                           
059200              MOVE 1              TO WS-KDFEL                             
059300           END-IF                                                         
059400        END-IF                                                            
059500     END-IF                                                               
059600*                                                                         
059700     IF SW-ORDERID = NEJ AND                                              
059800        MFS-IDPFK NOT = '8'                                               
059900        MOVE IDPRODNR-WS          TO W-420-IDPRODNR-MIN                   
060000                                     W-420-IDPRODNR-MAX                   
060100        MOVE 1                    TO W-420-IDPURAD-MIN                    
060200        MOVE 99999                TO W-420-IDPURAD-MAX                    
060300        PERFORM IMS-GU-KUNDORDER-SEK-INV-INT                              
060400*                                                                         
060500        IF SEGMENT-FINNS                                                  
060600           MOVE KORD-IDDISTR      TO IDDISTR-WS                           
060700           MOVE KORD-IDKUNDNR     TO IDKUNDNR-WS                          
060800           MOVE KORD-IDKUNDRF     TO WS-IDKUNDRF                          
060900        ELSE                                                              
061000           MOVE 1                 TO WS-KDFEL                             
061100        END-IF                                                            
061200     END-IF                                                               
061300*                                                                         
061400     IF WS-KDFEL = ZERO                                                   
061500        MOVE IDDISTR-WS           TO W-4A1-IDDISTR                        
061600        MOVE IDKUNDNR-WS          TO W-4A1-IDKUNDNR                       
061700        MOVE WS-IDKUNDRF          TO W-4A1-IDKUNDRF                       
061800        PERFORM IMS-GU-KUNDORDER-SEK                                      
061900*                                                                         
062000        IF KUNDORDER-SEK-FINNS                                            
062100           PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                          
062200                         SLINGA-KLAR                                      
062300           MOVE KORD-IDDISTR      TO W-401-IDDISTR                        
062400           MOVE KORD-IDKUNDNR     TO W-401-IDKUNDNR                       
062500           MOVE KORD-IDORDNR5     TO W-401-IDORDNR                        
062600           MOVE KORD-IDPRODNR     TO W-401-IDPRODNR                       
062700           MOVE KORD-IDPLKLST     TO W-401-IDPLKLST                       
062800           PERFORM IMS-GU-KUNDORDER                                       
062900                                                                          
063000           MOVE KORD-IDPRODNR     TO W-601-IDPRODNR                       
063100           PERFORM IMS-GU-IDPRODNR                                        
063200           IF SEGMENT-FINNS                                               
063300              IF SW-IDPRODNR = NEJ                                        
063400                 IF VORD-IDDC = WS-IDDC                                   
063500                    CONTINUE                                              
063600                 ELSE                                                     
063700                    SET SEGMENT-SAKNAS TO TRUE                            
063800                 END-IF                                                   
063900              ELSE                                                        
064000                 IF VORD-IDPRODNR = IDPRODNR-WS                           
064100                    CONTINUE                                              
064200                 ELSE                                                     
064300                    SET SEGMENT-SAKNAS TO TRUE                            
064400                 END-IF                                                   
064500              END-IF                                                      
064600           END-IF                                                         
064700*                                                                         
064800           IF SEGMENT-FINNS                                               
064900              IF VORD-IDDC     = WS-IDDC                                  
065000                 MOVE VORD-IDPRODNR  TO IDPRODNR-WS                       
065100                 MOVE VORD-KDORDSTA  TO WS-SPAR-KDORDSTA                  
065200                 MOVE 'J'            TO WS-SLINGA-KLAR                    
065300              END-IF                                                      
065400           END-IF                                                         
065500*                                                                         
065600           PERFORM IMS-GN-KUNDORDER-SEK                                   
065700           END-PERFORM                                                    
065800           IF NOT SLINGA-KLAR                                             
065900              MOVE 1                 TO WS-KDFEL                          
066000           END-IF                                                         
066100        ELSE                                                              
066200           MOVE 1                    TO WS-KDFEL                          
066300        END-IF                                                            
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 D-BEHANDLA-RADER        SECTION.                                         
066800     SKIP2                                                                
066900     MOVE 'J'                              TO WS-FIRST-TIME               
067000     MOVE 1                                TO WS-RAD-IND                  
067100*                                                                         
067200     MOVE IDDISTR-WS                       TO W-4A1-IDDISTR               
067300     MOVE IDKUNDNR-WS                      TO W-4A1-IDKUNDNR              
067400     MOVE WS-IDKUNDRF                      TO W-4A1-IDKUNDRF              
067500     PERFORM IMS-GU-KUNDORDER-SEK                                         
067600*                                                                         
067700     IF KUNDORDER-SEK-FINNS                                               
067800        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
067900                      WS-RAD-IND > 26                                     
068000        MOVE KORD-IDDISTR                  TO W-401-IDDISTR               
068100        MOVE KORD-IDKUNDNR                 TO W-401-IDKUNDNR              
068200        MOVE KORD-IDORDNR5                 TO W-401-IDORDNR               
068300        MOVE KORD-IDPRODNR                 TO W-401-IDPRODNR              
068400        MOVE KORD-IDPLKLST                 TO W-401-IDPLKLST              
068500        PERFORM IMS-GU-KUNDORDER                                          
068600*                                                                         
068700        MOVE KORD-IDPRODNR                 TO WS-JFR-IDPRODNR             
068800        MOVE KORD-IDUSER                   TO WS-JFR-IDANSTNR             
068900        IF IDPRODNR-WS = WS-JFR-IDPRODNR                                  
069000           MOVE KORD-IDPRODNR              TO WS-IDPRODNR-NYCKEL          
069100           IF WS-SPAR-KDORDSTA > 3                                        
069200              MOVE 10 TO WS-KDFEL                                         
069300           ELSE                                                           
069400           IF WS-KDFEL = ZERO                                             
069500              PERFORM DA-LAGG-UT-RADER                                    
069600           END-IF                                                         
069700           END-IF                                                         
069800        END-IF                                                            
069900*                                                                         
070000        PERFORM IMS-GN-KUNDORDER-SEK                                      
070100        END-PERFORM                                                       
070200     ELSE                                                                 
070300        MOVE 1 TO WS-KDFEL                                                
070400     END-IF                                                               
070500     .                                                                    
070600     EJECT                                                                
070700 DA-LAGG-UT-RADER        SECTION.                                         
070800     SKIP2                                                                
070900     IF (MFS-IDPFK NOT = '8')              OR                             
071000        (MFS-IDPFK = '8'                  AND                             
071100         KORD-IDDISTR    = W-OLD-IDDISTR  AND                             
071200         KORD-IDKUNDNR   = W-OLD-IDKUNDNR AND                             
071300         KORD-IDKUNDRF   = W-OLD-IDKUNDRF AND                             
071400         KORD-IDPRODNR   = W-OLD-IDPRODNR AND                             
071500         KORD-IDPLKLST   = W-OLD-IDPLKLST AND                             
071600         FIRST-TIME)                       OR                             
071700        (MFS-IDPFK = '8'                  AND                             
071800         NOT FIRST-TIME)                                                  
071900        MOVE 'N'                  TO WS-FIRST-TIME                        
072000        PERFORM IMS-GNP-RAD                                               
072100*                                                                         
072200        IF SEGMENT-FINNS                                                  
072300           COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -                       
072400                                     1                                    
072500           MOVE ORAD-IDPURAD       TO WS-START-IDPURAD                    
072600                                      WS-STOPP-IDPURAD                    
072700*                                                                         
072800           PERFORM UNTIL SEGMENT-SAKNAS OR                                
072900                         WS-RAD-IND > 26                                  
073000           IF (MFS-IDPFK NOT = '8')              OR                       
073100              (MFS-IDPFK = '8'                   AND                      
073200               ORAD-IDPURAD > WS-IDRADNR-ORD-TOM-NYCKEL)                  
073300              IF ORAD-KDRADSTA < 4                                        
073400                 IF ORAD-IDPURAD = WS-SPAR-IDPURAD + 1                    
073500                    MOVE ORAD-IDPURAD TO WS-SPAR-IDPURAD                  
073600                                         WS-STOPP-IDPURAD                 
073700                    COMPUTE WS-KVORAPP = WS-KVORAPP +                     
073800                                         ORAD-KVAVBART -                  
073900                                         ORAD-KVLEVART                    
074000                    PERFORM IMS-GNP-RAD                                   
074100                 ELSE                                                     
074200                    IF WS-KVORAPP > ZERO                                  
074300                       PERFORM S10-SKRIV-RAD                              
074400                       ADD 1             TO WS-RAD-IND                    
074500                       COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -           
074600                                                 1                        
074700                       MOVE ORAD-IDPURAD TO WS-START-IDPURAD              
074800                                            WS-STOPP-IDPURAD              
074900                                            WS-SPAR-IDPURAD               
075000                       COMPUTE WS-KVORAPP = WS-KVORAPP +                  
075100                                            ORAD-KVAVBART -               
075200                                            ORAD-KVLEVART                 
075300                    END-IF                                                
075400                    PERFORM IMS-GNP-RAD                                   
075500                 END-IF                                                   
075600              ELSE                                                        
075700                 IF WS-KVORAPP > ZERO                                     
075800                    PERFORM S10-SKRIV-RAD                                 
075900                    ADD 1                TO WS-RAD-IND                    
076000                 END-IF                                                   
076100                 PERFORM IMS-GNP-RAD                                      
076200                 IF SEGMENT-FINNS                                         
076300                    COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -              
076400                                              1                           
076500                    MOVE ORAD-IDPURAD TO WS-START-IDPURAD                 
076600                                         WS-STOPP-IDPURAD                 
076700                 END-IF                                                   
076800              END-IF                                                      
076900           ELSE                                                           
077000              PERFORM IMS-GNP-RAD                                         
077100              IF SEGMENT-FINNS                                            
077200                 COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -                 
077300                                           1                              
077400                 MOVE ORAD-IDPURAD TO WS-START-IDPURAD                    
077500                                      WS-STOPP-IDPURAD                    
077600              END-IF                                                      
077700           END-IF                                                         
077800           END-PERFORM                                                    
077900        END-IF                                                            
078000     END-IF                                                               
078100*                                                                         
078200     IF SEGMENT-SAKNAS AND                                                
078300        WS-RAD-IND NOT > 26                                               
078400        IF WS-KVORAPP > ZERO                                              
078500           PERFORM S10-SKRIV-RAD                                          
078600           ADD 1                TO WS-RAD-IND                             
078700           COMPUTE WS-SPAR-IDPURAD = ORAD-IDPURAD -                       
078800                                     1                                    
078900           MOVE ORAD-IDPURAD TO WS-START-IDPURAD                          
079000                                WS-STOPP-IDPURAD                          
079100        END-IF                                                            
079200     END-IF                                                               
079300*                                                                         
079400     PERFORM DAA-SPARA-NYCKEL-NAESTA                                      
079500     .                                                                    
079600     EJECT                                                                
079700 DAA-SPARA-NYCKEL-NAESTA SECTION.                                         
079800     MOVE WS-IDPRODNR-NYCKEL     TO MOD-IDPRODNR-NYCKEL                   
079900*                                                                         
080000     IF SEGMENT-FINNS                                                     
080100        MOVE WS-IDRADNR-ORD-TOM-NYCKEL TO                                 
080200                                      MOD-IDRADNR-ORD-TOM-NYCKEL          
080300        MOVE 15 TO WS-KDFEL                                               
080400     ELSE                                                                 
080500        MOVE ZERO                TO MOD-IDRADNR-ORD-TOM-NYCKEL            
080600     END-IF                                                               
080700     .                                                                    
080800     EJECT                                                                
080900 J-HAMTA-MEDDELANDE SECTION.                                              
081000     EVALUATE WS-KDFEL                                                    
081100       WHEN  1 MOVE FEL-1 (SPRAK-INDX) TO MOD-TEMFSFEL                    
081200       WHEN  2 MOVE FEL-2 (SPRAK-INDX) TO MOD-TEMFSFEL                    
081300       WHEN  3 MOVE FEL-3 (SPRAK-INDX) TO MOD-TEMFSFEL                    
081400       WHEN  4 MOVE FEL-4 (SPRAK-INDX) TO MOD-TEMFSFEL                    
081500       WHEN  5 MOVE FEL-5 (SPRAK-INDX) TO MOD-TEMFSFEL                    
081600       WHEN 15 MOVE MED-1 (SPRAK-INDX) TO MOD-TEMFSINF                    
081700     END-EVALUATE                                                         
081800     .                                                                    
081900     EJECT                                                                
082000 L-TOM-SKAERM SECTION.                                                    
082100     MOVE MFS-RENSA-FAELT     TO MOD-TEMFSFEL                             
082200                                 MOD-IDANSTNR-IN                          
082300                                 MOD-IDANSTNR-UT                          
082400                                 MOD-IDDISTR-IN                           
082500                                 MOD-IDDISTR-UT                           
082600                                 MOD-IDKUNDNR-IN                          
082700                                 MOD-IDKUNDNR-UT                          
082800                                 MOD-IDORDNR-IN                           
082900                                 MOD-IDORDNR-UT                           
083000                                 MOD-IDDC-IN                              
083100                                 MOD-IDDC-UT                              
083200                                 MOD-IDKOLLI-IN                           
083300                                 MOD-IDKOLLI-UT                           
083400                                 MOD-IDPRODNR-IN                          
083500                                 MOD-IDPRODNR-UT                          
083600                                 MOD-TEMFSINF                             
083700     MOVE 1 TO MOD-RAD-IND                                                
083800     PERFORM UNTIL MOD-RAD-IND > 26                                       
083900       MOVE MFS-RENSA-FAELT TO MOD-ADPACOMR      (MOD-RAD-IND)            
084000                               MOD-IDRADNR-ORD-FROM                       
084100                                                 (MOD-RAD-IND)            
084200                               MOD-IDRADNR-ORD-TOM                        
084300                                                 (MOD-RAD-IND)            
084400                               MOD-KVORAPP       (MOD-RAD-IND)            
084500                               MOD-IDANSTNR      (MOD-RAD-IND)            
084600       ADD 1 TO MOD-RAD-IND                                               
084700     END-PERFORM                                                          
084800     MOVE MFS-RENSA-FAELT     TO MOD-IDPRODNR-NYCKEL                      
084900                                 MOD-IDANSTNR-NYCKEL                      
085000                                 MOD-IDRADNR-ORD-TOM-NYCKEL               
085100     .                                                                    
085200     EJECT                                                                
085300 N-BLANKA-RADER SECTION.                                                  
085400     MOVE 1 TO MOD-RAD-IND                                                
085500     PERFORM UNTIL MOD-RAD-IND > 26                                       
085600       MOVE MFS-RENSA-FAELT TO MOD-ADPACOMR      (MOD-RAD-IND)            
085700                               MOD-IDRADNR-ORD-FROM                       
085800                                                 (MOD-RAD-IND)            
085900                               MOD-IDRADNR-ORD-TOM                        
086000                                                 (MOD-RAD-IND)            
086100                               MOD-KVORAPP       (MOD-RAD-IND)            
086200                               MOD-IDANSTNR      (MOD-RAD-IND)            
086300       ADD 1 TO MOD-RAD-IND                                               
086400     END-PERFORM                                                          
086500     .                                                                    
086600     EJECT                                                                
086700 S10-SKRIV-RAD SECTION.                                                   
086800*    MOVE WS-ADPACOMR       TO MOD-ADPACOMR         (WS-RAD-IND)          
086900     MOVE WS-START-IDPURAD  TO MOD-IDRADNR-ORD-FROM (WS-RAD-IND)          
087000     MOVE WS-STOPP-IDPURAD  TO WS-IDRADNR-ORD-TOM-NYCKEL                  
087100                               MOD-IDRADNR-ORD-TOM  (WS-RAD-IND)          
087200     MOVE WS-KVORAPP        TO MOD-KVORAPP          (WS-RAD-IND)          
087300     MOVE ZERO              TO WS-KVORAPP                                 
087400     MOVE WS-JFR-IDANSTNR-5 TO MOD-IDANSTNR         (WS-RAD-IND)          
087500     .                                                                    
087600     EJECT                                                                
087700* IMS SEKTIONER                                                           
087800     SKIP3                                                                
087900 IMS-GET-MSG SECTION.                                                     
088000                                                                          
088100     MOVE '  QC' TO GODK-STATUSKODER                                      
088200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
088300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088400     PERFORM IMS-STATUSKONTROLL                                           
088500     SKIP3                                                                
088600     .                                                                    
088700 IMS-INSERT-MSG SECTION.                                                  
088800                                                                          
088900     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
089000       MOVE '0' TO MFS-KDHUVOMR                                           
089100     END-IF                                                               
089200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
089300     MOVE SPACE TO GODK-STATUSKODER                                       
089400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
089500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089600     PERFORM IMS-STATUSKONTROLL                                           
089700     .                                                                    
089800     EJECT                                                                
089900 IMS-GU-KUNDORDER SECTION.                                                
090000     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
090100            DELIMITED BY SIZE INTO SSA1                                   
090200     MOVE '    ' TO GODK-STATUSKODER                                      
090300     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1                      
090400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
090500     PERFORM IMS-STATUSKONTROLL                                           
090600     SKIP3                                                                
090700     .                                                                    
090800 IMS-GNP-RAD  SECTION.                                                    
090900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
091000            DELIMITED BY SIZE INTO SSA1                                   
091100     MOVE 'WDE411   ' TO SSA2                                             
091200     MOVE '  GE' TO GODK-STATUSKODER                                      
091300     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-AREA SSA1 SSA2                
091400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
091500     PERFORM IMS-STATUSKONTROLL                                           
091600     SKIP3                                                                
091700     .                                                                    
091800 IMS-GU-IDPRODNR SECTION.                                                 
091900     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
092000            DELIMITED BY SIZE INTO SSA1                                   
092100     MOVE '  GE' TO GODK-STATUSKODER                                      
092200     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-AREA SSA1                     
092300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
092400     PERFORM IMS-STATUSKONTROLL                                           
092500     .                                                                    
092600     EJECT                                                                
092700 IMS-GU-KUNDORDER-SEK SECTION.                                            
092800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
092900            DELIMITED BY SIZE INTO SSA1                                   
093000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
093100     CALL CBLTDLI USING GU WDE42-PCB DLI-IO-AREA SSA1                     
093200     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
093300                               STATUS-KUNDORDER-SEK-WS                    
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     SKIP3                                                                
093600     .                                                                    
093700 IMS-GN-KUNDORDER-SEK SECTION.                                            
093800     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
093900            DELIMITED BY SIZE INTO SSA1                                   
094000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094100     CALL CBLTDLI USING GN WDE42-PCB DLI-IO-AREA SSA1                     
094200     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
094300                               STATUS-KUNDORDER-SEK-WS                    
094400     PERFORM IMS-STATUSKONTROLL                                           
094500     .                                                                    
094600     EJECT                                                                
094700 IMS-GU-KUNDORDER-SEK-INV-INT SECTION.                                    
094800     STRING 'WDE411  (WDE4BSEQ>=' W-WDE420-KEYSEQ-MIN-X                   
094900                    '&WDE4BSEQ<=' W-WDE420-KEYSEQ-MAX-X ')'               
095000            DELIMITED BY SIZE INTO SSA1                                   
095100     MOVE 'WDE401   ' TO SSA2                                             
095200     MOVE '  GE' TO GODK-STATUSKODER                                      
095300     CALL CBLTDLI USING GU WDE43-PCB DLI-IO-AREA SSA1 SSA2                
095400     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     .                                                                    
095700     EJECT                                                                
095800 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
095900     STRING 'WDE411  (WDE4BSEQ =' W-WDE420-KEYSEQ-X ')'                   
096000            DELIMITED BY SIZE INTO SSA1                                   
096100     MOVE 'WDE401   ' TO SSA2                                             
096200     MOVE '  GE' TO GODK-STATUSKODER                                      
096300     CALL CBLTDLI USING GU WDE43-PCB DLI-IO-AREA SSA1 SSA2                
096400     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
096500     PERFORM IMS-STATUSKONTROLL                                           
096600     SKIP3                                                                
096700     .                                                                    
096800 IMS-STATUSKONTROLL SECTION.                                              
096900     SKIP2                                                                
097000     SET STATUS-IX TO 1                                                   
097100     SEARCH GODK-STATUS AT END CALL FELLOG                                
097200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
097300     END-SEARCH                                                           
097400     CONTINUE                                                             
097500     .                                                                    
