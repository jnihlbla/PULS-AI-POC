000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4070900.                                                
000400 AUTHOR.         OLSSON SUSANNE.                                          
000500 DATE-WRITTEN.   08/07/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        BILDEN VISAR STATUS FÖR RETURER KOD 72 OCH 98, SAMT RETUR        
001000*        AV RETUR, SOM SKALL DEBITERAS FÖR HANTERINGSKOSTNAD MED          
001100*        PRIS PER RAD.REGLER FINNS UPPSATTA PÅ BILD 4707.LÄNGST           
001200*        NER PÅ SIDAN SUMMERAR MAN ALLT SOM HAR STATUS W=WAITING          
001300*        FÖR RESPEKTIVE KOD.MAN KAN FRÅGA PÅ ETT VISST STATUS             
001400*        ELLER ANGE BLANKT I STATUSFÄLTET OCH FÅR DÅ ALLA RADER           
001500*        SOM VÄNTAR PÅ ATT SKICKAS TILL BILL-IT.                          
001600*                                                                         
001700*        PROGRAMMET LÄSER      TABELL TP8LRET                             
001800*        PROGRAMMET LÄSER      WDB2                                       
001900*                                                                         
002000*    E'TRACKER: 880053 DATED 20080704                                     
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W4T709                                              
002400*        MID:         W4I70901                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W4O70901                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W4070900'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
004300 77  WS-IDPARTNR-IN              PIC X(9)    VALUE SPACE.                 
004400 77  WS-IDFTG-IN                 PIC X(2)    VALUE SPACE.                 
004500 77  WS-IDDISTR-IN               PIC X(4)    VALUE SPACE.                 
004600 77  WS-IDKUNDNR-IN              PIC X(6)    VALUE SPACE.                 
004700 77  WS-KDRAPPSTA-IN             PIC X(2)    VALUE SPACE.                 
004800 77  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
004900 77  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
005000 77  WS-SUMMA                    PIC S9(7)V9(2) VALUE ZERO COMP-3.        
005100 77  WS-SUMMA-TOT                PIC S9(7)V9(2) VALUE ZERO COMP-3.        
005200 77  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
005300 77  WS-IDDISTR-NUM              PIC 9(5)    VALUE ZERO.                  
005400 77  WS-IDKUNDNR-NUM             PIC 9(7)    VALUE ZERO.                  
005500                                                                          
005600 77  KDRAPPSTA-SW                PIC X(2)   VALUE SPACE.                  
005700   88  OK-KOD                                                             
005800         VALUE 'W ' 'S ' 'SF' 'D ' 'MS' 'MF'.                             
005900                                                                          
006000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
006200 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
006300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006400                                                                          
006500                                                                          
006600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006700     88  NYCKLAR-OK                          VALUE 'J'.                   
006800     88  NYCKLAR-FEL                         VALUE 'N'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  EGEN-MID                            VALUE '4709'.                
007200     88  GODK-MID                            VALUE '4707' '4709'.         
007300     88  HELP-MID                            VALUE '0551'.                
007400     EJECT                                                                
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008400*01 -COPY WMEDAREA                                                        
008500     SKIP3                                                                
008600 01  MESSAGE-CODES.                                                       
008700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009000     03  ERR-DIST-KUND-SAKNAS    PIC X(3)    VALUE '412'.                 
009100     03  ERR-BETALARE-SAKNAS     PIC X(3)    VALUE '145'.                 
009200     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
009300     03  ERR-INFO-MISSING        PIC X(3)    VALUE '413'.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009800     SKIP3                                                                
009900*01 -COPY WMSGINIT                                                        
010000     EJECT                                                                
010100*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010200*                                                                         
010300 01  SPAR-AREA.                                                           
010400     03  SPAR-IDTRANS           PIC X(4)    VALUE '4709'.                 
010500     03  SPAR-IDPARTNR-ENTER    PIC X(9)  VALUE SPACE.                    
010600     03  SPAR-IDPARTNR-NEXT     PIC X(9)  VALUE SPACE.                    
010700     03  SPAR-IDFTG-ENTER       PIC X(2)  VALUE SPACE.                    
010800     03  SPAR-IDFTG-NEXT        PIC X(2)  VALUE SPACE.                    
010900     03  SPAR-KDANMORS-ENTER    PIC X(2)  VALUE SPACE.                    
011000     03  SPAR-KDANMORS-NEXT     PIC X(2)  VALUE SPACE.                    
011100     03  SPAR-DAREGDAT-ENTER    PIC X(8)  VALUE SPACE.                    
011200     03  SPAR-DAREGDAT-NEXT     PIC X(8)  VALUE SPACE.                    
011300     03  SPAR-IDREF-ENTER       PIC X(15) VALUE SPACE.                    
011400     03  SPAR-IDREF-NEXT        PIC X(15) VALUE SPACE.                    
011500     03  SPAR-IDEXCUST-1-ENTER  PIC X(15) VALUE SPACE.                    
011600     03  SPAR-IDEXCUST-1-NEXT   PIC X(15) VALUE SPACE.                    
011700     03  SPAR-IDEXCUST-2-ENTER  PIC X(15) VALUE SPACE.                    
011800     03  SPAR-IDEXCUST-2-NEXT   PIC X(15) VALUE SPACE.                    
011900     03  SPAR-MID-IDPARTNR      PIC X(9)  VALUE SPACE.                    
012000     03  SPAR-MID-IDFTG         PIC X(2)  VALUE SPACE.                    
012100     03  SPAR-MID-IDDISTR       PIC X(4)  VALUE SPACE.                    
012200     03  SPAR-MID-IDKUNDNR      PIC X(6)  VALUE SPACE.                    
012300     03  SPAR-MID-KDRAPPSTA     PIC X(2)  VALUE SPACE.                    
012400     EJECT                                                                
012500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012800     SKIP3                                                                
012900*01  MID -COPY W4I70901                                                   
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013200     SKIP3                                                                
013300*01  -COPY WMSGAREA                                                       
013400     EJECT                                                                
013500     03  MOD REDEFINES MSG-AREA.                                          
013600*      05  -COPY W4O70901                                                 
013700     EJECT                                                                
013800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013900     SKIP3                                                                
014000*01  -COPY WMFSAREA                                                       
014100     EJECT                                                                
014200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014300*                                                                         
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500     SKIP3                                                                
014600 01  NYCKLAR-TILL-DLI.                                                    
014700*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
014800     03  W-TP8LRET-X.                                                     
014900         05  W-IDPARTNR         PIC X(9)  VALUE SPACE.                    
015000         05  W-IDFTG            PIC X(2)  VALUE SPACE.                    
015100                                                                          
015200     03  W-KDANMORS-X.                                                    
015300         05  W-KDANMORS         PIC X(2)  VALUE SPACE.                    
015400                                                                          
015500     03  W-DAREGDAT-X.                                                    
015600         05  W-DAREGDAT         PIC X(8)  VALUE SPACE.                    
015700                                                                          
015800     03  W-IDREF-X.                                                       
015900         05  W-IDREF            PIC X(15) VALUE SPACE.                    
016000                                                                          
016100     03  W-IDEXCUST-1-X.                                                  
016200         05  W-IDEXCUST-1       PIC X(15) VALUE SPACE.                    
016300                                                                          
016400     03  W-IDEXCUST-2-X.                                                  
016500         05  W-IDEXCUST-2       PIC X(15) VALUE SPACE.                    
016600                                                                          
016700     03  W-KDRAPPSTA-X.                                                   
016800         05  W-KDRAPPSTA        PIC X(2)  VALUE SPACE.                    
016900                                                                          
017000     03  W-IDGMT-X.                                                       
017100         05  W-IDDISTR          PIC S9(5)   VALUE ZERO COMP-3.            
017200         05  W-IDKUNDNR         PIC S9(7)   VALUE ZERO COMP-3.            
017300                                                                          
017400     03  W-IDGMT-MIN-X.                                                   
017500         05  W-IDDISTR-MIN      PIC S9(5)   VALUE ZERO COMP-3.            
017600         05  W-IDKUNDNR-MIN     PIC S9(7)   VALUE ZERO COMP-3.            
017700                                                                          
017800     03  W-IDGMT-MAX-X.                                                   
017900         05  W-IDDISTR-MAX      PIC S9(5)   VALUE ZERO COMP-3.            
018000         05  W-IDKUNDNR-MAX     PIC S9(7)   VALUE ZERO COMP-3.            
018100                                                                          
018200     SKIP2                                                                
018300*    --- STATUS-KOD FRÅN IMS                                              
018400 01  STATUS-WS                   PIC XX.                                  
018500     88  SEGMENT-FINNS                       VALUE '  '.                  
018600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018800     SKIP2                                                                
018900 01  GODK-STATUSKODER.                                                    
019000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019100     SKIP3                                                                
019200 01  SSA1                        PIC X(64).                               
019300 01  SSA2                        PIC X(64).                               
019400     EJECT                                                                
019500*    --- IMS FUNKTIONSKODER                                               
019600*01  -COPY W0003                                                          
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
019900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
020000                                                                          
020100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
020200 01  DB2-WS.                                                              
020300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
020400         88  CURSOR-OK                       VALUE 000.                   
020500         88  RADER-FINNS                     VALUE 000.                   
020600         88  RADER-SAKNAS                    VALUE 100.                   
020700         88  ATKOMST-FEL                     VALUE 904.                   
020800         88  RADER-SAKNAS-TOMT               VALUE 305.                   
020900                                                                          
021000     03  GODK-SQLCODEKODER.                                               
021100         05  GODK-SQLCODE OCCURS 5                                        
021200             INDEXED BY SQLCODE-IX PIC 9(3).                              
021300     EJECT                                                                
021400*    ---  DLI INPUT-OUTPUT AREA                                           
021500                                                                          
021600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
021700 01  DLI-IO-WDB201.                                                       
021800*    03  -COPY WDB201                                                     
021900     EJECT                                                                
022000*                                                                         
022100*                                                                         
022200*    ---  DB2 HOST-COPYTEXTER                                             
022300                                                                          
022400     EJECT                                                                
022500 01  FILLER                      PIC X(16)  VALUE 'TP8LRET-AREA'.         
022600*01  -COPY TP8LRET -PRE LRET-                                             
022700                                                                          
022800 01  FILLER                      PIC X(16)  VALUE 'TP8LRET-DCL'.          
022900     EXEC SQL INCLUDE TP8LRET END-EXEC.                                   
023000                                                                          
023100     EJECT                                                                
023200 LINKAGE SECTION.                                                         
023300*01  -COPY W0009   -PRE MSG-                                              
023400                                                                          
023500*01  -COPY W0008   -PRE WDP7-                                             
023600     05  FILLER                  PIC X.                                   
023700                                                                          
023800*01  -COPY W0008  -PRE WDB2-                                              
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB2-PCB.                     
024200 MAIN SECTION.                                                            
024300     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB2-PCB.                     
024400                                                                          
024500     PERFORM IMS-GET-MSG                                                  
024600     IF SEGMENT-FINNS                                                     
024700       PERFORM A-INIT                                                     
024800       PERFORM B-KOLLA-NYCKLAR                                            
024900       IF NYCKLAR-OK                                                      
025000           IF MFS-FIRST                                                   
025100             PERFORM C-FOERSTA-SIDA                                       
025200           ELSE                                                           
025300             IF MFS-NEXT                                                  
025400               PERFORM D-NAESTA-SIDA                                      
025500             ELSE                                                         
025600               PERFORM E-SAMMA-SIDA                                       
025700             END-IF                                                       
025800           END-IF                                                         
025900         PERFORM F-LAES-VISA-INFO                                         
026000       END-IF                                                             
026100       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O70901 + 4                      
026200       PERFORM IMS-INSERT-MSG                                             
026300     END-IF                                                               
026400                                                                          
026500     MOVE ZERO TO RETURN-CODE                                             
026600     GOBACK                                                               
026700     .                                                                    
026800     EJECT                                                                
026900 A-INIT SECTION.                                                          
027000                                                                          
027100     IF MSG-DUBBLA-TRANSKODER                                             
027200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I70901                 
027300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027500     ELSE                                                                 
027600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I70901                  
027700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027900     END-IF                                                               
028000                                                                          
028100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028400                                                                          
028500     MOVE LOW-VALUE TO MSG-AREA                                           
028600     MOVE 'W4O709N1' TO MFS-IDMOD                                         
028700     MOVE '4709' TO MOD-IDTRANS                                           
028800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028900                                                                          
029000     MOVE SPACE           TO MED-IDMFSINF                                 
029100     MOVE SPACE           TO MED-IDMFSFEL                                 
029200                                                                          
029300     IF EGEN-MID OR HELP-MID                                              
029400       CONTINUE                                                           
029500     ELSE                                                                 
029600       MOVE SPACE TO MFS-KDTRTYP                                          
029700       MOVE '7' TO MFS-IDPFK                                              
029800     END-IF                                                               
029900                                                                          
030000     MOVE LOW-VALUE         TO W-IDGMT-MIN-X                              
030100     MOVE HIGH-VALUE        TO W-IDGMT-MAX-X                              
030200                                                                          
030300     INITIALIZE GODK-SQLCODEKODER                                         
030400     .                                                                    
030500     EJECT                                                                
030600 B-KOLLA-NYCKLAR SECTION.                                                 
030700                                                                          
030800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
030900     MOVE '001'             TO MSGI-KDCALL                                
031000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
031100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031200     MOVE '4709'            TO MSGI-IDTRANS                               
031300     IF EGEN-MID                                                          
031400       MOVE MID-IDPARTNR-IN  TO MSGI-IDPARTNR                             
031500                                WS-IDPARTNR-IN                            
031600       MOVE MID-IDFTG-IN     TO MSGI-IDFTG-KEY                            
031700                                WS-IDFTG-IN                               
031800       MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                              
031900                                WS-IDDISTR-IN                             
032000       MOVE MID-IDKUNDNR-IN  TO MSGI-IDKUNDNR                             
032100                                WS-IDKUNDNR-IN                            
032200       MOVE MID-KDRAPPSTA-IN TO WS-KDRAPPSTA-IN                           
032300     END-IF                                                               
032400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
032500     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
032600                                                                          
032700*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
032800     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
032900     MOVE '2'             TO MFS-KDMFSFOR                                 
033000                                                                          
033100     MOVE JA TO NYCKLAR-SW                                                
033200                                                                          
033300*    -- KONTROLL AV IDPARTNR                                              
033400     MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-IN                              
033500                                                                          
033600     IF MID-IDPARTNR-IN NOT = ALL '+'                                     
033700       MOVE '7'         TO MFS-IDPFK                                      
033800       MOVE SPACE       TO MFS-KDTRTYP                                    
033900                                                                          
034000       MOVE MSGI-IDPARTNR TO W-IDPARTNR                                   
034100     END-IF                                                               
034200                                                                          
034300*    -- KONTROLL AV IDFTG                                                 
034400     MOVE MFS-RENSA-FAELT TO MOD-IDFTG-IN                                 
034500                                                                          
034600     IF MID-IDFTG-IN NOT = ALL '+'                                        
034700       MOVE '7'         TO MFS-IDPFK                                      
034800       MOVE SPACE       TO MFS-KDTRTYP                                    
034900                                                                          
035000       INSPECT MSGI-IDFTG-KEY REPLACING LEADING SPACE BY ZERO             
035100       IF MSGI-IDFTG-KEY NUMERIC                                          
035200         MOVE MSGI-IDFTG-KEY     TO W-IDFTG                               
035300       ELSE                                                               
035400         MOVE NEJ                TO NYCKLAR-SW                            
035500       END-IF                                                             
035600     ELSE                                                                 
035700       MOVE MSGI-IDFTG           TO W-IDFTG                               
035800     END-IF                                                               
035900                                                                          
036000*    -- KONTROLL AV IDDISTR                                               
036100     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
036200                                                                          
036300     IF MID-IDDISTR-IN NOT = ALL '+'                                      
036400       MOVE '7'         TO MFS-IDPFK                                      
036500       MOVE SPACE       TO MFS-KDTRTYP                                    
036600                                                                          
036700       INSPECT MSGI-IDDISTR REPLACING ALL SPACE BY ZERO                   
036800       IF MSGI-IDDISTR NUMERIC                                            
036900         MOVE SPACE         TO W-IDEXCUST-1                               
037000         MOVE WS-IDDISTR-IN TO W-IDEXCUST-1(1:4)                          
037100                                                                          
037200         MOVE +0     TO W-ANT                                             
037300         INSPECT W-IDEXCUST-1 TALLYING W-ANT FOR CHARACTERS               
037400                 BEFORE INITIAL ' '                                       
037500         IF W-ANT > ZERO                                                  
037600           MOVE W-IDEXCUST-1(1:W-ANT) TO WS-IDDISTR-NUM                   
037700           MOVE WS-IDDISTR-NUM TO W-IDDISTR                               
037800                                  W-IDDISTR-MIN                           
037900                                  W-IDDISTR-MAX                           
038000                                                                          
038100         ELSE                                                             
038200           MOVE ZERO           TO WS-IDDISTR-NUM                          
038300           MOVE WS-IDDISTR-NUM TO W-IDDISTR                               
038400                                  W-IDDISTR-MIN                           
038500                                  W-IDDISTR-MAX                           
038600                                                                          
038700         END-IF                                                           
038800       ELSE                                                               
038900         MOVE NEJ TO NYCKLAR-SW                                           
039000       END-IF                                                             
039100     END-IF                                                               
039200                                                                          
039300*    -- KONTROLL AV IDKUNDNR                                              
039400     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
039500                                                                          
039600     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
039700       MOVE '7'         TO MFS-IDPFK                                      
039800       MOVE SPACE       TO MFS-KDTRTYP                                    
039900                                                                          
040000       INSPECT MSGI-IDKUNDNR REPLACING ALL SPACE BY ZERO                  
040100       IF MSGI-IDKUNDNR NUMERIC                                           
040200         MOVE SPACE          TO W-IDEXCUST-2                              
040300         MOVE WS-IDKUNDNR-IN TO W-IDEXCUST-2(1:6)                         
040400                                                                          
040500         MOVE +0     TO W-ANT                                             
040600         INSPECT W-IDEXCUST-2 TALLYING W-ANT FOR CHARACTERS               
040700                 BEFORE INITIAL ' '                                       
040800                                                                          
040900         IF W-ANT > ZERO                                                  
041000           MOVE W-IDEXCUST-2(1:W-ANT) TO WS-IDKUNDNR-NUM                  
041100           MOVE WS-IDKUNDNR-NUM TO W-IDKUNDNR                             
041200                                   W-IDKUNDNR-MIN                         
041300                                   W-IDKUNDNR-MAX                         
041400                                                                          
041500         ELSE                                                             
041600           MOVE ZERO            TO WS-IDKUNDNR-NUM                        
041700           MOVE WS-IDKUNDNR-NUM TO W-IDKUNDNR                             
041800                                   W-IDKUNDNR-MIN                         
041900                                   W-IDKUNDNR-MAX                         
042000                                                                          
042100         END-IF                                                           
042200       ELSE                                                               
042300         MOVE NEJ TO NYCKLAR-SW                                           
042400       END-IF                                                             
042500     END-IF                                                               
042600                                                                          
042700*    -- KONTROLL AV KDRAPPSTA                                             
042800     MOVE MFS-RENSA-FAELT TO MOD-KDRAPPSTA-IN                             
042900                                                                          
043000     IF MID-KDRAPPSTA-IN NOT = ALL '+'                                    
043100       MOVE '7'         TO MFS-IDPFK                                      
043200       MOVE SPACE       TO MFS-KDTRTYP                                    
043300       MOVE MID-KDRAPPSTA-IN    TO KDRAPPSTA-SW                           
043400                                   W-KDRAPPSTA                            
043500       IF OK-KOD                                                          
043600         CONTINUE                                                         
043700       ELSE                                                               
043800         MOVE NEJ TO NYCKLAR-SW                                           
043900       END-IF                                                             
044000     END-IF                                                               
044100                                                                          
044200     IF EGEN-MID AND MFS-FIRST                                            
044300       IF MID-IDPARTNR-IN NOT = ALL '+' AND                               
044400          MID-IDFTG-IN NOT = ALL '+' AND                                  
044500          MID-IDDISTR-IN NOT = ALL '+'                                    
044600         MOVE ERR-FLERA-FUNKTIONER    TO MED-IDMFSFEL                     
044700         MOVE NEJ TO NYCKLAR-SW                                           
044800       END-IF                                                             
044900                                                                          
045000       IF MID-IDKUNDNR-IN NOT = ALL '+' AND                               
045100          MID-IDDISTR-IN = ALL '+'                                        
045200         MOVE NEJ TO NYCKLAR-SW                                           
045300       END-IF                                                             
045400     END-IF                                                               
045500                                                                          
045600     IF GODK-MID OR NYCKLAR-OK                                            
045700       IF EGEN-MID                                                        
045800         IF MFS-FIRST                                                     
045900           IF MID-IDPARTNR-IN = ALL '+'                                   
046000             MOVE SPACE            TO MOD-IDPARTNR-UT                     
046100           ELSE                                                           
046200             MOVE MSGI-IDPARTNR    TO MOD-IDPARTNR-UT                     
046300           END-IF                                                         
046400           IF MID-IDFTG-IN = ALL '+'                                      
046500             MOVE SPACE            TO MOD-IDFTG-UT                        
046600           ELSE                                                           
046700             MOVE MSGI-IDFTG-KEY   TO MOD-IDFTG-UT                        
046800           END-IF                                                         
046900           IF MID-IDDISTR-IN = ALL '+'                                    
047000             MOVE SPACE            TO MOD-IDDISTR-UT                      
047100           ELSE                                                           
047200             MOVE MID-IDDISTR-IN   TO MOD-IDDISTR-UT                      
047300           END-IF                                                         
047400           IF MID-IDKUNDNR-IN = ALL '+'                                   
047500             MOVE SPACE            TO MOD-IDKUNDNR-UT                     
047600           ELSE                                                           
047700             MOVE MID-IDKUNDNR-IN  TO MOD-IDKUNDNR-UT                     
047800           END-IF                                                         
047900           IF MID-KDRAPPSTA-IN = ALL '+'                                  
048000             MOVE SPACE            TO MOD-KDRAPPSTA-UT                    
048100           ELSE                                                           
048200             MOVE MID-KDRAPPSTA-IN TO MOD-KDRAPPSTA-UT                    
048300           END-IF                                                         
048400         ELSE                                                             
048500           IF SPAR-IDTRANS = '4709'                                       
048600             IF SPAR-MID-IDPARTNR = ALL '+'                               
048700               MOVE SPACE             TO MOD-IDPARTNR-UT                  
048800             ELSE                                                         
048900               MOVE SPAR-MID-IDPARTNR TO MOD-IDPARTNR-UT                  
049000             END-IF                                                       
049100             IF SPAR-MID-IDFTG = ALL '+'                                  
049200               MOVE SPACE             TO MOD-IDFTG-UT                     
049300             ELSE                                                         
049400               MOVE SPAR-MID-IDFTG    TO MOD-IDFTG-UT                     
049500             END-IF                                                       
049600             IF SPAR-MID-IDDISTR = ALL '+'                                
049700               MOVE SPACE             TO MOD-IDDISTR-UT                   
049800             ELSE                                                         
049900               MOVE SPAR-MID-IDDISTR  TO MOD-IDDISTR-UT                   
050000             END-IF                                                       
050100             IF SPAR-MID-IDKUNDNR = ALL '+'                               
050200               MOVE SPACE             TO MOD-IDKUNDNR-UT                  
050300             ELSE                                                         
050400               MOVE SPAR-MID-IDKUNDNR TO MOD-IDKUNDNR-UT                  
050500             END-IF                                                       
050600             IF SPAR-MID-KDRAPPSTA = ALL '+'                              
050700               MOVE SPACE              TO MOD-KDRAPPSTA-UT                
050800             ELSE                                                         
050900               MOVE SPAR-MID-KDRAPPSTA TO MOD-KDRAPPSTA-UT                
051000             END-IF                                                       
051100           END-IF                                                         
051200         END-IF                                                           
051300       END-IF                                                             
051400     ELSE                                                                 
051500       MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-UT                            
051600                               MOD-IDFTG-UT                               
051700                               MOD-IDDISTR-UT                             
051800                               MOD-IDKUNDNR-UT                            
051900                               MOD-KDRAPPSTA-UT                           
052000     END-IF                                                               
052100                                                                          
052200     IF NYCKLAR-FEL                                                       
052300       IF MED-IDMFSFEL  = SPACE                                           
052400         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
052500       END-IF                                                             
052600       CALL WMEDKONV USING MED-WMEDAREA                                   
052700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
052800       PERFORM MFS-RENSA-FAELT-UT                                         
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 C-FOERSTA-SIDA SECTION.                                                  
053300                                                                          
053400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
053500     CALL WMEDKONV USING MED-WMEDAREA                                     
053600     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
053700                                                                          
053800*FIX FÖR ATT KUNNA TRYCKA PF7.                                            
053900     IF (MID-IDPARTNR-IN = ALL '+') AND                                   
054000        (MID-IDFTG-IN = ALL '+') AND                                      
054100        (MID-IDDISTR-IN = ALL '+')  AND                                   
054200        (MID-IDKUNDNR-IN = ALL '+') AND                                   
054300        (MID-KDRAPPSTA-IN = ALL '+')                                      
054400                                                                          
054500       IF SPAR-IDTRANS = '4709'                                           
054600         MOVE SPAR-MID-IDPARTNR TO WS-IDPARTNR-IN                         
054700         IF SPAR-MID-IDPARTNR NOT = ALL '+'                               
054800           MOVE SPAR-MID-IDPARTNR TO W-IDPARTNR                           
054900         END-IF                                                           
055000                                                                          
055100         MOVE SPAR-MID-IDFTG    TO WS-IDFTG-IN                            
055200         IF SPAR-MID-IDFTG NOT = ALL '+'                                  
055300           MOVE SPAR-MID-IDFTG TO W-IDFTG                                 
055400         END-IF                                                           
055500                                                                          
055600         MOVE SPAR-MID-IDDISTR TO WS-IDDISTR-IN                           
055700         IF SPAR-MID-IDDISTR NOT = ALL '+'                                
055800           MOVE SPACE         TO W-IDEXCUST-1                             
055900           MOVE WS-IDDISTR-IN TO W-IDEXCUST-1(1:4)                        
056000         END-IF                                                           
056100                                                                          
056200         MOVE SPAR-MID-IDKUNDNR TO WS-IDKUNDNR-IN                         
056300         IF SPAR-MID-IDKUNDNR NOT = ALL '+'                               
056400           MOVE SPACE         TO W-IDEXCUST-2                             
056500           MOVE WS-IDKUNDNR-IN TO W-IDEXCUST-2(1:6)                       
056600         END-IF                                                           
056700                                                                          
056800         MOVE SPAR-MID-KDRAPPSTA TO WS-KDRAPPSTA-IN                       
056900         IF SPAR-MID-KDRAPPSTA NOT = ALL '+'                              
057000           MOVE WS-KDRAPPSTA-IN TO W-KDRAPPSTA                            
057100         END-IF                                                           
057200                                                                          
057300       END-IF                                                             
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 D-NAESTA-SIDA SECTION.                                                   
057800                                                                          
057900     IF SPAR-IDTRANS = '4709'                                             
058000       MOVE SPAR-IDPARTNR-NEXT TO W-IDPARTNR                              
058100       MOVE SPAR-IDFTG-NEXT    TO W-IDFTG                                 
058200       MOVE SPAR-KDANMORS-NEXT TO W-KDANMORS                              
058300       MOVE SPAR-DAREGDAT-NEXT TO W-DAREGDAT                              
058400       MOVE SPAR-IDREF-NEXT    TO W-IDREF                                 
058500       MOVE SPAR-IDEXCUST-1-NEXT TO W-IDEXCUST-1                          
058600       MOVE SPAR-IDEXCUST-2-NEXT TO W-IDEXCUST-2                          
058700                                                                          
058800       MOVE SPAR-MID-IDPARTNR    TO WS-IDPARTNR-IN                        
058900       MOVE SPAR-MID-IDFTG       TO WS-IDFTG-IN                           
059000       MOVE SPAR-MID-IDDISTR     TO WS-IDDISTR-IN                         
059100       MOVE SPAR-MID-IDKUNDNR    TO WS-IDKUNDNR-IN                        
059200       MOVE SPAR-MID-KDRAPPSTA   TO WS-KDRAPPSTA-IN                       
059300                                                                          
059400       IF WS-KDRAPPSTA-IN NOT = ALL '+'                                   
059500         MOVE SPAR-MID-KDRAPPSTA TO W-KDRAPPSTA                           
059600       END-IF                                                             
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 E-SAMMA-SIDA SECTION.                                                    
060100                                                                          
060200     IF SPAR-IDTRANS = '4709' OR '0551'                                   
060300       MOVE SPAR-IDPARTNR-ENTER TO W-IDPARTNR                             
060400       MOVE SPAR-IDFTG-ENTER    TO W-IDFTG                                
060500       MOVE SPAR-KDANMORS-ENTER TO W-KDANMORS                             
060600       MOVE SPAR-DAREGDAT-ENTER TO W-DAREGDAT                             
060700       MOVE SPAR-IDREF-ENTER    TO W-IDREF                                
060800       MOVE SPAR-IDEXCUST-1-ENTER TO W-IDEXCUST-1                         
060900       MOVE SPAR-IDEXCUST-2-ENTER TO W-IDEXCUST-2                         
061000                                                                          
061100       MOVE SPAR-MID-IDPARTNR    TO WS-IDPARTNR-IN                        
061200       MOVE SPAR-MID-IDFTG       TO WS-IDFTG-IN                           
061300       MOVE SPAR-MID-IDDISTR     TO WS-IDDISTR-IN                         
061400       MOVE SPAR-MID-IDKUNDNR    TO WS-IDKUNDNR-IN                        
061500       MOVE SPAR-MID-KDRAPPSTA   TO WS-KDRAPPSTA-IN                       
061600                                                                          
061700       IF WS-KDRAPPSTA-IN NOT = ALL '+'                                   
061800         MOVE SPAR-MID-KDRAPPSTA TO W-KDRAPPSTA                           
061900       END-IF                                                             
062000     END-IF                                                               
062100     .                                                                    
062200     EJECT                                                                
062300 F-LAES-VISA-INFO SECTION.                                                
062400                                                                          
062500     IF MFS-FIRST                                                         
062600       IF WS-IDPARTNR-IN NOT = ALL '+' AND                                
062700          WS-IDFTG-IN NOT = ALL '+'                                       
062800         IF WS-KDRAPPSTA-IN = ALL '+'                                     
062900           PERFORM FA-LAES-ALLA-IDPARTNR                                  
063000         ELSE                                                             
063100           PERFORM FB-LAES-IDPARTNR-KDRAPPSTA                             
063200         END-IF                                                           
063300       ELSE                                                               
063400         IF WS-IDDISTR-IN NOT = ALL '+'                                   
063500           IF WS-IDKUNDNR-IN = ALL '+'                                    
063600             PERFORM IMS-GU-WDB201-MIN-MAX                                
063700             IF SEGMENT-FINNS                                             
063800               MOVE GMT-IDPARTNR   TO W-IDPARTNR                          
063900                                      MOD-IDPARTNR-UT                     
064000                                      WS-IDPARTNR-IN                      
064100               MOVE GMT-IDFTG      TO W-IDFTG                             
064200                                      MOD-IDFTG-UT                        
064300                                      WS-IDFTG-IN                         
064400                                                                          
064500               IF WS-KDRAPPSTA-IN = ALL '+'                               
064600                 PERFORM FA-LAES-ALLA-IDPARTNR                            
064700               ELSE                                                       
064800                 PERFORM FB-LAES-IDPARTNR-KDRAPPSTA                       
064900               END-IF                                                     
065000             ELSE                                                         
065100               MOVE ERR-DIST-KUND-SAKNAS  TO MED-IDMFSFEL                 
065200               CALL WMEDKONV USING MED-WMEDAREA                           
065300               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
065400               PERFORM MFS-RENSA-FAELT-UT                                 
065500             END-IF                                                       
065600           ELSE                                                           
065700             IF WS-KDRAPPSTA-IN = ALL '+'                                 
065800               PERFORM FC-LAES-ALLA-DISTRIKT                              
065900             ELSE                                                         
066000               PERFORM FD-LAES-DIST-KDRAPPSTA                             
066100             END-IF                                                       
066200           END-IF                                                         
066300         END-IF                                                           
066400       END-IF                                                             
066500     ELSE                                                                 
066600       IF WS-IDPARTNR-IN NOT = ALL '+' AND                                
066700          WS-IDFTG-IN NOT = ALL '+'                                       
066800         IF WS-KDRAPPSTA-IN = ALL '+'                                     
066900           PERFORM FA-LAES-ALLA-IDPARTNR                                  
067000         ELSE                                                             
067100           PERFORM FB-LAES-IDPARTNR-KDRAPPSTA                             
067200         END-IF                                                           
067300       ELSE                                                               
067400         IF WS-KDRAPPSTA-IN = ALL '+'                                     
067500           PERFORM FC-LAES-ALLA-DISTRIKT                                  
067600         ELSE                                                             
067700           PERFORM FD-LAES-DIST-KDRAPPSTA                                 
067800         END-IF                                                           
067900       END-IF                                                             
068000     END-IF                                                               
068100                                                                          
068200     IF RADER-FINNS                                                       
068300       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-NEXT                         
068400       MOVE LRET-IDFTG      TO SPAR-IDFTG-NEXT                            
068500       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-NEXT                         
068600       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-NEXT                         
068700       MOVE LRET-IDREF      TO SPAR-IDREF-NEXT                            
068800       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-NEXT                       
068900       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-NEXT                       
069000       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
069100       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
069200       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
069300       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
069400       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
069500       IF MED-IDMFSFEL = '412'                                            
069600         CONTINUE                                                         
069700       ELSE                                                               
069800         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
069900         CALL WMEDKONV USING MED-WMEDAREA                                 
070000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
070100       END-IF                                                             
070200     ELSE                                                                 
070300       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-NEXT                         
070400       MOVE LRET-IDFTG      TO SPAR-IDFTG-NEXT                            
070500       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-NEXT                         
070600       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-NEXT                         
070700       MOVE LRET-IDREF      TO SPAR-IDREF-NEXT                            
070800       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-NEXT                       
070900       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-NEXT                       
071000       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
071100       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
071200       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
071300       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
071400       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
071500     END-IF                                                               
071600                                                                          
071700     IF MED-IDMFSFEL = '413' OR '412'                                     
071800       CONTINUE                                                           
071900     ELSE                                                                 
072000       IF WS-IDPARTNR-IN NOT = ALL '+' AND                                
072100          WS-IDFTG-IN NOT = ALL '+'                                       
072200         IF W-IDPARTNR NOT = SPACE AND                                    
072300            W-IDFTG    NOT = SPACE                                        
072400           PERFORM FE-SUM-ALLA-KODER-RAD19                                
072500           PERFORM FF-SUM-ALLA-KODER-TOT                                  
072600         END-IF                                                           
072700       END-IF                                                             
072800     END-IF                                                               
072900                                                                          
073000     MOVE '002'      TO MSGI-KDCALL                                       
073100     MOVE '4709'     TO SPAR-IDTRANS                                      
073200     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
073300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
073400     .                                                                    
073500     EJECT                                                                
073600 FA-LAES-ALLA-IDPARTNR SECTION.                                           
073700                                                                          
073800     PERFORM DB2-DCL-OPN-TP8LRET-CRS                                      
073900     PERFORM DB2-FETCH-TP8LRET-CRS                                        
074000                                                                          
074100     IF MFS-ENTER OR MFS-NEXT                                             
074200       PERFORM UNTIL RADER-SAKNAS OR RADER-SAKNAS-TOMT OR                 
074300              (LRET-IDPARTNR = W-IDPARTNR AND                             
074400               LRET-IDFTG    = W-IDFTG    AND                             
074500               LRET-KDANMORS = W-KDANMORS AND                             
074600               LRET-DAREGDAT = W-DAREGDAT AND                             
074700               LRET-IDREF    = W-IDREF    AND                             
074800               LRET-IDEXCUST-1 = W-IDEXCUST-1 AND                         
074900               LRET-IDEXCUST-2 = W-IDEXCUST-2)                            
075000                                                                          
075100         PERFORM DB2-FETCH-TP8LRET-CRS                                    
075200       END-PERFORM                                                        
075300     END-IF                                                               
075400                                                                          
075500     IF RADER-FINNS                                                       
075600       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-ENTER                        
075700       MOVE LRET-IDFTG      TO SPAR-IDFTG-ENTER                           
075800       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-ENTER                        
075900       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-ENTER                        
076000       MOVE LRET-IDREF      TO SPAR-IDREF-ENTER                           
076100       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-ENTER                      
076200       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-ENTER                      
076300                                                                          
076400       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
076500       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
076600       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
076700       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
076800       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
076900                                                                          
077000       MOVE +1  TO INDX                                                   
077100       PERFORM UNTIL INDX > MAX-INDX                                      
077200                                                                          
077300       IF RADER-FINNS                                                     
077400         MOVE LRET-KDANMORS        TO MOD-KDANMORS (INDX)                 
077500         MOVE LRET-IDEXCUST-1(1:4) TO MOD-IDDISTR  (INDX)                 
077600         MOVE LRET-IDEXCUST-2(1:6) TO MOD-IDKUNDNR (INDX)                 
077700                                                                          
077800         COMPUTE MOD-SUARTBTO (INDX) ROUNDED =                            
077900                 LRET-PRARTNTO * LRET-KVLEVART                            
078000         END-COMPUTE                                                      
078100                                                                          
078200**** WHEN INVOICED THE CURRENCY MAY NOT BE IN SEK                         
078300         IF LRET-KDRAPPSTA = 'MF'                                         
078400         OR LRET-KDRAPPSTA = 'SF'                                         
078500           MOVE LRET-KDVALISO      TO MOD-KDVALISO (INDX)                 
078600         ELSE                                                             
078700           MOVE 'SEK'              TO MOD-KDVALISO (INDX)                 
078800         END-IF                                                           
078900         MOVE LRET-IDREF(1:10)     TO MOD-IDRAPP   (INDX)                 
079000         MOVE LRET-KDRAPPSTA       TO MOD-KDRAPPSTA(INDX)                 
079100         MOVE LRET-IDFINDOC        TO MOD-IDFINDOC (INDX)                 
079200         MOVE LRET-IDUSER-2        TO MOD-IDUSER-2 (INDX)                 
079300                                                                          
079400* AH/OM FAKTURAN ÄR DELETAD SKALL MAN INTE VISA FAK.DAT. DETTA            
079500* -- DATUM FINNS MED FÖR RENSNINGSDATUM AV TABELL TP8LRET.                
079600         IF LRET-KDRAPPSTA = 'D '                                         
079700           MOVE LRET-DADELDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
079800           MOVE ZERO               TO MOD-DAFAKT   (INDX)                 
079900         ELSE                                                             
080000           MOVE LRET-DAUPPDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
080100           MOVE LRET-DAFAKT(3:6)   TO MOD-DAFAKT   (INDX)                 
080200         END-IF                                                           
080300                                                                          
080400         PERFORM DB2-FETCH-TP8LRET-CRS                                    
080500       ELSE                                                               
080600         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
080700       END-IF                                                             
080800       ADD +1  TO INDX                                                    
080900       END-PERFORM                                                        
081000     ELSE                                                                 
081100       MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                            
081200       CALL WMEDKONV USING MED-WMEDAREA                                   
081300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
081400       PERFORM MFS-RENSA-FAELT-UT                                         
081500                                                                          
081600       MOVE W-IDPARTNR       TO SPAR-IDPARTNR-ENTER                       
081700       MOVE W-IDFTG          TO SPAR-IDFTG-ENTER                          
081800       MOVE W-KDANMORS       TO SPAR-KDANMORS-ENTER                       
081900       MOVE W-DAREGDAT       TO SPAR-DAREGDAT-ENTER                       
082000       MOVE W-IDREF          TO SPAR-IDREF-ENTER                          
082100       MOVE W-IDEXCUST-1     TO SPAR-IDEXCUST-1-ENTER                     
082200       MOVE W-IDEXCUST-2     TO SPAR-IDEXCUST-2-ENTER                     
082300                                                                          
082400       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
082500       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
082600       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
082700       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
082800       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
082900     END-IF                                                               
083000                                                                          
083100     PERFORM DB2-CLOSE-TP8LRET-CRS                                        
083200     .                                                                    
083300     EJECT                                                                
083400 FB-LAES-IDPARTNR-KDRAPPSTA SECTION.                                      
083500                                                                          
083600     PERFORM DB2-DCL-OPN-TP8LRET-CRS2                                     
083700     PERFORM DB2-FETCH-TP8LRET-CRS2                                       
083800                                                                          
083900     IF MFS-ENTER OR MFS-NEXT                                             
084000       PERFORM UNTIL RADER-SAKNAS OR RADER-SAKNAS-TOMT OR                 
084100              (LRET-IDPARTNR = W-IDPARTNR AND                             
084200               LRET-IDFTG    = W-IDFTG    AND                             
084300               LRET-KDANMORS = W-KDANMORS AND                             
084400               LRET-DAREGDAT = W-DAREGDAT AND                             
084500               LRET-IDREF    = W-IDREF    AND                             
084600               LRET-IDEXCUST-1 = W-IDEXCUST-1 AND                         
084700               LRET-IDEXCUST-2 = W-IDEXCUST-2)                            
084800                                                                          
084900         PERFORM DB2-FETCH-TP8LRET-CRS2                                   
085000       END-PERFORM                                                        
085100     END-IF                                                               
085200                                                                          
085300     IF RADER-FINNS                                                       
085400       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-ENTER                        
085500       MOVE LRET-IDFTG      TO SPAR-IDFTG-ENTER                           
085600       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-ENTER                        
085700       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-ENTER                        
085800       MOVE LRET-IDREF      TO SPAR-IDREF-ENTER                           
085900       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-ENTER                      
086000       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-ENTER                      
086100                                                                          
086200       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
086300       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
086400       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
086500       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
086600       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
086700                                                                          
086800       MOVE +1  TO INDX                                                   
086900       PERFORM UNTIL INDX > MAX-INDX                                      
087000                                                                          
087100       IF RADER-FINNS                                                     
087200         MOVE LRET-KDANMORS        TO MOD-KDANMORS (INDX)                 
087300         MOVE LRET-IDEXCUST-1(1:4) TO MOD-IDDISTR  (INDX)                 
087400         MOVE LRET-IDEXCUST-2(1:6) TO MOD-IDKUNDNR (INDX)                 
087500                                                                          
087600         COMPUTE MOD-SUARTBTO (INDX) ROUNDED =                            
087700                 LRET-PRARTNTO * LRET-KVLEVART                            
087800         END-COMPUTE                                                      
087900                                                                          
088000**** WHEN INVOICED THE CURRENCY MAY NOT BE IN SEK                         
088100         IF LRET-KDRAPPSTA = 'MF'                                         
088200         OR LRET-KDRAPPSTA = 'SF'                                         
088300           MOVE LRET-KDVALISO      TO MOD-KDVALISO (INDX)                 
088400         ELSE                                                             
088500           MOVE 'SEK'              TO MOD-KDVALISO (INDX)                 
088600         END-IF                                                           
088700         MOVE LRET-IDREF(1:10)     TO MOD-IDRAPP   (INDX)                 
088800         MOVE LRET-KDRAPPSTA       TO MOD-KDRAPPSTA(INDX)                 
088900         MOVE LRET-IDFINDOC        TO MOD-IDFINDOC (INDX)                 
089000         MOVE LRET-IDUSER-2        TO MOD-IDUSER-2 (INDX)                 
089100         IF LRET-KDRAPPSTA = 'D '                                         
089200           MOVE LRET-DADELDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
089300           MOVE ZERO               TO MOD-DAFAKT   (INDX)                 
089400         ELSE                                                             
089500           MOVE LRET-DAUPPDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
089600           MOVE LRET-DAFAKT(3:6)   TO MOD-DAFAKT   (INDX)                 
089700         END-IF                                                           
089800                                                                          
089900         PERFORM DB2-FETCH-TP8LRET-CRS2                                   
090000       ELSE                                                               
090100         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
090200       END-IF                                                             
090300       ADD +1  TO INDX                                                    
090400       END-PERFORM                                                        
090500     ELSE                                                                 
090600       MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                            
090700       CALL WMEDKONV USING MED-WMEDAREA                                   
090800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
090900       PERFORM MFS-RENSA-FAELT-UT                                         
091000                                                                          
091100       MOVE W-IDPARTNR       TO SPAR-IDPARTNR-ENTER                       
091200       MOVE W-IDFTG          TO SPAR-IDFTG-ENTER                          
091300       MOVE W-KDANMORS       TO SPAR-KDANMORS-ENTER                       
091400       MOVE W-DAREGDAT       TO SPAR-DAREGDAT-ENTER                       
091500       MOVE W-IDREF          TO SPAR-IDREF-ENTER                          
091600       MOVE W-IDEXCUST-1     TO SPAR-IDEXCUST-1-ENTER                     
091700       MOVE W-IDEXCUST-2     TO SPAR-IDEXCUST-2-ENTER                     
091800                                                                          
091900       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
092000       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
092100       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
092200       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
092300       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
092400     END-IF                                                               
092500                                                                          
092600     PERFORM DB2-CLOSE-TP8LRET-CRS2                                       
092700     .                                                                    
092800     EJECT                                                                
092900 FC-LAES-ALLA-DISTRIKT SECTION.                                           
093000                                                                          
093100     PERFORM DB2-DCL-OPN-TP8LRET-CRS4                                     
093200     PERFORM DB2-FETCH-TP8LRET-CRS4                                       
093300                                                                          
093400     IF MFS-ENTER OR MFS-NEXT                                             
093500       PERFORM UNTIL RADER-SAKNAS OR RADER-SAKNAS-TOMT OR                 
093600              (LRET-IDPARTNR = W-IDPARTNR AND                             
093700               LRET-IDFTG    = W-IDFTG    AND                             
093800               LRET-KDANMORS = W-KDANMORS AND                             
093900               LRET-DAREGDAT = W-DAREGDAT AND                             
094000               LRET-IDREF    = W-IDREF    AND                             
094100               LRET-IDEXCUST-1 = W-IDEXCUST-1 AND                         
094200               LRET-IDEXCUST-2 = W-IDEXCUST-2)                            
094300                                                                          
094400         PERFORM DB2-FETCH-TP8LRET-CRS4                                   
094500       END-PERFORM                                                        
094600     END-IF                                                               
094700                                                                          
094800     IF RADER-FINNS                                                       
094900       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-ENTER                        
095000       MOVE LRET-IDFTG      TO SPAR-IDFTG-ENTER                           
095100       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-ENTER                        
095200       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-ENTER                        
095300       MOVE LRET-IDREF      TO SPAR-IDREF-ENTER                           
095400       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-ENTER                      
095500       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-ENTER                      
095600                                                                          
095700       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
095800       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
095900       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
096000       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
096100       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
096200                                                                          
096300       MOVE +1  TO INDX                                                   
096400       PERFORM UNTIL INDX > MAX-INDX                                      
096500                                                                          
096600       IF RADER-FINNS                                                     
096700         MOVE LRET-KDANMORS        TO MOD-KDANMORS (INDX)                 
096800         MOVE LRET-IDEXCUST-1(1:4) TO MOD-IDDISTR  (INDX)                 
096900         MOVE LRET-IDEXCUST-2(1:6) TO MOD-IDKUNDNR (INDX)                 
097000                                                                          
097100         COMPUTE MOD-SUARTBTO (INDX) ROUNDED =                            
097200                 LRET-PRARTNTO * LRET-KVLEVART                            
097300         END-COMPUTE                                                      
097400                                                                          
097500**** WHEN INVOICED THE CURRENCY MAY NOT BE IN SEK                         
097600         IF LRET-KDRAPPSTA = 'MF'                                         
097700         OR LRET-KDRAPPSTA = 'SF'                                         
097800           MOVE LRET-KDVALISO      TO MOD-KDVALISO (INDX)                 
097900         ELSE                                                             
098000           MOVE 'SEK'              TO MOD-KDVALISO (INDX)                 
098100         END-IF                                                           
098200         MOVE LRET-IDREF(1:10)     TO MOD-IDRAPP   (INDX)                 
098300         MOVE LRET-KDRAPPSTA       TO MOD-KDRAPPSTA(INDX)                 
098400         MOVE LRET-IDFINDOC        TO MOD-IDFINDOC (INDX)                 
098500         MOVE LRET-IDUSER-2        TO MOD-IDUSER-2 (INDX)                 
098600         IF LRET-KDRAPPSTA = 'D '                                         
098700           MOVE LRET-DADELDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
098800           MOVE ZERO               TO MOD-DAFAKT   (INDX)                 
098900         ELSE                                                             
099000           MOVE LRET-DAUPPDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
099100           MOVE LRET-DAFAKT(3:6)   TO MOD-DAFAKT   (INDX)                 
099200         END-IF                                                           
099300                                                                          
099400         PERFORM DB2-FETCH-TP8LRET-CRS4                                   
099500       ELSE                                                               
099600         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
099700       END-IF                                                             
099800       ADD +1  TO INDX                                                    
099900       END-PERFORM                                                        
100000     ELSE                                                                 
100100       MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                            
100200       CALL WMEDKONV USING MED-WMEDAREA                                   
100300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
100400       PERFORM MFS-RENSA-FAELT-UT                                         
100500                                                                          
100600       MOVE W-IDPARTNR       TO SPAR-IDPARTNR-ENTER                       
100700       MOVE W-IDFTG          TO SPAR-IDFTG-ENTER                          
100800       MOVE W-KDANMORS       TO SPAR-KDANMORS-ENTER                       
100900       MOVE W-DAREGDAT       TO SPAR-DAREGDAT-ENTER                       
101000       MOVE W-IDREF          TO SPAR-IDREF-ENTER                          
101100       MOVE W-IDEXCUST-1     TO SPAR-IDEXCUST-1-ENTER                     
101200       MOVE W-IDEXCUST-2     TO SPAR-IDEXCUST-2-ENTER                     
101300                                                                          
101400       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
101500       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
101600       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
101700       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
101800       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
101900     END-IF                                                               
102000                                                                          
102100     PERFORM DB2-CLOSE-TP8LRET-CRS4                                       
102200     .                                                                    
102300     EJECT                                                                
102400 FD-LAES-DIST-KDRAPPSTA SECTION.                                          
102500                                                                          
102600     PERFORM DB2-DCL-OPN-TP8LRET-CRS5                                     
102700     PERFORM DB2-FETCH-TP8LRET-CRS5                                       
102800                                                                          
102900     IF MFS-ENTER OR MFS-NEXT                                             
103000       PERFORM UNTIL RADER-SAKNAS OR RADER-SAKNAS-TOMT OR                 
103100              (LRET-IDPARTNR = W-IDPARTNR AND                             
103200               LRET-IDFTG    = W-IDFTG    AND                             
103300               LRET-KDANMORS = W-KDANMORS AND                             
103400               LRET-DAREGDAT = W-DAREGDAT AND                             
103500               LRET-IDREF    = W-IDREF    AND                             
103600               LRET-IDEXCUST-1 = W-IDEXCUST-1 AND                         
103700               LRET-IDEXCUST-2 = W-IDEXCUST-2)                            
103800                                                                          
103900         PERFORM DB2-FETCH-TP8LRET-CRS5                                   
104000       END-PERFORM                                                        
104100     END-IF                                                               
104200                                                                          
104300     IF RADER-FINNS                                                       
104400       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-ENTER                        
104500       MOVE LRET-IDFTG      TO SPAR-IDFTG-ENTER                           
104600       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-ENTER                        
104700       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-ENTER                        
104800       MOVE LRET-IDREF      TO SPAR-IDREF-ENTER                           
104900       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-ENTER                      
105000       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-ENTER                      
105100                                                                          
105200       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
105300       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
105400       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
105500       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
105600       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
105700                                                                          
105800       MOVE +1  TO INDX                                                   
105900       PERFORM UNTIL INDX > MAX-INDX                                      
106000                                                                          
106100       IF RADER-FINNS                                                     
106200         MOVE LRET-KDANMORS        TO MOD-KDANMORS (INDX)                 
106300         MOVE LRET-IDEXCUST-1(1:4) TO MOD-IDDISTR  (INDX)                 
106400         MOVE LRET-IDEXCUST-2(1:6) TO MOD-IDKUNDNR (INDX)                 
106500                                                                          
106600         COMPUTE MOD-SUARTBTO (INDX) ROUNDED =                            
106700                 LRET-PRARTNTO * LRET-KVLEVART                            
106800         END-COMPUTE                                                      
106900                                                                          
107000**** WHEN INVOICED THE CURRENCY MAY NOT BE IN SEK                         
107100         IF LRET-KDRAPPSTA = 'MF'                                         
107200         OR LRET-KDRAPPSTA = 'SF'                                         
107300           MOVE LRET-KDVALISO      TO MOD-KDVALISO (INDX)                 
107400         ELSE                                                             
107500           MOVE 'SEK'              TO MOD-KDVALISO (INDX)                 
107600         END-IF                                                           
107700         MOVE LRET-IDREF(1:10)     TO MOD-IDRAPP   (INDX)                 
107800         MOVE LRET-KDRAPPSTA       TO MOD-KDRAPPSTA(INDX)                 
107900         MOVE LRET-IDFINDOC        TO MOD-IDFINDOC (INDX)                 
108000         MOVE LRET-IDUSER-2        TO MOD-IDUSER-2 (INDX)                 
108100         IF LRET-KDRAPPSTA = 'D '                                         
108200           MOVE LRET-DADELDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
108300           MOVE ZERO               TO MOD-DAFAKT   (INDX)                 
108400         ELSE                                                             
108500           MOVE LRET-DAUPPDAT(3:6) TO MOD-DAUPPDAT (INDX)                 
108600           MOVE LRET-DAFAKT(3:6)   TO MOD-DAFAKT   (INDX)                 
108700         END-IF                                                           
108800                                                                          
108900         PERFORM DB2-FETCH-TP8LRET-CRS5                                   
109000       ELSE                                                               
109100         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
109200       END-IF                                                             
109300       ADD +1  TO INDX                                                    
109400       END-PERFORM                                                        
109500     ELSE                                                                 
109600       MOVE ERR-INFO-MISSING   TO MED-IDMFSFEL                            
109700       CALL WMEDKONV USING MED-WMEDAREA                                   
109800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
109900       PERFORM MFS-RENSA-FAELT-UT                                         
110000                                                                          
110100       MOVE W-IDPARTNR       TO SPAR-IDPARTNR-ENTER                       
110200       MOVE W-IDFTG          TO SPAR-IDFTG-ENTER                          
110300       MOVE W-KDANMORS       TO SPAR-KDANMORS-ENTER                       
110400       MOVE W-DAREGDAT       TO SPAR-DAREGDAT-ENTER                       
110500       MOVE W-IDREF          TO SPAR-IDREF-ENTER                          
110600       MOVE W-IDEXCUST-1     TO SPAR-IDEXCUST-1-ENTER                     
110700       MOVE W-IDEXCUST-2     TO SPAR-IDEXCUST-2-ENTER                     
110800                                                                          
110900       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
111000       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
111100       MOVE WS-IDDISTR-IN   TO SPAR-MID-IDDISTR                           
111200       MOVE WS-IDKUNDNR-IN  TO SPAR-MID-IDKUNDNR                          
111300       MOVE WS-KDRAPPSTA-IN TO SPAR-MID-KDRAPPSTA                         
111400     END-IF                                                               
111500                                                                          
111600     PERFORM DB2-CLOSE-TP8LRET-CRS5                                       
111700     .                                                                    
111800     EJECT                                                                
111900 FE-SUM-ALLA-KODER-RAD19  SECTION.                                        
112000                                                                          
112100     MOVE ZERO            TO WS-SUMMA                                     
112200     MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-72                              
112300                             MOD-KDVALISO-72                              
112400                             MOD-SUARTBTO-98                              
112500                             MOD-KDVALISO-98                              
112600                             MOD-SUARTBTO-RR                              
112700                             MOD-KDVALISO-RR                              
112800                                                                          
112900     PERFORM DB2-DCL-OPN-TP8LRET-CRS3                                     
113000     PERFORM DB2-FETCH-TP8LRET-CRS3                                       
113100     IF RADER-FINNS                                                       
113200       PERFORM UNTIL RADER-SAKNAS OR RADER-SAKNAS-TOMT                    
113300         PERFORM DB2-SELECT-TP8LRET-TAB                                   
113400         IF RADER-FINNS                                                   
113500           EVALUATE WS-KDANMORS                                           
113600             WHEN '72'                                                    
113700               MOVE 'SEK'       TO MOD-KDVALISO-72                        
113800               MOVE WS-SUMMA    TO MOD-SUARTBTO-72                        
113900             WHEN '98'                                                    
114000               MOVE 'SEK'       TO MOD-KDVALISO-98                        
114100               MOVE WS-SUMMA    TO MOD-SUARTBTO-98                        
114200             WHEN OTHER                                                   
114300               MOVE 'SEK'       TO MOD-KDVALISO-RR                        
114400               MOVE WS-SUMMA    TO MOD-SUARTBTO-RR                        
114500           END-EVALUATE                                                   
114600         END-IF                                                           
114700         PERFORM DB2-FETCH-TP8LRET-CRS3                                   
114800       END-PERFORM                                                        
114900     END-IF                                                               
115000                                                                          
115100     PERFORM DB2-CLOSE-TP8LRET-CRS3                                       
115200                                                                          
115300     .                                                                    
115400     EJECT                                                                
115500 FF-SUM-ALLA-KODER-TOT  SECTION.                                          
115600                                                                          
115700     MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-RET                             
115800     MOVE ZERO            TO WS-SUMMA-TOT                                 
115900                                                                          
116000     PERFORM DB2-SELECT-TP8LRET-TOT                                       
116100     IF RADER-FINNS                                                       
116200       MOVE WS-SUMMA-TOT    TO MOD-SUARTBTO-RET                           
116300     END-IF                                                               
116400                                                                          
116500     .                                                                    
116600     EJECT                                                                
116700 MFS-RENSA-FAELT-UT SECTION.                                              
116800                                                                          
116900     MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-72                              
117000                             MOD-KDVALISO-72                              
117100                             MOD-SUARTBTO-98                              
117200                             MOD-KDVALISO-98                              
117300                             MOD-SUARTBTO-RR                              
117400                             MOD-KDVALISO-RR                              
117500                                                                          
117600     MOVE +1 TO INDX                                                      
117700     PERFORM UNTIL INDX > MAX-INDX                                        
117800       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
117900       ADD +1 TO INDX                                                     
118000     END-PERFORM                                                          
118100     .                                                                    
118200     SKIP3                                                                
118300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
118400                                                                          
118500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
118600     MOVE MFS-RENSA-FAELT TO MOD-KDANMORS (INDX)                          
118700                             MOD-IDDISTR  (INDX)                          
118800                             MOD-IDKUNDNR (INDX)                          
118900                             MOD-SUARTBTO (INDX)                          
119000                             MOD-KDVALISO (INDX)                          
119100                             MOD-IDRAPP   (INDX)                          
119200                             MOD-KDRAPPSTA(INDX)                          
119300                             MOD-IDFINDOC (INDX)                          
119400                             MOD-DAFAKT   (INDX)                          
119500                             MOD-IDUSER-2 (INDX)                          
119600                             MOD-DAUPPDAT (INDX)                          
119700     .                                                                    
119800     SKIP3                                                                
119900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
120000                                                                          
120100     MOVE MFS-ROER-EJ-FAELT TO MOD-SUARTBTO-72                            
120200                               MOD-KDVALISO-72                            
120300                               MOD-SUARTBTO-98                            
120400                               MOD-KDVALISO-98                            
120500                               MOD-SUARTBTO-RR                            
120600                               MOD-KDVALISO-RR                            
120700                                                                          
120800     MOVE +1 TO INDX                                                      
120900     PERFORM UNTIL INDX > MAX-INDX                                        
121000       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
121100       ADD +1 TO INDX                                                     
121200     END-PERFORM                                                          
121300     .                                                                    
121400     SKIP2                                                                
121500 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
121600                                                                          
121700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
121800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS (INDX)                        
121900                               MOD-IDDISTR  (INDX)                        
122000                               MOD-IDKUNDNR (INDX)                        
122100                               MOD-SUARTBTO (INDX)                        
122200                               MOD-KDVALISO (INDX)                        
122300                               MOD-IDRAPP   (INDX)                        
122400                               MOD-KDRAPPSTA(INDX)                        
122500                               MOD-IDFINDOC (INDX)                        
122600                               MOD-DAFAKT   (INDX)                        
122700                               MOD-IDUSER-2 (INDX)                        
122800                               MOD-DAUPPDAT (INDX)                        
122900     .                                                                    
123000     EJECT                                                                
123100* --- IMS SEKTIONER ---                                                   
123200     SKIP3                                                                
123300 IMS-GET-MSG SECTION.                                                     
123400                                                                          
123500     MOVE '  QC' TO GODK-STATUSKODER                                      
123600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
123700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123800     PERFORM IMS-STATUSKONTROLL                                           
123900     .                                                                    
124000     SKIP3                                                                
124100 IMS-INSERT-MSG SECTION.                                                  
124200                                                                          
124300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
124400     MOVE SPACE TO GODK-STATUSKODER                                       
124500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
124600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124700     PERFORM IMS-STATUSKONTROLL                                           
124800     .                                                                    
124900     EJECT                                                                
125000 IMS-GU-WDB201 SECTION.                                                   
125100                                                                          
125200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
125300          DELIMITED BY SIZE INTO SSA1                                     
125400     MOVE '  GE' TO GODK-STATUSKODER                                      
125500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
125600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
125700     PERFORM IMS-STATUSKONTROLL                                           
125800     .                                                                    
125900     EJECT                                                                
126000 IMS-GU-WDB201-MIN-MAX SECTION.                                           
126100                                                                          
126200     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
126300                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
126400          DELIMITED BY SIZE INTO SSA1                                     
126500     MOVE '  GE' TO GODK-STATUSKODER                                      
126600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
126700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSKONTROLL                                           
126900     .                                                                    
127000     EJECT                                                                
127100 IMS-STATUSKONTROLL SECTION.                                              
127200                                                                          
127300     SET STATUS-IX TO 1                                                   
127400     SEARCH GODK-STATUS                                                   
127500       AT END                                                             
127600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
127700         DELIMITED BY SIZE INTO FELTEXT                                   
127800         CALL FELLOG                                                      
127900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
128000         CONTINUE                                                         
128100     END-SEARCH                                                           
128200     .                                                                    
128300     EJECT                                                                
128400 DB2-SELECT-TP8LRET-TAB  SECTION.                                         
128500                                                                          
128600     MOVE 000100305  TO GODK-SQLCODEKODER                                 
128700     EXEC SQL                                                             
128800         SELECT  SUM(PRARTNTO * KVLEVART)                                 
128900                                                                          
129000                                                                          
129100         INTO   :WS-SUMMA                                                 
129200                                                                          
129300                                                                          
129400         FROM    TP8LRET                                                  
129500                                                                          
129600         WHERE   IDPARTNR = :W-IDPARTNR                                   
129700           AND   IDFTG    = :W-IDFTG                                      
129800           AND   KDANMORS = :WS-KDANMORS                                  
129900           AND   KDRAPPSTA = 'W '                                         
130000                                                                          
130100     END-EXEC                                                             
130200                                                                          
130300     MOVE SQLCODE TO SQLCODE-WS                                           
130400     PERFORM DB2-STATUS-KONTROLL                                          
130500     .                                                                    
130600     EJECT                                                                
130700 DB2-SELECT-TP8LRET-TOT  SECTION.                                         
130800                                                                          
130900     MOVE 000100305  TO GODK-SQLCODEKODER                                 
131000     EXEC SQL                                                             
131100         SELECT  SUM(PRARTNTO * KVLEVART)                                 
131200                                                                          
131300                                                                          
131400         INTO   :WS-SUMMA-TOT                                             
131500                                                                          
131600                                                                          
131700         FROM    TP8LRET                                                  
131800                                                                          
131900         WHERE   IDPARTNR = :W-IDPARTNR                                   
132000           AND   IDFTG    = :W-IDFTG                                      
132100           AND   KDRAPPSTA = 'W '                                         
132200                                                                          
132300     END-EXEC                                                             
132400                                                                          
132500     MOVE SQLCODE TO SQLCODE-WS                                           
132600     PERFORM DB2-STATUS-KONTROLL                                          
132700     .                                                                    
132800     EJECT                                                                
132900 DB2-DCL-OPN-TP8LRET-CRS  SECTION.                                        
133000                                                                          
133100                                                                          
133200     EXEC SQL                                                             
133300         DECLARE TP8LRET-CRS CURSOR FOR                                   
133400                                                                          
133500           SELECT  IDPARTNR                                               
133600                  ,IDFTG                                                  
133700                  ,KDANMORS                                               
133800                  ,DAREGDAT                                               
133900                  ,IDREF                                                  
134000                  ,IDEXCUST_1                                             
134100                  ,IDEXCUST_2                                             
134200                  ,PRARTNTO                                               
134300                  ,KVLEVART                                               
134400                  ,KDVALISO                                               
134500                  ,KDRAPPSTA                                              
134600                  ,IDFINDOC                                               
134700                  ,DAFAKT                                                 
134800                  ,IDUSER_2                                               
134900                  ,DAUPPDAT                                               
135000                  ,DADELDAT                                               
135100                                                                          
135200           FROM    TP8LRET                                                
135300                                                                          
135400           WHERE   IDPARTNR   = :W-IDPARTNR                               
135500             AND   IDFTG      = :W-IDFTG                                  
135600                                                                          
135700           ORDER BY DAREGDAT DESC, KDANMORS, IDREF                        
135800     END-EXEC                                                             
135900                                                                          
136000     EXEC SQL OPEN TP8LRET-CRS END-EXEC                                   
136100                                                                          
136200     MOVE 000100305  TO GODK-SQLCODEKODER                                 
136300     MOVE SQLCODE TO SQLCODE-WS                                           
136400     PERFORM DB2-STATUS-KONTROLL                                          
136500     .                                                                    
136600     SKIP3                                                                
136700 DB2-FETCH-TP8LRET-CRS  SECTION.                                          
136800     SKIP2                                                                
136900     EXEC SQL                                                             
137000         FETCH TP8LRET-CRS                                                
137100       INTO                                                               
137200         :LRET-IDPARTNR                                                   
137300        ,:LRET-IDFTG                                                      
137400        ,:LRET-KDANMORS                                                   
137500        ,:LRET-DAREGDAT                                                   
137600        ,:LRET-IDREF                                                      
137700        ,:LRET-IDEXCUST-1                                                 
137800        ,:LRET-IDEXCUST-2                                                 
137900        ,:LRET-PRARTNTO                                                   
138000        ,:LRET-KVLEVART                                                   
138100        ,:LRET-KDVALISO                                                   
138200        ,:LRET-KDRAPPSTA                                                  
138300        ,:LRET-IDFINDOC                                                   
138400        ,:LRET-DAFAKT                                                     
138500        ,:LRET-IDUSER-2                                                   
138600        ,:LRET-DAUPPDAT                                                   
138700        ,:LRET-DADELDAT                                                   
138800     END-EXEC                                                             
138900                                                                          
139000     MOVE 000100305  TO GODK-SQLCODEKODER                                 
139100     MOVE SQLCODE TO SQLCODE-WS                                           
139200     PERFORM DB2-STATUS-KONTROLL                                          
139300     .                                                                    
139400     SKIP3                                                                
139500 DB2-CLOSE-TP8LRET-CRS  SECTION.                                          
139600                                                                          
139700     EXEC SQL CLOSE TP8LRET-CRS END-EXEC                                  
139800     .                                                                    
139900     EJECT                                                                
140000 DB2-DCL-OPN-TP8LRET-CRS2  SECTION.                                       
140100                                                                          
140200                                                                          
140300     EXEC SQL                                                             
140400         DECLARE TP8LRET-CRS2 CURSOR FOR                                  
140500                                                                          
140600           SELECT  IDPARTNR                                               
140700                  ,IDFTG                                                  
140800                  ,KDANMORS                                               
140900                  ,DAREGDAT                                               
141000                  ,IDREF                                                  
141100                  ,IDEXCUST_1                                             
141200                  ,IDEXCUST_2                                             
141300                  ,PRARTNTO                                               
141400                  ,KVLEVART                                               
141500                  ,KDVALISO                                               
141600                  ,KDRAPPSTA                                              
141700                  ,IDFINDOC                                               
141800                  ,DAFAKT                                                 
141900                  ,IDUSER_2                                               
142000                  ,DAUPPDAT                                               
142100                  ,DADELDAT                                               
142200                                                                          
142300                                                                          
142400           FROM    TP8LRET                                                
142500                                                                          
142600           WHERE   IDPARTNR   = :W-IDPARTNR                               
142700             AND   IDFTG      = :W-IDFTG                                  
142800             AND   KDRAPPSTA  = :W-KDRAPPSTA                              
142900                                                                          
143000           ORDER BY DAREGDAT DESC, KDANMORS, IDREF                        
143100     END-EXEC                                                             
143200                                                                          
143300     EXEC SQL OPEN TP8LRET-CRS2 END-EXEC                                  
143400                                                                          
143500     MOVE 000100305  TO GODK-SQLCODEKODER                                 
143600     MOVE SQLCODE TO SQLCODE-WS                                           
143700     PERFORM DB2-STATUS-KONTROLL                                          
143800     .                                                                    
143900     SKIP3                                                                
144000 DB2-FETCH-TP8LRET-CRS2  SECTION.                                         
144100     SKIP2                                                                
144200     EXEC SQL                                                             
144300         FETCH TP8LRET-CRS2                                               
144400       INTO                                                               
144500         :LRET-IDPARTNR                                                   
144600        ,:LRET-IDFTG                                                      
144700        ,:LRET-KDANMORS                                                   
144800        ,:LRET-DAREGDAT                                                   
144900        ,:LRET-IDREF                                                      
145000        ,:LRET-IDEXCUST-1                                                 
145100        ,:LRET-IDEXCUST-2                                                 
145200        ,:LRET-PRARTNTO                                                   
145300        ,:LRET-KVLEVART                                                   
145400        ,:LRET-KDVALISO                                                   
145500        ,:LRET-KDRAPPSTA                                                  
145600        ,:LRET-IDFINDOC                                                   
145700        ,:LRET-DAFAKT                                                     
145800        ,:LRET-IDUSER-2                                                   
145900        ,:LRET-DAUPPDAT                                                   
146000        ,:LRET-DADELDAT                                                   
146100     END-EXEC                                                             
146200                                                                          
146300     MOVE 000100305  TO GODK-SQLCODEKODER                                 
146400     MOVE SQLCODE TO SQLCODE-WS                                           
146500     PERFORM DB2-STATUS-KONTROLL                                          
146600     .                                                                    
146700     SKIP3                                                                
146800 DB2-CLOSE-TP8LRET-CRS2  SECTION.                                         
146900                                                                          
147000     EXEC SQL CLOSE TP8LRET-CRS2 END-EXEC                                 
147100     .                                                                    
147200     EJECT                                                                
147300 DB2-DCL-OPN-TP8LRET-CRS3  SECTION.                                       
147400                                                                          
147500                                                                          
147600     EXEC SQL                                                             
147700         DECLARE TP8LRET-CRS3 CURSOR FOR                                  
147800                                                                          
147900           SELECT DISTINCT (KDANMORS), KDVALISO                           
148000                                                                          
148100           FROM    TP8LRET                                                
148200                                                                          
148300           WHERE   IDPARTNR = :W-IDPARTNR                                 
148400             AND   IDFTG    = :W-IDFTG                                    
148500             AND   KDRAPPSTA ='W '                                        
148600                                                                          
148700     END-EXEC                                                             
148800                                                                          
148900     EXEC SQL OPEN TP8LRET-CRS3 END-EXEC                                  
149000                                                                          
149100     MOVE 000100305  TO GODK-SQLCODEKODER                                 
149200     MOVE SQLCODE TO SQLCODE-WS                                           
149300     PERFORM DB2-STATUS-KONTROLL                                          
149400     .                                                                    
149500     SKIP3                                                                
149600 DB2-FETCH-TP8LRET-CRS3  SECTION.                                         
149700     SKIP2                                                                
149800     EXEC SQL                                                             
149900         FETCH TP8LRET-CRS3                                               
150000       INTO                                                               
150100         :WS-KDANMORS                                                     
150200        ,:WS-KDVALISO                                                     
150300     END-EXEC                                                             
150400                                                                          
150500     MOVE 000100305  TO GODK-SQLCODEKODER                                 
150600     MOVE SQLCODE TO SQLCODE-WS                                           
150700     PERFORM DB2-STATUS-KONTROLL                                          
150800     .                                                                    
150900     SKIP3                                                                
151000 DB2-CLOSE-TP8LRET-CRS3 SECTION.                                          
151100                                                                          
151200     EXEC SQL CLOSE TP8LRET-CRS3 END-EXEC                                 
151300     .                                                                    
151400     EJECT                                                                
151500 DB2-DCL-OPN-TP8LRET-CRS4  SECTION.                                       
151600                                                                          
151700                                                                          
151800     EXEC SQL                                                             
151900         DECLARE TP8LRET-CRS4 CURSOR FOR                                  
152000                                                                          
152100           SELECT  IDPARTNR                                               
152200                  ,IDFTG                                                  
152300                  ,KDANMORS                                               
152400                  ,DAREGDAT                                               
152500                  ,IDREF                                                  
152600                  ,IDEXCUST_1                                             
152700                  ,IDEXCUST_2                                             
152800                  ,PRARTNTO                                               
152900                  ,KVLEVART                                               
153000                  ,KDVALISO                                               
153100                  ,KDRAPPSTA                                              
153200                  ,IDFINDOC                                               
153300                  ,DAFAKT                                                 
153400                  ,IDUSER_2                                               
153500                  ,DAUPPDAT                                               
153600                  ,DADELDAT                                               
153700                                                                          
153800           FROM    TP8LRET                                                
153900                                                                          
154000           WHERE   IDEXCUST_1 = :W-IDEXCUST-1                             
154100             AND   IDEXCUST_2 = :W-IDEXCUST-2                             
154200                                                                          
154300           ORDER BY DAREGDAT DESC, KDANMORS, IDREF                        
154400     END-EXEC                                                             
154500                                                                          
154600     EXEC SQL OPEN TP8LRET-CRS4 END-EXEC                                  
154700                                                                          
154800     MOVE 000100305  TO GODK-SQLCODEKODER                                 
154900     MOVE SQLCODE TO SQLCODE-WS                                           
155000     PERFORM DB2-STATUS-KONTROLL                                          
155100     .                                                                    
155200     SKIP3                                                                
155300 DB2-FETCH-TP8LRET-CRS4  SECTION.                                         
155400     SKIP2                                                                
155500     EXEC SQL                                                             
155600         FETCH TP8LRET-CRS4                                               
155700       INTO                                                               
155800         :LRET-IDPARTNR                                                   
155900        ,:LRET-IDFTG                                                      
156000        ,:LRET-KDANMORS                                                   
156100        ,:LRET-DAREGDAT                                                   
156200        ,:LRET-IDREF                                                      
156300        ,:LRET-IDEXCUST-1                                                 
156400        ,:LRET-IDEXCUST-2                                                 
156500        ,:LRET-PRARTNTO                                                   
156600        ,:LRET-KVLEVART                                                   
156700        ,:LRET-KDVALISO                                                   
156800        ,:LRET-KDRAPPSTA                                                  
156900        ,:LRET-IDFINDOC                                                   
157000        ,:LRET-DAFAKT                                                     
157100        ,:LRET-IDUSER-2                                                   
157200        ,:LRET-DAUPPDAT                                                   
157300        ,:LRET-DADELDAT                                                   
157400     END-EXEC                                                             
157500                                                                          
157600     MOVE 000100305  TO GODK-SQLCODEKODER                                 
157700     MOVE SQLCODE TO SQLCODE-WS                                           
157800     PERFORM DB2-STATUS-KONTROLL                                          
157900     .                                                                    
158000     SKIP3                                                                
158100 DB2-CLOSE-TP8LRET-CRS4 SECTION.                                          
158200                                                                          
158300     EXEC SQL CLOSE TP8LRET-CRS4 END-EXEC                                 
158400     .                                                                    
158500     EJECT                                                                
158600 DB2-DCL-OPN-TP8LRET-CRS5  SECTION.                                       
158700                                                                          
158800                                                                          
158900     EXEC SQL                                                             
159000         DECLARE TP8LRET-CRS5 CURSOR FOR                                  
159100                                                                          
159200           SELECT  IDPARTNR                                               
159300                  ,IDFTG                                                  
159400                  ,KDANMORS                                               
159500                  ,DAREGDAT                                               
159600                  ,IDREF                                                  
159700                  ,IDEXCUST_1                                             
159800                  ,IDEXCUST_2                                             
159900                  ,PRARTNTO                                               
160000                  ,KVLEVART                                               
160100                  ,KDVALISO                                               
160200                  ,KDRAPPSTA                                              
160300                  ,IDFINDOC                                               
160400                  ,DAFAKT                                                 
160500                  ,IDUSER_2                                               
160600                  ,DAUPPDAT                                               
160700                  ,DADELDAT                                               
160800                                                                          
160900           FROM    TP8LRET                                                
161000                                                                          
161100           WHERE   IDEXCUST_1 = :W-IDEXCUST-1                             
161200             AND   IDEXCUST_2 = :W-IDEXCUST-2                             
161300             AND   KDRAPPSTA  = :W-KDRAPPSTA                              
161400                                                                          
161500           ORDER BY DAREGDAT DESC, KDANMORS, IDREF                        
161600     END-EXEC                                                             
161700                                                                          
161800     EXEC SQL OPEN TP8LRET-CRS5 END-EXEC                                  
161900                                                                          
162000     MOVE 000100305  TO GODK-SQLCODEKODER                                 
162100     MOVE SQLCODE TO SQLCODE-WS                                           
162200     PERFORM DB2-STATUS-KONTROLL                                          
162300     .                                                                    
162400     SKIP3                                                                
162500 DB2-FETCH-TP8LRET-CRS5  SECTION.                                         
162600     SKIP2                                                                
162700     EXEC SQL                                                             
162800         FETCH TP8LRET-CRS5                                               
162900       INTO                                                               
163000         :LRET-IDPARTNR                                                   
163100        ,:LRET-IDFTG                                                      
163200        ,:LRET-KDANMORS                                                   
163300        ,:LRET-DAREGDAT                                                   
163400        ,:LRET-IDREF                                                      
163500        ,:LRET-IDEXCUST-1                                                 
163600        ,:LRET-IDEXCUST-2                                                 
163700        ,:LRET-PRARTNTO                                                   
163800        ,:LRET-KVLEVART                                                   
163900        ,:LRET-KDVALISO                                                   
164000        ,:LRET-KDRAPPSTA                                                  
164100        ,:LRET-IDFINDOC                                                   
164200        ,:LRET-DAFAKT                                                     
164300        ,:LRET-IDUSER-2                                                   
164400        ,:LRET-DAUPPDAT                                                   
164500        ,:LRET-DADELDAT                                                   
164600     END-EXEC                                                             
164700                                                                          
164800     MOVE 000100305  TO GODK-SQLCODEKODER                                 
164900     MOVE SQLCODE TO SQLCODE-WS                                           
165000     PERFORM DB2-STATUS-KONTROLL                                          
165100     .                                                                    
165200     SKIP3                                                                
165300 DB2-CLOSE-TP8LRET-CRS5 SECTION.                                          
165400                                                                          
165500     EXEC SQL CLOSE TP8LRET-CRS5 END-EXEC                                 
165600     .                                                                    
165700     EJECT                                                                
165800 DB2-STATUS-KONTROLL  SECTION.                                            
165900                                                                          
166000     SET SQLCODE-IX TO 1                                                  
166100     SEARCH GODK-SQLCODE                                                  
166200       AT END                                                             
166300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
166400          DELIMITED BY SIZE INTO FELTEXT                                  
166500          CALL ABEND USING RKOD-ABEND-DB2                                 
166600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
166700     END-SEARCH                                                           
166800     .                                                                    
