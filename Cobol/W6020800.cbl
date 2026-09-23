000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6020800.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   00/09/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MANUELL KREDITERING AV KR.                                       
000900*        SKAPAR W6H721-SEGMENT SAMT KRK-SEGMENT PÅ WDR301.                
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDP3                                       
001200*        PROGRAMMET UPPDATERAR W6H7                                       
001300*        PROGRAMMET UPPDATERAR WDR3                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W6T208                                              
001700*        MID:         W6I20801                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W6O20801                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)      VALUE 'W6020800'.         
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80)      VALUE SPACE.              
003400                                                                          
003500 77  JA                          PIC X          VALUE 'J'.                
003600 77  YES                         PIC X          VALUE 'Y'.                
003700 77  NEJ                         PIC X          VALUE 'N'.                
003800 77  DATUM                       PIC 9(6)       VALUE ZERO.               
003900 77  WS-BEKRANS                  PIC X(25)      VALUE SPACE.              
004000 77  WS-IDKRATLF                 PIC X(20)      VALUE SPACE.              
004100 77  WS-PRARTBEL-PR              PIC S9(8)V9(5) VALUE 0 COMP-3.           
004200 77  WS-SUMAT                    PIC S9(7)V9(2) VALUE 0 COMP-3.           
004300 77  WS-SUOMK-NUM-INT            PIC S9(7)V9(2) VALUE 0 COMP-3.           
004400 77  WS-SUOMK-NUM-EXT            PIC S9(7)V9(2) VALUE 0 COMP-3.           
004500 77  WS-FAKT-SUMMA               PIC S9(8)V9(7) VALUE 0 COMP-3.           
004600 77  WS-KRED-SUMMA               PIC S9(8)V9(7) VALUE 0 COMP-3.           
004700 77  WS-SUOMK-INT                PIC S9(7)V9(2) VALUE 0 COMP-3.           
004800 77  WS-SUOMK-EXT                PIC S9(7)V9(2) VALUE 0 COMP-3.           
004900 77  WS-PRMOMS                   PIC S9(7)V9(2) VALUE 0 COMP-3.           
005000 77  WS-SUBEL-EJMOMS             PIC S9(9)V9(2) VALUE 0 COMP-3.           
005100 77  FL-TEXT-UPPDAT              PIC X          VALUE 'N'.                
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400                                                                          
005500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005600     88  INDATA-OK                           VALUE 'J'.                   
005700     88  INDATA-FEL                          VALUE 'N'.                   
005800                                                                          
005900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006000     88  NYCKLAR-OK                          VALUE 'J'.                   
006100     88  NYCKLAR-FEL                         VALUE 'N'.                   
006200                                                                          
006300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006400     88  EGEN-MID                            VALUE '6208'.                
006500     88  GODK-MID                            VALUE '6201' '6202'          
006600                                                   '6203' '6204'          
006700                                                   '6205' '6206'          
006800                                                   '6207' '6208'          
006900                                                   '6209'.                
007000     88  HELP-MID                            VALUE '0551'.                
007100     EJECT                                                                
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008100*01 -COPY WMEDAREA                                                        
008200     SKIP3                                                                
008300 01  MESSAGE-CODES.                                                       
008400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008900     03  INVOICE-MISSING         PIC X(3)    VALUE '320'.                 
009000     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '777'.                 
009100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009200     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009700     SKIP3                                                                
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
009930*01 -COPY WWDC99                                                          
009940     EJECT                                                                
010000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010300     SKIP3                                                                
010400*01  MID -COPY W6I20801                                                   
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010700     SKIP3                                                                
010800*01  -COPY WMSGAREA                                                       
010900     EJECT                                                                
011000     03  MOD REDEFINES MSG-AREA.                                          
011100*      05  -COPY W6O20801                                                 
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011400     SKIP3                                                                
011500*01  -COPY WMFSAREA                                                       
011600     EJECT                                                                
011700*    ---  COPYTEXT FÖR WLFILC                                             
011800*    KREDITNOT                                                            
011900*01  -COPY W426PKR                                                        
012000     EJECT                                                                
012100*01  -COPY WDECAREA                                                       
012200     EJECT                                                                
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600     SKIP3                                                                
012700 01  NYCKLAR-TILL-DLI.                                                    
012800     03  W-IDKR-X.                                                        
012900         05  W-IDKR              PIC 9(5)    VALUE ZERO.                  
013000     03  W-IDPTYP-X.                                                      
013100         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
013200     03  W-KDARBTYP-X.                                                    
013300         05  W-KDARBTYP          PIC X(8)    VALUE 'QUAL    '.            
013400     03  W-IDPERSON-X.                                                    
013500         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
013600     03  W-IDSEGMNR-X.                                                    
013700         05  W-IDSEGMNR          PIC S9      VALUE +0 COMP-3.             
013800     SKIP2                                                                
013900*    --- STATUS-KOD FRÅN IMS                                              
014000 01  STATUS-WS                   PIC XX.                                  
014100     88  SEGMENT-FINNS                       VALUE '  '.                  
014200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014400     SKIP2                                                                
014500 01  GODK-STATUSKODER.                                                    
014600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014700     SKIP3                                                                
014800 01  SSA1                        PIC X(64).                               
014900 01  SSA2                        PIC X(64).                               
015000 01  SSA3                        PIC X(64).                               
015100     EJECT                                                                
015200*    --- IMS FUNKTIONSKODER                                               
015300*01  -COPY W0003                                                          
015400     EJECT                                                                
015500*    ---  DLI INPUT-OUTPUT AREA                                           
015600                                                                          
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6H701'.                      
015800 01  DLI-IO-W6H701.                                                       
015900*    03  -COPY W6H701                                                     
016000     EJECT                                                                
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6H712'.                      
016200 01  DLI-IO-W6H712.                                                       
016300*    03  -COPY W6H712                                                     
016400     EJECT                                                                
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6H721'.                      
016600 01  DLI-IO-W6H721.                                                       
016700*    03  -COPY W6H721                                                     
016800     EJECT                                                                
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-P311'.                        
017000 01  DLI-IO-P311.                                                         
017100*    03  -COPY WDP311                                                     
017200     EJECT                                                                
017300 01  DLI-IO-FILC.                                                         
017400     03  IO-FILC                 PIC X(300)  VALUE SPACE.                 
017500     03  WLFILC01 REDEFINES IO-FILC.                                      
017600*        05  -COPY WDR301  -PRE FILC-                                     
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900*01  -COPY W0009   -PRE MSG-                                              
018000*01  -COPY W0008   -PRE USEA-                                             
018100     05  FILLER                  PIC X.                                   
018200                                                                          
018300*01  -COPY W0008  -PRE W6H7-                                              
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE WDP3-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE FILC-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB W6H7-PCB WDP3-PCB             
019300                                                     FILC-PCB.            
019400 MAIN SECTION.                                                            
019500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB W6H7-PCB WDP3-PCB             
019600                                                     FILC-PCB.            
019700                                                                          
019800     PERFORM IMS-GET-MSG                                                  
019900     IF SEGMENT-FINNS                                                     
020000       PERFORM A-INIT                                                     
020100       PERFORM B-KOLLA-NYCKLAR                                            
020200       IF NYCKLAR-OK                                                      
020300         IF MFS-UPDATE                                                    
020400           PERFORM G-KOLLA-INPUT                                          
020500           IF INDATA-OK                                                   
020600             PERFORM H-UPPDATERA                                          
020700           END-IF                                                         
020800         ELSE                                                             
020900           IF MFS-FIRST                                                   
021000             PERFORM C-FOERSTA-SIDA                                       
021100           ELSE                                                           
021200             IF MFS-NEXT                                                  
021300               CONTINUE                                                   
021400             ELSE                                                         
021500               PERFORM E-SAMMA-SIDA                                       
021600             END-IF                                                       
021700           END-IF                                                         
021800         END-IF                                                           
021900         IF INDATA-OK                                                     
022000           PERFORM F-LAES-VISA-INFO                                       
022100         END-IF                                                           
022200       END-IF                                                             
022300       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O20801 + 4                      
022400       PERFORM IMS-INSERT-MSG                                             
022500     END-IF                                                               
022600                                                                          
022700     MOVE ZERO TO RETURN-CODE                                             
022800     GOBACK                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 A-INIT SECTION.                                                          
023200                                                                          
023300     IF MSG-DUBBLA-TRANSKODER                                             
023400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I20801                 
023500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
023600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023700     ELSE                                                                 
023800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I20801                  
023900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
024000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024100     END-IF                                                               
024200                                                                          
024300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
024500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024600                                                                          
024700     MOVE LOW-VALUE TO MSG-AREA                                           
024800     MOVE 'W6O208N1' TO MFS-IDMOD                                         
024900     MOVE '6208' TO MOD-IDTRANS                                           
025000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025100                                                                          
025200     IF EGEN-MID OR HELP-MID                                              
025300       CONTINUE                                                           
025400     ELSE                                                                 
025500       MOVE SPACE TO MFS-KDTRTYP                                          
025600       MOVE '7' TO MFS-IDPFK                                              
025700     END-IF                                                               
025800     ACCEPT DATUM FROM DATE                                               
025900     MOVE 1    TO W-IDSEGMNR                                              
026000     MOVE ZERO TO FILC-FIL-IDSEKVNR                                       
026100     .                                                                    
026200     EJECT                                                                
026300 B-KOLLA-NYCKLAR SECTION.                                                 
026400                                                                          
026500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026600     MOVE '001'             TO MSGI-KDCALL                                
026700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026900     MOVE '6208'            TO MSGI-IDTRANS                               
027000     IF GODK-MID                                                          
027100       IF MID-IDKR-IN = ALL '+'                                           
027200         MOVE MID-IDKR-UT TO MSGI-IDKR                                    
027300       ELSE                                                               
027400         MOVE MID-IDKR-IN TO MSGI-IDKR                                    
027500       END-IF                                                             
027600     END-IF                                                               
027700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027800                                                                          
027900     IF MSGI-IDLAND-SPR = 'SE'                                            
028000       MOVE 'S  ' TO MED-IDSKYLT                                          
028100     ELSE                                                                 
028200       MOVE 'GB ' TO MED-IDSKYLT                                          
028300     END-IF                                                               
028400                                                                          
028500     MOVE JA TO NYCKLAR-SW                                                
028600                                                                          
028700                                                                          
028800*    -- KONTROLL AV IDKR                                                  
028900     MOVE MFS-RENSA-FAELT TO MOD-IDKR-IN                                  
029000                                                                          
029100     IF MID-IDKR-IN NOT = ALL '+'                                         
029200       MOVE '7'         TO MFS-IDPFK                                      
029300       MOVE SPACE       TO MFS-KDTRTYP                                    
029400     END-IF                                                               
029500     INSPECT MSGI-IDKR REPLACING LEADING SPACE BY ZERO                    
029600     IF MSGI-IDKR NUMERIC                                                 
029700       MOVE MSGI-IDKR TO W-IDKR                                           
029800     ELSE                                                                 
029900       MOVE NEJ TO NYCKLAR-SW                                             
030000     END-IF                                                               
030100                                                                          
030200     IF GODK-MID OR NYCKLAR-OK                                            
030300       MOVE MSGI-IDKR       TO MOD-IDKR-UT                                
030400       INSPECT MOD-IDKR-UT REPLACING LEADING ZERO BY SPACE                
030500     ELSE                                                                 
030600       MOVE MFS-RENSA-FAELT TO MOD-IDKR-UT                                
030700     END-IF                                                               
030800                                                                          
030900     IF NYCKLAR-FEL                                                       
031000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
031100       CALL WMEDKONV USING MED-WMEDAREA                                   
031200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
031300       PERFORM MFS-RENSA-FAELT-IN                                         
031400       PERFORM MFS-RENSA-FAELT-UT                                         
031500     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 C-FOERSTA-SIDA SECTION.                                                  
031900     PERFORM MFS-RENSA-FAELT-IN                                           
032000     .                                                                    
032100     EJECT                                                                
032200 E-SAMMA-SIDA SECTION.                                                    
032300                                                                          
032400     IF EGEN-MID OR HELP-MID                                              
032500       IF MID-INPUT = ALL '+'                                             
032600         PERFORM MFS-RENSA-FAELT-IN                                       
032700       ELSE                                                               
032800         PERFORM EA-MID-INDATA-TILL-MOD                                   
032900       END-IF                                                             
033000     ELSE                                                                 
033100       PERFORM MFS-RENSA-FAELT-IN                                         
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 EA-MID-INDATA-TILL-MOD SECTION.                                          
033600     IF MID-KDPERSON NOT = ALL '+'                                        
033700        MOVE MID-KDPERSON          TO MOD-KDPERSON                        
033800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPERSON-ATTR                   
033900     ELSE                                                                 
034000        MOVE MFS-RENSA-FAELT       TO MOD-KDPERSON                        
034100     END-IF                                                               
034200*    IF MID-SUOMK-INT-2DEC NOT = ALL '+'                                  
034300*       MOVE MID-SUOMK-INT-2DEC    TO MOD-SUOMK-KRED-INT                  
034400*       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUOMK-KRED-INT-ATTR             
034500*    ELSE                                                                 
034600        MOVE MFS-RENSA-FAELT       TO MOD-SUOMK-KRED-INT                  
034700*    END-IF                                                               
034800*    IF MID-SUOMK-EXT-2DEC NOT = ALL '+'                                  
034900*       MOVE MID-SUOMK-EXT-2DEC    TO MOD-SUOMK-KRED-EXT                  
035000*       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUOMK-KRED-EXT-ATTR             
035100*    ELSE                                                                 
035200        MOVE MFS-RENSA-FAELT       TO MOD-SUOMK-KRED-EXT                  
035300*    END-IF                                                               
035400*    IF MID-SUMAT NOT = ALL '+'                                           
035500*       MOVE MID-SUMAT             TO MOD-SUMAT-KRED                      
035600*       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUMAT-KRED-ATTR                 
035700*    ELSE                                                                 
035800        MOVE MFS-RENSA-FAELT       TO MOD-SUMAT-KRED                      
035900*    END-IF                                                               
036000     IF MID-TEKREKON-INT NOT = ALL '+'                                    
036100        MOVE MID-TEKREKON-INT      TO MOD-TEKREKON-INT                    
036200        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKREKON-INT-ATTR               
036300     ELSE                                                                 
036400        MOVE MFS-RENSA-FAELT       TO MOD-TEKREKON-INT                    
036500     END-IF                                                               
036600     IF MID-TEKREKON-EXT NOT = ALL '+'                                    
036700        MOVE MID-TEKREKON-EXT      TO MOD-TEKREKON-EXT                    
036800        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKREKON-EXT-ATTR               
036900     ELSE                                                                 
037000        MOVE MFS-RENSA-FAELT       TO MOD-TEKREKON-EXT                    
037100     END-IF                                                               
037200     IF MID-TEKREKON NOT = ALL '+'                                        
037300        MOVE MID-TEKREKON          TO MOD-TEKREKON                        
037400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKREKON-ATTR                   
037500     ELSE                                                                 
037600        MOVE MFS-RENSA-FAELT       TO MOD-TEKREKON                        
037700     END-IF                                                               
037800     MOVE MFS-RENSA-FAELT          TO MOD-FLALL-SUOMK-INT                 
037900                                      MOD-FLALL-SUOMK-EXT                 
038000                                      MOD-FLALL-SUMAT                     
038100     .                                                                    
038200     EJECT                                                                
038300 F-LAES-VISA-INFO SECTION.                                                
038400                                                                          
038500     PERFORM IMS-GU-W6H701                                                
038600                                                                          
038700     IF SEGMENT-SAKNAS                                                    
038800        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
038900        CALL WMEDKONV USING MED-WMEDAREA                                  
039000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
039100        PERFORM MFS-RENSA-FAELT-UT                                        
039200        PERFORM MFS-SPAERRA-ALLA-FAELT                                    
039300     ELSE                                                                 
039400        MOVE KR-IDARTNR TO MOD-IDARTNR                                    
039500        MOVE KR-IDLEVNR TO MOD-IDLEVNR                                    
039600        PERFORM FA-LAES-FAKTURADATA                                       
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 FA-LAES-FAKTURADATA SECTION.                                             
040100     PERFORM IMS-GU-W6H712                                                
040200     IF SEGMENT-FINNS                                                     
040300       MOVE EK-IDVERNR     TO MOD-IDVERNR-FAKT                            
040400       MOVE EK-TIFAKT      TO MOD-TIFAKT                                  
040500       COMPUTE WS-PRARTBEL-PR ROUNDED =                                   
040600                                    EK-PRARTBEL-PR * EK-KVKRRET           
040700       MOVE WS-PRARTBEL-PR TO MOD-PRARTBEL-FAKT                           
040800       COMPUTE WS-SUOMK-INT ROUNDED =                                     
040900                                    EK-SUOMK-INT * -1 / EK-PRKURS         
041000       MOVE WS-SUOMK-INT TO MOD-SUOMK-FAKT-INT                            
041100       COMPUTE WS-SUOMK-EXT ROUNDED =                                     
041200                                    EK-SUOMK-EXT * -1 / EK-PRKURS         
041300       MOVE WS-SUOMK-EXT TO MOD-SUOMK-FAKT-EXT                            
041400       COMPUTE WS-SUMAT ROUNDED = EK-SUMAT * -1 / EK-PRKURS               
041500       MOVE WS-SUMAT TO MOD-SUMAT-FAKT                                    
041600                                                                          
041700       COMPUTE WS-SUBEL-EJMOMS = WS-PRARTBEL-PR +                         
041800           WS-SUOMK-INT + WS-SUOMK-EXT + WS-SUMAT                         
041900       MOVE WS-SUBEL-EJMOMS TO MOD-SUBEL-FAKT-EJMOMS                      
042000                                                                          
042100       COMPUTE WS-PRMOMS ROUNDED = (EK-PRMOMS / EK-PRKURS) * -1           
042200       MOVE WS-PRMOMS TO MOD-PRMOMS-FAKT                                  
042300       COMPUTE MOD-SUBEL-FAKT-MOMS = WS-SUBEL-EJMOMS + WS-PRMOMS          
042400       MOVE EK-KDVALISO    TO MOD-KDVALISO                                
042500       MOVE EK-TEKREKON(1) TO MOD-TEKREKON                                
042600       PERFORM FB-LAES-KREDITDATA                                         
042700     ELSE                                                                 
042800       MOVE INVOICE-MISSING TO MED-IDMFSFEL                               
042900       CALL WMEDKONV USING MED-WMEDAREA                                   
043000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043100       PERFORM MFS-SPAERRA-ALLA-FAELT                                     
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 FB-LAES-KREDITDATA SECTION.                                              
043600     PERFORM IMS-GU-W6H721                                                
043700     IF SEGMENT-FINNS                                                     
043800       PERFORM FC-LAEGG-UT-KRED-DATA                                      
043900       PERFORM MFS-SPAERRA-FAELT-EJ-NOT                                   
044000                                                                          
044100       ADD 1 TO W-IDSEGMNR                                                
044200                                                                          
044300       PERFORM IMS-GU-W6H721                                              
044400                                                                          
044500       IF SEGMENT-FINNS                                                   
044600         IF MFS-NEXT                                                      
044700           PERFORM FC-LAEGG-UT-KRED-DATA                                  
044800           PERFORM MFS-SPAERRA-ALLA-FAELT                                 
044900           MOVE INF-LAST-PAGE TO MED-IDMFSFEL                             
045000           CALL WMEDKONV USING MED-WMEDAREA                               
045100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
045200         ELSE                                                             
045300           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSFEL                      
045400           CALL WMEDKONV USING MED-WMEDAREA                               
045500           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
045600         END-IF                                                           
045700       END-IF                                                             
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 FC-LAEGG-UT-KRED-DATA SECTION.                                           
046200     MOVE KRED-IDPTYP       TO MOD-IDPTYP                                 
046300     MOVE KRED-IDVERNR      TO MOD-IDVERNR-KRED                           
046400     MOVE KRED-BENAEMN      TO MOD-BENAEMN                                
046500     MOVE KRED-TIKRED       TO MOD-TIKRED                                 
046600     MOVE KRED-PRARTBEL-PR TO WS-PRARTBEL-PR                              
046700                              MOD-PRARTBEL-KRED                           
046800     COMPUTE WS-SUOMK-INT ROUNDED = KRED-SUOMK-INT-5DEC                   
046900                                                 / EK-PRKURS              
047000     MOVE WS-SUOMK-INT TO MOD-SUOMK-KRED-INT                              
047100     COMPUTE WS-SUOMK-EXT ROUNDED = KRED-SUOMK-EXT-5DEC                   
047200                                                 / EK-PRKURS              
047300     MOVE WS-SUOMK-EXT TO MOD-SUOMK-KRED-EXT                              
047400     COMPUTE WS-SUMAT ROUNDED = KRED-SUMAT-5DEC / EK-PRKURS               
047500     MOVE WS-SUMAT TO MOD-SUMAT-KRED-NUM                                  
047600     COMPUTE WS-SUBEL-EJMOMS = WS-PRARTBEL-PR +                           
047700                      WS-SUOMK-INT + WS-SUOMK-EXT + WS-SUMAT              
047800     MOVE WS-SUBEL-EJMOMS TO MOD-SUBEL-KRED-EJMOMS                        
047900     COMPUTE WS-PRMOMS ROUNDED = KRED-PRMOMS / EK-PRKURS                  
048000     MOVE WS-PRMOMS TO MOD-PRMOMS-KRED-NUM                                
048100     COMPUTE MOD-SUBEL-KRED-MOMS = WS-SUBEL-EJMOMS + WS-PRMOMS            
048200                                                                          
048300     MOVE KRED-TEKREKON-INT TO MOD-TEKREKON-INT                           
048400     MOVE KRED-TEKREKON-EXT TO MOD-TEKREKON-EXT                           
048500     .                                                                    
048600     EJECT                                                                
048700 G-KOLLA-INPUT SECTION.                                                   
048800                                                                          
048900     MOVE JA  TO INDATA-SW                                                
049000     MOVE NEJ TO FL-TEXT-UPPDAT                                           
049100     IF MID-INPUT = ALL '+'                                               
049200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
049300       CALL WMEDKONV USING MED-WMEDAREA                                   
049400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
049500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
049600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
049700       MOVE NEJ TO INDATA-SW                                              
049800     ELSE                                                                 
049900       PERFORM IMS-GU-W6H701                                              
050000       IF SEGMENT-FINNS                                                   
050001         MOVE KR-IDDC    TO WS-IDDC                                       
050100         PERFORM IMS-GU-W6H712                                            
050200         IF SEGMENT-FINNS                                                 
050300           PERFORM IMS-GNP-W6H721                                         
050400           IF SEGMENT-FINNS                                               
050500             IF MID-TEKREKON-INT NOT = ALL '+' AND SPACE                  
050600                MOVE JA TO FL-TEXT-UPPDAT                                 
050700             ELSE                                                         
050800               MOVE UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
050900               CALL WMEDKONV USING MED-WMEDAREA                           
051000               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
051100               MOVE NEJ TO INDATA-SW                                      
051200               PERFORM MFS-SPAERRA-ALLA-FAELT                             
051300             END-IF                                                       
051400           ELSE                                                           
051500             IF (MID-TEKREKON NOT = ALL '+' AND SPACE)                    
051600                 AND (MID-TEKREKON NOT = EK-TEKREKON(1))                  
051700               MOVE JA TO FL-TEXT-UPPDAT                                  
051800             END-IF                                                       
051900           END-IF                                                         
052000         ELSE                                                             
052100           MOVE INVOICE-MISSING TO MED-IDMFSFEL                           
052200           CALL WMEDKONV USING MED-WMEDAREA                               
052300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
052400           MOVE NEJ TO INDATA-SW                                          
052500           PERFORM MFS-RENSA-FAELT-IN                                     
052600           PERFORM MFS-RENSA-FAELT-UT                                     
052700         END-IF                                                           
052800       ELSE                                                               
052900         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
053000         CALL WMEDKONV USING MED-WMEDAREA                                 
053100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
053200         MOVE NEJ TO INDATA-SW                                            
053300         PERFORM MFS-RENSA-FAELT-IN                                       
053400         PERFORM MFS-RENSA-FAELT-UT                                       
053500       END-IF                                                             
053600                                                                          
053700       IF INDATA-OK                                                       
053800         IF MID-KDPERSON = ALL '+' OR MID-KDPERSON = SPACE                
053900            MOVE MFS-NUM-FAELT-FEL TO MOD-KDPERSON-ATTR                   
054000            MOVE NEJ               TO INDATA-SW                           
054100         ELSE                                                             
054200            INSPECT MID-KDPERSON REPLACING LEADING SPACE BY ZERO          
054300            IF MID-KDPERSON NOT NUMERIC                                   
054400               MOVE MFS-NUM-FAELT-FEL TO MOD-KDPERSON-ATTR                
054500               MOVE NEJ               TO INDATA-SW                        
054600            ELSE                                                          
054700               MOVE MID-KDPERSON      TO W-IDPERSON                       
054800               PERFORM IMS-GU-WDP311                                      
054900               IF SEGMENT-SAKNAS                                          
055000                  MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPERSON-ATTR           
055100                  MOVE NEJ                 TO INDATA-SW                   
055200               ELSE                                                       
055300                  MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPERSON-ATTR           
055400                  MOVE PERS-IDTFN          TO WS-IDKRATLF                 
055500                  MOVE PERS-IDNAMN         TO WS-BEKRANS                  
055600               END-IF                                                     
055700            END-IF                                                        
055800         END-IF                                                           
055900                                                                          
056000         IF MID-SUOMK-INT-2DEC NOT = ALL '+'                              
056100           IF MID-FLALL-SUOMK-INT NOT = ALL '+'                           
056200             MOVE MFS-NUM-FAELT-FEL  TO MOD-SUOMK-KRED-INT-ATTR           
056300             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLALL-SUOMK-INT-ATTR          
056400             MOVE NEJ TO INDATA-SW                                        
056500           ELSE                                                           
056600             MOVE MID-SUOMK-INT-2DEC TO DEC-IDFRIDATA                     
056700             MOVE 7                  TO DEC-KVHELTAL                      
056800             MOVE 2                  TO DEC-KVDECIMAL                     
056900             CALL WDECEDIT USING DEC-WDECAREA                             
057000             IF DEC-KDSVAR-OK                                             
057100               MOVE DEC-IDEDITDATA       TO WS-SUOMK-NUM-INT              
057200               IF (WS-SUOMK-NUM-INT > (EK-SUOMK-INT * -1) /               
057300                   EK-PRKURS) OR WS-SUOMK-NUM-INT = 0                     
057400                 MOVE MFS-NUM-FAELT-FEL TO MOD-SUOMK-KRED-INT-ATTR        
057500                 MOVE NEJ TO INDATA-SW                                    
057600               ELSE                                                       
057700                 MOVE MFS-NUM-FAELT-RAETT TO                              
057800                                           MOD-SUOMK-KRED-INT-ATTR        
057900               END-IF                                                     
058000             ELSE                                                         
058100               MOVE MFS-NUM-FAELT-FEL TO MOD-SUOMK-KRED-INT-ATTR          
058200               MOVE NEJ TO INDATA-SW                                      
058300             END-IF                                                       
058400           END-IF                                                         
058500         ELSE                                                             
058600            MOVE MFS-NUM-FAELT-RAETT  TO MOD-SUOMK-KRED-INT-ATTR          
058700            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLALL-SUOMK-INT-ATTR         
058800         END-IF                                                           
058900                                                                          
059000         IF MID-SUOMK-EXT-2DEC NOT = ALL '+'                              
059100           IF MID-FLALL-SUOMK-EXT NOT = ALL '+'                           
059200             MOVE MFS-NUM-FAELT-FEL  TO MOD-SUOMK-KRED-EXT-ATTR           
059300             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLALL-SUOMK-EXT-ATTR          
059400             MOVE NEJ TO INDATA-SW                                        
059500           ELSE                                                           
059600             MOVE MID-SUOMK-EXT-2DEC TO DEC-IDFRIDATA                     
059700             MOVE 7                  TO DEC-KVHELTAL                      
059800             MOVE 2                  TO DEC-KVDECIMAL                     
059900             CALL WDECEDIT USING DEC-WDECAREA                             
060000             IF DEC-KDSVAR-OK                                             
060100               MOVE DEC-IDEDITDATA       TO WS-SUOMK-NUM-EXT              
060200               IF (WS-SUOMK-NUM-EXT > (EK-SUOMK-EXT * -1) /               
060300                   EK-PRKURS) OR WS-SUOMK-NUM-EXT = 0                     
060400                 MOVE MFS-NUM-FAELT-FEL TO MOD-SUOMK-KRED-EXT-ATTR        
060500                 MOVE NEJ TO INDATA-SW                                    
060600               ELSE                                                       
060700                 MOVE MFS-NUM-FAELT-RAETT TO                              
060800                                        MOD-SUOMK-KRED-EXT-ATTR           
060900               END-IF                                                     
061000             ELSE                                                         
061100               MOVE MFS-NUM-FAELT-FEL TO MOD-SUOMK-KRED-EXT-ATTR          
061200               MOVE NEJ TO INDATA-SW                                      
061300             END-IF                                                       
061400           END-IF                                                         
061500         ELSE                                                             
061600            MOVE MFS-NUM-FAELT-RAETT  TO MOD-SUOMK-KRED-EXT-ATTR          
061700            MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLALL-SUOMK-EXT-ATTR         
061800         END-IF                                                           
061900                                                                          
062000         IF MID-SUMAT NOT = ALL '+'                                       
062100           IF MID-FLALL-SUMAT NOT = ALL '+'                               
062200             MOVE MFS-ALFA-FAELT-FEL   TO MOD-SUMAT-KRED-ATTR             
062300                                          MOD-FLALL-SUMAT-ATTR            
062400             MOVE NEJ                  TO INDATA-SW                       
062500           ELSE                                                           
062600             MOVE MID-SUMAT  TO DEC-IDFRIDATA                             
062700             MOVE 7          TO DEC-KVHELTAL                              
062800             MOVE 2          TO DEC-KVDECIMAL                             
062900             CALL WDECEDIT USING DEC-WDECAREA                             
063000             IF DEC-KDSVAR-OK                                             
063100               MOVE DEC-IDEDITDATA       TO WS-SUMAT                      
063200               IF (WS-SUMAT > (EK-SUMAT * -1) / EK-PRKURS) OR             
063300                   WS-SUMAT = 0                                           
063400                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-SUMAT-KRED-ATTR         
063500                 MOVE NEJ                  TO INDATA-SW                   
063600               ELSE                                                       
063700                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-SUMAT-KRED-ATTR         
063800               END-IF                                                     
063900             ELSE                                                         
064000               MOVE MFS-ALFA-FAELT-FEL   TO MOD-SUMAT-KRED-ATTR           
064100               MOVE NEJ                  TO INDATA-SW                     
064200             END-IF                                                       
064300           END-IF                                                         
064400         ELSE                                                             
064500            MOVE MFS-ALFA-FAELT-RAETT    TO MOD-SUMAT-KRED-ATTR           
064600                                            MOD-FLALL-SUMAT-ATTR          
064700         END-IF                                                           
064800                                                                          
064900         IF MID-TEKREKON-INT NOT = ALL '+'                                
065000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEKREKON-INT-ATTR             
065100         END-IF                                                           
065200                                                                          
065300         IF MID-TEKREKON-EXT NOT = ALL '+'                                
065400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEKREKON-EXT-ATTR             
065500         END-IF                                                           
065600                                                                          
065700         IF MID-TEKREKON NOT = ALL '+'                                    
065800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEKREKON-ATTR                 
065900         END-IF                                                           
066000                                                                          
066100         IF INDATA-OK                                                     
066200           IF MID-FLALL-SUOMK-INT NOT = ALL '+'                           
066300             IF (MID-FLALL-SUOMK-INT NOT = JA AND YES) OR                 
066400                 EK-SUOMK-INT = 0                                         
066500               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLALL-SUOMK-INT-ATTR        
066600               MOVE NEJ                  TO INDATA-SW                     
066700             END-IF                                                       
066800           END-IF                                                         
066900           IF MID-FLALL-SUOMK-EXT NOT = ALL '+'                           
067000             IF (MID-FLALL-SUOMK-EXT NOT = JA AND YES) OR                 
067100                 EK-SUOMK-EXT = 0                                         
067200               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLALL-SUOMK-EXT-ATTR        
067300               MOVE NEJ                  TO INDATA-SW                     
067400             END-IF                                                       
067500           END-IF                                                         
067600           IF MID-FLALL-SUMAT NOT = ALL '+'                               
067700             IF (MID-FLALL-SUMAT NOT = JA AND YES) OR                     
067800                 EK-SUMAT = 0                                             
067900               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLALL-SUMAT-ATTR            
068000               MOVE NEJ                  TO INDATA-SW                     
068100             END-IF                                                       
068200           END-IF                                                         
068300           IF FL-TEXT-UPPDAT = NEJ AND (MID-SUMMOR = ALL '+' OR           
068400              SPACE)                                                      
068500              MOVE NEJ TO INDATA-SW                                       
068600           END-IF                                                         
068700         END-IF                                                           
068800                                                                          
068900         IF INDATA-FEL                                                    
069000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
069100           CALL WMEDKONV USING MED-WMEDAREA                               
069200           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
069300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
069400           PERFORM MFS-ROER-EJ-FAELT-IN                                   
069500         END-IF                                                           
069600       END-IF                                                             
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 H-UPPDATERA SECTION.                                                     
070100                                                                          
070200     PERFORM IMS-GU-W6H712                                                
070300     IF SEGMENT-FINNS                                                     
070400       IF MID-TEKREKON NOT = ALL '+' AND MID-TEKREKON NOT =               
070500          EK-TEKREKON(1)                                                  
070600         PERFORM HE-REPL-W6H712                                           
070700       ELSE                                                               
070800         IF FL-TEXT-UPPDAT = JA AND (MID-TEKREKON-INT NOT =               
070900            ALL '+' AND SPACE)                                            
071000           PERFORM HD-REPL-W6H721                                         
071100         ELSE                                                             
071200           PERFORM HA-SKAPA-W6H721                                        
071300           PERFORM HB-SKAPA-KRK                                           
071400           PERFORM HC-SKAPA-KEK                                           
071500         END-IF                                                           
071600       END-IF                                                             
071700                                                                          
071800       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
071900       CALL WMEDKONV USING MED-WMEDAREA                                   
072000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
072100       PERFORM MFS-FORM-ATTR                                              
072200       PERFORM MFS-RENSA-FAELT-IN                                         
072300     END-IF                                                               
072400     .                                                                    
072500     EJECT                                                                
072600 HA-SKAPA-W6H721 SECTION.                                                 
072700                                                                          
072800     MOVE '1'         TO KRED-IDSEGMNR                                    
072900     MOVE 'MAN'       TO KRED-IDPTYP                                      
073202     MOVE '1'               TO KRED-IDVERNR(1:1)                          
073210     MOVE EK-IDVERNR        TO KRED-IDVERNR(2:8)                          
073300     MOVE WS-BEKRANS  TO KRED-BENAEMN                                     
073400     MOVE WS-IDKRATLF TO KRED-IDTFN                                       
073500     MOVE DATUM       TO KRED-TIKRED                                      
073600     MOVE 0           TO KRED-PRARTBEL-PR                                 
073700                                                                          
073800     IF MID-FLALL-SUOMK-INT = JA OR YES                                   
073900       COMPUTE KRED-SUOMK-INT-5DEC = EK-SUOMK-INT * -1                    
074000     ELSE                                                                 
074100       IF MID-SUOMK-INT-2DEC NOT = ALL '+'                                
074200         COMPUTE KRED-SUOMK-INT-5DEC ROUNDED =                            
074300                                     WS-SUOMK-NUM-INT * EK-PRKURS         
074400       ELSE                                                               
074500         MOVE 0             TO  KRED-SUOMK-INT-5DEC                       
074600       END-IF                                                             
074700     END-IF                                                               
074800                                                                          
074900     IF MID-FLALL-SUOMK-EXT = JA OR YES                                   
075000       COMPUTE KRED-SUOMK-EXT-5DEC = EK-SUOMK-EXT * -1                    
075100     ELSE                                                                 
075200       IF MID-SUOMK-EXT-2DEC NOT = ALL '+'                                
075300         COMPUTE KRED-SUOMK-EXT-5DEC ROUNDED =                            
075400                                      WS-SUOMK-NUM-EXT * EK-PRKURS        
075500       ELSE                                                               
075600         MOVE 0             TO  KRED-SUOMK-EXT-5DEC                       
075700       END-IF                                                             
075800     END-IF                                                               
075900                                                                          
076000     IF MID-FLALL-SUMAT = JA OR YES                                       
076100       COMPUTE KRED-SUMAT-5DEC = EK-SUMAT * -1                            
076200     ELSE                                                                 
076300       IF MID-SUMAT NOT = ALL '+'                                         
076400         COMPUTE KRED-SUMAT-5DEC ROUNDED = WS-SUMAT * EK-PRKURS           
076500       ELSE                                                               
076600         MOVE 0        TO KRED-SUMAT-5DEC                                 
076700       END-IF                                                             
076800     END-IF                                                               
076900                                                                          
077000     IF EK-PRMOMS = 0                                                     
077100       MOVE 0         TO KRED-PRMOMS                                      
077200     ELSE                                                                 
077300** FAKTURERAT BELOPP                                                      
077400       COMPUTE WS-FAKT-SUMMA ROUNDED = (EK-PRARTBEL-PR *                  
077500           EK-KVKRRET) + (((EK-SUOMK-INT + EK-SUOMK-EXT +                 
077600                            EK-SUMAT) * -1) / EK-PRKURS)                  
077700** KREDITERAT BELOPP                                                      
077800       COMPUTE WS-KRED-SUMMA ROUNDED =                                    
077900              (KRED-SUOMK-INT-5DEC + KRED-SUOMK-EXT-5DEC                  
078000                            + KRED-SUMAT-5DEC) / EK-PRKURS                
078100                                                                          
078200       COMPUTE KRED-PRMOMS ROUNDED = WS-KRED-SUMMA /                      
078300                               WS-FAKT-SUMMA * EK-PRMOMS * -1             
078400     END-IF                                                               
078500     IF MID-TEKREKON-INT NOT = ALL '+'                                    
078600       MOVE MID-TEKREKON-INT TO KRED-TEKREKON-INT                         
078700     ELSE                                                                 
078800       MOVE SPACE            TO KRED-TEKREKON-INT                         
078900     END-IF                                                               
079000     IF MID-TEKREKON-EXT NOT = ALL '+'                                    
079100       MOVE MID-TEKREKON-EXT TO KRED-TEKREKON-EXT                         
079200     ELSE                                                                 
079300       MOVE SPACE            TO KRED-TEKREKON-EXT                         
079400     END-IF                                                               
079500                                                                          
079600     PERFORM IMS-ISRT-W6H721                                              
079700     .                                                                    
079800     EJECT                                                                
079900 HB-SKAPA-KRK SECTION.                                                    
080000*                                                                         
080100* KREDITNOTA-TRANS SKRIVS SOM SEGMENT PÅ WLFILC (WDR3)                    
080200* FÖR SENARE NEDLÄSNING AV PGM: W42690 (W426D2-RTN).                      
080300*                                                                         
080400     MOVE 'KRK'           TO PKR-IDPTYP                                   
080500     MOVE  KR-IDKR        TO PKR-IDKR                                     
080600                                                                          
080700     ACCEPT FILC-FIL-TIREGDAT FROM DATE                                   
080800     ACCEPT FILC-FIL-TIKLOCK  FROM TIME                                   
080900     ADD  +1              TO FILC-FIL-IDSEKVNR                            
081000     MOVE 'W426KRK '      TO FILC-FIL-IDCPYTXT                            
081100     MOVE W426PKR         TO FILC-FIL-WDR301-DATA(1:8)                    
081200     MOVE '-1'            TO FILC-FIL-WDR301-DATA(9:2)                    
081300                                                                          
081400     PERFORM IMS-ISRT-WLFILC01                                            
081500     .                                                                    
081600     EJECT                                                                
081700 HC-SKAPA-KEK SECTION.                                                    
081800*                                                                         
081900* EKONOMITRANS KREDIT SKRIVS SOM SEGMENT PÅ WLFILC (WDR3)                 
082000* FÖR SENARE NEDLÄSNING AV PGM: W42695 (W426V1-RTN).                      
082100*                                                                         
082200     MOVE 'KEK'           TO PKR-IDPTYP                                   
082300     MOVE  KR-IDKR        TO PKR-IDKR                                     
082400                                                                          
082500     ACCEPT FILC-FIL-TIREGDAT FROM DATE                                   
082600     ACCEPT FILC-FIL-TIKLOCK  FROM TIME                                   
082700     ADD  +1              TO FILC-FIL-IDSEKVNR                            
082800     MOVE 'W426KEK '      TO FILC-FIL-IDCPYTXT                            
082900     MOVE W426PKR         TO FILC-FIL-WDR301-DATA(1:8)                    
083000     MOVE '-1'            TO FILC-FIL-WDR301-DATA(9:2)                    
083100                                                                          
083200     PERFORM IMS-ISRT-WLFILC01                                            
083300     .                                                                    
083400     EJECT                                                                
083500 HD-REPL-W6H721 SECTION.                                                  
083600     MOVE 1 TO W-IDSEGMNR                                                 
083700     PERFORM IMS-GHU-W6H721                                               
083800     MOVE WS-BEKRANS  TO KRED-BENAEMN                                     
083900     MOVE MID-TEKREKON-INT TO KRED-TEKREKON-INT                           
084000     PERFORM IMS-REPL-W6H721                                              
084100     .                                                                    
084200     EJECT                                                                
084300 HE-REPL-W6H712 SECTION.                                                  
084400     PERFORM IMS-GHU-W6H712                                               
084500     MOVE MID-TEKREKON TO EK-TEKREKON(1)                                  
084600     PERFORM IMS-REPL-W6H712                                              
084700     .                                                                    
084800     EJECT                                                                
084900 MFS-RENSA-FAELT-UT SECTION.                                              
085000                                                                          
085100*    --- ALLA UTDATA-FÄLT                                                 
085200     MOVE MFS-RENSA-FAELT TO MOD-IDVERNR-FAKT                             
085300                             MOD-TIFAKT                                   
085400                             MOD-IDVERNR-KRED                             
085500                             MOD-TIKRED                                   
085600                             MOD-IDARTNR                                  
085700                             MOD-IDLEVNR                                  
085800                             MOD-BENAEMN                                  
085900                             MOD-PRARTBEL-FAKT                            
086000                             MOD-PRARTBEL-KRED                            
086100                             MOD-SUOMK-FAKT-INT                           
086200                             MOD-SUOMK-KRED-INT                           
086300                             MOD-FLALL-SUOMK-INT                          
086400                             MOD-SUOMK-FAKT-EXT                           
086500                             MOD-SUOMK-KRED-EXT                           
086600                             MOD-FLALL-SUOMK-EXT                          
086700                             MOD-SUMAT-FAKT                               
086800                             MOD-SUMAT-KRED                               
086900                             MOD-FLALL-SUMAT                              
087000                             MOD-SUBEL-FAKT-EJMOMS                        
087100                             MOD-SUBEL-KRED-EJMOMS                        
087200                             MOD-PRMOMS-FAKT                              
087300                             MOD-PRMOMS-KRED                              
087400                             MOD-SUBEL-FAKT-MOMS                          
087500                             MOD-SUBEL-KRED-MOMS                          
087600                             MOD-KDVALISO                                 
087700                             MOD-TEKREKON-INT                             
087800                             MOD-TEKREKON-EXT                             
087900                             MOD-TEKREKON                                 
088000     .                                                                    
088100     SKIP3                                                                
088200 MFS-RENSA-FAELT-IN SECTION.                                              
088300                                                                          
088400*    --- ALLA INDATA-FÄLT                                                 
088500     MOVE MFS-RENSA-FAELT TO MOD-KDPERSON                                 
088600                             MOD-SUOMK-KRED-INT                           
088700                             MOD-FLALL-SUOMK-INT                          
088800                             MOD-SUOMK-KRED-EXT                           
088900                             MOD-FLALL-SUOMK-EXT                          
089000                             MOD-SUMAT-KRED                               
089100                             MOD-FLALL-SUMAT                              
089200                             MOD-TEKREKON-INT                             
089300                             MOD-TEKREKON-EXT                             
089400                             MOD-TEKREKON                                 
089500     .                                                                    
089600     EJECT                                                                
089700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
089800                                                                          
089900*    --- ALLA UTDATA-FÄLT                                                 
090000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDVERNR-FAKT                           
090100                             MOD-TIFAKT                                   
090200                             MOD-IDVERNR-KRED                             
090300                             MOD-TIKRED                                   
090400                             MOD-IDARTNR                                  
090500                             MOD-IDLEVNR                                  
090600                             MOD-BENAEMN                                  
090700                             MOD-PRARTBEL-FAKT                            
090800                             MOD-PRARTBEL-KRED                            
090900                             MOD-SUOMK-FAKT-INT                           
091000                             MOD-SUOMK-KRED-INT                           
091100                             MOD-FLALL-SUOMK-INT                          
091200                             MOD-SUOMK-FAKT-EXT                           
091300                             MOD-SUOMK-KRED-EXT                           
091400                             MOD-FLALL-SUOMK-EXT                          
091500                             MOD-SUMAT-FAKT                               
091600                             MOD-SUMAT-KRED                               
091700                             MOD-FLALL-SUMAT                              
091800                             MOD-SUBEL-FAKT-EJMOMS                        
091900                             MOD-SUBEL-KRED-EJMOMS                        
092000                             MOD-PRMOMS-FAKT                              
092100                             MOD-PRMOMS-KRED                              
092200                             MOD-SUBEL-FAKT-MOMS                          
092300                             MOD-SUBEL-KRED-MOMS                          
092400                             MOD-KDVALISO                                 
092500                             MOD-TEKREKON-INT                             
092600                             MOD-TEKREKON-EXT                             
092700                             MOD-TEKREKON                                 
092800     .                                                                    
092900     SKIP3                                                                
093000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
093100                                                                          
093200*    --- ALLA INDATA-FÄLT                                                 
093300     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPERSON                               
093400                             MOD-SUOMK-KRED-INT                           
093500                             MOD-FLALL-SUOMK-INT                          
093600                             MOD-SUOMK-KRED-EXT                           
093700                             MOD-FLALL-SUOMK-EXT                          
093800                             MOD-SUMAT-KRED                               
093900                             MOD-FLALL-SUMAT                              
094000                             MOD-TEKREKON-INT                             
094100                             MOD-TEKREKON-EXT                             
094200                             MOD-TEKREKON                                 
094300                                                                          
094400     .                                                                    
094500     EJECT                                                                
094600 MFS-FORM-ATTR SECTION.                                                   
094700                                                                          
094800*    --- ALLA INDATA-FÄLT                                                 
094900     MOVE MFS-FORMATETS-ATTR TO MOD-KDPERSON-ATTR                         
095000                                MOD-SUOMK-KRED-INT-ATTR                   
095100                                MOD-FLALL-SUOMK-INT-ATTR                  
095200                                MOD-SUOMK-KRED-EXT-ATTR                   
095300                                MOD-FLALL-SUOMK-EXT-ATTR                  
095400                                MOD-SUMAT-KRED-ATTR                       
095500                                MOD-FLALL-SUMAT-ATTR                      
095600                                MOD-TEKREKON-INT-ATTR                     
095700                                MOD-TEKREKON-EXT-ATTR                     
095800                                MOD-TEKREKON-ATTR                         
095900     .                                                                    
096000     SKIP2                                                                
096100 MFS-SPAERRA-ALLA-FAELT  SECTION.                                         
096200                                                                          
096300     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDPERSON-ATTR                     
096400                                MOD-SUOMK-KRED-INT-ATTR                   
096500                                MOD-FLALL-SUOMK-INT-ATTR                  
096600                                MOD-SUOMK-KRED-EXT-ATTR                   
096700                                MOD-FLALL-SUOMK-EXT-ATTR                  
096800                                MOD-SUMAT-KRED-ATTR                       
096900                                MOD-FLALL-SUMAT-ATTR                      
097000                                MOD-TEKREKON-INT-ATTR                     
097100                                MOD-TEKREKON-EXT-ATTR                     
097200                                MOD-TEKREKON-ATTR                         
097300     .                                                                    
097400     SKIP2                                                                
097500 MFS-SPAERRA-FAELT-EJ-NOT  SECTION.                                       
097600                                                                          
097700     MOVE MFS-STAENG-FAELT-NOMOD TO                                       
097800                                MOD-SUOMK-KRED-INT-ATTR                   
097900                                MOD-FLALL-SUOMK-INT-ATTR                  
098000                                MOD-SUOMK-KRED-EXT-ATTR                   
098100                                MOD-FLALL-SUOMK-EXT-ATTR                  
098200                                MOD-SUMAT-KRED-ATTR                       
098300                                MOD-FLALL-SUMAT-ATTR                      
098400                                MOD-TEKREKON-EXT-ATTR                     
098500     .                                                                    
098600     EJECT                                                                
098700* --- IMS SEKTIONER ---                                                   
098800     SKIP3                                                                
098900 IMS-GET-MSG SECTION.                                                     
099000     MOVE '  QC' TO GODK-STATUSKODER                                      
099100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
099200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099300     PERFORM IMS-STATUSKONTROLL                                           
099400     .                                                                    
099500     SKIP3                                                                
099600 IMS-INSERT-MSG SECTION.                                                  
099700     IF MSGI-IDLAND-SPR = 'SE'                                            
099800       MOVE '0' TO MFS-KDHUVOMR                                           
099900     END-IF                                                               
100000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
100100     MOVE SPACE TO GODK-STATUSKODER                                       
100200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
100300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600     EJECT                                                                
100700 IMS-GU-W6H701 SECTION.                                                   
100800     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
100900          DELIMITED BY SIZE INTO SSA1                                     
101000     MOVE '  GE' TO GODK-STATUSKODER                                      
101100     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-W6H701 SSA1                    
101200     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUSKONTROLL                                           
101400     .                                                                    
101500     EJECT                                                                
101600 IMS-GU-W6H712 SECTION.                                                   
101700     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
101800          DELIMITED BY SIZE INTO SSA1                                     
101900     MOVE 'W6H712   '         TO SSA2                                     
102000     MOVE '  GE' TO GODK-STATUSKODER                                      
102100     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-W6H712 SSA1 SSA2               
102200     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
102300     PERFORM IMS-STATUSKONTROLL                                           
102400     .                                                                    
102500     EJECT                                                                
102600 IMS-GHU-W6H712 SECTION.                                                  
102700     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
102800          DELIMITED BY SIZE INTO SSA1                                     
102900     MOVE 'W6H712   '         TO SSA2                                     
103000     MOVE '  GE' TO GODK-STATUSKODER                                      
103100     CALL CBLTDLI USING GHU W6H7-PCB DLI-IO-W6H712 SSA1 SSA2              
103200     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
103300     PERFORM IMS-STATUSKONTROLL                                           
103400     .                                                                    
103500     EJECT                                                                
103600 IMS-REPL-W6H712 SECTION.                                                 
103700     MOVE '  ' TO GODK-STATUSKODER                                        
103800     CALL CBLTDLI USING REPL W6H7-PCB DLI-IO-W6H712                       
103900     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
104000     PERFORM IMS-STATUSKONTROLL                                           
104100     .                                                                    
104200     EJECT                                                                
104300 IMS-GNP-W6H721 SECTION.                                                  
104400     MOVE 'W6H721   '         TO SSA1                                     
104500     MOVE '  GE' TO GODK-STATUSKODER                                      
104600     CALL CBLTDLI USING GNP W6H7-PCB DLI-IO-W6H721 SSA1                   
104700     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
104800     PERFORM IMS-STATUSKONTROLL                                           
104900     .                                                                    
105000     SKIP3                                                                
105100 IMS-GU-W6H721 SECTION.                                                   
105200     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
105300          DELIMITED BY SIZE INTO SSA1                                     
105400     MOVE 'W6H712   '         TO SSA2                                     
105500     STRING 'W6H721  (IDSEGMNR =' W-IDSEGMNR-X ')'                        
105600          DELIMITED BY SIZE INTO SSA3                                     
105700     MOVE '  GE' TO GODK-STATUSKODER                                      
105800     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-W6H721 SSA1 SSA2 SSA3          
105900     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
106000     PERFORM IMS-STATUSKONTROLL                                           
106100     .                                                                    
106200     SKIP3                                                                
106300 IMS-GHU-W6H721 SECTION.                                                  
106400     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
106500          DELIMITED BY SIZE INTO SSA1                                     
106600     MOVE 'W6H712   '         TO SSA2                                     
106700     STRING 'W6H721  (IDSEGMNR =' W-IDSEGMNR-X ')'                        
106800          DELIMITED BY SIZE INTO SSA3                                     
106900     MOVE '  ' TO GODK-STATUSKODER                                        
107000     CALL CBLTDLI USING GHU W6H7-PCB DLI-IO-W6H721 SSA1 SSA2 SSA3         
107100     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
107200     PERFORM IMS-STATUSKONTROLL                                           
107300     .                                                                    
107400     SKIP3                                                                
107500 IMS-REPL-W6H721 SECTION.                                                 
107600     MOVE '  ' TO GODK-STATUSKODER                                        
107700     CALL CBLTDLI USING REPL W6H7-PCB DLI-IO-W6H721                       
107800     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
107900     PERFORM IMS-STATUSKONTROLL                                           
108000     .                                                                    
108100     EJECT                                                                
108200 IMS-ISRT-W6H721 SECTION.                                                 
108300     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
108400          DELIMITED BY SIZE INTO SSA1                                     
108500     MOVE 'W6H712 ' TO SSA2                                               
108600     MOVE 'W6H721 ' TO SSA3                                               
108700     MOVE '  '  TO GODK-STATUSKODER                                       
108800     CALL CBLTDLI USING ISRT W6H7-PCB DLI-IO-W6H721 SSA1 SSA2 SSA3        
108900     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     .                                                                    
109200     EJECT                                                                
109300 IMS-GU-WDP311 SECTION.                                                   
109400     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
109500          DELIMITED BY SIZE INTO SSA1                                     
109600     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
109700          DELIMITED BY SIZE INTO SSA2                                     
109800     MOVE '  GE' TO GODK-STATUSKODER                                      
109900     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
110000     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
110100     PERFORM IMS-STATUSKONTROLL                                           
110200     .                                                                    
110300     EJECT                                                                
110400 IMS-ISRT-WLFILC01 SECTION.                                               
110500     MOVE 'WLFILC01' TO SSA1                                              
110600     MOVE '  ' TO GODK-STATUSKODER                                        
110700     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-FILC SSA1                    
110800     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
110900     PERFORM IMS-STATUSKONTROLL                                           
111000     .                                                                    
111100     EJECT                                                                
111200 IMS-STATUSKONTROLL SECTION.                                              
111300     SET STATUS-IX TO 1                                                   
111400     SEARCH GODK-STATUS                                                   
111500       AT END                                                             
111600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
111700         DELIMITED BY SIZE INTO FELTEXT                                   
111800         CALL FELLOG                                                      
111900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
112000         CONTINUE                                                         
112100     END-SEARCH                                                           
112200     .                                                                    
