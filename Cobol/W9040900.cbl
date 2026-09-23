000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9040900.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   DECEMBER-88.                                             
000500*    FUNKTION.                                                            
000600****************************************************************          
000700*                                                              *          
000800*                     NYPON-BILD 9409                          *          
000900*                                                              *          
001000*                   ARTIKELINFORMATION NYPON                   *          
001100*                                                              *          
001200*  BILD FÖR BEREDNINGSAVDELNINGEN DÄR MAN KAN REGISTRERA       *          
001300*  KDRESBED, TINEDBRY OCH TEARTNOT,SAMTLIGA ÄR FÄLT PÅ NYPON-  *          
001400*  BASEN.                                                      *          
001500*                                                              *          
001600*  BORTTAG (SLÄCK) AV 9-KOMPLEMENTET KAN OCKSÅ GÖRAS.          *          
001700*                                                              *          
001800****************************************************************          
001900*                                                                         
002000     EJECT                                                                
002100*    INDATA.                                                              
002200*        TRANSAKTION: W90409T                                             
002300*                     W90409U                                             
002400*        MID:         W90409I1                                            
002500*    UTDATA.                                                              
002600*        MOD:         W90409O1                                            
002700*    SUBPROGRAM.                                                          
002800*        FELLOG                                                           
002900*        CBLTDLI                                                          
003000*                                                                         
003100     EJECT                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP3                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP3                                                                
003700*    -COPY WY2000W3                                                       
003800     SKIP3                                                                
003900*                                                                         
004000******************************************************************        
004100*          W O R K I N G  S T O R A G E  S E C T I O N           *        
004200******************************************************************        
004300*                                                                         
004400 77  PROGRAM-NAMN                PIC X(08)  VALUE 'W9040900'.             
004500 77  JA                          PIC X(01)  VALUE 'J'.                    
004600 77  NEJ                         PIC X(01)  VALUE 'N'.                    
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +557 COMP SYNC.         
004800 77  INDX                        PIC S9(3)  VALUE ZERO COMP-3.            
004900 77  WS-IDARTNR                  PIC X(09).                               
005000 77  WS-TEST-KDPRODSL            PIC 9(2)   VALUE ZERO.                   
005100                                                                          
005200 01  WS-IDTRANS                  PIC X(04).                               
005300     88  EGEN-BILD                          VALUE '9409'.                 
005400                                                                          
005410*01  -COPY WWPRODSL                                                       
005500     EJECT                                                                
005600*                                                                         
005700******************************************************************        
005800*                     S W I T C H A R                            *        
005900******************************************************************        
006000*                                                                         
006100 01  SWITCHAR.                                                            
006200     05  SW-INPUT-RAETT          PIC X(01)  VALUE 'J'.                    
006300     05  SW-TRAFF                PIC X(01)  VALUE 'N'.                    
006400                                                                          
006500*                                                                         
006600******************************************************************        
006700*               D I V E R S E  S P A R F Ä L T                   *        
006800******************************************************************        
006900*                                                                         
007000 01  SPAR-DIVERSE.                                                        
007100     05  SPAR-DAGENS-DATUM           PIC 9(06)  VALUE ZERO.               
007200     05  SPAR-DAGENS-DATUM-AAVV.                                          
007300         10  SPAR-DAGENS-AA          PIC 9(02)  VALUE ZERO.               
007400         10  SPAR-DAGENS-VV          PIC 9(02)  VALUE ZERO.               
007500     05  SPAR-DAGENS-DATUM-R  REDEFINES                                   
007600              SPAR-DAGENS-DATUM-AAVV PIC 9(04).                           
007700                                                                          
007800     05  SPAR-DATUM-AAVV.                                                 
007900         10  SPAR-AA                 PIC 9(02)  VALUE ZERO.               
008000         10  SPAR-VV                 PIC 9(02)  VALUE ZERO.               
008100     05  SPAR-DATUM-R  REDEFINES                                          
008200              SPAR-DATUM-AAVV        PIC 9(04).                           
008300                                                                          
008400     05  SPAR-9KOMPL-IDARTNR         PIC 9(09)  VALUE ZERO.               
008500                                                                          
008600     05  SPAR-TINEDBRY-AAMMDD        PIC 9(06)  VALUE ZERO.               
008700     05  SPAR-TINEDBRY-AAVV.                                              
008800         10  SPAR-TINEDBRY-AA        PIC 9(02)  VALUE ZERO.               
008900         10  SPAR-TINEDBRY-VV        PIC 9(02)  VALUE ZERO.               
009000     05  SPAR-TINEDBRY-AAVV-R  REDEFINES  SPAR-TINEDBRY-AAVV              
009100                                     PIC 9(04).                           
009200                                                                          
009300     05  SPAR-IDLOGLOP              PIC S9(1) COMP-3 VALUE ZERO.          
009400     05  SPAR-KDPRODSL               PIC 9(02)  VALUE ZERO.               
009500                                                                          
009600     EJECT                                                                
009700*      --- VALID IDDC CODES                                               
009800*                                                                         
009900*01    -COPY WWDC99                                                       
010000       EJECT                                                              
010100*                                                                         
010200******************************************************************        
010300*           D Y N A M I S K A  S U B P R O G R A M               *        
010400******************************************************************        
010500*                                                                         
010600 01  DYNAMISKA-SUBPROGRAM.                                                
010700     05  WDATKONV                PIC X(08)   VALUE 'WDATKONV'.            
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011100     EJECT                                                                
011200******************************************************************        
011300*    F E L M E D D E L A N D E N                                          
011400******************************************************************        
011500*                                                                         
011600 01  MEDDELANDE.                                                          
011700     03  FEL1.                                                            
011800        05  FILLER               PIC X(32) VALUE                          
011900            'ARTIKELNUMMER EJ NUMERISKT      '.                           
012000        05  FILLER               PIC X(32) VALUE                          
012100            'PARTNUMBER NOT NUMERIC          '.                           
012200     03 FILLER REDEFINES FEL1.                                            
012300        05  FEL-1                PIC X(32) OCCURS 2.                      
012400                                                                          
012500     03  FEL2.                                                            
012600        05  FILLER               PIC X(32) VALUE                          
012700            'UPPLYSTA FÄLT FEL               '.                           
012800        05  FILLER               PIC X(32) VALUE                          
012900            'CORRECT HIGHLIGHTED FIELDS      '.                           
013000     03 FILLER REDEFINES FEL2.                                            
013100        05  FEL-2                PIC X(32) OCCURS 2.                      
013200                                                                          
013300     03  FEL3.                                                            
013400        05  FILLER               PIC X(32) VALUE                          
013500            'ARTIKELNUMMER SAKNAS PÅ NYPON   '.                           
013600        05  FILLER               PIC X(32) VALUE                          
013700            'PART NUMBER IS MISSING          '.                           
013800     03 FILLER REDEFINES FEL3.                                            
013900        05  FEL-3                PIC X(32) OCCURS 2.                      
014000                                                                          
014100     03  FEL4.                                                            
014200        05  FILLER               PIC X(32) VALUE                          
014300            'MASKIN. KÖP VIA INKÖP EJ MÖJLIGT'.                           
014400        05  FILLER               PIC X(32) VALUE                          
014500            'MACHINE. PURCHASE NOT ALLOWED   '.                           
014600     03 FILLER REDEFINES FEL4.                                            
014700        05  FEL-4                PIC X(32) OCCURS 2.                      
014800                                                                          
014900     03  MED4.                                                            
015000        05  FILLER               PIC X(32) VALUE                          
015100            'PRODUKTSLAG ÄNDRAT              '.                           
015200        05  FILLER               PIC X(32) VALUE                          
015300            'NEW PROD.GROUP                  '.                           
015400     03 FILLER REDEFINES MED4.                                            
015500        05  MED-4                PIC X(32) OCCURS 2.                      
015600                                                                          
015700     03  MED5.                                                            
015800        05  FILLER               PIC X(32) VALUE                          
015900            'UPPDATERING UTFÖRD              '.                           
016000        05  FILLER               PIC X(32) VALUE                          
016100            'UPDATED                         '.                           
016200     03 FILLER REDEFINES MED5.                                            
016300        05  MED-5                PIC X(32) OCCURS 2.                      
016400                                                                          
016500     03  MED6.                                                            
016600        05  FILLER               PIC X(32) VALUE                          
016700            'TRYCK PF11 FÖR UPPDATERING      '.                           
016800        05  FILLER               PIC X(32) VALUE                          
016900            'PRESS PF11 FOR UPDATING         '.                           
017000     03 FILLER REDEFINES MED6.                                            
017100        05  MED-6                PIC X(32) OCCURS 2.                      
017200                                                                          
017300     03  MED7.                                                            
017400        05  FILLER               PIC X(32) VALUE                          
017500            'UPPDATERING EJ TILLÅTEN         '.                           
017600        05  FILLER               PIC X(32) VALUE                          
017700            'UPDATE NOT ALLOWED              '.                           
017800     03 FILLER REDEFINES MED7.                                            
017900        05  MED-7                PIC X(32) OCCURS 2.                      
018000                                                                          
018100     EJECT                                                                
018200*                                                                         
018300******************************************************************        
018400*                    K D P  /  K O L A                           *        
018500******************************************************************        
018600*                                                                         
018700 01  IMS-WS-1.                                                            
018800     03  FILLER                  PIC X(16)   VALUE 'KDP-KOLA '.           
018900     SKIP3                                                                
019000*01  -COPY W10111                                                         
019100     EJECT                                                                
019200 01  IMS-WS-2.                                                            
019300     03  FILLER                  PIC X(16)   VALUE 'W092-AREA'.           
019400     SKIP3                                                                
019500*01  AREA -COPY W092W001        -PRE W092-                                
019600     EJECT                                                                
019700*                                                                         
019800******************************************************************        
019900*                    C O P Y T E X T E R    (DYNAMISKA ANROP)    *        
020000******************************************************************        
020100*                                                                         
020200 01  IMS-WS-3.                                                            
020300     03  FILLER                  PIC X(16)   VALUE 'RDAT-AREA'.           
020400     SKIP3                                                                
020500*01  -COPY WDATAREA                                                       
020600     SKIP3                                                                
020700*                    ****   PARAMETRAR TILL W005INIT                      
020800*01  -COPY WMSGINIT                                                       
020900     EJECT                                                                
021000*                                                                         
021100******************************************************************        
021200*              N Y C K L A R  T I L L  D L I                     *        
021300******************************************************************        
021400*                                                                         
021500 01  NYCKLAR-TILL-DLI.                                                    
021600     03  W-IDARTNR-X.                                                     
021700         05  W-IDARTNR            PIC S9(09) COMP-3 VALUE ZERO.           
021800                                                                          
021900     03  W-1135KEY-X.                                                     
022000         05  FILLER               PIC X(04)  VALUE '1135'.                
022100         05  W-KDPRODSL           PIC S9(3)  VALUE ZERO COMP-3.           
022200         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
022300                                                                          
022400     03  W-1136KEY-X.                                                     
022500         05  W-IDFKNGRP           PIC S9(5)  VALUE ZERO COMP-3.           
022600         05  FILLER               PIC X(02)  VALUE LOW-VALUE.             
022700                                                                          
022800     03  W-1139KEY-X.                                                     
022900         05  FILLER               PIC X(04)  VALUE '1139'.                
023000         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
023100                                                                          
023200                                                                          
023300*SÖKBEGREPP:                                                              
023400     03  W-IDFKNGRP-X.                                                    
023500         05  W-IDFKNGRP-SBGP      PIC S9(5)  VALUE ZERO COMP-3.           
023600                                                                          
023700     EJECT                                                                
023800*                                                                         
023900******************************************************************        
024000*                    M I D-C O P Y T E X T                       *        
024100******************************************************************        
024200*                                                                         
024300*                        ****    MFS OCH SKÄRMHANTERING                   
024400 01  IMS-WS-3.                                                            
024500     03  FILLER                  PIC X(16)   VALUE 'MFS-WS'.              
024600     SKIP3                                                                
024700*01  MID -COPY W90409I1                                                   
024800     EJECT                                                                
024900*                                                                         
025000******************************************************************        
025100*                    M S G - A R E A                             *        
025200******************************************************************        
025300*                                                                         
025400 01  IMS-WS-4.                                                            
025500     03  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.            
025600     SKIP3                                                                
025700*01  -COPY WMSGAREA                                                       
025800     EJECT                                                                
025900*                                                                         
026000******************************************************************        
026100*                    M O D-C O P Y T E X T                       *        
026200******************************************************************        
026300*                                                                         
026400*    03  MOD -COPY W90409O1  -RED MSG-AREA.                               
026500     EJECT                                                                
026600*                                                                         
026700******************************************************************        
026800*                    M F S - A R E A                             *        
026900******************************************************************        
027000*                                                                         
027100 01  IMS-WS-6.                                                            
027200     03  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.            
027300     SKIP3                                                                
027400*01  -COPY WMFSAREA.                                                      
027500     EJECT                                                                
027600*                                                                         
027700******************************************************************        
027800*    A R B E T S A R E O R  I M S - S E K T I O N E R N A        *        
027900******************************************************************        
028000*                                                                         
028100 01  IMS-WS-7.                                                            
028200     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
028300     SKIP3                                                                
028400*****                    **** STATUS-KOD FRÅN IMS                         
028500     03  STATUS-WS               PIC X(2).                                
028600         88  SEGMENT-FINNS                   VALUE '  '.                  
028700         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
028800     SKIP3                                                                
028900     03  GODK-STATUSKODER.                                                
029000         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
029100     SKIP3                                                                
029200 01  IMS-WS-8.                                                            
029300     03  FILLER                  PIC X(09)   VALUE 'SSA:ER   '.           
029400     SKIP3                                                                
029500 01  SSA1                        PIC X(128).                              
029600 01  SSA2                        PIC X(128).                              
029700     EJECT                                                                
029800*                                                                         
029900******************************************************************        
030000*            I M S  F U N K T I O N S K O D E R                  *        
030100******************************************************************        
030200*                                                                         
030300*                                                                         
030400 01  IMS-WS-9.                                                            
030500     03  FILLER                  PIC X(16)   VALUE ' IMS-FUNK'.           
030600     SKIP3                                                                
030700*01  -COPY W0003                                                          
030800     EJECT                                                                
030900*                                                                         
031000******************************************************************        
031100*            D L I  I N P U T-O U T P U T A R E A -1             *        
031200******************************************************************        
031300*                                                                         
031400 01  IMS-WS-10.                                                           
031500     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA1'.           
031600     SKIP3                                                                
031700 01  DLI-IO-AREA1.                                                        
031800     03  IO-AREA1                  PIC X(900) VALUE SPACE.                
031900     SKIP3                                                                
032000*                                                                         
032100******************************************************************        
032200*            S E G M E N T C O P Y T E X T E R                   *        
032300******************************************************************        
032400*                                                                         
032500*    03  ARTC -COPY WDK601                   -RED IO-AREA1.               
032600     EJECT                                                                
032700*    03  ARTC -COPY WDK611                   -RED IO-AREA1.               
032800     EJECT                                                                
032900*    03  ARTC -COPY WDK622                   -RED IO-AREA1.               
033000     EJECT                                                                
033100*    03  ARTC -COPY WDK623                   -RED IO-AREA1.               
033200     EJECT                                                                
033300*    03  XXAS -COPY WDGX1135    -PRE XXAS-   -RED IO-AREA1.               
033400     EJECT                                                                
033500*    03  XXAS -COPY WDGX1136    -PRE XXAS-   -RED IO-AREA1.               
033600     EJECT                                                                
033700*    03  ZZAC -COPY WDGZ01      -PRE ZZAC-   -RED IO-AREA1.               
033800     EJECT                                                                
033900*                                                                         
034000******************************************************************        
034100*            D L I  I N P U T-O U T P U T A R E A -2             *        
034200******************************************************************        
034300*                                                                         
034400 01  IMS-WS-11.                                                           
034500     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA2'.           
034600     SKIP3                                                                
034700 01  DLI-IO-AREA2.                                                        
034800     03  IO-AREA2                  PIC X(600)   VALUE SPACE.              
034900     SKIP3                                                                
035000*    03  ARTG -COPY WDD201        -PRE NYPON-  -RED IO-AREA2.             
035100     EJECT                                                                
035200*                                                                         
035300******************************************************************        
035400*            D L I  I N P U T-O U T P U T A R E A -3             *        
035500******************************************************************        
035600*                                                                         
035700 01  IMS-WS-12.                                                           
035800     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA3'.           
035900     SKIP3                                                                
036000 01  DLI-IO-AREA3.                                                        
036100     03  IO-AREA3                  PIC X(600)   VALUE SPACE.              
036200     SKIP3                                                                
036300*    03  ARTG -COPY WDD201        -PRE 9KOMPL- -RED IO-AREA3.             
036400     EJECT                                                                
036500*                                                                         
036600******************************************************************        
036700*            D L I  I N P U T-O U T P U T A R E A -4             *        
036800******************************************************************        
036900*                                                                         
037000 01  IMS-WS-13.                                                           
037100     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA4'.           
037200     SKIP3                                                                
037300 01  DLI-IO-AREA4.                                                        
037400     03  IO-AREA4                  PIC X(1500)  VALUE SPACE.              
037500     EJECT                                                                
037600*                                                                         
037700******************************************************************        
037800*            L I N K A G E  S E C T I O N                        *        
037900******************************************************************        
038000*                                                                         
038100 LINKAGE SECTION.                                                         
038200     SKIP2                                                                
038300*01  -COPY W0009     -PRE MSG-                                            
038400     EJECT                                                                
038500*01  -COPY W0008     -PRE USEA-                                           
038600         05  FILLER              PIC X.                                   
038700     EJECT                                                                
038800*01  -COPY W0008     -PRE ARTG1-                                          
038900         05  FILLER              PIC X.                                   
039000     EJECT                                                                
039100*01  -COPY W0008     -PRE ARTG2-                                          
039200         05  FILLER              PIC X.                                   
039300     EJECT                                                                
039400*01  -COPY W0008     -PRE ARTC-                                           
039500         05  FILLER              PIC X.                                   
039600     EJECT                                                                
039700*01  -COPY W0008     -PRE XXAS-                                           
039800         05  FILLER              PIC X.                                   
039900     EJECT                                                                
040000     EJECT                                                                
040100*01  -COPY W0008     -PRE ZZAC-                                           
040200         05  FILLER              PIC X.                                   
040300     EJECT                                                                
040400 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB                               
040500                                   ARTG1-PCB ARTG2-PCB ARTC-PCB           
040600                                   XXAS-PCB                               
040700                                   ZZAC-PCB.                              
040800     SKIP1                                                                
040900     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
041000                                    ARTG1-PCB ARTG2-PCB ARTC-PCB          
041100                                    XXAS-PCB                              
041200                                    ZZAC-PCB.                             
041300                                                                          
041400     PERFORM IMS-GET-MSG                                                  
041500     IF SEGMENT-FINNS                                                     
041600        PERFORM A-INIT-SPARA-INPUT                                        
041700        IF WS-IDARTNR NUMERIC                                             
041800           MOVE WS-IDARTNR  TO W-IDARTNR                                  
041900           PERFORM IMS-GHU-ARTG01-IO2-PCB1                                
042000           IF SEGMENT-FINNS                                               
042100              MOVE NYPON-ART-KDPRODSL TO WS-TEST-KDPRODSL                 
042200              IF MFS-UPDATE                                               
042300                 PERFORM C-KOLLA-INPUT                                    
042400                 IF SW-INPUT-RAETT = JA                                   
042500                    PERFORM D-UPPDATERA                                   
042600                 ELSE                                                     
042700                    MOVE FEL-2(INDX) TO MOD-TEMFSFEL                      
042800                 END-IF                                                   
042900              END-IF                                                      
043000              PERFORM B-VISA-BILD                                         
043100           ELSE                                                           
043200              MOVE FEL-3(INDX)TO MOD-TEMFSFEL                             
043300           END-IF                                                         
043400        ELSE                                                              
043500           MOVE FEL-1(INDX) TO MOD-TEMFSFEL                               
043600        END-IF                                                            
043700                                                                          
043800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
043900        PERFORM IMS-INSERT-MSG                                            
044000     END-IF                                                               
044100                                                                          
044200     MOVE ZERO TO RETURN-CODE                                             
044300     GOBACK.                                                              
044400     EJECT                                                                
044500 A-INIT-SPARA-INPUT SECTION.                                              
044600     SKIP2                                                                
044700     IF MSG-DUBBLA-TRANSKODER                                             
044800         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
044900                                   TO MID-W90409I1-CTX                    
045000         MOVE MSG-IDTRANS-2        TO MFS-IDTRANS WS-IDTRANS              
045100         MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                        
045200         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
045300         MOVE MSG-IDPFK            TO MFS-IDPFK                           
045400     ELSE                                                                 
045500         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
045600                                   TO MID-W90409I1-CTX                    
045700         MOVE MSG-IDTRANS-1        TO MFS-IDTRANS WS-IDTRANS              
045800         MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                        
045900         MOVE SPACE                TO MFS-KDTRTYP                         
046000                                      MFS-IDPFK                           
046100     END-IF                                                               
046200                                                                          
046300     IF EGEN-BILD                                                         
046400        CONTINUE                                                          
046500     ELSE                                                                 
046600        MOVE SPACE                 TO MFS-KDTRTYP                         
046700                                      MFS-IDPFK                           
046800     END-IF                                                               
046900                                                                          
047000     MOVE LOW-VALUE                TO MOD-W90409O1-CTX                    
047100     MOVE 'W90409O1'               TO MFS-IDMOD                           
047200     MOVE '9409'                   TO MOD-IDTRANS                         
047300                                                                          
047400     MOVE MFS-RENSA-FAELT          TO MOD-TEMFSFEL                        
047500                                      MOD-TEMFSINF                        
047600                                      MOD-IDARTNR-IN                      
047700                                                                          
047800     ACCEPT SPAR-DAGENS-DATUM FROM DATE                                   
047900                                                                          
048000     MOVE 'AAMMDD'                 TO DAT-KDDATFORM                       
048100     MOVE SPAR-DAGENS-DATUM        TO DAT-I-TIDATUM                       
048200     PERFORM S99-WDATKONV                                                 
048300                                                                          
048400     IF DAT-KDSVAR-OK                                                     
048500        MOVE DAT-TIAA              TO SPAR-DAGENS-AA                      
048600        MOVE DAT-TIVV              TO SPAR-DAGENS-VV                      
048700     END-IF                                                               
048800                                                                          
048900     IF MFS-UPDATE                    AND                                 
049000        MID-KDRESBED      = ALL '+'   AND                                 
049100*       MID-TINEDBRY      = ALL '+'   AND                                 
049200        MID-AVSL-NOT      = ALL '+'                                       
049300*       MID-SLAECK-9KOMPL = ALL '+'                                       
049400        MOVE SPACE                 TO MFS-KDTRTYP                         
049500                                      MFS-IDPFK                           
049600     END-IF                                                               
049700                                                                          
049800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
049900     MOVE '001'             TO MSGI-KDCALL                                
050000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
050100                               MSGI-IDLTERM-USER                          
050200     MOVE '9409'            TO MSGI-IDTRANS                               
050300     IF MFS-IDTRANS = '9409'                                              
050400     OR (MID-IDARTNR-IN NUMERIC                                           
050500     AND MID-IDARTNR-IN > ZERO)                                           
050600         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
050700     END-IF                                                               
050800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
050900     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
051000     MOVE MSGI-IDDC    TO WS-IDDC                                         
051100     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
051200                                                                          
051300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
051400       MOVE +1 TO INDX                                                    
051500     ELSE                                                                 
051600       MOVE +2 TO INDX                                                    
051700     END-IF                                                               
051800                                                                          
051900     IF MID-IDARTNR-IN = ALL '+'                                          
052000        CONTINUE                                                          
052100     ELSE                                                                 
052200        MOVE SPACE                 TO MFS-KDTRTYP                         
052300                                      MFS-IDPFK                           
052400     END-IF                                                               
052500                                                                          
052600*    MOVE WS-IDARTNR        TO MOD-IDARTNR-UT                             
052700*    INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE.              
052800                                                                          
052900     EJECT                                                                
053000     .                                                                    
053100 B-VISA-BILD SECTION.                                                     
053200     SKIP2                                                                
053300*    MOVE NYPON-ART-IDPROJ           TO MOD-IDPROJ                        
053400*    MOVE NYPON-ART-IDPROJK          TO MOD-IDPROJK                       
053500*    MOVE NYPON-ART-IDAO             TO MOD-IDAO                          
053600*    MOVE NYPON-ART-IDRITUTG         TO MOD-IDRITUTG                      
053700*    MOVE NYPON-ART-IDAVD            TO MOD-IDAVD                         
053800*    MOVE NYPON-ART-KVARTAR1         TO MOD-KVARTAR1                      
053900*    MOVE NYPON-ART-FLRITB           TO MOD-FLRITB                        
054000*    MOVE NYPON-ART-IDPROENH         TO MOD-IDPROENH                      
054100*    MOVE NYPON-ART-KVARTAR2         TO MOD-KVARTAR2                      
054200*    MOVE NYPON-ART-FLRITC           TO MOD-FLRITC                        
054300*    MOVE NYPON-ART-PRARTBES         TO MOD-PRARTSTD                      
054400*    MOVE NYPON-ART-KVARTAR3         TO MOD-KVARTAR3                      
054500*    MOVE NYPON-ART-FLRITP           TO MOD-FLRITP                        
054600     MOVE NYPON-ART-IDRITN           TO MOD-IDRITN                        
054700*    MOVE NYPON-ART-IDFKNGRP         TO MOD-IDFKNGRP                      
054800*    MOVE NYPON-ART-IDINK            TO MOD-IDINK                         
054900*    MOVE NYPON-ART-KDSORT           TO MOD-KDSORT                        
055000*    MOVE NYPON-ART-IDLEVNR          TO MOD-IDLEVNR                       
055100*    MOVE NYPON-ART-IDLEVNR-FORB (1) TO MOD-IDLEVNR-FORB1                 
055200*    MOVE NYPON-ART-IDLEVNR-FORB (2) TO MOD-IDLEVNR-FORB2                 
055300*    MOVE NYPON-ART-IDLEVNR-FORB (3) TO MOD-IDLEVNR-FORB3                 
055400*    MOVE NYPON-ART-IDLEVNR-FORB (4) TO MOD-IDLEVNR-FORB4                 
055500*    MOVE NYPON-ART-IDLEVNR-FORB (5) TO MOD-IDLEVNR-FORB5                 
055600                                                                          
055700                                                                          
055800*    IF NYPON-ART-FLUPG = JA                                              
055900*       MOVE 'G'                     TO MOD-FLUPG                         
056000*    ELSE                                                                 
056100*       IF NYPON-ART-FLUPG = NEJ                                          
056200*          MOVE 'U'                  TO MOD-FLUPG                         
056300*       ELSE                                                              
056400*          MOVE SPACE                TO MOD-FLUPG                         
056500*       END-IF                                                            
056600**   END-IF                                                               
056700                                                                          
056800*    MOVE NYPON-ART-KVNK             TO MOD-KVNK                          
056900*    MOVE NYPON-ART-KVLEVBEG         TO MOD-KVLEVBEG                      
057000*    MOVE NYPON-ART-KVPROG           TO MOD-KVPROG                        
057100*    MOVE NYPON-ART-BEART-SVE        TO MOD-BEART                         
057200*    MOVE NYPON-ART-TETEKNIK         TO MOD-TETEKNIK                      
057300     MOVE NYPON-ART-KDRESBED         TO MOD-KDRESBED-UT                   
057400                                                                          
057500*    MOVE 'AAMMDD'                   TO DAT-KDDATFORM                     
057600*    MOVE NYPON-ART-TISTABER         TO DAT-I-TIDATUM                     
057700*    PERFORM S99-WDATKONV                                                 
057800*                                                                         
057900*    IF DAT-KDSVAR-OK                                                     
058000*       MOVE DAT-TIAA                TO SPAR-AA                           
058100*       MOVE DAT-TIVV                TO SPAR-VV                           
058200*       MOVE SPAR-DATUM-R            TO MOD-TISTABER                      
058300*    ELSE                                                                 
058400*       MOVE ZERO                    TO MOD-TISTABER                      
058500*       INSPECT MOD-TISTABER REPLACING LEADING ZERO BY SPACE              
058600*    END-IF                                                               
058700                                                                          
058800*    MOVE NYPON-ART-TIRITB           TO DAT-I-TIDATUM                     
058900*    PERFORM S99-WDATKONV                                                 
059000*                                                                         
059100*    IF DAT-KDSVAR-OK                                                     
059200*       MOVE DAT-TIAA                TO SPAR-AA                           
059300*       MOVE DAT-TIVV                TO SPAR-VV                           
059400*       MOVE SPAR-DATUM-R            TO MOD-TIRITB                        
059500*    ELSE                                                                 
059600*       MOVE ZERO                    TO MOD-TIRITB                        
059700*       INSPECT MOD-TIRITB REPLACING LEADING ZERO BY SPACE                
059800*    END-IF                                                               
059900                                                                          
060000*    MOVE NYPON-ART-TISLUBER         TO DAT-I-TIDATUM                     
060100*    PERFORM S99-WDATKONV                                                 
060200*                                                                         
060300*    IF DAT-KDSVAR-OK                                                     
060400*       MOVE DAT-TIAA                TO SPAR-AA                           
060500*       MOVE DAT-TIVV                TO SPAR-VV                           
060600*       MOVE SPAR-DATUM-R            TO MOD-TISLUBER                      
060700*    ELSE                                                                 
060800*       MOVE ZERO                    TO MOD-TISLUBER                      
060900*       INSPECT MOD-TISLUBER REPLACING LEADING ZERO BY SPACE              
061000*    END-IF                                                               
061100                                                                          
061200*    MOVE NYPON-ART-TIRITC           TO DAT-I-TIDATUM                     
061300*    PERFORM S99-WDATKONV                                                 
061400*                                                                         
061500*    IF DAT-KDSVAR-OK                                                     
061600*       MOVE DAT-TIAA                TO SPAR-AA                           
061700*       MOVE DAT-TIVV                TO SPAR-VV                           
061800*       MOVE SPAR-DATUM-R            TO MOD-TIRITC                        
061900*    ELSE                                                                 
062000*       MOVE ZERO                    TO MOD-TIRITC                        
062100*       INSPECT MOD-TIRITC REPLACING LEADING ZERO BY SPACE                
062200*    END-IF                                                               
062300                                                                          
062400*    MOVE NYPON-ART-TIPLAKOP         TO DAT-I-TIDATUM                     
062500*    PERFORM S99-WDATKONV                                                 
062600*                                                                         
062700*    IF DAT-KDSVAR-OK                                                     
062800*       MOVE DAT-TIAA                TO SPAR-AA                           
062900*       MOVE DAT-TIVV                TO SPAR-VV                           
063000*       MOVE SPAR-DATUM-R            TO MOD-TIPLAKOP                      
063100*    ELSE                                                                 
063200*       MOVE ZERO                    TO MOD-TIPLAKOP                      
063300*       INSPECT MOD-TIPLAKOP REPLACING LEADING ZERO BY SPACE              
063400*    END-IF                                                               
063500                                                                          
063600*    MOVE NYPON-ART-TIRITP           TO DAT-I-TIDATUM                     
063700*    PERFORM S99-WDATKONV                                                 
063800*                                                                         
063900*    IF DAT-KDSVAR-OK                                                     
064000*       MOVE DAT-TIAA                TO SPAR-AA                           
064100*       MOVE DAT-TIVV                TO SPAR-VV                           
064200*       MOVE SPAR-DATUM-R            TO MOD-TIRITP                        
064300*    ELSE                                                                 
064400*       MOVE ZERO                    TO MOD-TIRITP                        
064500*       INSPECT MOD-TIRITP REPLACING LEADING ZERO BY SPACE                
064600*    END-IF                                                               
064700                                                                          
064800*    MOVE NYPON-ART-TIANSKREG        TO DAT-I-TIDATUM                     
064900*    PERFORM S99-WDATKONV                                                 
065000*                                                                         
065100*    IF DAT-KDSVAR-OK                                                     
065200*       MOVE DAT-TIAA                TO SPAR-AA                           
065300*       MOVE DAT-TIVV                TO SPAR-VV                           
065400*       MOVE SPAR-DATUM-R            TO MOD-TIANSKREG                     
065500*    ELSE                                                                 
065600*       MOVE ZERO                    TO MOD-TIANSKREG                     
065700*       INSPECT MOD-TIANSKREG REPLACING LEADING ZERO BY SPACE             
065800*    END-IF                                                               
065900                                                                          
066000*    MOVE NYPON-ART-TIUPG            TO DAT-I-TIDATUM                     
066100*    PERFORM S99-WDATKONV                                                 
066200*                                                                         
066300*    IF DAT-KDSVAR-OK                                                     
066400*       MOVE DAT-TIAA                TO SPAR-AA                           
066500*       MOVE DAT-TIVV                TO SPAR-VV                           
066600*       MOVE SPAR-DATUM-R            TO MOD-TIUPG                         
066700*    ELSE                                                                 
066800*       MOVE ZERO                    TO MOD-TIUPG                         
066900*       INSPECT MOD-TIUPG REPLACING LEADING ZERO BY SPACE                 
067000*    END-IF                                                               
067100                                                                          
067200*    MOVE NYPON-ART-TISERLEV     (1) TO DAT-I-TIDATUM                     
067300*    PERFORM S99-WDATKONV                                                 
067400*                                                                         
067500*    IF DAT-KDSVAR-OK                                                     
067600*       MOVE DAT-TIAA                TO SPAR-AA                           
067700*       MOVE DAT-TIVV                TO SPAR-VV                           
067800*       MOVE SPAR-DATUM-R            TO MOD-TISERLEV1                     
067900*    ELSE                                                                 
068000*       MOVE ZERO                    TO MOD-TISERLEV1                     
068100*       INSPECT MOD-TISERLEV1 REPLACING LEADING ZERO BY SPACE             
068200*    END-IF                                                               
068300                                                                          
068400*    MOVE NYPON-ART-TISERLEV     (2) TO DAT-I-TIDATUM                     
068500*    PERFORM S99-WDATKONV                                                 
068600*                                                                         
068700*    IF DAT-KDSVAR-OK                                                     
068800*       MOVE DAT-TIAA                TO SPAR-AA                           
068900*       MOVE DAT-TIVV                TO SPAR-VV                           
069000*       MOVE SPAR-DATUM-R            TO MOD-TISERLEV2                     
069100*    ELSE                                                                 
069200*       MOVE ZERO                    TO MOD-TISERLEV2                     
069300*       INSPECT MOD-TISERLEV2 REPLACING LEADING ZERO BY SPACE             
069400*    END-IF                                                               
069500                                                                          
069600*    MOVE NYPON-ART-TISERLEV     (3) TO DAT-I-TIDATUM                     
069700*    PERFORM S99-WDATKONV                                                 
069800*                                                                         
069900*    IF DAT-KDSVAR-OK                                                     
070000*       MOVE DAT-TIAA                TO SPAR-AA                           
070100*       MOVE DAT-TIVV                TO SPAR-VV                           
070200*       MOVE SPAR-DATUM-R            TO MOD-TISERLEV3                     
070300*    ELSE                                                                 
070400*       MOVE ZERO                    TO MOD-TISERLEV3                     
070500*       INSPECT MOD-TISERLEV3 REPLACING LEADING ZERO BY SPACE             
070600*    END-IF                                                               
070700                                                                          
070800*    MOVE NYPON-ART-TISERLEV     (4) TO DAT-I-TIDATUM                     
070900*    PERFORM S99-WDATKONV                                                 
071000*                                                                         
071100*    IF DAT-KDSVAR-OK                                                     
071200*       MOVE DAT-TIAA                TO SPAR-AA                           
071300*       MOVE DAT-TIVV                TO SPAR-VV                           
071400*       MOVE SPAR-DATUM-R            TO MOD-TISERLEV4                     
071500*    ELSE                                                                 
071600*       MOVE ZERO                    TO MOD-TISERLEV4                     
071700*       INSPECT MOD-TISERLEV4 REPLACING LEADING ZERO BY SPACE             
071800*    END-IF                                                               
071900                                                                          
072000*    MOVE NYPON-ART-TISERLEV     (5) TO DAT-I-TIDATUM                     
072100*    PERFORM S99-WDATKONV                                                 
072200*                                                                         
072300*    IF DAT-KDSVAR-OK                                                     
072400*       MOVE DAT-TIAA                TO SPAR-AA                           
072500*       MOVE DAT-TIVV                TO SPAR-VV                           
072600*       MOVE SPAR-DATUM-R            TO MOD-TISERLEV5                     
072700*    ELSE                                                                 
072800*       MOVE ZERO                    TO MOD-TISERLEV5                     
072900*       INSPECT MOD-TISERLEV5 REPLACING LEADING ZERO BY SPACE             
073000*    END-IF                                                               
073100                                                                          
073200*    MOVE NYPON-ART-TILEVBEG         TO DAT-I-TIDATUM                     
073300*    PERFORM S99-WDATKONV                                                 
073400*                                                                         
073500*    IF DAT-KDSVAR-OK                                                     
073600*       MOVE DAT-TIAA                TO SPAR-AA                           
073700*       MOVE DAT-TIVV                TO SPAR-VV                           
073800*       MOVE SPAR-DATUM-R            TO MOD-TILEVBEG                      
073900*    ELSE                                                                 
074000*       MOVE ZERO                    TO MOD-TILEVBEG                      
074100*       INSPECT MOD-TILEVBEG REPLACING LEADING ZERO BY SPACE              
074200*    END-IF                                                               
074300                                                                          
074400*    MOVE NYPON-ART-TINEDBRY         TO DAT-I-TIDATUM                     
074500*    PERFORM S99-WDATKONV                                                 
074600*                                                                         
074700*    IF DAT-KDSVAR-OK                                                     
074800*       MOVE DAT-TIAA                TO SPAR-AA                           
074900*       MOVE DAT-TIVV                TO SPAR-VV                           
075000*       MOVE SPAR-DATUM-R            TO MOD-TINEDBRY-UT                   
075100*    ELSE                                                                 
075200*       MOVE ZERO                    TO MOD-TINEDBRY-UT                   
075300*       INSPECT MOD-TINEDBRY-UT REPLACING LEADING ZERO BY SPACE           
075400*    END-IF                                                               
075500                                                                          
075600     IF (MID-AVSL-NOT = ALL '+')  OR (NOT EGEN-BILD)                      
075700        MOVE NYPON-ART-TEARTNOT      TO MOD-AVSL-NOT-IN-UT                
075800     ELSE                                                                 
075900        IF SW-INPUT-RAETT = JA  AND  MFS-UPDATE                           
076000           MOVE NYPON-ART-TEARTNOT   TO MOD-AVSL-NOT-IN-UT                
076100        ELSE                                                              
076200           MOVE MFS-ROER-EJ-FAELT    TO MOD-AVSL-NOT-IN-UT                
076300        END-IF                                                            
076400     END-IF                                                               
076500                                                                          
076600     PERFORM BA-LAS-9KOMPL-JAMFOR                                         
076700                                                                          
076800     PERFORM BB-LAS-ARTREG                                                
076900                                                                          
077000     IF MFS-UPDATE                                                        
077100*       INFÄLT REDAN KONTROLLERADE OCH KLARA......                        
077200        CONTINUE                                                          
077300     ELSE                                                                 
077400        IF MID-IDARTNR-IN = ALL '+'  AND  EGEN-BILD                       
077500           IF MID-KDRESBED      = ALL '+'  AND                            
077600*             MID-TINEDBRY      = ALL '+'  AND                            
077700              MID-AVSL-NOT      = ALL '+'                                 
077800*             MID-SLAECK-9KOMPL = ALL '+'                                 
077900*             GAMMAL NYCKEL, INGENTING INMATAT                            
078000              CONTINUE                                                    
078100           ELSE                                                           
078200              MOVE MED-6(INDX)             TO MOD-TEMFSINF                
078300*             IF MID-KDRESBED = ALL '+'                                   
078400*                MOVE MFS-RENSA-FAELT      TO MOD-KDRESBED-IN             
078500*             ELSE                                                        
078600*                MOVE MFS-ROER-EJ-FAELT    TO MOD-KDRESBED-IN             
078700*                MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDRESBED-IN-ATTR        
078800*             END-IF                                                      
078900                                                                          
079000*             IF MID-TINEDBRY = ALL '+'                                   
079100*                MOVE MFS-RENSA-FAELT      TO MOD-TINEDBRY-IN             
079200*             ELSE                                                        
079300*                MOVE MFS-ROER-EJ-FAELT    TO MOD-TINEDBRY-IN             
079400*                MOVE MFS-NUM-FAELT-RAETT  TO MOD-TINEDBRY-IN-ATTR        
079500*             END-IF                                                      
079600                                                                          
079700*             IF MID-SLAECK-9KOMPL = ALL '+'                              
079800*                MOVE MFS-RENSA-FAELT      TO MOD-SLAECK-9KOMPL-IN        
079900*             ELSE                                                        
080000*                MOVE MFS-ROER-EJ-FAELT    TO MOD-SLAECK-9KOMPL-IN        
080100*                MOVE MFS-ALFA-FAELT-RAETT TO                             
080200*                                    MOD-SLAECK-9KOMPL-IN-ATTR            
080300*             END-IF                                                      
080400                                                                          
080500              IF MID-AVSL-NOT      = ALL '+'                              
080600                 CONTINUE                                                 
080700              ELSE                                                        
080800                 MOVE MFS-ROER-EJ-FAELT    TO MOD-AVSL-NOT-IN-UT          
080900                 MOVE MFS-ALFA-FAELT-RAETT TO                             
081000                                      MOD-AVSL-NOT-IN-UT-ATTR             
081100              END-IF                                                      
081200           END-IF                                                         
081300        ELSE                                                              
081400*          NY NYCKEL, RENSA ALLA INFÄLT                                   
081500*          PERFORM S02-RENSA-MOD-INMATNINGSFAELT                          
081600           CONTINUE                                                       
081700        END-IF                                                            
081800     END-IF.                                                              
081900                                                                          
082000     EJECT                                                                
082100 BA-LAS-9KOMPL-JAMFOR SECTION.                                            
082200     SKIP2                                                                
082300     PERFORM S98-COMPUTE-9KOMPL                                           
082400                                                                          
082500     MOVE SPAR-9KOMPL-IDARTNR        TO W-IDARTNR                         
082600                                                                          
082700     PERFORM IMS-GHU-ARTG01-IO3-PCB2                                      
082800                                                                          
082900     IF SEGMENT-FINNS                                                     
083000        PERFORM BAA-JAMFOR                                                
083100     END-IF.                                                              
083200                                                                          
083300     EJECT                                                                
083400 BAA-JAMFOR SECTION.                                                      
083500     SKIP2                                                                
083600     IF NYPON-ART-KDPRODSL    = 9KOMPL-ART-KDPRODSL                       
083700        CONTINUE                                                          
083800     ELSE                                                                 
083900        MOVE MED-4(INDX)             TO MOD-TEMFSFEL                      
084000*       LÄGGS I FELRADEN FÖR ATT INTE KONFLIKTA MED                       
084100*       ÖVRIGA MEDDELANDEN......                                          
084200     END-IF                                                               
084300                                                                          
084400*    IF NYPON-ART-IDPROJ      = 9KOMPL-ART-IDPROJ                         
084500*       CONTINUE                                                          
084600*    ELSE                                                                 
084700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDPROJ-ATTR                   
084800*    END-IF                                                               
084900                                                                          
085000*    IF NYPON-ART-IDPROJK     = 9KOMPL-ART-IDPROJK                        
085100*       CONTINUE                                                          
085200*    ELSE                                                                 
085300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDPROJK-ATTR                  
085400*    END-IF                                                               
085500                                                                          
085600*    IF NYPON-ART-IDAO        = 9KOMPL-ART-IDAO                           
085700*       CONTINUE                                                          
085800*    ELSE                                                                 
085900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDAO-ATTR                     
086000*    END-IF                                                               
086100                                                                          
086200*    IF NYPON-ART-IDRITUTG    = 9KOMPL-ART-IDRITUTG                       
086300*       CONTINUE                                                          
086400*    ELSE                                                                 
086500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDRITUTG-ATTR                 
086600*    END-IF                                                               
086700                                                                          
086800*    IF NYPON-ART-TISTABER    = 9KOMPL-ART-TISTABER                       
086900*       CONTINUE                                                          
087000*    ELSE                                                                 
087100*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISTABER-ATTR                 
087200*    END-IF                                                               
087300                                                                          
087400*    IF NYPON-ART-IDAVD       = 9KOMPL-ART-IDAVD                          
087500*       CONTINUE                                                          
087600*    ELSE                                                                 
087700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDAVD-ATTR                    
087800*    END-IF                                                               
087900                                                                          
088000*    IF NYPON-ART-KVARTAR1    = 9KOMPL-ART-KVARTAR1                       
088100*       CONTINUE                                                          
088200*    ELSE                                                                 
088300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KVARTAR1-ATTR                 
088400*    END-IF                                                               
088500                                                                          
088600*    IF NYPON-ART-TIRITB      = 9KOMPL-ART-TIRITB                         
088700*       CONTINUE                                                          
088800*    ELSE                                                                 
088900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIRITB-ATTR                   
089000*    END-IF                                                               
089100                                                                          
089200*    IF NYPON-ART-FLRITB      = 9KOMPL-ART-FLRITB                         
089300*       CONTINUE                                                          
089400*    ELSE                                                                 
089500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLRITB-ATTR                   
089600*    END-IF                                                               
089700                                                                          
089800*    IF NYPON-ART-TISLUBER    = 9KOMPL-ART-TISLUBER                       
089900*       CONTINUE                                                          
090000*    ELSE                                                                 
090100*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISLUBER-ATTR                 
090200*    END-IF                                                               
090300                                                                          
090400*    IF NYPON-ART-IDPROENH    = 9KOMPL-ART-IDPROENH                       
090500*       CONTINUE                                                          
090600*    ELSE                                                                 
090700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDPROENH-ATTR                 
090800*    END-IF                                                               
090900                                                                          
091000*    IF NYPON-ART-KVARTAR2    = 9KOMPL-ART-KVARTAR2                       
091100*       CONTINUE                                                          
091200*    ELSE                                                                 
091300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KVARTAR2-ATTR                 
091400*    END-IF                                                               
091500                                                                          
091600*    IF NYPON-ART-TIRITC      = 9KOMPL-ART-TIRITC                         
091700*       CONTINUE                                                          
091800*    ELSE                                                                 
091900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIRITC-ATTR                   
092000*    END-IF                                                               
092100                                                                          
092200*    IF NYPON-ART-FLRITC      = 9KOMPL-ART-FLRITC                         
092300*       CONTINUE                                                          
092400*    ELSE                                                                 
092500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLRITC-ATTR                   
092600*    END-IF                                                               
092700                                                                          
092800*    IF NYPON-ART-TIPLAKOP    = 9KOMPL-ART-TIPLAKOP                       
092900*       CONTINUE                                                          
093000*    ELSE                                                                 
093100*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIPLAKOP-ATTR                 
093200*    END-IF                                                               
093300                                                                          
093400*    IF NYPON-ART-PRARTBES    = 9KOMPL-ART-PRARTBES                       
093500*       CONTINUE                                                          
093600*    ELSE                                                                 
093700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-PRARTSTD-ATTR                 
093800*    END-IF                                                               
093900                                                                          
094000*    IF NYPON-ART-KDSTAINK    = 9KOMPL-ART-KDSTAINK                       
094100*       CONTINUE                                                          
094200*    ELSE                                                                 
094300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDSTAINK-ATTR                 
094400*    END-IF                                                               
094500                                                                          
094600*    IF NYPON-ART-KVARTAR3    = 9KOMPL-ART-KVARTAR3                       
094700*       CONTINUE                                                          
094800*    ELSE                                                                 
094900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KVARTAR3-ATTR                 
095000*    END-IF                                                               
095100                                                                          
095200*    IF NYPON-ART-TIRITP      = 9KOMPL-ART-TIRITP                         
095300*       CONTINUE                                                          
095400*    ELSE                                                                 
095500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIRITP-ATTR                   
095600*    END-IF                                                               
095700                                                                          
095800*    IF NYPON-ART-FLRITP      = 9KOMPL-ART-FLRITP                         
095900*       CONTINUE                                                          
096000*    ELSE                                                                 
096100*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLRITP-ATTR                   
096200*    END-IF                                                               
096300                                                                          
096400*    IF NYPON-ART-IDRITN      = 9KOMPL-ART-IDRITN                         
096500*       CONTINUE                                                          
096600*    ELSE                                                                 
096700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDRITN-ATTR                   
096800*    END-IF                                                               
096900                                                                          
097000*    IF NYPON-ART-IDFKNGRP    = 9KOMPL-ART-IDFKNGRP                       
097100*       CONTINUE                                                          
097200*    ELSE                                                                 
097300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDFKNGRP-ATTR                 
097400*    END-IF                                                               
097500                                                                          
097600*    IF NYPON-ART-IDINK       = 9KOMPL-ART-IDINK                          
097700*       CONTINUE                                                          
097800*    ELSE                                                                 
097900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDINK-ATTR                    
098000*    END-IF                                                               
098100                                                                          
098200*    IF NYPON-ART-KDSORT      = 9KOMPL-ART-KDSORT                         
098300*       CONTINUE                                                          
098400*    ELSE                                                                 
098500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDSORT-ATTR                   
098600*    END-IF                                                               
098700                                                                          
098800*    IF NYPON-ART-IDLEVNR     = 9KOMPL-ART-IDLEVNR                        
098900*       CONTINUE                                                          
099000*    ELSE                                                                 
099100*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-ATTR                  
099200*    END-IF                                                               
099300                                                                          
099400*    IF NYPON-ART-IDLEVNR-FORB (1) = 9KOMPL-ART-IDLEVNR-FORB (1)          
099500*       CONTINUE                                                          
099600*    ELSE                                                                 
099700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB1-ATTR            
099800*    END-IF                                                               
099900                                                                          
100000*    IF NYPON-ART-IDLEVNR-FORB (2) = 9KOMPL-ART-IDLEVNR-FORB (2)          
100100*       CONTINUE                                                          
100200*    ELSE                                                                 
100300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB2-ATTR            
100400*    END-IF                                                               
100500                                                                          
100600*    IF NYPON-ART-IDLEVNR-FORB (3) = 9KOMPL-ART-IDLEVNR-FORB (3)          
100700*       CONTINUE                                                          
100800*    ELSE                                                                 
100900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB3-ATTR            
101000*    END-IF                                                               
101100                                                                          
101200*    IF NYPON-ART-IDLEVNR-FORB (4) = 9KOMPL-ART-IDLEVNR-FORB (4)          
101300*       CONTINUE                                                          
101400*    ELSE                                                                 
101500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB4-ATTR            
101600*    END-IF                                                               
101700                                                                          
101800*    IF NYPON-ART-IDLEVNR-FORB (5) = 9KOMPL-ART-IDLEVNR-FORB (5)          
101900*       CONTINUE                                                          
102000*    ELSE                                                                 
102100*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB5-ATTR            
102200*    END-IF                                                               
102300                                                                          
102400*    IF NYPON-ART-TIUPG            = 9KOMPL-ART-TIUPG                     
102500*       CONTINUE                                                          
102600*    ELSE                                                                 
102700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIUPG-ATTR                    
102800*    END-IF                                                               
102900                                                                          
103000*    IF NYPON-ART-FLUPG            = 9KOMPL-ART-FLUPG                     
103100*       CONTINUE                                                          
103200*    ELSE                                                                 
103300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLUPG-ATTR                    
103400*    END-IF                                                               
103500                                                                          
103600*    IF NYPON-ART-TISERLEV     (1) = 9KOMPL-ART-TISERLEV     (1)          
103700*       CONTINUE                                                          
103800*    ELSE                                                                 
103900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV1-ATTR                
104000*    END-IF                                                               
104100                                                                          
104200*    IF NYPON-ART-TISERLEV     (2) = 9KOMPL-ART-TISERLEV     (2)          
104300*       CONTINUE                                                          
104400*    ELSE                                                                 
104500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV2-ATTR                
104600*    END-IF                                                               
104700                                                                          
104800*    IF NYPON-ART-TISERLEV     (3) = 9KOMPL-ART-TISERLEV     (3)          
104900*       CONTINUE                                                          
105000*    ELSE                                                                 
105100*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV3-ATTR                
105200*    END-IF                                                               
105300                                                                          
105400*    IF NYPON-ART-TISERLEV     (4) = 9KOMPL-ART-TISERLEV     (4)          
105500*       CONTINUE                                                          
105600*    ELSE                                                                 
105700*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV4-ATTR                
105800*    END-IF                                                               
105900                                                                          
106000*    IF NYPON-ART-TISERLEV     (5) = 9KOMPL-ART-TISERLEV     (5)          
106100*       CONTINUE                                                          
106200*    ELSE                                                                 
106300*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV5-ATTR                
106400*    END-IF                                                               
106500                                                                          
106600*    IF NYPON-ART-BEART-SVE        = 9KOMPL-ART-BEART-SVE                 
106700*       CONTINUE                                                          
106800*    ELSE                                                                 
106900*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-BEART-ATTR                    
107000*    END-IF                                                               
107100                                                                          
107200*    IF NYPON-ART-TETEKNIK         = 9KOMPL-ART-TETEKNIK                  
107300*       CONTINUE                                                          
107400*    ELSE                                                                 
107500*       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TETEKNIK-ATTR                 
107600*    END-IF.                                                              
107700                                                                          
107800     EJECT                                                                
107900     .                                                                    
108000 BB-LAS-ARTREG SECTION.                                                   
108100     SKIP2                                                                
108200     MOVE NYPON-ART-IDARTNR          TO W-IDARTNR                         
108300     PERFORM IMS-GU-ARTC01                                                
108400                                                                          
108500*    IF SEGMENT-FINNS  AND  ART-KDERS-UTG = ZERO                          
108600*                                                                         
108700*       PERFORM IMS-GNP-ARTC11                                            
108800*       MOVE CLAG-IDPROJUP           TO MOD-IDPROJUP                      
108900*                                                                         
109000*       PERFORM IMS-GNP-ARTC22                                            
109100*       IF SEGMENT-FINNS                                                  
109200*          MOVE BEST-TIBEST          TO DAT-I-TIDATUM                     
109300*          PERFORM S99-WDATKONV                                           
109400*          IF DAT-KDSVAR-OK                                               
109500*             MOVE DAT-TIAA          TO SPAR-AA                           
109600*             MOVE DAT-TIVV          TO SPAR-VV                           
109700*             MOVE SPAR-DATUM-R      TO MOD-TIBEST                        
109800*          ELSE                                                           
109900*             MOVE MFS-RENSA-FAELT   TO MOD-TIBEST                        
110000*          END-IF                                                         
110100*       ELSE                                                              
110200*          PERFORM IMS-GNP-ARTC23                                         
110300*          IF SEGMENT-FINNS                                               
110400*             MOVE AVT-TIAVTAL       TO DAT-I-TIDATUM                     
110500*             PERFORM S99-WDATKONV                                        
110600*             IF DAT-KDSVAR-OK                                            
110700*                MOVE DAT-TIAA          TO SPAR-AA                        
110800*                MOVE DAT-TIVV          TO SPAR-VV                        
110900*                MOVE SPAR-DATUM-R      TO MOD-TIBEST                     
111000*             ELSE                                                        
111100*                MOVE MFS-RENSA-FAELT   TO MOD-TIBEST                     
111200*             END-IF                                                      
111300*          ELSE                                                           
111400*             MOVE MFS-RENSA-FAELT      TO MOD-TIBEST                     
111500*          END-IF                                                         
111600*       END-IF                                                            
111700*    ELSE                                                                 
111800*       MOVE MFS-RENSA-FAELT         TO                                   
111900*                                       MOD-IDPROJUP                      
112000*                                       MOD-TIBEST                        
112100*    END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400 C-KOLLA-INPUT SECTION.                                                   
112500     SKIP2                                                                
112600     MOVE JA                         TO SW-INPUT-RAETT                    
112700                                                                          
112800     MOVE WS-TEST-KDPRODSL       TO TEST-KDPRODSL                         
112900     IF KDPRODSL-VOLVO-BIMA                                               
113000        IF CDC OR SDC                                                     
113100           CONTINUE                                                       
113200        ELSE                                                              
113300           MOVE NEJ TO SW-INPUT-RAETT                                     
113400           MOVE MED-7(INDX) TO MOD-TEMFSINF                               
113500        END-IF                                                            
113600     END-IF                                                               
113700                                                                          
113800     IF MID-KDRESBED = ALL '+'                                            
113900*       MOVE MFS-RENSA-FAELT   TO MOD-KDRESBED-IN                         
114000        CONTINUE                                                          
114100     ELSE                                                                 
114200        IF MID-KDRESBED = '-'  AND  NYPON-ART-KDRESBED = SPACE            
114300*          A V S L A G                                                    
114400           MOVE MFS-ALFA-FAELT-RAETT                                      
114500                               TO MOD-KDRESBED-IN-ATTR                    
114600        ELSE                                                              
114700           MOVE MFS-ALFA-FAELT-FEL                                        
114800                               TO MOD-KDRESBED-IN-ATTR                    
114900           MOVE NEJ            TO SW-INPUT-RAETT                          
115000        END-IF                                                            
115100*       MOVE MFS-ROER-EJ-FAELT TO MOD-KDRESBED-IN                         
115200     END-IF                                                               
115300                                                                          
115400*    IF MID-TINEDBRY = ALL '+'                                            
115500*       MOVE MFS-RENSA-FAELT   TO MOD-TINEDBRY-IN                         
115600*    ELSE                                                                 
115700*       MOVE 'AAVV'            TO DAT-KDDATFORM                           
115800*       MOVE MID-TINEDBRY      TO DAT-I-TIDATUM                           
115900*       PERFORM S99-WDATKONV                                              
116000*       IF DAT-KDSVAR-OK                                                  
116100*          F O R M E L L T  R Ä T T                                       
116200*          MOVE MID-TINEDBRY          TO TMP1-YYWW                        
116300*          MOVE SPAR-DAGENS-DATUM-R   TO TMP2-YYWW                        
116400*          PERFORM WY2000P3                                               
116500*          IF TMP1-YYWW   < TMP2-YYWW                                     
116600*             MOVE MFS-NUM-FAELT-FEL                                      
116700*                                 TO MOD-TINEDBRY-IN-ATTR                 
116800*             MOVE NEJ            TO SW-INPUT-RAETT                       
116900*          ELSE                                                           
117000*             MOVE DAT-TIAAMMDD   TO SPAR-TINEDBRY-AAMMDD                 
117100*             MOVE DAT-TIAA       TO SPAR-TINEDBRY-AA                     
117200*             MOVE DAT-TIVV       TO SPAR-TINEDBRY-VV                     
117300*             MOVE MFS-NUM-FAELT-RAETT                                    
117400*                                 TO MOD-TINEDBRY-IN-ATTR                 
117500*          END-IF                                                         
117600*       ELSE                                                              
117700*          MOVE MFS-NUM-FAELT-FEL TO MOD-TINEDBRY-IN-ATTR                 
117800*          MOVE NEJ               TO SW-INPUT-RAETT                       
117900*       END-IF                                                            
118000*       MOVE MFS-ROER-EJ-FAELT    TO MOD-TINEDBRY-IN                      
118100*    END-IF                                                               
118200                                                                          
118300     IF MID-AVSL-NOT = ALL '+'                                            
118400        CONTINUE                                                          
118500     ELSE                                                                 
118600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-AVSL-NOT-IN-UT-ATTR              
118700        MOVE MFS-ROER-EJ-FAELT    TO MOD-AVSL-NOT-IN-UT                   
118800     END-IF                                                               
118900                                                                          
119000*    IF MID-SLAECK-9KOMPL = ALL '+'                                       
119100*       MOVE MFS-RENSA-FAELT      TO MOD-SLAECK-9KOMPL-IN                 
119200*    ELSE                                                                 
119300*       IF MID-SLAECK-9KOMPL = JA                                         
119400*                                                                         
119500*          PERFORM S98-COMPUTE-9KOMPL                                     
119600*                                                                         
119700*          MOVE SPAR-9KOMPL-IDARTNR        TO W-IDARTNR                   
119800*                                                                         
119900*          PERFORM IMS-GHU-ARTG01-IO3-PCB2                                
120000*                                                                         
120100*          IF SEGMENT-FINNS                                               
120200*             MOVE MFS-ALFA-FAELT-RAETT                                   
120300*                                 TO MOD-SLAECK-9KOMPL-IN-ATTR            
120400*          ELSE                                                           
120500*             MOVE MFS-ALFA-FAELT-FEL                                     
120600*                                 TO MOD-SLAECK-9KOMPL-IN-ATTR            
120700*             MOVE NEJ            TO SW-INPUT-RAETT                       
120800*          END-IF                                                         
120900*       ELSE                                                              
121000*          MOVE MFS-ALFA-FAELT-FEL                                        
121100*                                 TO MOD-SLAECK-9KOMPL-IN-ATTR            
121200*          MOVE NEJ               TO SW-INPUT-RAETT                       
121300*       END-IF                                                            
121400*       MOVE MFS-ROER-EJ-FAELT    TO MOD-SLAECK-9KOMPL-IN                 
121500*    END-IF.                                                              
121600                                                                          
121700     EJECT                                                                
121800     .                                                                    
121900 D-UPPDATERA SECTION.                                                     
122000     SKIP2                                                                
122100*    IF MID-SLAECK-9KOMPL = ALL '+'                                       
122200*       CONTINUE                                                          
122300*    ELSE                                                                 
122400*       MOVE SPAR-9KOMPL-IDARTNR     TO W-IDARTNR                         
122500*       PERFORM IMS-GHU-ARTG01-IO3-PCB2                                   
122600*                                                                         
122700*       PERFORM IMS-DELETE-NYPON                                          
122800*                                                                         
122900*       MOVE NEJ                     TO NYPON-ART-FLAENDR                 
123000*                                                                         
123100*       MOVE SPAR-TINEDBRY-AAVV-R   TO TMP1-YYWW                          
123200*       MOVE SPAR-DAGENS-DATUM-R    TO TMP2-YYWW                          
123300*       PERFORM WY2000P3                                                  
123400*       IF TMP1-YYWW   > TMP2-YYWW                                        
123500*          EV. INMATAD TINEDBRY ÄR STÖRRE ÄN INNEVARANDE VECKA....        
123600*          CONTINUE                                                       
123700*       ELSE                                                              
123800*          IF NYPON-ART-TINEDBRY > ZERO                                   
123900*             MOVE 'AAMMDD'             TO DAT-KDDATFORM                  
124000*             MOVE NYPON-ART-TINEDBRY   TO DAT-I-TIDATUM                  
124100*             PERFORM S99-WDATKONV                                        
124200*                                                                         
124300*             IF DAT-KDSVAR-OK                                            
124400*                MOVE DAT-TIAA          TO SPAR-AA                        
124500*                MOVE DAT-TIVV          TO SPAR-VV                        
124600*             ELSE                                                        
124700*                MOVE ZERO              TO SPAR-DATUM-AAVV                
124800*             END-IF                                                      
124900*          ELSE                                                           
125000*             MOVE ZERO                 TO SPAR-DATUM-AAVV                
125100*          END-IF                                                         
125200*                                                                         
125300*          MOVE SPAR-DATUM-R          TO TMP1-YYWW                        
125400*          MOVE SPAR-DAGENS-DATUM-R   TO TMP2-YYWW                        
125500*          PERFORM WY2000P3                                               
125600*          IF TMP1-YYWW > TMP2-YYWW                                       
125700*             NYPON-TINEDBRY ÄR STÖRRE ÄN INNEVARANDE VECKA....           
125800*             CONTINUE                                                    
125900*          ELSE                                                           
126000*             IF MID-KDRESBED = '-'  OR                                   
126100*                NYPON-ART-KDRESBED = 'R' OR 'E' OR '-'                   
126200*                MOVE NEJ         TO NYPON-ART-FLBERQ                     
126300*                MOVE ZERO        TO NYPON-ART-TINEDBRY                   
126400*             ELSE                                                        
126500*                IF NYPON-ART-KDRESBED = 'U'  AND                         
126600*                   NYPON-ART-FLAENDR  = JA                               
126700*                   MOVE NEJ         TO NYPON-ART-FLBERQ                  
126800*                   MOVE ZERO        TO NYPON-ART-TINEDBRY                
126900*                END-IF                                                   
127000*             END-IF                                                      
127100*          END-IF                                                         
127200*       END-IF                                                            
127300*    END-IF                                                               
127400                                                                          
127500     IF MID-KDRESBED      = ALL '+'  AND                                  
127600*       MID-TINEDBRY      = ALL '+'  AND                                  
127700        MID-AVSL-NOT      = ALL '+'                                       
127800        CONTINUE                                                          
127900     ELSE                                                                 
128000        IF MID-KDRESBED = ALL '+'                                         
128100           CONTINUE                                                       
128200        ELSE                                                              
128300           MOVE MID-KDRESBED            TO NYPON-ART-KDRESBED             
128400                                                                          
128500*          IF MID-TINEDBRY       = ALL '+'  AND                           
128600           IF NYPON-ART-TINEDBRY = ZERO                                   
128700              MOVE NEJ                  TO NYPON-ART-FLBERQ               
128800           END-IF                                                         
128900                                                                          
129000           MOVE NYPON-ART-KDPRODSL                                        
129100                                 TO TEST-KDPRODSL                         
129200           IF KDPRODSL-VCBV                                               
129300              CONTINUE                                                    
129400           ELSE                                                           
129500              PERFORM DB-KDP-KOLA                                         
129600           END-IF                                                         
129700        END-IF                                                            
129800                                                                          
129900*       IF MID-TINEDBRY = ALL '+'                                         
130000*          CONTINUE                                                       
130100*       ELSE                                                              
130200*          MOVE SPAR-TINEDBRY-AAMMDD    TO NYPON-ART-TINEDBRY             
130300*          MOVE JA                      TO NYPON-ART-FLBERQ               
130400*                                                                         
130500*          IF NYPON-ART-IDBERED = ZERO                                    
130600*             PERFORM IMS-GU-ARTC11                                       
130700*             IF SEGMENT-FINNS                                            
130800*                IF CLAG-IDBERED = ZERO                                   
130900*                   PERFORM DC-HAMTA-IDBERED-XXAS                         
131000*                ELSE                                                     
131100*                   MOVE CLAG-IDBERED TO NYPON-ART-IDBERED                
131200*                END-IF                                                   
131300*             ELSE                                                        
131400*                PERFORM DC-HAMTA-IDBERED-XXAS                            
131500*             END-IF                                                      
131600*          END-IF                                                         
131700*       END-IF                                                            
131800                                                                          
131900        IF MID-AVSL-NOT = ALL '+'                                         
132000           CONTINUE                                                       
132100        ELSE                                                              
132200           MOVE MID-AVSL-NOT            TO NYPON-ART-TEARTNOT             
132300        END-IF                                                            
132400                                                                          
132500     END-IF                                                               
132600                                                                          
132700     PERFORM IMS-REPL-NYPON                                               
132800                                                                          
132900     MOVE MED-5(INDX)                   TO MOD-TEMFSINF                   
133000*    PERFORM S02-RENSA-MOD-INMATNINGSFAELT                                
133100     PERFORM S03-FORMATETS-ATTRIBUT.                                      
133200                                                                          
133300     EJECT                                                                
133400 DB-KDP-KOLA SECTION.                                                     
133500     SKIP2                                                                
133600     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
133700     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
133800******************************************************************        
133900*    IDLOGLOP = 2, FÖR ATT SKILJA TRANSAR FRÅN 1113,9409,1117,1142        
134000******************************************************************        
134100     MOVE 2                               TO SPAR-IDLOGLOP                
134200     MOVE SPAR-IDLOGLOP                   TO ZZAC-IDLOGLOP                
134300                                                                          
134400     MOVE 'RZU'                           TO KDP-IDPTYP                   
134500     MOVE '-'                             TO KDP-KDUART                   
134600     MOVE NYPON-ART-IDARTNR               TO KDP-IDARTNR                  
134700                                             W092-SORTBGP                 
134800                                                                          
134900     MOVE KDP-W10111                      TO ZZAC-LOGGPOST                
135000     MOVE W092-AREA                       TO ZZAC-SORTPOST                
135100     PERFORM IMS-ISRT-ZZAC.                                               
135200     EJECT                                                                
135300 DC-HAMTA-IDBERED-XXAS SECTION.                                           
135400     SKIP2                                                                
135500     MOVE NYPON-ART-KDPRODSL              TO W-KDPRODSL                   
135600     MOVE NYPON-ART-IDFKNGRP              TO W-IDFKNGRP                   
135700                                             W-IDFKNGRP-SBGP              
135800                                                                          
135900     PERFORM IMS-GU-WLXXAS01                                              
136000     PERFORM IMS-GNP-WLXXAS11                                             
136100                                                                          
136200     IF SEGMENT-FINNS                                                     
136300        MOVE XXAS-1136-IDBERED            TO NYPON-ART-IDBERED            
136400     END-IF.                                                              
136500     EJECT                                                                
136600*S02-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
136700*    SKIP3                                                                
136800*    MOVE MFS-RENSA-FAELT             TO                                  
136900*                                        MOD-KDRESBED-IN                  
137000*                                        MOD-TINEDBRY-IN                  
137100*                                        MOD-SLAECK-9KOMPL-IN.            
137200*    EJECT                                                                
137300*    .                                                                    
137400 S03-FORMATETS-ATTRIBUT SECTION.                                          
137500     SKIP3                                                                
137600     MOVE MFS-FORMATETS-ATTR         TO MOD-KDRESBED-IN-ATTR              
137700*                                       MOD-TINEDBRY-IN-ATTR              
137800                                        MOD-AVSL-NOT-IN-UT-ATTR           
137900*                                       MOD-SLAECK-9KOMPL-IN-ATTR.        
138000     EJECT                                                                
138100     .                                                                    
138200 S98-COMPUTE-9KOMPL SECTION.                                              
138300     SKIP3                                                                
138400     MOVE WS-IDARTNR      TO W-IDARTNR                                    
138500                                                                          
138600     COMPUTE SPAR-9KOMPL-IDARTNR = 999999999 - W-IDARTNR.                 
138700     EJECT                                                                
138800 S99-WDATKONV SECTION.                                                    
138900     SKIP3                                                                
139000     CALL WDATKONV USING DAT-KDDATFORM                                    
139100                         DAT-I-TIDATUM                                    
139200                         DAT-O-TIDATUM                                    
139300                         DAT-KDSVAR.                                      
139400     EJECT                                                                
139500* IMS SEKTIONER                                                           
139600     SKIP3                                                                
139700 IMS-GET-MSG SECTION.                                                     
139800     SKIP2                                                                
139900     MOVE '  QC' TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
140100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
140200     PERFORM IMS-STATUS-KONTROLL.                                         
140300     SKIP3                                                                
140400 IMS-INSERT-MSG SECTION.                                                  
140500     SKIP2                                                                
140600*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
140700*       MOVE '0' TO MFS-KDHUVOMR                                          
140800*    END-IF                                                               
140900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
141000     MOVE SPACE TO GODK-STATUSKODER                                       
141100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
141200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
141300     PERFORM IMS-STATUS-KONTROLL.                                         
141400     EJECT                                                                
141500 IMS-GU-ARTC01 SECTION.                                                   
141600     SKIP2                                                                
141700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
141800             DELIMITED BY SIZE INTO SSA1                                  
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
142100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM IMS-STATUS-KONTROLL.                                         
142300     SKIP3                                                                
142400 IMS-GNP-ARTC11 SECTION.                                                  
142500     SKIP2                                                                
142600     MOVE 'WLARTC11(KDSEGKEY =1)'    TO SSA1                              
142700     MOVE '  ' TO GODK-STATUSKODER                                        
142800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1                    
142900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
143000     PERFORM IMS-STATUS-KONTROLL.                                         
143100     SKIP3                                                                
143200 IMS-GU-ARTC11   SECTION.                                                 
143300     SKIP2                                                                
143400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
143500             DELIMITED BY SIZE INTO SSA1                                  
143600     MOVE 'WLARTC11 ' TO SSA2                                             
143700     MOVE '  GE' TO GODK-STATUSKODER                                      
143800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1 SSA2                
143900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
144000     PERFORM IMS-STATUS-KONTROLL.                                         
144100     EJECT                                                                
144200 IMS-GNP-ARTC22 SECTION.                                                  
144300     SKIP2                                                                
144400     MOVE 'WLARTC11 ' TO SSA1                                             
144500     MOVE 'WLARTC22 ' TO SSA2                                             
144600     MOVE '  GE' TO GODK-STATUSKODER                                      
144700     CALL CBLTDLI USING GNP    ARTC-PCB DLI-IO-AREA1 SSA1 SSA2            
144800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
144900     PERFORM IMS-STATUS-KONTROLL.                                         
145000     SKIP3                                                                
145100 IMS-GNP-ARTC23 SECTION.                                                  
145200     SKIP2                                                                
145300     MOVE 'WLARTC11 ' TO SSA1                                             
145400     MOVE 'WLARTC23 ' TO SSA2                                             
145500     MOVE '  GE' TO GODK-STATUSKODER                                      
145600     CALL CBLTDLI USING GNP    ARTC-PCB DLI-IO-AREA1 SSA1 SSA2            
145700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
145800     PERFORM IMS-STATUS-KONTROLL.                                         
145900     EJECT                                                                
146000 IMS-GHU-ARTG01-IO2-PCB1    SECTION.                                      
146100     SKIP2                                                                
146200     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
146300             DELIMITED BY SIZE INTO SSA1                                  
146400     MOVE '  GE' TO GODK-STATUSKODER                                      
146500     CALL CBLTDLI USING GHU ARTG1-PCB DLI-IO-AREA2 SSA1                   
146600     MOVE ARTG1-STATUS-CODE TO STATUS-WS                                  
146700     PERFORM IMS-STATUS-KONTROLL.                                         
146800     SKIP3                                                                
146900 IMS-GHU-ARTG01-IO3-PCB2    SECTION.                                      
147000     SKIP2                                                                
147100*    F Ö R  J Ä M F Ö R E L S E  A V  9 -  K O M P L E M E N T            
147200     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
147300             DELIMITED BY SIZE INTO SSA1                                  
147400     MOVE '  GE' TO GODK-STATUSKODER                                      
147500     CALL CBLTDLI USING GHU ARTG2-PCB DLI-IO-AREA3 SSA1                   
147600     MOVE ARTG2-STATUS-CODE TO STATUS-WS                                  
147700     PERFORM IMS-STATUS-KONTROLL.                                         
147800     EJECT                                                                
147900 IMS-GU-WLXXAS01 SECTION.                                                 
148000     SKIP2                                                                
148100     STRING 'WLXXAS01(WDGXKEY  =' W-1135KEY-X ')'                         
148200             DELIMITED BY SIZE INTO SSA1                                  
148300     MOVE '  GE' TO GODK-STATUSKODER                                      
148400     CALL CBLTDLI USING GU XXAS-PCB DLI-IO-AREA1 SSA1                     
148500     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
148600     PERFORM IMS-STATUS-KONTROLL.                                         
148700     SKIP3                                                                
148800 IMS-GNP-WLXXAS11 SECTION.                                                
148900     SKIP2                                                                
149000     STRING 'WLXXAS11(WDGXKEY <=' W-1136KEY-X                             
149100                    '&IDFKNGRP>=' W-IDFKNGRP-X ')'                        
149200             DELIMITED BY SIZE INTO SSA1                                  
149300     MOVE '  GE' TO GODK-STATUSKODER                                      
149400     CALL CBLTDLI USING GNP XXAS-PCB DLI-IO-AREA1 SSA1                    
149500     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
149600     PERFORM IMS-STATUS-KONTROLL.                                         
149700     EJECT                                                                
149800 IMS-ISRT-ZZAC SECTION.                                                   
149900     SKIP2                                                                
150000     MOVE 'WLZZAC01 '     TO SSA1                                         
150100     MOVE '  '   TO GODK-STATUSKODER                                      
150200     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA1 SSA1                   
150300     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
150400     PERFORM IMS-STATUS-KONTROLL.                                         
150500     EJECT                                                                
150600 IMS-DELETE-NYPON SECTION.                                                
150700*    F Ö R  D E L E T E  A V  9 - K O M P L E M E N T E T                 
150800     SKIP2                                                                
150900     MOVE '  '   TO GODK-STATUSKODER                                      
151000     CALL CBLTDLI USING DLET ARTG2-PCB DLI-IO-AREA3                       
151100     MOVE ARTG2-STATUS-CODE TO STATUS-WS                                  
151200     PERFORM IMS-STATUS-KONTROLL.                                         
151300     SKIP3                                                                
151400 IMS-REPL-NYPON SECTION.                                                  
151500*    F Ö R  R E P L A C E  A V  D E T  "RIKTIGA ARTIKELNUMRET"            
151600     SKIP2                                                                
151700     MOVE '  '   TO GODK-STATUSKODER                                      
151800     CALL CBLTDLI USING REPL ARTG1-PCB DLI-IO-AREA2                       
151900     MOVE ARTG1-STATUS-CODE TO STATUS-WS                                  
152000     PERFORM IMS-STATUS-KONTROLL.                                         
152100     SKIP3                                                                
152200 IMS-STATUS-KONTROLL SECTION.                                             
152300     SET STATUS-IX TO 1                                                   
152400     SEARCH GODK-STATUS                                                   
152500       AT END                                                             
152600         CALL FELLOG                                                      
152700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
152800     END-SEARCH.                                                          
152900     EJECT                                                                
153000     EJECT                                                                
153100*    -COPY WY2000P3                                                       
