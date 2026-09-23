000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1011500.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   DECEMBER-88.                                             
000500*    FUNKTION.                                                            
000600****************************************************************          
000700*                                                              *          
000800*                     NYPON-BILD 1115                          *          
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
002200*        TRANSAKTION: W1T115                                              
002300*                     W1T115U                                             
002400*        MID:         W1I11501                                            
002500*    UTDATA.                                                              
002600*        MOD:         W1O11501                                            
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
004400 77  PROGRAM-NAMN                PIC X(08)  VALUE 'W1011500'.             
004500 77  JA                          PIC X(01)  VALUE 'J'.                    
004600 77  NEJ                         PIC X(01)  VALUE 'N'.                    
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +557 COMP SYNC.         
004800 77  INDX                        PIC S9(3)  VALUE ZERO COMP-3.            
004900 77  WS-IDARTNR                  PIC X(09).                               
005000 77  WS-TEST-KDPRODSL            PIC 9(2)   VALUE ZERO.                   
005100                                                                          
005200 01  WS-IDTRANS                  PIC X(04).                               
005300     88  EGEN-BILD                          VALUE '1115'.                 
005400                                                                          
005500*01  -COPY WWPRODSL                                                       
005600     EJECT                                                                
005700*                                                                         
005800******************************************************************        
005900*                     S W I T C H A R                            *        
006000******************************************************************        
006100*                                                                         
006200 01  SWITCHAR.                                                            
006300     05  SW-INPUT-RAETT          PIC X(01)  VALUE 'J'.                    
006400     05  SW-TRAFF                PIC X(01)  VALUE 'N'.                    
006500                                                                          
006600*                                                                         
006700******************************************************************        
006800*               D I V E R S E  S P A R F Ä L T                   *        
006900******************************************************************        
007000*                                                                         
007100 01  SPAR-DIVERSE.                                                        
007200     05  SPAR-DAGENS-DATUM           PIC 9(06)  VALUE ZERO.               
007300     05  SPAR-DAGENS-DATUM-AAVV.                                          
007400         10  SPAR-DAGENS-AA          PIC 9(02)  VALUE ZERO.               
007500         10  SPAR-DAGENS-VV          PIC 9(02)  VALUE ZERO.               
007600     05  SPAR-DAGENS-DATUM-R  REDEFINES                                   
007700              SPAR-DAGENS-DATUM-AAVV PIC 9(04).                           
007800                                                                          
007900     05  SPAR-DATUM-AAVV.                                                 
008000         10  SPAR-AA                 PIC 9(02)  VALUE ZERO.               
008100         10  SPAR-VV                 PIC 9(02)  VALUE ZERO.               
008200     05  SPAR-DATUM-R  REDEFINES                                          
008300              SPAR-DATUM-AAVV        PIC 9(04).                           
008400                                                                          
008500     05  SPAR-9KOMPL-IDARTNR         PIC 9(09)  VALUE ZERO.               
008600                                                                          
008700     05  SPAR-TINEDBRY-AAMMDD        PIC 9(06)  VALUE ZERO.               
008800     05  SPAR-TINEDBRY-AAVV.                                              
008900         10  SPAR-TINEDBRY-AA        PIC 9(02)  VALUE ZERO.               
009000         10  SPAR-TINEDBRY-VV        PIC 9(02)  VALUE ZERO.               
009100     05  SPAR-TINEDBRY-AAVV-R  REDEFINES  SPAR-TINEDBRY-AAVV              
009200                                     PIC 9(04).                           
009300                                                                          
009400     05  SPAR-IDLOGLOP              PIC S9(1) COMP-3 VALUE ZERO.          
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
024700*01  MID -COPY W1I11501                                                   
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
026400*    03  MOD -COPY W1O11501  -RED MSG-AREA.                               
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
031800     03  IO-AREA1                  PIC X(928) VALUE SPACE.                
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
037200     EJECT                                                                
037300*                                                                         
037400******************************************************************        
037500*            L I N K A G E  S E C T I O N                        *        
037600******************************************************************        
037700*                                                                         
037800 LINKAGE SECTION.                                                         
037900     SKIP2                                                                
038000*01  -COPY W0009     -PRE MSG-                                            
038100     EJECT                                                                
038200*01  -COPY W0008     -PRE USEA-                                           
038300         05  FILLER              PIC X.                                   
038400     EJECT                                                                
038500*01  -COPY W0008     -PRE ARTG1-                                          
038600         05  FILLER              PIC X.                                   
038700     EJECT                                                                
038800*01  -COPY W0008     -PRE ARTG2-                                          
038900         05  FILLER              PIC X.                                   
039000     EJECT                                                                
039100*01  -COPY W0008     -PRE ARTC-                                           
039200         05  FILLER              PIC X.                                   
039300     EJECT                                                                
039400*01  -COPY W0008     -PRE XXAS-                                           
039500         05  FILLER              PIC X.                                   
039600     EJECT                                                                
039700     EJECT                                                                
039800*01  -COPY W0008     -PRE ZZAC-                                           
039900         05  FILLER              PIC X.                                   
040000     EJECT                                                                
040100 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB                               
040200                                   ARTG1-PCB ARTG2-PCB ARTC-PCB           
040300                                   XXAS-PCB                               
040400                                   ZZAC-PCB.                              
040500     SKIP1                                                                
040600     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
040700                                    ARTG1-PCB ARTG2-PCB ARTC-PCB          
040800                                    XXAS-PCB                              
040900                                    ZZAC-PCB.                             
041000                                                                          
041100     PERFORM IMS-GET-MSG                                                  
041200     IF SEGMENT-FINNS                                                     
041300        PERFORM A-INIT-SPARA-INPUT                                        
041400        IF WS-IDARTNR NUMERIC                                             
041500           MOVE WS-IDARTNR  TO W-IDARTNR                                  
041600           PERFORM IMS-GHU-ARTG01-IO2-PCB1                                
041700           IF SEGMENT-FINNS                                               
041800              MOVE NYPON-ART-KDPRODSL TO WS-TEST-KDPRODSL                 
041900              IF MFS-UPDATE                                               
042000                 PERFORM C-KOLLA-INPUT                                    
042100                 IF SW-INPUT-RAETT = JA                                   
042200                    PERFORM D-UPPDATERA                                   
042300                 ELSE                                                     
042400                    MOVE FEL-2(INDX) TO MOD-TEMFSFEL                      
042500                 END-IF                                                   
042600              END-IF                                                      
042700              PERFORM B-VISA-BILD                                         
042800           ELSE                                                           
042900              MOVE FEL-3(INDX)TO MOD-TEMFSFEL                             
043000           END-IF                                                         
043100        ELSE                                                              
043200           MOVE FEL-1(INDX) TO MOD-TEMFSFEL                               
043300        END-IF                                                            
043400                                                                          
043500        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
043600        PERFORM IMS-INSERT-MSG                                            
043700     END-IF                                                               
043800                                                                          
043900     MOVE ZERO TO RETURN-CODE                                             
044000     GOBACK.                                                              
044100     EJECT                                                                
044200 A-INIT-SPARA-INPUT SECTION.                                              
044300     SKIP2                                                                
044400     IF MSG-DUBBLA-TRANSKODER                                             
044500         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
044600                                   TO MID-W1I11501                        
044700         MOVE MSG-IDTRANS-2        TO MFS-IDTRANS WS-IDTRANS              
044800         MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                        
044900         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
045000         MOVE MSG-IDPFK            TO MFS-IDPFK                           
045100     ELSE                                                                 
045200         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
045300                                   TO MID-W1I11501                        
045400         MOVE MSG-IDTRANS-1        TO MFS-IDTRANS WS-IDTRANS              
045500         MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                        
045600         MOVE SPACE                TO MFS-KDTRTYP                         
045700                                      MFS-IDPFK                           
045800     END-IF                                                               
045900                                                                          
046000     IF EGEN-BILD                                                         
046100        CONTINUE                                                          
046200     ELSE                                                                 
046300        MOVE SPACE                 TO MFS-KDTRTYP                         
046400                                      MFS-IDPFK                           
046500     END-IF                                                               
046600                                                                          
046700     MOVE LOW-VALUE                TO MOD-W1O11501-CTX                    
046800     MOVE 'W1O115N1'               TO MFS-IDMOD                           
046900     MOVE '1115'                   TO MOD-IDTRANS                         
047000                                                                          
047100     MOVE MFS-RENSA-FAELT          TO MOD-TEMFSFEL                        
047200                                      MOD-TEMFSINF                        
047300                                      MOD-IDARTNR-IN                      
047400                                                                          
047500     ACCEPT SPAR-DAGENS-DATUM FROM DATE                                   
047600                                                                          
047700     MOVE 'AAMMDD'                 TO DAT-KDDATFORM                       
047800     MOVE SPAR-DAGENS-DATUM        TO DAT-I-TIDATUM                       
047900     PERFORM S99-WDATKONV                                                 
048000                                                                          
048100     IF DAT-KDSVAR-OK                                                     
048200        MOVE DAT-TIAA              TO SPAR-DAGENS-AA                      
048300        MOVE DAT-TIVV              TO SPAR-DAGENS-VV                      
048400     END-IF                                                               
048500                                                                          
048600     IF MFS-UPDATE                    AND                                 
048700        MID-KDRESBED      = ALL '+'   AND                                 
048800        MID-TINEDBRY      = ALL '+'   AND                                 
048900        MID-AVSL-NOT      = ALL '+'   AND                                 
049000        MID-SLAECK-9KOMPL = ALL '+'                                       
049100        MOVE SPACE                 TO MFS-KDTRTYP                         
049200                                      MFS-IDPFK                           
049300     END-IF                                                               
049400                                                                          
049500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
049600     MOVE '001'             TO MSGI-KDCALL                                
049700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
049800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
049900     MOVE '1115'            TO MSGI-IDTRANS                               
050000     IF MFS-IDTRANS = '1115'                                              
050100     OR (MID-IDARTNR-IN NUMERIC                                           
050200     AND MID-IDARTNR-IN > ZERO)                                           
050300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
050400     END-IF                                                               
050500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
050600     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
050700     MOVE MSGI-IDDC    TO WS-IDDC                                         
050800     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
050900                                                                          
051000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
051100       MOVE +1 TO INDX                                                    
051200     ELSE                                                                 
051300       MOVE +2 TO INDX                                                    
051400     END-IF                                                               
051500                                                                          
051600     IF MID-IDARTNR-IN = ALL '+'                                          
051700        CONTINUE                                                          
051800     ELSE                                                                 
051900        MOVE SPACE                 TO MFS-KDTRTYP                         
052000                                      MFS-IDPFK                           
052100     END-IF                                                               
052200                                                                          
052300     MOVE WS-IDARTNR        TO MOD-IDARTNR-UT                             
052400     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE.              
052500                                                                          
052600     EJECT                                                                
052700 B-VISA-BILD SECTION.                                                     
052800     SKIP2                                                                
052900     MOVE NYPON-ART-IDPROJ           TO MOD-IDPROJ                        
053000     MOVE NYPON-ART-IDPROJK          TO MOD-IDPROJK                       
053100     MOVE NYPON-ART-IDAO             TO MOD-IDAO                          
053200     MOVE NYPON-ART-IDRITUTG         TO MOD-IDRITUTG                      
053300     MOVE NYPON-ART-IDAVD            TO MOD-IDAVD                         
053400     MOVE NYPON-ART-KVARTAR1         TO MOD-KVARTAR1                      
053500     MOVE NYPON-ART-FLRITB           TO MOD-FLRITB                        
053600     MOVE NYPON-ART-IDPROENH         TO MOD-IDPROENH                      
053700     MOVE NYPON-ART-KVARTAR2         TO MOD-KVARTAR2                      
053800     MOVE NYPON-ART-FLRITC           TO MOD-FLRITC                        
053900     MOVE NYPON-ART-PRARTBES         TO MOD-PRARTSTD                      
054000     MOVE NYPON-ART-KVARTAR3         TO MOD-KVARTAR3                      
054100     MOVE NYPON-ART-FLRITP           TO MOD-FLRITP                        
054200     MOVE NYPON-ART-IDRITN           TO MOD-IDRITN                        
054300     MOVE NYPON-ART-IDFKNGRP         TO MOD-IDFKNGRP                      
054400     MOVE NYPON-ART-IDINK            TO MOD-IDINK                         
054500     MOVE NYPON-ART-KDSORT           TO MOD-KDSORT                        
054600     MOVE NYPON-ART-IDLEVNR          TO MOD-IDLEVNR                       
054700     MOVE NYPON-ART-IDLEVNR-FORB (1) TO MOD-IDLEVNR-FORB1                 
054800     MOVE NYPON-ART-IDLEVNR-FORB (2) TO MOD-IDLEVNR-FORB2                 
054900     MOVE NYPON-ART-IDLEVNR-FORB (3) TO MOD-IDLEVNR-FORB3                 
055000     MOVE NYPON-ART-IDLEVNR-FORB (4) TO MOD-IDLEVNR-FORB4                 
055100     MOVE NYPON-ART-IDLEVNR-FORB (5) TO MOD-IDLEVNR-FORB5                 
055200                                                                          
055300     IF NYPON-ART-FLUPG = JA OR 'A'                                       
055400        MOVE 'G'                     TO MOD-FLUPG                         
055500     ELSE                                                                 
055600        IF NYPON-ART-FLUPG = 'R'                                          
055700           MOVE 'U'                  TO MOD-FLUPG                         
055800        ELSE                                                              
055900           IF NYPON-ART-FLUPG = 'N'                                       
056000              IF NYPON-ART-TIUPG = ZERO                                   
056100                 CONTINUE                                                 
056200              ELSE                                                        
056300                 MOVE 'U'            TO MOD-FLUPG                         
056400              END-IF                                                      
056500           ELSE                                                           
056600              MOVE SPACE                TO MOD-FLUPG                      
056700           END-IF                                                         
056800        END-IF                                                            
056900     END-IF                                                               
057000                                                                          
057100*    MOVE NYPON-ART-KVNK             TO MOD-KVNK                          
057200     MOVE NYPON-ART-KVLEVBEG         TO MOD-KVLEVBEG                      
057300     MOVE NYPON-ART-KVPROG           TO MOD-KVPROG                        
057400     MOVE NYPON-ART-BEART-SVE        TO MOD-BEART                         
057500     MOVE NYPON-ART-TETEKNIK         TO MOD-TETEKNIK                      
057600     MOVE NYPON-ART-KDRESBED         TO MOD-KDRESBED-UT                   
057700                                                                          
057800     MOVE 'AAMMDD'                   TO DAT-KDDATFORM                     
057900     MOVE NYPON-ART-TISTABER         TO DAT-I-TIDATUM                     
058000     PERFORM S99-WDATKONV                                                 
058100                                                                          
058200     IF DAT-KDSVAR-OK                                                     
058300        MOVE DAT-TIAA                TO SPAR-AA                           
058400        MOVE DAT-TIVV                TO SPAR-VV                           
058500        MOVE SPAR-DATUM-R            TO MOD-TISTABER                      
058600     ELSE                                                                 
058700        MOVE ZERO                    TO MOD-TISTABER                      
058800        INSPECT MOD-TISTABER REPLACING LEADING ZERO BY SPACE              
058900     END-IF                                                               
059000                                                                          
059100     MOVE NYPON-ART-TIRITB           TO DAT-I-TIDATUM                     
059200     PERFORM S99-WDATKONV                                                 
059300                                                                          
059400     IF DAT-KDSVAR-OK                                                     
059500        MOVE DAT-TIAA                TO SPAR-AA                           
059600        MOVE DAT-TIVV                TO SPAR-VV                           
059700        MOVE SPAR-DATUM-R            TO MOD-TIRITB                        
059800     ELSE                                                                 
059900        MOVE ZERO                    TO MOD-TIRITB                        
060000        INSPECT MOD-TIRITB REPLACING LEADING ZERO BY SPACE                
060100     END-IF                                                               
060200                                                                          
060300     MOVE NYPON-ART-TISLUBER         TO DAT-I-TIDATUM                     
060400     PERFORM S99-WDATKONV                                                 
060500                                                                          
060600     IF DAT-KDSVAR-OK                                                     
060700        MOVE DAT-TIAA                TO SPAR-AA                           
060800        MOVE DAT-TIVV                TO SPAR-VV                           
060900        MOVE SPAR-DATUM-R            TO MOD-TISLUBER                      
061000     ELSE                                                                 
061100        MOVE ZERO                    TO MOD-TISLUBER                      
061200        INSPECT MOD-TISLUBER REPLACING LEADING ZERO BY SPACE              
061300     END-IF                                                               
061400                                                                          
061500     MOVE NYPON-ART-TIRITC           TO DAT-I-TIDATUM                     
061600     PERFORM S99-WDATKONV                                                 
061700                                                                          
061800     IF DAT-KDSVAR-OK                                                     
061900        MOVE DAT-TIAA                TO SPAR-AA                           
062000        MOVE DAT-TIVV                TO SPAR-VV                           
062100        MOVE SPAR-DATUM-R            TO MOD-TIRITC                        
062200     ELSE                                                                 
062300        MOVE ZERO                    TO MOD-TIRITC                        
062400        INSPECT MOD-TIRITC REPLACING LEADING ZERO BY SPACE                
062500     END-IF                                                               
062600                                                                          
062700     MOVE NYPON-ART-TIPLAKOP         TO DAT-I-TIDATUM                     
062800     PERFORM S99-WDATKONV                                                 
062900                                                                          
063000     IF DAT-KDSVAR-OK                                                     
063100        MOVE DAT-TIAA                TO SPAR-AA                           
063200        MOVE DAT-TIVV                TO SPAR-VV                           
063300        MOVE SPAR-DATUM-R            TO MOD-TIPLAKOP                      
063400     ELSE                                                                 
063500        MOVE ZERO                    TO MOD-TIPLAKOP                      
063600        INSPECT MOD-TIPLAKOP REPLACING LEADING ZERO BY SPACE              
063700     END-IF                                                               
063800                                                                          
063900     MOVE NYPON-ART-TIRITP           TO DAT-I-TIDATUM                     
064000     PERFORM S99-WDATKONV                                                 
064100                                                                          
064200     IF DAT-KDSVAR-OK                                                     
064300        MOVE DAT-TIAA                TO SPAR-AA                           
064400        MOVE DAT-TIVV                TO SPAR-VV                           
064500        MOVE SPAR-DATUM-R            TO MOD-TIRITP                        
064600     ELSE                                                                 
064700        MOVE ZERO                    TO MOD-TIRITP                        
064800        INSPECT MOD-TIRITP REPLACING LEADING ZERO BY SPACE                
064900     END-IF                                                               
065000                                                                          
065100     MOVE NYPON-ART-TIANSKREG        TO DAT-I-TIDATUM                     
065200     PERFORM S99-WDATKONV                                                 
065300                                                                          
065400     IF DAT-KDSVAR-OK                                                     
065500        MOVE DAT-TIAA                TO SPAR-AA                           
065600        MOVE DAT-TIVV                TO SPAR-VV                           
065700        MOVE SPAR-DATUM-R            TO MOD-TIANSKREG                     
065800     ELSE                                                                 
065900        MOVE ZERO                    TO MOD-TIANSKREG                     
066000        INSPECT MOD-TIANSKREG REPLACING LEADING ZERO BY SPACE             
066100     END-IF                                                               
066200                                                                          
066300     MOVE NYPON-ART-TIUPG            TO DAT-I-TIDATUM                     
066400     PERFORM S99-WDATKONV                                                 
066500                                                                          
066600     IF DAT-KDSVAR-OK                                                     
066700        IF NYPON-ART-FLUPG =  JA OR 'R' OR 'A' OR NEJ                     
066800          MOVE DAT-TIAA                TO SPAR-AA                         
066900          MOVE DAT-TIVV                TO SPAR-VV                         
067000          MOVE SPAR-DATUM-R            TO MOD-TIUPG                       
067100        END-IF                                                            
067200     ELSE                                                                 
067300        MOVE ZERO                    TO MOD-TIUPG                         
067400        INSPECT MOD-TIUPG REPLACING LEADING ZERO BY SPACE                 
067500     END-IF                                                               
067600                                                                          
067700     MOVE NYPON-ART-TISERLEV     (1) TO DAT-I-TIDATUM                     
067800     PERFORM S99-WDATKONV                                                 
067900                                                                          
068000     IF DAT-KDSVAR-OK                                                     
068100        MOVE DAT-TIAA                TO SPAR-AA                           
068200        MOVE DAT-TIVV                TO SPAR-VV                           
068300        MOVE SPAR-DATUM-R            TO MOD-TISERLEV1                     
068400     ELSE                                                                 
068500        MOVE ZERO                    TO MOD-TISERLEV1                     
068600        INSPECT MOD-TISERLEV1 REPLACING LEADING ZERO BY SPACE             
068700     END-IF                                                               
068800                                                                          
068900     MOVE NYPON-ART-TISERLEV     (2) TO DAT-I-TIDATUM                     
069000     PERFORM S99-WDATKONV                                                 
069100                                                                          
069200     IF DAT-KDSVAR-OK                                                     
069300        MOVE DAT-TIAA                TO SPAR-AA                           
069400        MOVE DAT-TIVV                TO SPAR-VV                           
069500        MOVE SPAR-DATUM-R            TO MOD-TISERLEV2                     
069600     ELSE                                                                 
069700        MOVE ZERO                    TO MOD-TISERLEV2                     
069800        INSPECT MOD-TISERLEV2 REPLACING LEADING ZERO BY SPACE             
069900     END-IF                                                               
070000                                                                          
070100     MOVE NYPON-ART-TISERLEV     (3) TO DAT-I-TIDATUM                     
070200     PERFORM S99-WDATKONV                                                 
070300                                                                          
070400     IF DAT-KDSVAR-OK                                                     
070500        MOVE DAT-TIAA                TO SPAR-AA                           
070600        MOVE DAT-TIVV                TO SPAR-VV                           
070700        MOVE SPAR-DATUM-R            TO MOD-TISERLEV3                     
070800     ELSE                                                                 
070900        MOVE ZERO                    TO MOD-TISERLEV3                     
071000        INSPECT MOD-TISERLEV3 REPLACING LEADING ZERO BY SPACE             
071100     END-IF                                                               
071200                                                                          
071300     MOVE NYPON-ART-TISERLEV     (4) TO DAT-I-TIDATUM                     
071400     PERFORM S99-WDATKONV                                                 
071500                                                                          
071600     IF DAT-KDSVAR-OK                                                     
071700        MOVE DAT-TIAA                TO SPAR-AA                           
071800        MOVE DAT-TIVV                TO SPAR-VV                           
071900        MOVE SPAR-DATUM-R            TO MOD-TISERLEV4                     
072000     ELSE                                                                 
072100        MOVE ZERO                    TO MOD-TISERLEV4                     
072200        INSPECT MOD-TISERLEV4 REPLACING LEADING ZERO BY SPACE             
072300     END-IF                                                               
072400                                                                          
072500     MOVE NYPON-ART-TISERLEV     (5) TO DAT-I-TIDATUM                     
072600     PERFORM S99-WDATKONV                                                 
072700                                                                          
072800     IF DAT-KDSVAR-OK                                                     
072900        MOVE DAT-TIAA                TO SPAR-AA                           
073000        MOVE DAT-TIVV                TO SPAR-VV                           
073100        MOVE SPAR-DATUM-R            TO MOD-TISERLEV5                     
073200     ELSE                                                                 
073300        MOVE ZERO                    TO MOD-TISERLEV5                     
073400        INSPECT MOD-TISERLEV5 REPLACING LEADING ZERO BY SPACE             
073500     END-IF                                                               
073600                                                                          
073700     MOVE NYPON-ART-TILEVBEG         TO DAT-I-TIDATUM                     
073800     PERFORM S99-WDATKONV                                                 
073900                                                                          
074000     IF DAT-KDSVAR-OK                                                     
074100        MOVE DAT-TIAA                TO SPAR-AA                           
074200        MOVE DAT-TIVV                TO SPAR-VV                           
074300        MOVE SPAR-DATUM-R            TO MOD-TILEVBEG                      
074400     ELSE                                                                 
074500        MOVE ZERO                    TO MOD-TILEVBEG                      
074600        INSPECT MOD-TILEVBEG REPLACING LEADING ZERO BY SPACE              
074700     END-IF                                                               
074800                                                                          
074900     MOVE NYPON-ART-TINEDBRY         TO DAT-I-TIDATUM                     
075000     PERFORM S99-WDATKONV                                                 
075100                                                                          
075200     IF DAT-KDSVAR-OK                                                     
075300        MOVE DAT-TIAA                TO SPAR-AA                           
075400        MOVE DAT-TIVV                TO SPAR-VV                           
075500        MOVE SPAR-DATUM-R            TO MOD-TINEDBRY-UT                   
075600     ELSE                                                                 
075700        MOVE ZERO                    TO MOD-TINEDBRY-UT                   
075800        INSPECT MOD-TINEDBRY-UT REPLACING LEADING ZERO BY SPACE           
075900     END-IF                                                               
076000                                                                          
076100     IF (MID-AVSL-NOT = ALL '+')  OR (NOT EGEN-BILD)                      
076200        MOVE NYPON-ART-TEARTNOT      TO MOD-AVSL-NOT-IN-UT                
076300     ELSE                                                                 
076400        IF SW-INPUT-RAETT = JA  AND  MFS-UPDATE                           
076500           MOVE NYPON-ART-TEARTNOT   TO MOD-AVSL-NOT-IN-UT                
076600        ELSE                                                              
076700           MOVE MFS-ROER-EJ-FAELT    TO MOD-AVSL-NOT-IN-UT                
076800        END-IF                                                            
076900     END-IF                                                               
077000                                                                          
077100     PERFORM BA-LAS-9KOMPL-JAMFOR                                         
077200                                                                          
077300     PERFORM BB-LAS-ARTREG                                                
077400                                                                          
077500     IF MFS-UPDATE                                                        
077600*       INFÄLT REDAN KONTROLLERADE OCH KLARA......                        
077700        CONTINUE                                                          
077800     ELSE                                                                 
077900        IF MID-IDARTNR-IN = ALL '+'  AND  EGEN-BILD                       
078000           IF MID-KDRESBED      = ALL '+'  AND                            
078100              MID-TINEDBRY      = ALL '+'  AND                            
078200              MID-AVSL-NOT      = ALL '+'  AND                            
078300              MID-SLAECK-9KOMPL = ALL '+'                                 
078400*             GAMMAL NYCKEL, INGENTING INMATAT                            
078500              CONTINUE                                                    
078600           ELSE                                                           
078700              MOVE MED-6(INDX)             TO MOD-TEMFSINF                
078800              IF MID-KDRESBED = ALL '+'                                   
078900                 MOVE MFS-RENSA-FAELT      TO MOD-KDRESBED-IN             
079000              ELSE                                                        
079100                 MOVE MFS-ROER-EJ-FAELT    TO MOD-KDRESBED-IN             
079200                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDRESBED-IN-ATTR        
079300              END-IF                                                      
079400                                                                          
079500              IF MID-TINEDBRY = ALL '+'                                   
079600                 MOVE MFS-RENSA-FAELT      TO MOD-TINEDBRY-IN             
079700              ELSE                                                        
079800                 MOVE MFS-ROER-EJ-FAELT    TO MOD-TINEDBRY-IN             
079900                 MOVE MFS-NUM-FAELT-RAETT  TO MOD-TINEDBRY-IN-ATTR        
080000              END-IF                                                      
080100                                                                          
080200              IF MID-SLAECK-9KOMPL = ALL '+'                              
080300                 MOVE MFS-RENSA-FAELT      TO MOD-SLAECK-9KOMPL-IN        
080400              ELSE                                                        
080500                 MOVE MFS-ROER-EJ-FAELT    TO MOD-SLAECK-9KOMPL-IN        
080600                 MOVE MFS-ALFA-FAELT-RAETT TO                             
080700                                     MOD-SLAECK-9KOMPL-IN-ATTR            
080800              END-IF                                                      
080900                                                                          
081000              IF MID-AVSL-NOT      = ALL '+'                              
081100                 CONTINUE                                                 
081200              ELSE                                                        
081300                 MOVE MFS-ROER-EJ-FAELT    TO MOD-AVSL-NOT-IN-UT          
081400                 MOVE MFS-ALFA-FAELT-RAETT TO                             
081500                                      MOD-AVSL-NOT-IN-UT-ATTR             
081600              END-IF                                                      
081700           END-IF                                                         
081800        ELSE                                                              
081900*          NY NYCKEL, RENSA ALLA INFÄLT                                   
082000           PERFORM S02-RENSA-MOD-INMATNINGSFAELT                          
082100        END-IF                                                            
082200     END-IF.                                                              
082300                                                                          
082400     EJECT                                                                
082500 BA-LAS-9KOMPL-JAMFOR SECTION.                                            
082600     SKIP2                                                                
082700     PERFORM S98-COMPUTE-9KOMPL                                           
082800                                                                          
082900     MOVE SPAR-9KOMPL-IDARTNR        TO W-IDARTNR                         
083000                                                                          
083100     PERFORM IMS-GHU-ARTG01-IO3-PCB2                                      
083200                                                                          
083300     IF SEGMENT-FINNS                                                     
083400        PERFORM BAA-JAMFOR                                                
083500     END-IF.                                                              
083600                                                                          
083700     EJECT                                                                
083800 BAA-JAMFOR SECTION.                                                      
083900     SKIP2                                                                
084000     IF NYPON-ART-KDPRODSL    = 9KOMPL-ART-KDPRODSL                       
084100        CONTINUE                                                          
084200     ELSE                                                                 
084300        MOVE MED-4(INDX)             TO MOD-TEMFSFEL                      
084400*       LÄGGS I FELRADEN FÖR ATT INTE KONFLIKTA MED                       
084500*       ÖVRIGA MEDDELANDEN......                                          
084600     END-IF                                                               
084700                                                                          
084800     IF NYPON-ART-IDPROJ      = 9KOMPL-ART-IDPROJ                         
084900        CONTINUE                                                          
085000     ELSE                                                                 
085100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDPROJ-ATTR                   
085200     END-IF                                                               
085300                                                                          
085400     IF NYPON-ART-IDPROJK     = 9KOMPL-ART-IDPROJK                        
085500        CONTINUE                                                          
085600     ELSE                                                                 
085700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDPROJK-ATTR                  
085800     END-IF                                                               
085900                                                                          
086000     IF NYPON-ART-IDAO        = 9KOMPL-ART-IDAO                           
086100        CONTINUE                                                          
086200     ELSE                                                                 
086300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDAO-ATTR                     
086400     END-IF                                                               
086500                                                                          
086600     IF NYPON-ART-IDRITUTG    = 9KOMPL-ART-IDRITUTG                       
086700        CONTINUE                                                          
086800     ELSE                                                                 
086900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDRITUTG-ATTR                 
087000     END-IF                                                               
087100                                                                          
087200     IF NYPON-ART-TISTABER    = 9KOMPL-ART-TISTABER                       
087300        CONTINUE                                                          
087400     ELSE                                                                 
087500        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISTABER-ATTR                 
087600     END-IF                                                               
087700                                                                          
087800     IF NYPON-ART-IDAVD       = 9KOMPL-ART-IDAVD                          
087900        CONTINUE                                                          
088000     ELSE                                                                 
088100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDAVD-ATTR                    
088200     END-IF                                                               
088300                                                                          
088400     IF NYPON-ART-KVARTAR1    = 9KOMPL-ART-KVARTAR1                       
088500        CONTINUE                                                          
088600     ELSE                                                                 
088700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KVARTAR1-ATTR                 
088800     END-IF                                                               
088900                                                                          
089000     IF NYPON-ART-TIRITB      = 9KOMPL-ART-TIRITB                         
089100        CONTINUE                                                          
089200     ELSE                                                                 
089300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIRITB-ATTR                   
089400     END-IF                                                               
089500                                                                          
089600     IF NYPON-ART-FLRITB      = 9KOMPL-ART-FLRITB                         
089700        CONTINUE                                                          
089800     ELSE                                                                 
089900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLRITB-ATTR                   
090000     END-IF                                                               
090100                                                                          
090200     IF NYPON-ART-TISLUBER    = 9KOMPL-ART-TISLUBER                       
090300        CONTINUE                                                          
090400     ELSE                                                                 
090500        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISLUBER-ATTR                 
090600     END-IF                                                               
090700                                                                          
090800     IF NYPON-ART-IDPROENH    = 9KOMPL-ART-IDPROENH                       
090900        CONTINUE                                                          
091000     ELSE                                                                 
091100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDPROENH-ATTR                 
091200     END-IF                                                               
091300                                                                          
091400     IF NYPON-ART-KVARTAR2    = 9KOMPL-ART-KVARTAR2                       
091500        CONTINUE                                                          
091600     ELSE                                                                 
091700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KVARTAR2-ATTR                 
091800     END-IF                                                               
091900                                                                          
092000     IF NYPON-ART-TIRITC      = 9KOMPL-ART-TIRITC                         
092100        CONTINUE                                                          
092200     ELSE                                                                 
092300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIRITC-ATTR                   
092400     END-IF                                                               
092500                                                                          
092600     IF NYPON-ART-FLRITC      = 9KOMPL-ART-FLRITC                         
092700        CONTINUE                                                          
092800     ELSE                                                                 
092900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLRITC-ATTR                   
093000     END-IF                                                               
093100                                                                          
093200     IF NYPON-ART-TIPLAKOP    = 9KOMPL-ART-TIPLAKOP                       
093300        CONTINUE                                                          
093400     ELSE                                                                 
093500        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIPLAKOP-ATTR                 
093600     END-IF                                                               
093700                                                                          
093800     IF NYPON-ART-PRARTBES    = 9KOMPL-ART-PRARTBES                       
093900        CONTINUE                                                          
094000     ELSE                                                                 
094100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-PRARTSTD-ATTR                 
094200     END-IF                                                               
094300                                                                          
094400     IF NYPON-ART-KDSTAINK    = 9KOMPL-ART-KDSTAINK                       
094500        CONTINUE                                                          
094600     ELSE                                                                 
094700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDSTAINK-ATTR                 
094800     END-IF                                                               
094900                                                                          
095000     IF NYPON-ART-KVARTAR3    = 9KOMPL-ART-KVARTAR3                       
095100        CONTINUE                                                          
095200     ELSE                                                                 
095300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KVARTAR3-ATTR                 
095400     END-IF                                                               
095500                                                                          
095600     IF NYPON-ART-TIRITP      = 9KOMPL-ART-TIRITP                         
095700        CONTINUE                                                          
095800     ELSE                                                                 
095900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIRITP-ATTR                   
096000     END-IF                                                               
096100                                                                          
096200     IF NYPON-ART-FLRITP      = 9KOMPL-ART-FLRITP                         
096300        CONTINUE                                                          
096400     ELSE                                                                 
096500        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLRITP-ATTR                   
096600     END-IF                                                               
096700                                                                          
096800     IF NYPON-ART-IDRITN      = 9KOMPL-ART-IDRITN                         
096900        CONTINUE                                                          
097000     ELSE                                                                 
097100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDRITN-ATTR                   
097200     END-IF                                                               
097300                                                                          
097400     IF NYPON-ART-IDFKNGRP    = 9KOMPL-ART-IDFKNGRP                       
097500        CONTINUE                                                          
097600     ELSE                                                                 
097700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDFKNGRP-ATTR                 
097800     END-IF                                                               
097900                                                                          
098000     IF NYPON-ART-IDINK       = 9KOMPL-ART-IDINK                          
098100        CONTINUE                                                          
098200     ELSE                                                                 
098300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDINK-ATTR                    
098400     END-IF                                                               
098500                                                                          
098600     IF NYPON-ART-KDSORT      = 9KOMPL-ART-KDSORT                         
098700        CONTINUE                                                          
098800     ELSE                                                                 
098900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDSORT-ATTR                   
099000     END-IF                                                               
099100                                                                          
099200     IF NYPON-ART-IDLEVNR     = 9KOMPL-ART-IDLEVNR                        
099300        CONTINUE                                                          
099400     ELSE                                                                 
099500        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-ATTR                  
099600     END-IF                                                               
099700                                                                          
099800     IF NYPON-ART-IDLEVNR-FORB (1) = 9KOMPL-ART-IDLEVNR-FORB (1)          
099900        CONTINUE                                                          
100000     ELSE                                                                 
100100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB1-ATTR            
100200     END-IF                                                               
100300                                                                          
100400     IF NYPON-ART-IDLEVNR-FORB (2) = 9KOMPL-ART-IDLEVNR-FORB (2)          
100500        CONTINUE                                                          
100600     ELSE                                                                 
100700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB2-ATTR            
100800     END-IF                                                               
100900                                                                          
101000     IF NYPON-ART-IDLEVNR-FORB (3) = 9KOMPL-ART-IDLEVNR-FORB (3)          
101100        CONTINUE                                                          
101200     ELSE                                                                 
101300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB3-ATTR            
101400     END-IF                                                               
101500                                                                          
101600     IF NYPON-ART-IDLEVNR-FORB (4) = 9KOMPL-ART-IDLEVNR-FORB (4)          
101700        CONTINUE                                                          
101800     ELSE                                                                 
101900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB4-ATTR            
102000     END-IF                                                               
102100                                                                          
102200     IF NYPON-ART-IDLEVNR-FORB (5) = 9KOMPL-ART-IDLEVNR-FORB (5)          
102300        CONTINUE                                                          
102400     ELSE                                                                 
102500        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-FORB5-ATTR            
102600     END-IF                                                               
102700                                                                          
102800     IF NYPON-ART-TIUPG            = 9KOMPL-ART-TIUPG                     
102900        CONTINUE                                                          
103000     ELSE                                                                 
103100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TIUPG-ATTR                    
103200     END-IF                                                               
103300                                                                          
103400     IF NYPON-ART-FLUPG            = 9KOMPL-ART-FLUPG                     
103500        CONTINUE                                                          
103600     ELSE                                                                 
103700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLUPG-ATTR                    
103800     END-IF                                                               
103900                                                                          
104000     IF NYPON-ART-TISERLEV     (1) = 9KOMPL-ART-TISERLEV     (1)          
104100        CONTINUE                                                          
104200     ELSE                                                                 
104300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV1-ATTR                
104400     END-IF                                                               
104500                                                                          
104600     IF NYPON-ART-TISERLEV     (2) = 9KOMPL-ART-TISERLEV     (2)          
104700        CONTINUE                                                          
104800     ELSE                                                                 
104900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV2-ATTR                
105000     END-IF                                                               
105100                                                                          
105200     IF NYPON-ART-TISERLEV     (3) = 9KOMPL-ART-TISERLEV     (3)          
105300        CONTINUE                                                          
105400     ELSE                                                                 
105500        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV3-ATTR                
105600     END-IF                                                               
105700                                                                          
105800     IF NYPON-ART-TISERLEV     (4) = 9KOMPL-ART-TISERLEV     (4)          
105900        CONTINUE                                                          
106000     ELSE                                                                 
106100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV4-ATTR                
106200     END-IF                                                               
106300                                                                          
106400     IF NYPON-ART-TISERLEV     (5) = 9KOMPL-ART-TISERLEV     (5)          
106500        CONTINUE                                                          
106600     ELSE                                                                 
106700        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TISERLEV5-ATTR                
106800     END-IF                                                               
106900                                                                          
107000     IF NYPON-ART-BEART-SVE        = 9KOMPL-ART-BEART-SVE                 
107100        CONTINUE                                                          
107200     ELSE                                                                 
107300        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-BEART-ATTR                    
107400     END-IF                                                               
107500                                                                          
107600     IF NYPON-ART-TETEKNIK         = 9KOMPL-ART-TETEKNIK                  
107700        CONTINUE                                                          
107800     ELSE                                                                 
107900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TETEKNIK-ATTR                 
108000     END-IF.                                                              
108100                                                                          
108200     EJECT                                                                
108300 BB-LAS-ARTREG SECTION.                                                   
108400     SKIP2                                                                
108500     MOVE NYPON-ART-IDARTNR          TO W-IDARTNR                         
108600     PERFORM IMS-GU-ARTC01                                                
108700                                                                          
108800     IF SEGMENT-FINNS  AND  ART-KDERS-UTG = ZERO                          
108900                                                                          
109000        PERFORM IMS-GNP-ARTC11                                            
109100        MOVE CLAG-IDPROJUP           TO MOD-IDPROJUP                      
109200                                                                          
109300        PERFORM IMS-GNP-ARTC22                                            
109400        IF SEGMENT-FINNS                                                  
109500           MOVE BEST-TIBEST          TO DAT-I-TIDATUM                     
109600           PERFORM S99-WDATKONV                                           
109700           IF DAT-KDSVAR-OK                                               
109800              MOVE DAT-TIAA          TO SPAR-AA                           
109900              MOVE DAT-TIVV          TO SPAR-VV                           
110000              MOVE SPAR-DATUM-R      TO MOD-TIBEST                        
110100           ELSE                                                           
110200              MOVE MFS-RENSA-FAELT   TO MOD-TIBEST                        
110300           END-IF                                                         
110400        ELSE                                                              
110500           PERFORM IMS-GNP-ARTC23                                         
110600           IF SEGMENT-FINNS                                               
110700              MOVE AVT-TIAVTAL       TO DAT-I-TIDATUM                     
110800              PERFORM S99-WDATKONV                                        
110900              IF DAT-KDSVAR-OK                                            
111000                 MOVE DAT-TIAA          TO SPAR-AA                        
111100                 MOVE DAT-TIVV          TO SPAR-VV                        
111200                 MOVE SPAR-DATUM-R      TO MOD-TIBEST                     
111300              ELSE                                                        
111400                 MOVE MFS-RENSA-FAELT   TO MOD-TIBEST                     
111500              END-IF                                                      
111600           ELSE                                                           
111700              MOVE MFS-RENSA-FAELT      TO MOD-TIBEST                     
111800           END-IF                                                         
111900        END-IF                                                            
112000     ELSE                                                                 
112100        MOVE MFS-RENSA-FAELT         TO MOD-IDPROJUP                      
112200                                        MOD-TIBEST                        
112300     END-IF                                                               
112400     .                                                                    
112500     EJECT                                                                
112600 C-KOLLA-INPUT SECTION.                                                   
112700     SKIP2                                                                
112800     MOVE JA                         TO SW-INPUT-RAETT                    
112900                                                                          
113000     MOVE WS-TEST-KDPRODSL     TO TEST-KDPRODSL                           
113100     IF KDPRODSL-VOLVO-BIMA                                               
113200        IF CDC OR SDC                                                     
113300           CONTINUE                                                       
113400        ELSE                                                              
113500           MOVE NEJ TO SW-INPUT-RAETT                                     
113600           MOVE MED-7(INDX) TO MOD-TEMFSINF                               
113700        END-IF                                                            
113800     END-IF                                                               
113900                                                                          
114000     IF MID-KDRESBED = ALL '+'                                            
114100        MOVE MFS-RENSA-FAELT   TO MOD-KDRESBED-IN                         
114200     ELSE                                                                 
114300        IF MID-KDRESBED = '-'  AND  NYPON-ART-KDRESBED = SPACE            
114400*          A V S L A G                                                    
114500           MOVE MFS-ALFA-FAELT-RAETT                                      
114600                               TO MOD-KDRESBED-IN-ATTR                    
114700        ELSE                                                              
114800           MOVE MFS-ALFA-FAELT-FEL                                        
114900                               TO MOD-KDRESBED-IN-ATTR                    
115000           MOVE NEJ            TO SW-INPUT-RAETT                          
115100        END-IF                                                            
115200        MOVE MFS-ROER-EJ-FAELT TO MOD-KDRESBED-IN                         
115300     END-IF                                                               
115400                                                                          
115500     IF MID-TINEDBRY = ALL '+'                                            
115600        MOVE MFS-RENSA-FAELT   TO MOD-TINEDBRY-IN                         
115700     ELSE                                                                 
115800        MOVE 'AAVV'            TO DAT-KDDATFORM                           
115900        MOVE MID-TINEDBRY      TO DAT-I-TIDATUM                           
116000        PERFORM S99-WDATKONV                                              
116100        IF DAT-KDSVAR-OK                                                  
116200*          F O R M E L L T  R Ä T T                                       
116300           MOVE MID-TINEDBRY          TO TMP1-YYWW                        
116400           MOVE SPAR-DAGENS-DATUM-R   TO TMP2-YYWW                        
116500           PERFORM WY2000P3                                               
116600           IF TMP1-YYWW   < TMP2-YYWW                                     
116700              MOVE MFS-NUM-FAELT-FEL                                      
116800                                  TO MOD-TINEDBRY-IN-ATTR                 
116900              MOVE NEJ            TO SW-INPUT-RAETT                       
117000           ELSE                                                           
117100              MOVE DAT-TIAAMMDD   TO SPAR-TINEDBRY-AAMMDD                 
117200              MOVE DAT-TIAA       TO SPAR-TINEDBRY-AA                     
117300              MOVE DAT-TIVV       TO SPAR-TINEDBRY-VV                     
117400              MOVE MFS-NUM-FAELT-RAETT                                    
117500                                  TO MOD-TINEDBRY-IN-ATTR                 
117600           END-IF                                                         
117700        ELSE                                                              
117800           MOVE MFS-NUM-FAELT-FEL TO MOD-TINEDBRY-IN-ATTR                 
117900           MOVE NEJ               TO SW-INPUT-RAETT                       
118000        END-IF                                                            
118100        MOVE MFS-ROER-EJ-FAELT    TO MOD-TINEDBRY-IN                      
118200     END-IF                                                               
118300                                                                          
118400     IF MID-AVSL-NOT = ALL '+'                                            
118500        CONTINUE                                                          
118600     ELSE                                                                 
118700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-AVSL-NOT-IN-UT-ATTR              
118800        MOVE MFS-ROER-EJ-FAELT    TO MOD-AVSL-NOT-IN-UT                   
118900     END-IF                                                               
119000                                                                          
119100     IF MID-SLAECK-9KOMPL = ALL '+'                                       
119200        MOVE MFS-RENSA-FAELT      TO MOD-SLAECK-9KOMPL-IN                 
119300     ELSE                                                                 
119400        IF MID-SLAECK-9KOMPL = JA                                         
119500                                                                          
119600           PERFORM S98-COMPUTE-9KOMPL                                     
119700                                                                          
119800           MOVE SPAR-9KOMPL-IDARTNR        TO W-IDARTNR                   
119900                                                                          
120000           PERFORM IMS-GHU-ARTG01-IO3-PCB2                                
120100                                                                          
120200           IF SEGMENT-FINNS                                               
120300              MOVE MFS-ALFA-FAELT-RAETT                                   
120400                                  TO MOD-SLAECK-9KOMPL-IN-ATTR            
120500           ELSE                                                           
120600              MOVE MFS-ALFA-FAELT-FEL                                     
120700                                  TO MOD-SLAECK-9KOMPL-IN-ATTR            
120800              MOVE NEJ            TO SW-INPUT-RAETT                       
120900           END-IF                                                         
121000        ELSE                                                              
121100           MOVE MFS-ALFA-FAELT-FEL                                        
121200                                  TO MOD-SLAECK-9KOMPL-IN-ATTR            
121300           MOVE NEJ               TO SW-INPUT-RAETT                       
121400        END-IF                                                            
121500        MOVE MFS-ROER-EJ-FAELT    TO MOD-SLAECK-9KOMPL-IN                 
121600     END-IF.                                                              
121700                                                                          
121800     EJECT                                                                
121900 D-UPPDATERA SECTION.                                                     
122000     SKIP2                                                                
122100     IF MID-SLAECK-9KOMPL = ALL '+'                                       
122200        CONTINUE                                                          
122300     ELSE                                                                 
122400        MOVE SPAR-9KOMPL-IDARTNR     TO W-IDARTNR                         
122500        PERFORM IMS-GHU-ARTG01-IO3-PCB2                                   
122600                                                                          
122700        PERFORM IMS-DELETE-NYPON                                          
122800                                                                          
122900        MOVE NEJ                     TO NYPON-ART-FLAENDR                 
123000                                                                          
123100        MOVE SPAR-TINEDBRY-AAVV-R   TO TMP1-YYWW                          
123200        MOVE SPAR-DAGENS-DATUM-R    TO TMP2-YYWW                          
123300        PERFORM WY2000P3                                                  
123400        IF TMP1-YYWW   > TMP2-YYWW                                        
123500*          EV. INMATAD TINEDBRY ÄR STÖRRE ÄN INNEVARANDE VECKA....        
123600           CONTINUE                                                       
123700        ELSE                                                              
123800           IF NYPON-ART-TINEDBRY > ZERO                                   
123900              MOVE 'AAMMDD'             TO DAT-KDDATFORM                  
124000              MOVE NYPON-ART-TINEDBRY   TO DAT-I-TIDATUM                  
124100              PERFORM S99-WDATKONV                                        
124200                                                                          
124300              IF DAT-KDSVAR-OK                                            
124400                 MOVE DAT-TIAA          TO SPAR-AA                        
124500                 MOVE DAT-TIVV          TO SPAR-VV                        
124600              ELSE                                                        
124700                 MOVE ZERO              TO SPAR-DATUM-AAVV                
124800              END-IF                                                      
124900           ELSE                                                           
125000              MOVE ZERO                 TO SPAR-DATUM-AAVV                
125100           END-IF                                                         
125200                                                                          
125300           MOVE SPAR-DATUM-R          TO TMP1-YYWW                        
125400           MOVE SPAR-DAGENS-DATUM-R   TO TMP2-YYWW                        
125500           PERFORM WY2000P3                                               
125600           IF TMP1-YYWW > TMP2-YYWW                                       
125700*             NYPON-TINEDBRY ÄR STÖRRE ÄN INNEVARANDE VECKA....           
125800              CONTINUE                                                    
125900           ELSE                                                           
126000              IF MID-KDRESBED = '-'  OR                                   
126100                 NYPON-ART-KDRESBED = 'R' OR 'E' OR '-'                   
126200                 MOVE NEJ         TO NYPON-ART-FLBERQ                     
126300                 MOVE ZERO        TO NYPON-ART-TINEDBRY                   
126400              ELSE                                                        
126500                 IF NYPON-ART-KDRESBED = 'U'  AND                         
126600                    NYPON-ART-FLAENDR  = JA                               
126700                    MOVE NEJ         TO NYPON-ART-FLBERQ                  
126800                    MOVE ZERO        TO NYPON-ART-TINEDBRY                
126900                 END-IF                                                   
127000              END-IF                                                      
127100           END-IF                                                         
127200        END-IF                                                            
127300     END-IF                                                               
127400                                                                          
127500     IF MID-KDRESBED      = ALL '+'  AND                                  
127600        MID-TINEDBRY      = ALL '+'  AND                                  
127700        MID-AVSL-NOT      = ALL '+'                                       
127800        CONTINUE                                                          
127900     ELSE                                                                 
128000        IF MID-KDRESBED = ALL '+'                                         
128100           CONTINUE                                                       
128200        ELSE                                                              
128300           MOVE MID-KDRESBED            TO NYPON-ART-KDRESBED             
128400                                                                          
128500           IF MID-TINEDBRY       = ALL '+'  AND                           
128600              NYPON-ART-TINEDBRY = ZERO                                   
128700              MOVE NEJ                  TO NYPON-ART-FLBERQ               
128800           END-IF                                                         
128900                                                                          
129000           MOVE NYPON-ART-KDPRODSL      TO TEST-KDPRODSL                  
129100           IF KDPRODSL-VCBV                                               
129200              CONTINUE                                                    
129300           ELSE                                                           
129400              PERFORM DB-KDP-KOLA                                         
129500           END-IF                                                         
129600        END-IF                                                            
129700                                                                          
129800        IF MID-TINEDBRY = ALL '+'                                         
129900           CONTINUE                                                       
130000        ELSE                                                              
130100           MOVE SPAR-TINEDBRY-AAMMDD    TO NYPON-ART-TINEDBRY             
130200           MOVE JA                      TO NYPON-ART-FLBERQ               
130300                                                                          
130400           IF NYPON-ART-IDBERED = ZERO                                    
130500              PERFORM IMS-GU-ARTC11                                       
130600              IF SEGMENT-FINNS                                            
130700                 IF CLAG-IDBERED = ZERO                                   
130800                    PERFORM DC-HAMTA-IDBERED-XXAS                         
130900                 ELSE                                                     
131000                    MOVE CLAG-IDBERED TO NYPON-ART-IDBERED                
131100                 END-IF                                                   
131200              ELSE                                                        
131300                 PERFORM DC-HAMTA-IDBERED-XXAS                            
131400              END-IF                                                      
131500           END-IF                                                         
131600        END-IF                                                            
131700                                                                          
131800        IF MID-AVSL-NOT = ALL '+'                                         
131900           CONTINUE                                                       
132000        ELSE                                                              
132100           MOVE MID-AVSL-NOT            TO NYPON-ART-TEARTNOT             
132200        END-IF                                                            
132300                                                                          
132400     END-IF                                                               
132500                                                                          
132600     PERFORM IMS-REPL-NYPON                                               
132700                                                                          
132800     MOVE MED-5(INDX)                   TO MOD-TEMFSINF                   
132900     PERFORM S02-RENSA-MOD-INMATNINGSFAELT                                
133000     PERFORM S03-FORMATETS-ATTRIBUT.                                      
133100                                                                          
133200     EJECT                                                                
133300 DB-KDP-KOLA SECTION.                                                     
133400     SKIP2                                                                
133500     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
133600     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
133700******************************************************************        
133800*    IDLOGLOP = 2, FÖR ATT SKILJA TRANSAR FRÅN 1113,1115,1117,1142        
133900******************************************************************        
134000     MOVE 2                               TO SPAR-IDLOGLOP                
134100     MOVE SPAR-IDLOGLOP                   TO ZZAC-IDLOGLOP                
134200                                                                          
134300     MOVE 'RZU'                           TO KDP-IDPTYP                   
134400     MOVE '-'                             TO KDP-KDUART                   
134500     MOVE NYPON-ART-IDARTNR               TO KDP-IDARTNR                  
134600                                             W092-SORTBGP                 
134700                                                                          
134800     MOVE KDP-W10111                      TO ZZAC-LOGGPOST                
134900     MOVE W092-AREA                       TO ZZAC-SORTPOST                
135000     PERFORM IMS-ISRT-ZZAC.                                               
135100     EJECT                                                                
135200 DC-HAMTA-IDBERED-XXAS SECTION.                                           
135300     SKIP2                                                                
135400     MOVE NYPON-ART-KDPRODSL              TO W-KDPRODSL                   
135500     MOVE NYPON-ART-IDFKNGRP              TO W-IDFKNGRP                   
135600                                             W-IDFKNGRP-SBGP              
135700                                                                          
135800     PERFORM IMS-GU-WLXXAS01                                              
135900     PERFORM IMS-GNP-WLXXAS11                                             
136000                                                                          
136100     IF SEGMENT-FINNS                                                     
136200        MOVE XXAS-1136-IDBERED            TO NYPON-ART-IDBERED            
136300     END-IF.                                                              
136400     EJECT                                                                
136500 S02-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
136600     SKIP3                                                                
136700     MOVE MFS-RENSA-FAELT             TO MOD-KDRESBED-IN                  
136800                                         MOD-TINEDBRY-IN                  
136900                                         MOD-SLAECK-9KOMPL-IN.            
137000     EJECT                                                                
137100 S03-FORMATETS-ATTRIBUT SECTION.                                          
137200     SKIP3                                                                
137300     MOVE MFS-FORMATETS-ATTR         TO MOD-KDRESBED-IN-ATTR              
137400                                        MOD-TINEDBRY-IN-ATTR              
137500                                        MOD-AVSL-NOT-IN-UT-ATTR           
137600                                        MOD-SLAECK-9KOMPL-IN-ATTR.        
137700     EJECT                                                                
137800 S98-COMPUTE-9KOMPL SECTION.                                              
137900     SKIP3                                                                
138000     MOVE WS-IDARTNR      TO W-IDARTNR                                    
138100                                                                          
138200     COMPUTE SPAR-9KOMPL-IDARTNR = 999999999 - W-IDARTNR.                 
138300     EJECT                                                                
138400 S99-WDATKONV SECTION.                                                    
138500     SKIP3                                                                
138600     CALL WDATKONV USING DAT-KDDATFORM                                    
138700                         DAT-I-TIDATUM                                    
138800                         DAT-O-TIDATUM                                    
138900                         DAT-KDSVAR.                                      
139000     EJECT                                                                
139100* IMS SEKTIONER                                                           
139200     SKIP3                                                                
139300 IMS-GET-MSG SECTION.                                                     
139400     SKIP2                                                                
139500     MOVE '  QC' TO GODK-STATUSKODER                                      
139600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
139700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
139800     PERFORM IMS-STATUS-KONTROLL.                                         
139900     SKIP3                                                                
140000 IMS-INSERT-MSG SECTION.                                                  
140100     SKIP2                                                                
140200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
140300        MOVE '0' TO MFS-KDHUVOMR                                          
140400     END-IF                                                               
140500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
140600     MOVE SPACE TO GODK-STATUSKODER                                       
140700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
140800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
140900     PERFORM IMS-STATUS-KONTROLL.                                         
141000     EJECT                                                                
141100 IMS-GU-ARTC01 SECTION.                                                   
141200     SKIP2                                                                
141300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
141400             DELIMITED BY SIZE INTO SSA1                                  
141500     MOVE '  GE' TO GODK-STATUSKODER                                      
141600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1                     
141700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
141800     PERFORM IMS-STATUS-KONTROLL.                                         
141900     SKIP3                                                                
142000 IMS-GNP-ARTC11 SECTION.                                                  
142100     SKIP2                                                                
142200     MOVE 'WLARTC11(KDSEGKEY =1)'    TO SSA1                              
142300     MOVE '  ' TO GODK-STATUSKODER                                        
142400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1                    
142500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
142600     PERFORM IMS-STATUS-KONTROLL.                                         
142700     SKIP3                                                                
142800 IMS-GU-ARTC11   SECTION.                                                 
142900     SKIP2                                                                
143000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
143100             DELIMITED BY SIZE INTO SSA1                                  
143200     MOVE 'WLARTC11 ' TO SSA2                                             
143300     MOVE '  GE' TO GODK-STATUSKODER                                      
143400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA1 SSA1 SSA2                
143500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
143600     PERFORM IMS-STATUS-KONTROLL.                                         
143700     EJECT                                                                
143800 IMS-GNP-ARTC22 SECTION.                                                  
143900     SKIP2                                                                
144000     MOVE 'WLARTC11 ' TO SSA1                                             
144100     MOVE 'WLARTC22 ' TO SSA2                                             
144200     MOVE '  GE' TO GODK-STATUSKODER                                      
144300     CALL CBLTDLI USING GNP    ARTC-PCB DLI-IO-AREA1 SSA1 SSA2            
144400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
144500     PERFORM IMS-STATUS-KONTROLL.                                         
144600     SKIP3                                                                
144700 IMS-GNP-ARTC23 SECTION.                                                  
144800     SKIP2                                                                
144900     MOVE 'WLARTC11 ' TO SSA1                                             
145000     MOVE 'WLARTC23 ' TO SSA2                                             
145100     MOVE '  GE' TO GODK-STATUSKODER                                      
145200     CALL CBLTDLI USING GNP    ARTC-PCB DLI-IO-AREA1 SSA1 SSA2            
145300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
145400     PERFORM IMS-STATUS-KONTROLL.                                         
145500     EJECT                                                                
145600 IMS-GHU-ARTG01-IO2-PCB1    SECTION.                                      
145700     SKIP2                                                                
145800     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
145900             DELIMITED BY SIZE INTO SSA1                                  
146000     MOVE '  GE' TO GODK-STATUSKODER                                      
146100     CALL CBLTDLI USING GHU ARTG1-PCB DLI-IO-AREA2 SSA1                   
146200     MOVE ARTG1-STATUS-CODE TO STATUS-WS                                  
146300     PERFORM IMS-STATUS-KONTROLL.                                         
146400     SKIP3                                                                
146500 IMS-GHU-ARTG01-IO3-PCB2    SECTION.                                      
146600     SKIP2                                                                
146700*    F Ö R  J Ä M F Ö R E L S E  A V  9 -  K O M P L E M E N T            
146800     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
146900             DELIMITED BY SIZE INTO SSA1                                  
147000     MOVE '  GE' TO GODK-STATUSKODER                                      
147100     CALL CBLTDLI USING GHU ARTG2-PCB DLI-IO-AREA3 SSA1                   
147200     MOVE ARTG2-STATUS-CODE TO STATUS-WS                                  
147300     PERFORM IMS-STATUS-KONTROLL.                                         
147400     EJECT                                                                
147500 IMS-GU-WLXXAS01 SECTION.                                                 
147600     SKIP2                                                                
147700     STRING 'WLXXAS01(WDGXKEY  =' W-1135KEY-X ')'                         
147800             DELIMITED BY SIZE INTO SSA1                                  
147900     MOVE '  GE' TO GODK-STATUSKODER                                      
148000     CALL CBLTDLI USING GU XXAS-PCB DLI-IO-AREA1 SSA1                     
148100     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
148200     PERFORM IMS-STATUS-KONTROLL.                                         
148300     SKIP3                                                                
148400 IMS-GNP-WLXXAS11 SECTION.                                                
148500     SKIP2                                                                
148600     STRING 'WLXXAS11(WDGXKEY <=' W-1136KEY-X                             
148700                    '&IDFKNGRP>=' W-IDFKNGRP-X ')'                        
148800             DELIMITED BY SIZE INTO SSA1                                  
148900     MOVE '  GE' TO GODK-STATUSKODER                                      
149000     CALL CBLTDLI USING GNP XXAS-PCB DLI-IO-AREA1 SSA1                    
149100     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
149200     PERFORM IMS-STATUS-KONTROLL.                                         
149300     EJECT                                                                
149400 IMS-ISRT-ZZAC SECTION.                                                   
149500     SKIP2                                                                
149600     MOVE 'WLZZAC01 '     TO SSA1                                         
149700     MOVE '  '   TO GODK-STATUSKODER                                      
149800     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA1 SSA1                   
149900     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUS-KONTROLL.                                         
150100     EJECT                                                                
150200 IMS-DELETE-NYPON SECTION.                                                
150300*    F Ö R  D E L E T E  A V  9 - K O M P L E M E N T E T                 
150400     SKIP2                                                                
150500     MOVE '  '   TO GODK-STATUSKODER                                      
150600     CALL CBLTDLI USING DLET ARTG2-PCB DLI-IO-AREA3                       
150700     MOVE ARTG2-STATUS-CODE TO STATUS-WS                                  
150800     PERFORM IMS-STATUS-KONTROLL.                                         
150900     SKIP3                                                                
151000 IMS-REPL-NYPON SECTION.                                                  
151100*    F Ö R  R E P L A C E  A V  D E T  "RIKTIGA ARTIKELNUMRET"            
151200     SKIP2                                                                
151300     MOVE '  '   TO GODK-STATUSKODER                                      
151400     CALL CBLTDLI USING REPL ARTG1-PCB DLI-IO-AREA2                       
151500     MOVE ARTG1-STATUS-CODE TO STATUS-WS                                  
151600     PERFORM IMS-STATUS-KONTROLL.                                         
151700     SKIP3                                                                
151800 IMS-STATUS-KONTROLL SECTION.                                             
151900     SET STATUS-IX TO 1                                                   
152000     SEARCH GODK-STATUS                                                   
152100       AT END                                                             
152200         CALL FELLOG                                                      
152300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
152400     END-SEARCH.                                                          
152500     EJECT                                                                
152600     EJECT                                                                
152700*    -COPY WY2000P3                                                       
