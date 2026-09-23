000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5030500.                                                
000400 AUTHOR.         GUN ANDERSSON.                                           
000500 DATE-WRITTEN.   JUNI 1986.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001100*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0177               
001200*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001300*        INVENTERING - INRAPPORTERING AV RE0-UPPGIFTER.                   
001400*                    - START AV BMP W513B1 VIA PGM W00606.                
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W5T305                                              
001800*                     W5T305U                                             
001900*        MID:         W5I30501                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        TRANSAKTION: W0T606U                                             
002300*        MOD:         W5O30501                                            
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77   PROGRAM-NAMN           VALUE 'W5030500'                             
003300                                 PIC X(8).                                
003400 77    JA                        PIC X       VALUE 'J'.                   
003500 77    NEJ                       PIC X       VALUE 'N'.                   
003600 77    WS-IDDC-NUM               PIC X(2)    VALUE SPACE.                 
003700 77    DETTA-CL                  PIC S9      VALUE +0    COMP-3.          
003800     EJECT                                                                
003900 01  SUBPROGRAM.                                                          
004000     03  WKPSKONV                PIC X(8)  VALUE 'WKPSKONV'.              
004100     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
004200     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
004300     03  W005INIT                PIC X(8)  VALUE 'W005INIT'.              
004400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
004500*01 -COPY WMSGINIT                                                        
004600     EJECT                                                                
004700 01    FILLER                    PIC X(11) VALUE 'MEDDELANDEN'.           
004800 01    MEDDELANDE.                                                        
004900   03    FILLER1-1               PIC X(60)   VALUE                        
005000             'FYLL I INMATNINGSFÄLT'.                                     
005100   03    FILLER1-2               PIC X(60)   VALUE                        
005200             'GIVE INPUT'.                                                
005300   03    FILLER2-1               PIC X(60)   VALUE                        
005400             'TRYCK PF11 FÖR START AV BMP'.                               
005500   03    FILLER2-2               PIC X(60)   VALUE                        
005600             'PRESS PF11 FOR START OF BMP'.                               
005700   03    FILLER3-1               PIC X(60)   VALUE                        
005800             'UPPLYSTA FÄLT FEL'.                                         
005900   03    FILLER3-2               PIC X(60)   VALUE                        
006000             'HIGHLIGHTED FIELDS WRONG'.                                  
006100   03    FILLER4-1               PIC X(60)   VALUE                        
006200             'BMP STARTAD                '.                               
006300   03    FILLER4-2               PIC X(60)   VALUE                        
006400             'BMP STARTED                '.                               
006500   03    FILLER5-1               PIC X(60)   VALUE                        
006600             'PRODUKTSLAG EJ TILLÅTET    '.                               
006700   03    FILLER5-2               PIC X(60)   VALUE                        
006800             'PRODUCT GROUP NOT ALLOWED  '.                               
006900   03    FILLER6-1               PIC X(60)   VALUE                        
007000             'OMRÅDE EJ TILLÅTET         '.                               
007100   03    FILLER6-2               PIC X(60)   VALUE                        
007200             'AREA NOT ALLOWED           '.                               
007300                                                                          
007400 01  MED-MEDDEL  REDEFINES MEDDELANDE.                                    
007500   03    MED-1   OCCURS 2        PIC X(60).                               
007600   03    MED-2   OCCURS 2        PIC X(60).                               
007700   03    MED-3   OCCURS 2        PIC X(60).                               
007800   03    MED-4   OCCURS 2        PIC X(60).                               
007900   03    MED-5   OCCURS 2        PIC X(60).                               
008000   03    MED-6   OCCURS 2        PIC X(60).                               
008100   EJECT                                                                  
008200 01  FILLER                      PIC X(7)    VALUE 'DIVERSE'.             
008300 01  DIVERSE.                                                             
008400   03  DAGENS-DATUM              PIC S9(6).                               
008500   03  MEDTEXT                   PIC X(60)   VALUE SPACE.                 
008600   03  FELFLAGGA                 PIC X       VALUE 'N'.                   
008700   03  RAPP-KOLL                 PIC X.                                   
008800       88  RAPPORT-FINNS                     VALUE 'J'.                   
008900       88  RAPPORT-SAKNAS                    VALUE 'N'.                   
009000   03  INVENT-FINNS              PIC X       VALUE 'N'.                   
009100   03  UPPDATERING               PIC X       VALUE 'N'.                   
009200   03  WS-IDDC.                                                           
009300       05  WS-IDDC-1             PIC X(2)    VALUE 'DC'.                  
009400       05  WS-IDDC-2             PIC X(2).                                
009500     EJECT                                                                
009600 01  WS-PARAMETRAR.                                                       
009700   03  WS-KVINVBEG-1             PIC  X(1).                               
009800   03  WS-KVINVBEG-2             PIC  X(2).                               
009900   03  WS-ADLAGOMR-1             PIC  X(1).                               
010000   03  WS-ADGANG-1               PIC  X(1).                               
010100   03  WS-ADPLATS-1              PIC  X(1).                               
010200   03  WS-ADPLATS-2              PIC  X(2).                               
010300   03  WS-ADPLATS-3              PIC  X(3).                               
010400   03  WS-ADPLATS-4              PIC  X(4).                               
010500   03  WS-KDPRODSL-1             PIC  X(1).                               
010600   03  WS-IDFKNGRP-1             PIC  X(1).                               
010700   03  WS-IDFKNGRP-2             PIC  X(2).                               
010800   03  WS-IDFKNGRP-3             PIC  X(3).                               
010900 01  BMP-PARAMETRAR.                                                      
011000   03  SKICKA-KVINVBEG           PIC  9(3).                               
011100   03  SKICKA-KDVVKL             PIC  9(1).                               
011200   03  SKICKA-ADLAGOMR           PIC  9(2).                               
011300   03  SKICKA-ADGANG             PIC  9(2).                               
011400   03  SKICKA-ADPLATS            PIC  9(5).                               
011500   03  SKICKA-KDPRODSL           PIC  9(2).                               
011600   03  SKICKA-IDFKNGRP           PIC  9(4).                               
011700     EJECT                                                                
011800*                            WKPSKONV  PARAMETRAR                         
011900*01    -COPY WKPSAREA                                                     
012000     EJECT                                                                
012100******************************************************************        
012200*                                                                         
012300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500 01    IMS-WS.                                                            
012600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
012700     SKIP3                                                                
012800*                        **** STATUS-KOD FRÅN IMS                         
012900   03    STATUS-WS               PIC XX.                                  
013000     88    SEGMENT-FINNS                     VALUE '  '.                  
013100     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
013200     SKIP3                                                                
013300   03    GODK-STATUSKODER.                                                
013400     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
013500     SKIP3                                                                
013600 01    SSA1                      PIC X(64).                               
013700 01    SSA2                      PIC X(64).                               
013800     EJECT                                                                
013900*                            IMS FUNKTIONSKODER                           
014000*01    -COPY W0003                                                        
014100     EJECT                                                                
014200******************************************************************        
014300*                                                                         
014400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014500*                                                                         
014600 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
014700     SKIP3                                                                
014800*01    MID -COPY W5I30501.                                                
014900     EJECT                                                                
015000*01    -COPY WMSGAREA                                                     
015100     EJECT                                                                
015200*  03    MOD -COPY W5O30501  -RED MSG-AREA.                               
015300     EJECT                                                                
015400*01    -COPY WMFSAREA                                                     
015500     EJECT                                                                
015600 01  W-PROG-TO-PROG-SW.                                                   
015700*  03    -COPY WMSGSOP                                                    
015800     EJECT                                                                
015900 LINKAGE SECTION.                                                         
016000*01    -COPY W0009     -PRE MSG-                                          
016100     EJECT                                                                
016200*01    -COPY W0009     -PRE ALT-                                          
016300     EJECT                                                                
016400*01    -COPY W0008     -PRE USEA-                                         
016500     05  FILLER                  PIC X.                                   
016600     EJECT                                                                
016700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB.                      
016800 MAIN SECTION.                                                            
016900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB.                      
017000                                                                          
017100     PERFORM IMS-GET-MSG                                                  
017200     IF SEGMENT-FINNS                                                     
017300       PERFORM A-INIT-SPARA-INPUT                                         
017400       IF MFS-IDTRANS = '5305'                                            
017500         PERFORM B-KOLLA-BEFRAPP                                          
017600         IF MFS-UPDATE                                                    
017700           IF RAPPORT-FINNS                                               
017800             PERFORM C-KONTROLLERA-INDATA                                 
017900             IF MEDTEXT = SPACE                                           
020500                PERFORM S05-STARTA-BMP                                    
020600                MOVE MED-4 (DETTA-CL) TO MEDTEXT                          
020610                MOVE MEDTEXT TO MOD-TEMFSFEL                              
020700             ELSE                                                         
020710                PERFORM S04-SIGNAL-BILD                                   
020800             END-IF                                                       
020900           ELSE                                                           
021000             MOVE MED-1 (DETTA-CL) TO MEDTEXT                             
021010             PERFORM S04-SIGNAL-BILD                                      
021100           END-IF                                                         
021300         ELSE                                                             
021400           IF RAPPORT-FINNS                                               
021500             PERFORM C-KONTROLLERA-INDATA                                 
021600             IF MEDTEXT = SPACE                                           
021700               MOVE MED-2 (DETTA-CL) TO MEDTEXT                           
021800               PERFORM S03-ADD-LAES-IN                                    
021900             END-IF                                                       
022000             PERFORM S04-SIGNAL-BILD                                      
022100           END-IF                                                         
022200         END-IF                                                           
022300       END-IF                                                             
022400       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O30501 + 4                      
022500       PERFORM IMS-INSERT-MSG                                             
022600     END-IF                                                               
022700     MOVE ZERO TO RETURN-CODE                                             
022800     GOBACK                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 A-INIT-SPARA-INPUT SECTION.                                              
023200     IF MSG-DUBBLA-TRANSKODER                                             
023300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I30501                 
023400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023600       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
023700     ELSE                                                                 
023800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I30501                  
023900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
024000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024100       MOVE ' ' TO MFS-KDTRTYP                                            
024200     END-IF                                                               
024300     MOVE LOW-VALUE TO MSG-AREA                                           
024400     MOVE 'W5O30501' TO MFS-IDMOD                                         
024500     MOVE '5305' TO MOD-IDTRANS                                           
024600                                                                          
024700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024800     MOVE '001'             TO MSGI-KDCALL                                
024900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025000     MOVE '5305'            TO MSGI-IDTRANS                               
025100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025300                                                                          
025400     MOVE MSGI-IDDC         TO WS-IDDC-NUM                                
025500                               WS-IDDC-2                                  
025600                                                                          
025700     IF SWEDISH-TEXT                                                      
025800       MOVE +1 TO DETTA-CL                                                
025900     ELSE                                                                 
026000       MOVE +2 TO DETTA-CL                                                
026100     END-IF                                                               
026200                                                                          
026300     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
026400                                                                          
026500     MOVE WS-IDDC-NUM TO MOD-IDDC-UT                                      
026600     INSPECT MOD-IDDC-UT REPLACING LEADING SPACE BY ZERO                  
026700                                                                          
026800     PERFORM S01-RENSA-FAELT                                              
026900     ACCEPT  DAGENS-DATUM  FROM DATE                                      
027000     .                                                                    
027100     EJECT                                                                
027200 B-KOLLA-BEFRAPP   SECTION.                                               
027300*                                                                         
027400*  KONTROLLERA OM NÅGON RAPPORTERING HAR GJORTS                           
027500*                                                                         
027600     MOVE NEJ  TO RAPP-KOLL                                               
027700                                                                          
027800     IF MID-KVINVBEG        NOT = ALL '+' OR                              
027900        MID-KDVVKL          NOT = ALL '+' OR                              
028000        MID-ADLAGOMR        NOT = ALL '+' OR                              
028100        MID-ADGANG          NOT = ALL '+' OR                              
028200        MID-ADPLATS         NOT = ALL '+' OR                              
028300        MID-KDPRODSL        NOT = ALL '+' OR                              
028400        MID-IDFKNGRP        NOT = ALL '+'                                 
028500       MOVE JA TO RAPP-KOLL                                               
028600     END-IF                                                               
028700     .                                                                    
028800     EJECT                                                                
028900 C-KONTROLLERA-INDATA SECTION.                                            
029000*                                                                         
029100*  INDATAKONTROLL, FORMELLA KONTROLLER                                    
029200*                                                                         
029300     MOVE NEJ                         TO FELFLAGGA                        
029400                                                                          
029500     IF (MID-INV-RE0-GRP NOT = ALL '+')                                   
029600       IF MID-KVINVBEG  = ALL '+'                                         
029700         MOVE JA                      TO FELFLAGGA                        
029800         MOVE MFS-NUM-FAELT-FEL       TO MOD-KVINVBEG-ATTR                
029900       ELSE                                                               
030000         IF MID-KVINVBEG (2:1) = SPACE AND                                
030100            MID-KVINVBEG (3:1) = SPACE                                    
030200            MOVE MID-KVINVBEG TO WS-KVINVBEG-1                            
030300            MOVE WS-KVINVBEG-1 TO MID-KVINVBEG                            
030400         ELSE                                                             
030500         IF MID-KVINVBEG (3:1) = SPACE                                    
030600            MOVE MID-KVINVBEG TO WS-KVINVBEG-2                            
030700            MOVE WS-KVINVBEG-2 TO MID-KVINVBEG                            
030800         END-IF                                                           
030900         END-IF                                                           
031000         INSPECT MID-KVINVBEG REPLACING LEADING SPACE BY ZERO             
031100         IF MID-KVINVBEG NOT NUMERIC                                      
031200           MOVE JA                    TO FELFLAGGA                        
031300           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVINVBEG-ATTR                
031400         ELSE                                                             
031500           IF MID-KVINVBEG < 0                                            
031600             MOVE JA                  TO FELFLAGGA                        
031700             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVINVBEG-ATTR                
031800           ELSE                                                           
031900             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVINVBEG-ATTR                
032000             MOVE MID-KVINVBEG        TO SKICKA-KVINVBEG                  
032100           END-IF                                                         
032200         END-IF                                                           
032300       END-IF                                                             
032400       IF MID-KDVVKL NOT = ALL '+'                                        
032500         IF MID-KDVVKL NOT NUMERIC                                        
032600           MOVE JA                    TO FELFLAGGA                        
032700           MOVE MFS-NUM-FAELT-FEL     TO MOD-KDVVKL-ATTR                  
032800         ELSE                                                             
032900           IF MID-KDVVKL = 0 OR 1 OR 2 OR 3 OR 4 OR 5                     
033000             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDVVKL-ATTR                  
033100             MOVE MID-KDVVKL          TO SKICKA-KDVVKL                    
033200           ELSE                                                           
033300             MOVE JA                  TO FELFLAGGA                        
033400             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDVVKL-ATTR                  
033500           END-IF                                                         
033600         END-IF                                                           
033700       ELSE                                                               
033800         MOVE '0'                     TO SKICKA-KDVVKL                    
033900       END-IF                                                             
034000       IF MID-ADLAGOMR NOT = ALL '+'                                      
034100         IF MID-ADLAGOMR (2:1) = SPACE                                    
034200            MOVE MID-ADLAGOMR TO WS-ADLAGOMR-1                            
034300            MOVE WS-ADLAGOMR-1 TO MID-ADLAGOMR                            
034400         END-IF                                                           
034500         INSPECT MID-ADLAGOMR REPLACING LEADING SPACE BY ZERO             
034600         IF MID-ADLAGOMR NOT NUMERIC                                      
034700           MOVE JA                    TO FELFLAGGA                        
034800           MOVE MFS-NUM-FAELT-FEL     TO MOD-ADLAGOMR-ATTR                
034900         ELSE                                                             
035000           MOVE MFS-NUM-FAELT-RAETT   TO MOD-ADLAGOMR-ATTR                
035100           MOVE MID-ADLAGOMR          TO SKICKA-ADLAGOMR                  
035200         END-IF                                                           
035300       ELSE                                                               
035400         MOVE '00'                    TO SKICKA-ADLAGOMR                  
035500       END-IF                                                             
035600       IF MID-ADGANG NOT = ALL '+'                                        
035700         IF MID-ADGANG (2:1) = SPACE                                      
035800            MOVE MID-ADGANG TO WS-ADGANG-1                                
035900            MOVE WS-ADGANG-1 TO MID-ADGANG                                
036000         END-IF                                                           
036100         INSPECT MID-ADGANG REPLACING LEADING SPACE BY ZERO               
036200         IF MID-ADGANG NOT = '00'                                         
036300           IF MID-ADGANG NOT NUMERIC                                      
036400             MOVE JA                  TO FELFLAGGA                        
036500             MOVE MFS-NUM-FAELT-FEL   TO MOD-ADGANG-ATTR                  
036600           ELSE                                                           
036700             MOVE MFS-NUM-FAELT-RAETT TO MOD-ADGANG-ATTR                  
036800             MOVE MID-ADGANG          TO SKICKA-ADGANG                    
036900           END-IF                                                         
037000         ELSE                                                             
037100           MOVE '00'                    TO SKICKA-ADGANG                  
037200         END-IF                                                           
037300       ELSE                                                               
037400         MOVE '00'                      TO SKICKA-ADGANG                  
037500       END-IF                                                             
037600       IF MID-ADPLATS NOT = ALL '+'                                       
037700         IF MID-ADPLATS (2:1) = SPACE AND                                 
037800            MID-ADPLATS (3:1) = SPACE AND                                 
037900            MID-ADPLATS (4:1) = SPACE AND                                 
038000            MID-ADPLATS (5:1) = SPACE                                     
038100            MOVE MID-ADPLATS TO WS-ADPLATS-1                              
038200            MOVE WS-ADPLATS-1 TO MID-ADPLATS                              
038300         ELSE                                                             
038400         IF MID-ADPLATS (3:1) = SPACE AND                                 
038500            MID-ADPLATS (4:1) = SPACE AND                                 
038600            MID-ADPLATS (5:1) = SPACE                                     
038700            MOVE MID-ADPLATS TO WS-ADPLATS-2                              
038800            MOVE WS-ADPLATS-2 TO MID-ADPLATS                              
038900         ELSE                                                             
039000         IF MID-ADPLATS (4:1) = SPACE AND                                 
039100            MID-ADPLATS (5:1) = SPACE                                     
039200            MOVE MID-ADPLATS TO WS-ADPLATS-3                              
039300            MOVE WS-ADPLATS-3 TO MID-ADPLATS                              
039400         ELSE                                                             
039500         IF MID-ADPLATS (5:1) = SPACE                                     
039600            MOVE MID-ADPLATS TO WS-ADPLATS-4                              
039700            MOVE WS-ADPLATS-4 TO MID-ADPLATS                              
039800         END-IF                                                           
039900         END-IF                                                           
040000         END-IF                                                           
040100         END-IF                                                           
040200         INSPECT MID-ADPLATS REPLACING LEADING SPACE BY ZERO              
040300         IF MID-ADPLATS NOT = '00000'                                     
040400           IF MID-ADPLATS NOT NUMERIC                                     
040500             MOVE JA                  TO FELFLAGGA                        
040600             MOVE MFS-NUM-FAELT-FEL   TO MOD-ADPLATS-ATTR                 
040700           ELSE                                                           
040800             MOVE MFS-NUM-FAELT-RAETT TO MOD-ADPLATS-ATTR                 
040900             MOVE MID-ADPLATS         TO SKICKA-ADPLATS                   
041000           END-IF                                                         
041100         ELSE                                                             
041200           MOVE '00000'                 TO SKICKA-ADPLATS                 
041300         END-IF                                                           
041400       ELSE                                                               
041500         MOVE '00000'                 TO SKICKA-ADPLATS                   
041600       END-IF                                                             
041700       IF MID-KDPRODSL NOT = ALL '+'                                      
041800         IF MID-KDPRODSL (2:1) = SPACE                                    
041900            MOVE MID-KDPRODSL TO WS-KDPRODSL-1                            
042000            MOVE WS-KDPRODSL-1 TO MID-KDPRODSL                            
042100         END-IF                                                           
042200         INSPECT MID-KDPRODSL REPLACING LEADING SPACE BY ZERO             
042300         IF MID-KDPRODSL NOT NUMERIC                                      
042400           MOVE JA                    TO FELFLAGGA                        
042500           MOVE MFS-NUM-FAELT-FEL     TO MOD-KDPRODSL-ATTR                
042600         ELSE                                                             
042700           MOVE MID-KDPRODSL          TO KPS-KDPRODSL                     
042800           MOVE 002                   TO KPS-KDCALL                       
042900           MOVE SPACE                 TO KPS-FLPRODSL                     
043000           CALL WKPSKONV USING KPS-WKPSAREA                               
043100***** TEST OM PRODUKTSLAG EXISTERAR OCH ÄR GILTIGT NU                     
043200           IF KPS-FLPRODSL = JA                                           
043300             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-ATTR                
043400             MOVE MID-KDPRODSL        TO SKICKA-KDPRODSL                  
043500           ELSE                                                           
043600             MOVE JA                  TO FELFLAGGA                        
043700             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-ATTR                
043800           END-IF                                                         
043900         END-IF                                                           
044000       ELSE                                                               
044100         MOVE '00'                    TO SKICKA-KDPRODSL                  
044200       END-IF                                                             
044300       IF MID-IDFKNGRP NOT = ALL '+'                                      
044400         IF MID-IDFKNGRP (2:1) = SPACE AND                                
044500            MID-IDFKNGRP (3:1) = SPACE AND                                
044600            MID-IDFKNGRP (4:1) = SPACE                                    
044700            MOVE MID-IDFKNGRP TO WS-IDFKNGRP-1                            
044800            MOVE WS-IDFKNGRP-1 TO MID-IDFKNGRP                            
044900         ELSE                                                             
045000         IF MID-IDFKNGRP (3:1) = SPACE AND                                
045100            MID-IDFKNGRP (4:1) = SPACE                                    
045200            MOVE MID-IDFKNGRP TO WS-IDFKNGRP-2                            
045300            MOVE WS-IDFKNGRP-2 TO MID-IDFKNGRP                            
045400         ELSE                                                             
045500         IF MID-IDFKNGRP (4:1) = SPACE                                    
045600            MOVE MID-IDFKNGRP TO WS-IDFKNGRP-3                            
045700            MOVE WS-IDFKNGRP-3 TO MID-IDFKNGRP                            
045800         END-IF                                                           
045900         END-IF                                                           
046000         END-IF                                                           
046100         INSPECT MID-IDFKNGRP REPLACING LEADING SPACE BY ZERO             
046200         IF MID-IDFKNGRP NOT NUMERIC                                      
046300           MOVE JA                    TO FELFLAGGA                        
046400           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDFKNGRP-ATTR                
046500         ELSE                                                             
046600           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDFKNGRP-ATTR                
046700           MOVE MID-IDFKNGRP          TO SKICKA-IDFKNGRP                  
046800         END-IF                                                           
046900       ELSE                                                               
047000         MOVE '0000'                  TO SKICKA-IDFKNGRP                  
047100       END-IF                                                             
047200     END-IF                                                               
049000                                                                          
049100     IF FELFLAGGA = JA                                                    
049200       MOVE MED-3 (DETTA-CL) TO MEDTEXT                                   
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 S01-RENSA-FAELT  SECTION.                                                
049700*                                                                         
049800*  RENSA RAPPORTERINGSFÄLT FRÅN BILDEN                                    
049900*                                                                         
050000     MOVE MFS-RENSA-FAELT  TO MOD-KVINVBEG-IN                             
050100                              MOD-KDVVKL-IN                               
050200                              MOD-ADLAGOMR-IN                             
050300                              MOD-ADGANG-IN                               
050400                              MOD-ADPLATS-IN                              
050500                              MOD-KDPRODSL-IN                             
050600                              MOD-IDFKNGRP-IN                             
050700     .                                                                    
050800     EJECT                                                                
050900 S03-ADD-LAES-IN  SECTION.                                                
051000*                                                                         
051100*  LÄS IN RAPPORTERINGSFÄLTEN IGEN                                        
051200*                                                                         
051300     IF MID-KVINVBEG NOT = ALL '+'                                        
051400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVINVBEG-ATTR                    
051500     END-IF                                                               
051600     IF MID-KDVVKL NOT = ALL '+'                                          
051700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDVVKL-ATTR                      
051800     END-IF                                                               
051900     IF MID-ADLAGOMR NOT = ALL '+'                                        
052000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLAGOMR-ATTR                    
052100     END-IF                                                               
052200     IF MID-ADGANG NOT = ALL '+'                                          
052300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADGANG-ATTR                      
052400     END-IF                                                               
052500     IF MID-ADPLATS NOT = ALL '+'                                         
052600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADPLATS-ATTR                     
052700     END-IF                                                               
052800     IF MID-KDPRODSL NOT = ALL '+'                                        
052900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-ATTR                    
053000     END-IF                                                               
053100     IF MID-IDFKNGRP NOT = ALL '+'                                        
053200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-ATTR                    
053300     END-IF                                                               
053400     .                                                                    
053500     EJECT                                                                
053600 S04-SIGNAL-BILD  SECTION.                                                
053700*                                                                         
053800*  HÅLL KVAR FÄLTEN PÅ BILDEN VID FELSIGNAL                               
053900*                                                                         
054000     MOVE MFS-ROER-EJ-FAELT TO MOD-KVINVBEG-IN                            
054100     MOVE MFS-ROER-EJ-FAELT TO MOD-KDVVKL-IN                              
054200     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-IN                            
054300     MOVE MFS-ROER-EJ-FAELT TO MOD-ADGANG-IN                              
054400     MOVE MFS-ROER-EJ-FAELT TO MOD-ADPLATS-IN                             
054500     MOVE MFS-ROER-EJ-FAELT TO MOD-KDPRODSL-IN                            
054600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-IN                            
054700                                                                          
054800     MOVE MEDTEXT TO MOD-TEMFSFEL                                         
054900     .                                                                    
055000     EJECT                                                                
055100 S05-STARTA-BMP SECTION.                                                  
055200*                                                                         
055300*  STARTA BMP W513B1                                                      
055400*                                                                         
055500     MOVE '5305'       TO MSGSOP-IDTRANS                                  
055600     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
055700     MOVE 'W513B1'     TO MSGSOP-IDPROCESS                                
055800     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
055900                                                                          
056000     STRING 'IDDC(' WS-IDDC ') KVINVBEG(' SKICKA-KVINVBEG ') KDVVK        
056100-           'L(' SKICKA-KDVVKL ') ADLAGOMR(' SKICKA-ADLAGOMR ') AD        
056200-           'GANG(' SKICKA-ADGANG ') ADPLATS(' SKICKA-ADPLATS ') K        
056300-           'DPRODSL(' SKICKA-KDPRODSL ')                                 
056400-           'IDFKNGRP(' SKICKA-IDFKNGRP ')'                               
056600          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
056700     PERFORM IMS-INSERT-ALTMSG                                            
056800     .                                                                    
056900     EJECT                                                                
057000 IMS-GET-MSG SECTION.                                                     
057100     MOVE '  QC' TO GODK-STATUSKODER                                      
057200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
057300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057400     PERFORM IMS-STATUSKONTROLL                                           
057500     .                                                                    
057600     SKIP3                                                                
057700 IMS-INSERT-MSG SECTION.                                                  
057800     IF ENGLISH-TEXT                                                      
057900       MOVE 'N' TO MFS-KDHUVOMR                                           
058000     END-IF                                                               
058100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
058200     MOVE +301 TO MSG-KVLL                                                
058300     MOVE SPACE TO GODK-STATUSKODER                                       
058400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
058500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058600     PERFORM IMS-STATUSKONTROLL                                           
058700     .                                                                    
058800     EJECT                                                                
058900 IMS-INSERT-ALTMSG SECTION.                                               
059000     MOVE '  ' TO GODK-STATUSKODER                                        
059100     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
059200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     EJECT                                                                
059600 IMS-STATUSKONTROLL SECTION.                                              
059700                                                                          
059800     SET STATUS-IX TO 1                                                   
059900     SEARCH GODK-STATUS AT END CALL FELLOG                                
060000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
060100     END-SEARCH                                                           
060200     .                                                                    
