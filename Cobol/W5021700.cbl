001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021700.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/07/02.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET HANTERAR NYREGISTRERING AV EKONOMISK                  
002100*        KLIENT M.H.A INMATAD DATA FRÅN BILD 5217                         
002200*                                                                         
002310*        PROGRAMMET LÄSER/UPPDATERAR WDH5                                 
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T217                                              
002700*        MID:         W5I21701                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W5O21701                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W5021700'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004501*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004502 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004510 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004901     88  ALLT-OK                             VALUE 'J'.                   
004902                                                                          
004903 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004904     88  INDATA-OK                           VALUE 'J'.                   
004910     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '5217'.                
005700     88  GODK-MID                            VALUE '5211' '5212'          
005800                                                   '5213' '5214'          
005900                                                   '5215' '5216'          
006000                                                   '5217' '5218'          
006100                                                   '5219'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007601     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007602     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007603     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007610     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007701     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007710     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007810     03  RAD-FINNS-REDAN         PIC X(3)    VALUE '245'.                 
007900     03  KDEKHHT-MISSING         PIC X(3)    VALUE '269'.                 
007910     03  KDEKSHT-MISSING         PIC X(3)    VALUE '270'.                 
007920     03  KDEKNIVA-MISSING        PIC X(3)    VALUE '271'.                 
007930     03  SYSTNAME-MISSING        PIC X(3)    VALUE '272'.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008601     EJECT                                                                
008602*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008603*                                                                         
008604 01  SPAR-AREA.                                                           
008605     03  SPAR-IDTRANS           PIC X(4)    VALUE '5217'.                 
008607     03  SPAR-IDFTG-NEXT        PIC 9(2).                                 
008608     03  SPAR-KDEKHHT-NEXT      PIC X(3).                                 
008609     03  SPAR-KDEKSHT-NEXT      PIC X(3).                                 
008611     03  SPAR-KDEKNIVA-NEXT     PIC X(5).                                 
008612     03  SPAR-IDSYSMOT-NEXT     PIC X(6).                                 
008613     03  SPAR-IDPTYP-NEXT       PIC X(3).                                 
008700     EJECT                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W5I21701                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W5O21701                                                 
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200     SKIP3                                                                
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011101*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011117     03  W-WDH501KY-X.                                                    
011118         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011119         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
011120     03  W-KDEKSHT-X.                                                     
011121         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
011122     03  W-KDEKNIVA-X.                                                    
011123         05  W-KDEKNIVA          PIC X(5)    VALUE SPACE.                 
011124     03  W-WDH531KY-X.                                                    
011125         05  W-IDSYSMOT          PIC X(6)    VALUE SPACE.                 
011126         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
011127         05  W-IDSEKVNR          PIC S9(3)   COMP-3 VALUE +1.             
011200     SKIP2                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011610     88  SEGMENT-FINNS-REDAN-2               VALUE 'NI'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012310 01  SSA3                        PIC X(64).                               
012320 01  SSA4                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000                                                                          
013101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
013102 01  DLI-IO-WDH501.                                                       
013103*    03  -COPY WDH501                                                     
013104     EJECT                                                                
013105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
013106 01  DLI-IO-WDH511.                                                       
013107*    03  -COPY WDH511                                                     
013108     EJECT                                                                
013109 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
013110 01  DLI-IO-WDH521.                                                       
013111*    03  -COPY WDH521                                                     
013112     EJECT                                                                
013113 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
013114 01  DLI-IO-WDH531.                                                       
013120*    03  -COPY WDH531                                                     
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013700*01  -COPY W0008   -PRE USEA-                                             
013800     05  FILLER                  PIC X.                                   
013901                                                                          
013902*01  -COPY W0008  -PRE WDH5-                                              
013910     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014101 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDH5-PCB.                     
014102 MAIN SECTION.                                                            
014110     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDH5-PCB.                     
014200                                                                          
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600       PERFORM A-INIT                                                     
014700       PERFORM B-KOLLA-NYCKLAR                                            
014800       IF NYCKLAR-OK                                                      
014901         IF MFS-UPDATE                                                    
014902           PERFORM G-KOLLA-INPUT                                          
014903           IF INDATA-OK                                                   
014904             PERFORM H-UPPDATERA                                          
014905           END-IF                                                         
014910         ELSE                                                             
015001           IF MFS-FIRST                                                   
015002             PERFORM C-FOERSTA-SIDA                                       
015003           ELSE                                                           
015004             IF MFS-NEXT                                                  
015005               PERFORM D-NAESTA-SIDA                                      
015006             ELSE                                                         
015007               PERFORM E-SAMMA-SIDA                                       
015008             END-IF                                                       
015010           END-IF                                                         
015210         END-IF                                                           
015220         IF ALLT-OK                                                       
015300           PERFORM F-LAES-VISA-INFO                                       
015310         END-IF                                                           
015400       END-IF                                                             
015700       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21701 + 4                      
015800       PERFORM IMS-INSERT-MSG                                             
015900     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     IF MSG-DUBBLA-TRANSKODER                                             
016900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21701                 
017000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017200     ELSE                                                                 
017300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21701                  
017400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018100                                                                          
018200     MOVE LOW-VALUE  TO MSG-AREA                                          
018300     MOVE 'W5O217N1' TO MFS-IDMOD                                         
018400     MOVE '5217'     TO MOD-IDTRANS                                       
018500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018600                                                                          
018700     IF EGEN-MID OR HELP-MID                                              
018800       CONTINUE                                                           
018900     ELSE                                                                 
019000       MOVE SPACE TO MFS-KDTRTYP                                          
019100       MOVE '7' TO MFS-IDPFK                                              
019200     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 B-KOLLA-NYCKLAR SECTION.                                                 
019800                                                                          
019900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020000     MOVE '001'             TO MSGI-KDCALL                                
020100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020300     MOVE '5217'            TO MSGI-IDTRANS                               
020400     IF EGEN-MID                                                          
020500       MOVE MID-KDEKHHT-IN  TO MSGI-KDEKHHT                               
020510       MOVE MID-KDEKSHT-IN  TO MSGI-KDEKSHT                               
020520       MOVE MID-KDEKNIVA-IN TO MSGI-KDEKNIVA                              
021000     END-IF                                                               
021100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021200     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
021210                                                                          
021220     IF MSGI-IDLAND-SPR = 'GB'                                            
021230       MOVE 'GB' TO MED-IDSKYLT                                           
021240     ELSE                                                                 
021250       MOVE 'S' TO MED-IDSKYLT                                            
021260     END-IF                                                               
021300                                                                          
021400     MOVE JA TO ALLT-SW                                                   
021500     MOVE JA TO NYCKLAR-SW                                                
021510     MOVE SPACE TO MED-IDMFSFEL                                           
021520     MOVE SPACE TO MED-IDMFSINF                                           
021600                                                                          
021610     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021620       MOVE MSGI-IDFTG      TO W-IDFTG                                    
021630     END-IF                                                               
021700                                                                          
021800*    -- KONTROLL AV KDEKHHT                                               
021900     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
022000                                                                          
022060     IF MSGI-KDEKHHT NUMERIC AND MSGI-KDEKHHT > ZERO                      
022070       MOVE MSGI-KDEKHHT    TO W-KDEKHHT                                  
022080     ELSE                                                                 
022090       MOVE NEJ             TO NYCKLAR-SW                                 
022091     END-IF                                                               
022092                                                                          
022093*    -- KONTROLL AV KDEKSHT                                               
022094     MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT-IN                               
022095                                                                          
022101     IF MSGI-KDEKSHT NOT = ALL '+'                                        
022102       MOVE MSGI-KDEKSHT    TO W-KDEKSHT                                  
022103     ELSE                                                                 
022104       MOVE NEJ             TO NYCKLAR-SW                                 
022105     END-IF                                                               
022106                                                                          
022107*    -- KONTROLL AV KDEKNIVA                                              
022108     MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-IN                              
022109                                                                          
022114     IF MSGI-KDEKNIVA NOT = ALL '+'                                       
022116       MOVE MSGI-KDEKNIVA   TO W-KDEKNIVA                                 
022117     ELSE                                                                 
022118       MOVE NEJ             TO NYCKLAR-SW                                 
022119     END-IF                                                               
022121                                                                          
022122     IF GODK-MID OR NYCKLAR-OK                                            
022123       MOVE MSGI-IDFTG          TO MOD-IDFTG-UT                           
022124       MOVE MSGI-KDEKHHT        TO MOD-KDEKHHT-UT                         
022125       INSPECT MOD-KDEKHHT-UT REPLACING LEADING ZERO BY SPACE             
022126       MOVE MSGI-KDEKSHT        TO MOD-KDEKSHT-UT                         
022127       INSPECT MOD-KDEKHHT-UT REPLACING LEADING ZERO BY SPACE             
022128       MOVE MSGI-KDEKNIVA       TO MOD-KDEKNIVA-UT                        
022131     ELSE                                                                 
022132       MOVE MFS-RENSA-FAELT TO MOD-IDFTG-UT                               
022133                               MOD-KDEKHHT-UT                             
022134                               MOD-KDEKSHT-UT                             
022135                               MOD-KDEKNIVA-UT                            
022136     END-IF                                                               
022137                                                                          
022138     IF NYCKLAR-FEL                                                       
022139*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
022140*---GODKÄND BILD                                                          
022141       IF GODK-MID                                                        
022150         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
022160         CALL WMEDKONV USING MED-WMEDAREA                                 
022170         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022180         PERFORM MFS-RENSA-FAELT-IN                                       
022190         PERFORM MFS-RENSA-FAELT-UT                                       
022191       END-IF                                                             
022192     END-IF                                                               
022200     .                                                                    
022201     EJECT                                                                
022202 C-FOERSTA-SIDA SECTION.                                                  
022203                                                                          
022204     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
022205     CALL WMEDKONV USING MED-WMEDAREA                                     
022206     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
022207                                                                          
022208     PERFORM MFS-RENSA-FAELT-IN                                           
022209     .                                                                    
022210     EJECT                                                                
022211 D-NAESTA-SIDA SECTION.                                                   
022212                                                                          
022213     IF SPAR-IDTRANS = '5217'                                             
022214       MOVE SPAR-IDFTG-NEXT    TO W-IDFTG                                 
022215       MOVE SPAR-KDEKHHT-NEXT  TO W-KDEKHHT                               
022216       MOVE SPAR-KDEKSHT-NEXT  TO W-KDEKSHT                               
022217       MOVE SPAR-KDEKNIVA-NEXT TO W-KDEKNIVA                              
022218       MOVE SPAR-IDSYSMOT-NEXT TO W-IDSYSMOT                              
022219       MOVE SPAR-IDPTYP-NEXT   TO W-IDPTYP                                
022220     ELSE                                                                 
022221       PERFORM MFS-RENSA-FAELT-IN                                         
022222     END-IF                                                               
022223     .                                                                    
022224     EJECT                                                                
022225 E-SAMMA-SIDA SECTION.                                                    
022226                                                                          
022227     IF SPAR-IDTRANS = '5217' OR '0551'                                   
022228*---OM MID-KDEKHHT-IN ÄNDRATS OCH 'ENTER' AKTIVERATS                      
022229*---MÅSTE FÖLJANDE IF-SATS ANVÄNDAS FÖR ATT FÅ UT NÅGON DATA              
022230       IF MID-IDSYSMOT-NY NOT = ALL '+'                                   
022231        OR  MID-IDPTYP-NY NOT = ALL '+'                                   
022236         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
022237         CALL WMEDKONV USING MED-WMEDAREA                                 
022238         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022239         PERFORM MFS-ROER-EJ-FAELT-IN                                     
022240         PERFORM MFS-LAES-IN-IGEN                                         
022241         MOVE NEJ TO ALLT-SW                                              
022242       END-IF                                                             
022243     ELSE                                                                 
022244       PERFORM MFS-RENSA-FAELT-IN                                         
022245     END-IF                                                               
022246     .                                                                    
022247     EJECT                                                                
022500 F-LAES-VISA-INFO SECTION.                                                
023559                                                                          
023560     PERFORM IMS-GU-HHT                                                   
023561                                                                          
023564     IF SEGMENT-SAKNAS                                                    
023565        IF MED-IDMFSFEL = SPACE                                           
023566          MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                            
023567        END-IF                                                            
023568        CALL WMEDKONV USING MED-WMEDAREA                                  
023569        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023570        PERFORM MFS-RENSA-FAELT-UT                                        
023571     ELSE                                                                 
023572       MOVE HHT-IDFTG   TO SPAR-IDFTG-NEXT                                
023573       MOVE HHT-KDEKHHT TO MOD-KDEKHHT                                    
023574                           MOD-KDEKHHT-NY                                 
023575                           SPAR-KDEKHHT-NEXT                              
023576       MOVE HHT-BEEKHHT TO MOD-BEEKHHT                                    
023578                           MOD-BEEKHHT-NY                                 
023582       PERFORM FA-LAES-VISA-SHT                                           
023583       PERFORM FB-LAES-VISA-NIVA                                          
023584     END-IF                                                               
023595                                                                          
023596     MOVE '002'     TO MSGI-KDCALL                                        
023597     MOVE '5217'    TO SPAR-IDTRANS                                       
023598     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
023599     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023700     .                                                                    
023800     EJECT                                                                
023900 FA-LAES-VISA-SHT SECTION.                                                
023910                                                                          
024000       PERFORM IMS-GU-SHT                                                 
024010                                                                          
024100       IF SEGMENT-SAKNAS                                                  
024110          IF MED-IDMFSFEL = SPACE                                         
024200            MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                          
024210          END-IF                                                          
024300          CALL WMEDKONV USING MED-WMEDAREA                                
024400          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
024500          PERFORM MFS-RENSA-FAELT-UT                                      
024600       ELSE                                                               
024700         MOVE SHT-KDEKSHT TO MOD-KDEKSHT                                  
024701                             MOD-KDEKSHT-NY                               
024702                             SPAR-KDEKSHT-NEXT                            
024703         MOVE SHT-BEEKSHT TO MOD-BEEKSHT                                  
024706                             MOD-BEEKSHT-NY                               
024710       END-IF                                                             
024711     .                                                                    
024712     EJECT                                                                
024713 FB-LAES-VISA-NIVA SECTION.                                               
024714                                                                          
024717     PERFORM IMS-GU-NIVA                                                  
024719     IF SEGMENT-SAKNAS                                                    
024720       IF MED-IDMFSFEL = SPACE                                            
024721         MOVE KDEKNIVA-MISSING TO MED-IDMFSFEL                            
024722       END-IF                                                             
024724       CALL WMEDKONV USING MED-WMEDAREA                                   
024725       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024726       PERFORM MFS-RENSA-FAELT-UT                                         
024727     ELSE                                                                 
024730       MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA                                 
024732                             MOD-KDEKNIVA-NY                              
024733                             SPAR-KDEKNIVA-NEXT                           
024736       PERFORM FBA-LAES-VISA-SYST                                         
024741     END-IF                                                               
024743     .                                                                    
024744     EJECT                                                                
024745 FBA-LAES-VISA-SYST SECTION.                                              
024746*---MFS-NEXT ANVÄNDS NÄR MAN SKALL LÄSA NÄSTA SIDAS POSTER                
024747*---1:A POSTEN LÄSES DÅ UNIKT                                             
024748     IF MFS-NEXT                                                          
024749       PERFORM IMS-GU-SYST                                                
024751     ELSE                                                                 
024771       PERFORM IMS-GNP-SYST                                               
024774     END-IF                                                               
024775     IF SEGMENT-SAKNAS                                                    
024776        IF MED-IDMFSFEL = SPACE                                           
024777          MOVE SYSTNAME-MISSING TO MED-IDMFSFEL                           
024778        END-IF                                                            
024779        CALL WMEDKONV USING MED-WMEDAREA                                  
024780        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024781*       PERFORM MFS-RENSA-FAELT-UT                                        
024782     END-IF                                                               
024783     MOVE +1 TO INDX                                                      
024785     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                      
024786       IF SEGMENT-FINNS                                                   
024787         MOVE SYST-IDSYSMOT  TO MOD-IDSYSMOT (INDX)                       
024788         MOVE SYST-IDPTYP    TO MOD-IDPTYP   (INDX)                       
024789       ELSE                                                               
024790         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
024791       END-IF                                                             
024800       ADD +1 TO INDX                                                     
024801         PERFORM IMS-GNP-SYST                                             
024804     END-PERFORM                                                          
024805                                                                          
024806     IF SEGMENT-FINNS                                                     
024807       MOVE SYST-IDSYSMOT TO SPAR-IDSYSMOT-NEXT                           
024808       MOVE SYST-IDPTYP   TO SPAR-IDPTYP-NEXT                             
024809       IF MED-IDMFSINF = SPACE                                            
024810         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
024811         CALL WMEDKONV USING MED-WMEDAREA                                 
024812       END-IF                                                             
024813       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
024814     END-IF                                                               
024815     .                                                                    
024816     EJECT                                                                
024817 G-KOLLA-INPUT SECTION.                                                   
024818                                                                          
024819     MOVE JA  TO INDATA-SW                                                
024820     IF MID-KDEKHHT-IN = ALL '+'                                          
024821      AND MID-KDEKSHT-IN = ALL '+'                                        
024822      AND MID-KDEKNIVA-IN = ALL '+'                                       
024823      AND MID-IDSYSMOT-NY = ALL '+'                                       
024824      AND MID-IDPTYP-NY = ALL '+'                                         
024825       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024826       CALL WMEDKONV USING MED-WMEDAREA                                   
024827       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024828       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024829       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024830       MOVE NEJ TO INDATA-SW                                              
024831     ELSE                                                                 
024832                                                                          
024833*---IDSYSMOT FÅR EJ VARA TOMT                                             
024834       IF MID-IDSYSMOT-NY = ALL '+' OR SPACE                              
024835          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSYSMOT-NY-ATTR                 
024836          MOVE NEJ TO INDATA-SW                                           
024837       ELSE                                                               
024838          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSYSMOT-NY-ATTR               
024839       END-IF                                                             
024840                                                                          
024841*---IDPTYP FÅR EJ VARA TOMT                                               
024842       IF MID-IDPTYP-NY = ALL '+' OR SPACE                                
024843          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPTYP-NY-ATTR                   
024844          MOVE NEJ TO INDATA-SW                                           
024845       ELSE                                                               
024846          MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPTYP-NY-ATTR                 
024847       END-IF                                                             
024848                                                                          
024849       IF INDATA-FEL                                                      
024850         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024851         CALL WMEDKONV USING MED-WMEDAREA                                 
024852         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024853         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024854         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024855       ELSE                                                               
024856*---KOLLAR ATT KDEKHHT FINNS REGISTRERAD I BASEN                          
024857         PERFORM IMS-GU-HHT                                               
024858         IF SEGMENT-SAKNAS                                                
024859           IF MED-IDMFSFEL = SPACE                                        
024860             MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                         
024861           END-IF                                                         
024862         ELSE                                                             
024870*---KOLLAR ATT KDEKSHT FINNS REGISTRERAD I BASEN                          
024871           PERFORM IMS-GU-SHT                                             
024872           IF SEGMENT-SAKNAS                                              
024873             IF MED-IDMFSFEL = SPACE                                      
024874               MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                       
024875             END-IF                                                       
024876           ELSE                                                           
024877*---KOLLAR ATT KDEKNIVA FINNS REGISTRERAD I BASEN                         
024878             PERFORM IMS-GU-NIVA                                          
024879             IF SEGMENT-SAKNAS                                            
024880               IF MED-IDMFSFEL = SPACE                                    
024881                 MOVE KDEKNIVA-MISSING TO MED-IDMFSFEL                    
024882               END-IF                                                     
024883             END-IF                                                       
024892           END-IF                                                         
024893         END-IF                                                           
024894       END-IF                                                             
024895     END-IF                                                               
024896                                                                          
024897     IF SEGMENT-SAKNAS                                                    
024898       CALL WMEDKONV USING MED-WMEDAREA                                   
024899       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024900       PERFORM MFS-RENSA-FAELT-IN                                         
024901       PERFORM MFS-RENSA-FAELT-UT                                         
024902*---OM SEGMENT SAKNAS KAN INGEN UPPDATERING SKE                           
024903*---DÄRFÖR ANVÄNDS DENNA SWITCH                                           
024904       MOVE NEJ TO INDATA-SW                                              
024905*---OM INTE SEGMENT FINNS BEHÖVER MAN EJ GÅ IN I                          
024906*---F-SECTIONEN. DÄRFÖR ANVÄNDS DENNA SWITCH                              
024907       MOVE NEJ TO ALLT-SW                                                
024908     END-IF                                                               
024909     .                                                                    
024910     EJECT                                                                
024911 H-UPPDATERA SECTION.                                                     
024912                                                                          
024913     MOVE MID-IDSYSMOT-NY TO SYST-IDSYSMOT                                
024914     MOVE MID-IDPTYP-NY   TO SYST-IDPTYP                                  
024915     MOVE 1               TO SYST-IDSEKVNR                                
024916     MOVE ZERO            TO SYST-IDKONTO                                 
024917     MOVE SPACE           TO SYST-KDPOST                                  
024918                             SYST-IDKST                                   
024919     MOVE SPACE           TO SYST-KDDOKTYP                                
024920     MOVE SPACE           TO SYST-IDANALYS                                
024921     MOVE SPACE           TO SYST-KDANALYS                                
024922     MOVE SPACE           TO SYST-FLALLOC                                 
024923     MOVE SPACE           TO SYST-FLPRSEGM                                
024924     PERFORM IMS-ISRT-SYST                                                
024925     IF SEGMENT-FINNS-REDAN                                               
024926       MOVE RAD-FINNS-REDAN TO MED-IDMFSINF                               
024927     ELSE                                                                 
024928       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
024929     END-IF                                                               
024930                                                                          
024931     CALL WMEDKONV USING MED-WMEDAREA                                     
024932     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
024933     PERFORM MFS-FORM-ATTR                                                
024934     PERFORM MFS-RENSA-FAELT-IN                                           
024935     MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDSYSMOT-NY-ATTR                    
024936     .                                                                    
024937     EJECT                                                                
024940 MFS-RENSA-FAELT-UT SECTION.                                              
025000                                                                          
025100*    --- ALLA UTDATA-FÄLT                                                 
025210*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025220     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-NY                               
025240                             MOD-BEEKHHT-NY                               
025250                             MOD-KDEKSHT-NY                               
025260                             MOD-BEEKSHT-NY                               
025261                             MOD-KDEKNIVA-NY                              
025262                             MOD-IDSYSMOT-NY                              
025263                             MOD-IDPTYP-NY                                
025264                             MOD-KDEKHHT                                  
025265                             MOD-BEEKHHT                                  
025266                             MOD-KDEKSHT                                  
025267                             MOD-BEEKSHT                                  
025268                             MOD-KDEKNIVA                                 
025270     MOVE +1 TO INDX                                                      
025280     PERFORM UNTIL INDX > MAX-INDX                                        
025290       MOVE MFS-RENSA-FAELT TO MOD-IDSYSMOT (INDX)                        
025296                               MOD-IDPTYP  (INDX)                         
025297     ADD +1 TO INDX                                                       
025298     END-PERFORM                                                          
025500     .                                                                    
025601     SKIP3                                                                
025602 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025603                                                                          
025604*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025605     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT                                  
025606                             MOD-KDEKSHT                                  
025607                             MOD-KDEKNIVA                                 
025608                             MOD-IDSYSMOT (INDX)                          
025609                             MOD-IDPTYP (INDX)                            
025610     .                                                                    
025700     SKIP3                                                                
025800 MFS-RENSA-FAELT-IN SECTION.                                              
025900                                                                          
026000*    --- ALLA INDATA-FÄLT                                                 
026100     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
026200                             MOD-KDEKSHT-IN                               
026210                             MOD-KDEKNIVA-IN                              
026220                             MOD-IDSYSMOT-NY                              
026230                             MOD-IDPTYP-NY                                
026300     .                                                                    
026400     EJECT                                                                
026500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026600                                                                          
026700*    --- ALLA UTDATA-FÄLT                                                 
026810*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
026820     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFTG-UT                               
026821                               MOD-KDEKHHT-UT                             
026830                               MOD-KDEKSHT-UT                             
026831                               MOD-KDEKNIVA-UT                            
026832                               MOD-KDEKHHT-NY                             
026840                               MOD-BEEKHHT-NY                             
026850                               MOD-KDEKSHT-NY                             
026860                               MOD-BEEKSHT-NY                             
026870                               MOD-KDEKNIVA-NY                            
026880                               MOD-IDSYSMOT-NY                            
026890                               MOD-IDPTYP-NY                              
026900                               MOD-KDEKHHT                                
026910                               MOD-KDEKSHT                                
027000                               MOD-KDEKNIVA                               
027101     MOVE +1 TO INDX                                                      
027102     PERFORM UNTIL INDX > MAX-INDX                                        
027103       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027104       ADD +1 TO INDX                                                     
027105     END-PERFORM                                                          
027106     SKIP2                                                                
027107     .                                                                    
027108 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027109                                                                          
027110*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027111     MOVE MFS-ROER-EJ-FAELT TO MOD-IDSYSMOT (INDX)                        
027115                               MOD-IDPTYP   (INDX)                        
027200     .                                                                    
027300     SKIP3                                                                
027400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027500                                                                          
027600*    --- ALLA INDATA-FÄLT                                                 
027700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKHHT-IN                             
027800                               MOD-KDEKSHT-IN                             
027810                               MOD-KDEKNIVA-IN                            
027820                               MOD-IDSYSMOT-NY                            
027830                               MOD-IDPTYP-NY                              
027900     .                                                                    
028000     EJECT                                                                
028100 MFS-FORM-ATTR SECTION.                                                   
028200                                                                          
028300*    --- ALLA INDATA-FÄLT                                                 
028400     MOVE MFS-FORMATETS-ATTR TO MOD-IDSYSMOT-NY-ATTR                      
028500                                MOD-IDPTYP-NY-ATTR                        
028600     .                                                                    
028700     SKIP2                                                                
028800 MFS-LAES-IN-IGEN SECTION.                                                
028900                                                                          
029000*    --- ALLA INDATA-FÄLT                                                 
029100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDSYSMOT-NY-ATTR                   
029200                                   MOD-IDPTYP-NY-ATTR                     
029300     .                                                                    
029400     EJECT                                                                
029500* --- IMS SEKTIONER ---                                                   
029600     SKIP3                                                                
029700 IMS-GET-MSG SECTION.                                                     
029800                                                                          
029900     MOVE '  QC' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     SKIP3                                                                
030500 IMS-INSERT-MSG SECTION.                                                  
030600                                                                          
031000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031100     MOVE SPACE TO GODK-STATUSKODER                                       
031200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031601     EJECT                                                                
031602 IMS-GU-HHT SECTION.                                                      
031603                                                                          
031604     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031605          DELIMITED BY SIZE INTO SSA1                                     
031606     MOVE '  GE' TO GODK-STATUSKODER                                      
031607     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
031608     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031609     PERFORM IMS-STATUSKONTROLL                                           
031610     .                                                                    
031611     EJECT                                                                
031612 IMS-GU-SHT SECTION.                                                      
031613                                                                          
031614     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031615          DELIMITED BY SIZE INTO SSA1                                     
031616     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031617          DELIMITED BY SIZE INTO SSA2                                     
031618     MOVE '  GE' TO GODK-STATUSKODER                                      
031619     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH511 SSA1                    
031620                                                  SSA2                    
031621     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031622     PERFORM IMS-STATUSKONTROLL                                           
031623     .                                                                    
031624     EJECT                                                                
031625 IMS-GU-NIVA SECTION.                                                     
031626                                                                          
031627     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031628          DELIMITED BY SIZE INTO SSA1                                     
031629     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031630          DELIMITED BY SIZE INTO SSA2                                     
031631     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031632          DELIMITED BY SIZE INTO SSA3                                     
031633     MOVE '  GE' TO GODK-STATUSKODER                                      
031634     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1                    
031635                                                  SSA2                    
031636                                                  SSA3                    
031637     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031638     PERFORM IMS-STATUSKONTROLL                                           
031639     .                                                                    
031640     EJECT                                                                
031667 IMS-GU-SYST SECTION.                                                     
031668                                                                          
031669     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031670          DELIMITED BY SIZE INTO SSA1                                     
031671     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031672          DELIMITED BY SIZE INTO SSA2                                     
031673     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031674          DELIMITED BY SIZE INTO SSA3                                     
031675     STRING 'WDH531  (WDH531KY =' W-WDH531KY-X ')'                        
031676          DELIMITED BY SIZE INTO SSA4                                     
031677     MOVE '  GE' TO GODK-STATUSKODER                                      
031678     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
031679                                                   SSA2                   
031680                                                   SSA3                   
031681                                                   SSA4                   
031682     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031683     PERFORM IMS-STATUSKONTROLL                                           
031684     .                                                                    
031685     EJECT                                                                
031686 IMS-GNP-SYST SECTION.                                                    
031687                                                                          
031689     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031690          DELIMITED BY SIZE INTO SSA1                                     
031691     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031692          DELIMITED BY SIZE INTO SSA2                                     
031693     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031694          DELIMITED BY SIZE INTO SSA3                                     
031695     MOVE '  GE' TO GODK-STATUSKODER                                      
031696     MOVE 'WDH531   ' TO SSA4                                             
031697     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
031698                                                   SSA2                   
031699                                                   SSA3                   
031700                                                   SSA4                   
031701     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031702     PERFORM IMS-STATUSKONTROLL                                           
031703     .                                                                    
031704     EJECT                                                                
031723 IMS-ISRT-SYST SECTION.                                                   
031724                                                                          
031725     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031726          DELIMITED BY SIZE INTO SSA1                                     
031727     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031728          DELIMITED BY SIZE INTO SSA2                                     
031729     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031730          DELIMITED BY SIZE INTO SSA3                                     
031731     MOVE 'WDH531   ' TO SSA4                                             
031732     MOVE '  II' TO GODK-STATUSKODER                                      
031733     CALL CBLTDLI USING ISRT WDH5-PCB DLI-IO-WDH531 SSA1                  
031734                                                    SSA2                  
031735                                                    SSA3                  
031736                                                    SSA4                  
031737     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031738     PERFORM IMS-STATUSKONTROLL                                           
031739     .                                                                    
031740     EJECT                                                                
031800 IMS-STATUSKONTROLL SECTION.                                              
031900                                                                          
032000     SET STATUS-IX TO 1                                                   
032100     SEARCH GODK-STATUS                                                   
032200       AT END                                                             
032300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032400         DELIMITED BY SIZE INTO FELTEXT                                   
032500         CALL FELLOG                                                      
032600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032700         CONTINUE                                                         
032800     END-SEARCH                                                           
032900     .                                                                    
