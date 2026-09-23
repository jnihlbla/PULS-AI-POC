001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3012100.                                                
001500 AUTHOR.         GAVIN SMITH.                                             
001600 DATE-WRITTEN.   99/11/30.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        UPDATE  PGM FOR MAINT OF EXCHANGE CUSTOMER PARAMETERS.           
002100*        UPDATES WDR1 SEGMENTS WDGX3158.                                  
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WLR1   (WDGX)                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W3T121                                              
002700*        MID:         W3I12101                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W3O12101                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W3012100'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004310 01  INDIEX                      PIC 99      VALUE ZERO.                  
004400                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004901 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004902     88  INDATA-OK                           VALUE 'J'.                   
004910     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '3121'.                
005700     88  GODK-MID                            VALUE '3121'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006310*    --- ARBETSFÄLT FÖR AKTUELLA INPUT-VÄRDEN FRÅN SKÄRMEN                
006320 01  SW-IDDISTR-BET              PIC  X       VALUE 'J'.                  
006321 01  SW-FLEXCRET                 PIC  X       VALUE 'J'.                  
006330 01  SW-FLEXCREP                 PIC  X       VALUE 'J'.                  
006340 01  SW-FLEXCDET                 PIC  X       VALUE 'J'.                  
006350 01  SW-IDMAIL                   PIC  X       VALUE 'J'.                  
006360 01  SW-FLFAKT                   PIC  X       VALUE 'J'.                  
006370 01  SW-FLRETREM                 PIC  X       VALUE 'J'.                  
006380 01  SW-KVVECKOR-BYFA            PIC  X       VALUE 'J'.                  
006390 01  SW-KVVECKOR-BYRE            PIC  X       VALUE 'J'.                  
006391 01  SW-EXCLUDE.                                                          
006392     03  SW-IDFKNGRP-FOM OCCURS 16   PIC X    VALUE 'J' .                 
006393     03  SW-IDFKNGRP-TOM OCCURS 16   PIC X    VALUE 'J' .                 
006394 01  W-FGRP                          PIC 9999 VALUE ZERO.                 
006395 01  X-FGRP                          PIC XXXX VALUE SPACE.                
006396 01  X-FGR                           PIC XXX  VALUE SPACE.                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006710     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
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
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008710*    --- PARAMETRAR TILL SUBPROGRAM W009EMAD (E-ADDRESS VALIDITY)         
008720*                                                                         
008730 01  FILLER                      PIC X(16)  VALUE 'W009EMAD-AREA'.        
008740                                                                          
008750*01 -COPY W009EMAD                                                        
008760     EJECT                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W3I12101                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W3O12101 -PRE MOD-                                       
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
011101     03  W-WDGXKEY-X.                                                     
011102         05  FILLER              PIC X(4)    VALUE '3157'.                
011103         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011104     03  W-KEY3158-X.                                                     
011110         05  W-IDDISTR           PIC S9(4) VALUE ZERO   COMP-3.           
011120         05  W-KDEXCHA           PIC S9(3) VALUE ZERO   COMP-3.           
011200     SKIP2                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012800     EJECT                                                                
012900*    ---  DLI INPUT-OUTPUT AREA                                           
013000                                                                          
013101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR101'.                      
013102 01  DLI-IO-WLR101.                                                       
013103*    03  -COPY WDGX01 -PRE WDR101-                                        
013104     EJECT                                                                
013105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3158'.                    
013106 01  DLI-IO-WDGX3158.                                                     
013110*    03  -COPY WDGX3158                                                   
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013700*01  -COPY W0008   -PRE USEA-                                             
013800     05  FILLER                  PIC X.                                   
013901                                                                          
013902*01  -COPY W0008  -PRE WDR1-                                              
013910     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014101 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDR1-PCB.                     
014102 MAIN SECTION.                                                            
014110     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDR1-PCB.                     
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
015104             PERFORM E-SAMMA-SIDA                                         
015210         END-IF                                                           
015220         IF INDATA-OK                                                     
015300         PERFORM F-LAES-VISA-INFO                                         
015310         END-IF                                                           
015400       END-IF                                                             
015500*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015600*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
015700       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O12101 + 4                      
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
016900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I12101                 
017000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017200     ELSE                                                                 
017300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I12101                  
017400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018100                                                                          
018200     MOVE LOW-VALUE TO MSG-AREA                                           
018300     MOVE 'W3O121N1' TO MFS-IDMOD                                         
018400     MOVE '3121' TO MOD-IDTRANS                                           
018500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018600                                                                          
018700     IF EGEN-MID OR HELP-MID                                              
018800       CONTINUE                                                           
018900     ELSE                                                                 
019000       MOVE SPACE TO MFS-KDTRTYP                                          
019100       MOVE '7' TO MFS-IDPFK                                              
019110       MOVE ALL '+' TO MID-W3I12101                                       
019200     END-IF                                                               
019300     MOVE 'GB'             TO MED-IDSKYLT                                 
019500     .                                                                    
019600     EJECT                                                                
019700 B-KOLLA-NYCKLAR SECTION.                                                 
019800                                                                          
019900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020000     MOVE '001'             TO MSGI-KDCALL                                
020100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020300     MOVE '3121'            TO MSGI-IDTRANS                               
020400     IF GODK-MID                                                          
020510         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
020520         MOVE MID-KDEXCHA-IN     TO MSGI-KDEXCHA                          
020600     END-IF                                                               
020700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020800                                                                          
020900     MOVE JA TO NYCKLAR-SW                                                
021000*    -- KONTROLL AV IDDISTR *********************                         
021100     MOVE MFS-RENSA-FAELT  TO MOD-IDDISTR-IN                              
021101                                                                          
021102     IF MID-IDDISTR-IN NOT = ALL '+'                                      
021103       MOVE '7'         TO MFS-IDPFK                                      
021104       MOVE SPACE       TO MFS-KDTRTYP                                    
021105       MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                                
021106     END-IF                                                               
021107     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
021108     IF MSGI-IDDISTR NUMERIC                                              
021109       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
021112     ELSE                                                                 
021113       MOVE NEJ TO NYCKLAR-SW                                             
021114     END-IF                                                               
021115*************************************************                         
021116*    -- KONTROLL AV KDEXCHA *********************                         
021117     MOVE MFS-RENSA-FAELT TO MOD-KDEXCHA-IN                               
021118                                                                          
021119     IF MID-KDEXCHA-IN NOT = ALL '+'                                      
021120       MOVE '7'         TO MFS-IDPFK                                      
021121       MOVE SPACE       TO MFS-KDTRTYP                                    
021122       MOVE MID-KDEXCHA-IN TO MSGI-KDEXCHA                                
021123     END-IF                                                               
021124     INSPECT MSGI-KDEXCHA REPLACING LEADING SPACE BY ZERO                 
021125     IF MSGI-KDEXCHA NUMERIC                                              
021126       MOVE MSGI-KDEXCHA TO W-KDEXCHA                                     
021127     ELSE                                                                 
021128       MOVE NEJ TO NYCKLAR-SW                                             
021129     END-IF                                                               
021130*************************************************                         
021131     IF NYCKLAR-OK                                                        
021132       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
021133       MOVE MSGI-KDEXCHA        TO MOD-KDEXCHA-UT                         
021134*      INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
021135*      INSPECT MOD-KDEXCHA-UT REPLACING LEADING ZERO BY SPACE             
021136     ELSE                                                                 
021137       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
021138       MOVE MFS-RENSA-FAELT TO MOD-KDEXCHA-UT                             
021139     END-IF                                                               
021140*************************************************                         
021141*    -- IF NYCKEL ERROR                                                   
021300                                                                          
021400     IF NYCKLAR-FEL                                                       
021500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021600       CALL WMEDKONV USING MED-WMEDAREA                                   
021700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021800       PERFORM MFS-RENSA-FAELT-IN                                         
021900       PERFORM MFS-RENSA-FAELT-UT                                         
022000     END-IF                                                               
022100     .                                                                    
022300     EJECT                                                                
022400***************************************************                       
022406 E-SAMMA-SIDA SECTION.                                                    
022407                                                                          
022408     IF EGEN-MID OR HELP-MID                                              
022418       PERFORM MFS-RENSA-FAELT-IN                                         
022419     END-IF                                                               
022420     .                                                                    
022421     EJECT                                                                
022450*************************************************************             
022460     IF MID-FLEXCREP    NOT = ALL '+'                                     
022470          MOVE MID-FLEXCREP    TO  MOD-FLEXCREP                           
022480          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLEXCREP-ATTR                 
022491          MOVE 'N' TO    SW-FLEXCREP                                      
022492     END-IF                                                               
022493     .                                                                    
022494     EJECT                                                                
022495*************************************************************             
022496     IF MID-FLEXCDET    NOT = ALL '+'                                     
022497          MOVE MID-FLEXCDET    TO  MOD-FLEXCDET                           
022498          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLEXCDET-ATTR                 
022499          MOVE 'N' TO    SW-FLEXCDET                                      
022500     END-IF                                                               
022501     .                                                                    
022502     EJECT                                                                
022503*************************************************************             
022504     IF MID-IDMAIL      NOT = ALL '+'                                     
022505          MOVE MID-IDMAIL      TO  MOD-IDMAIL                             
022506          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL-ATTR                   
022507          MOVE 'N' TO    SW-IDMAIL                                        
022508     END-IF                                                               
022509     .                                                                    
022510     EJECT                                                                
022511*************************************************************             
022512     IF MID-FLFAKT      NOT = ALL '+'                                     
022513          MOVE MID-FLFAKT      TO  MOD-FLFAKT                             
022514          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFAKT-ATTR                   
022515          MOVE 'N' TO    SW-FLFAKT                                        
022516     END-IF                                                               
022517     .                                                                    
022518     EJECT                                                                
022519*************************************************************             
022520     IF MID-FLRETREM    NOT = ALL '+'                                     
022521          MOVE MID-FLRETREM    TO  MOD-FLRETREM                           
022522          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLRETREM-ATTR                 
022523          MOVE 'N' TO    SW-FLRETREM                                      
022524     END-IF                                                               
022525     .                                                                    
022526     EJECT                                                                
022527*************************************************************             
022528     IF MID-KVVECKOR-BYFA   NOT = ALL '+'                                 
022529          MOVE MID-KVVECKOR-BYFA  TO  MOD-KVVECKOR-BYFA                   
022530          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-BYFA-ATTR            
022531          MOVE 'N' TO    SW-KVVECKOR-BYFA                                 
022532     END-IF                                                               
022533     .                                                                    
022534     EJECT                                                                
022535*************************************************************             
022536     IF MID-KVVECKOR-BYRE   NOT = ALL '+'                                 
022537          MOVE MID-KVVECKOR-BYRE  TO  MOD-KVVECKOR-BYRE                   
022538          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-BYRE-ATTR            
022539          MOVE 'N' TO    SW-KVVECKOR-BYRE                                 
022540     END-IF                                                               
022541     .                                                                    
022542     EJECT                                                                
022543*************************************************************             
022544     MOVE 1 TO INDIEX                                                     
022545     PERFORM UNTIL INDIEX = 17                                            
022546     IF MID-IDFKNGRP-FOM(INDIEX)   NOT = ALL '+'                          
022547      MOVE MID-IDFKNGRP-FOM(INDIEX) TO MOD-IDFKNGRP-FOM(INDIEX)           
022548      MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-FOM-ATTR(INDIEX)         
022549      MOVE 'N' TO    SW-IDFKNGRP-FOM(INDIEX)                              
022550     END-IF                                                               
022551     IF MID-IDFKNGRP-TOM(INDIEX)   NOT = ALL '+'                          
022552      MOVE MID-IDFKNGRP-TOM(INDIEX) TO MOD-IDFKNGRP-TOM(INDIEX)           
022553      MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-TOM-ATTR(INDIEX)         
022554      MOVE 'N' TO    SW-IDFKNGRP-TOM(INDIEX)                              
022555     END-IF                                                               
022556     ADD 1 TO INDIEX GIVING INDIEX                                        
022557     END-PERFORM                                                          
022558     .                                                                    
022559     EJECT                                                                
022560*************************************************************             
022570 F-LAES-VISA-INFO SECTION.                                                
022600                                                                          
022700     PERFORM IMS-GU-WDGX3158                                              
022800                                                                          
022900     IF SEGMENT-SAKNAS                                                    
023000     MOVE 'PF11 TO ADD NEW ACCOUNT' TO MOD-TEMFSFEL                       
023300        PERFORM MFS-RENSA-FAELT-UT                                        
023400     ELSE                                                                 
023410**************************************************'                       
023500        IF SW-IDDISTR-BET  = 'J'                                          
023510          MOVE 3158-IDDISTR-BET        TO MOD-IDDISTR-BET                 
023520        END-IF                                                            
023530**************************************************                        
023531        IF SW-FLEXCRET     = 'J'                                          
023532          MOVE 3158-FLEXCRET           TO MOD-FLEXCRET                    
023533        END-IF                                                            
023534**************************************************                        
023535        IF SW-FLEXCREP     = 'J'                                          
023536          MOVE 3158-FLEXCREP           TO MOD-FLEXCREP                    
023537        END-IF                                                            
023538**************************************************                        
023539        IF SW-FLEXCDET     = 'J'                                          
023540          MOVE 3158-FLEXCDET           TO MOD-FLEXCDET                    
023541        END-IF                                                            
023542**************************************************                        
023543        IF SW-IDMAIL       = 'J'                                          
023544          MOVE 3158-IDMAIL             TO MOD-IDMAIL                      
023545        END-IF                                                            
023546**************************************************                        
023547        IF SW-FLFAKT       = 'J'                                          
023548          MOVE 3158-FLFAKT             TO MOD-FLFAKT                      
023549        END-IF                                                            
023550**************************************************                        
023551        IF SW-FLRETREM     = 'J'                                          
023552          MOVE 3158-FLRETREM           TO MOD-FLRETREM                    
023553        END-IF                                                            
023554**************************************************                        
023555        IF SW-KVVECKOR-BYFA = 'J'                                         
023556          MOVE 3158-KVVECKOR-BYFA                                         
023557                                        TO MOD-KVVECKOR-BYFA              
023558        END-IF                                                            
023559**************************************************                        
023560        IF SW-KVVECKOR-BYRE = 'J'                                         
023561          MOVE 3158-KVVECKOR-BYRE                                         
023562                                        TO MOD-KVVECKOR-BYRE              
023563        END-IF                                                            
023564**************************************************                        
023565***OUTPUT FIELDS COME NOW*************************                        
023566        MOVE 3158-SUPOINT-BAL          TO MOD-SUPOINT-BAL                 
023567        MOVE 3158-SUPOINT-RIT          TO MOD-SUPOINT-RIT                 
023568        MOVE 3158-SUPOINT-PP           TO MOD-SUPOINT-PP                  
023569**************************************************                        
023570        MOVE 1 TO INDIEX                                                  
023571        PERFORM UNTIL INDIEX = 17                                         
023572                                                                          
023573        IF SW-IDFKNGRP-FOM(INDIEX) = 'J'                                  
023574         MOVE 3158-IDFKNGRP-FOM(INDIEX)                                   
023575                             TO MOD-IDFKNGRP-FOM(INDIEX)                  
023576        END-IF                                                            
023577        IF SW-IDFKNGRP-TOM(INDIEX) = 'J'                                  
023578         MOVE 3158-IDFKNGRP-TOM(INDIEX)                                   
023579                             TO MOD-IDFKNGRP-TOM(INDIEX)                  
023580        END-IF                                                            
023581        ADD 1 TO INDIEX GIVING INDIEX                                     
023582        END-PERFORM                                                       
023583**************************************************                        
023584        MOVE '002'                     TO MSGI-KDCALL                     
023585        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
024000                                                                          
024702 G-KOLLA-INPUT SECTION.                                                   
024703                                                                          
024704     MOVE JA  TO INDATA-SW                                                
024705     PERFORM IMS-GU-WDGX3158                                              
024707     IF  SEGMENT-SAKNAS                                                   
024708* LÄGG UPP NY ACCOUNT FÖR DISTRIKTET***********                           
024709      MOVE W-IDDISTR TO  3158-IDDISTR                                     
024710      MOVE W-KDEXCHA TO  3158-KDEXCHA                                     
024711      MOVE FUNCTION CURRENT-DATE(1:8)    TO  3158-DAREGDAT                
024712      MOVE FUNCTION CURRENT-DATE(9:8)   TO  3158-TIKLOCK                  
024713      MOVE 1 TO INDIEX                                                    
024714      PERFORM UNTIL INDIEX =  17                                          
024715       MOVE ZERO TO 3158-IDFKNGRP-FOM(INDIEX)                             
024716       MOVE ZERO TO 3158-IDFKNGRP-TOM(INDIEX)                             
024717       ADD 1 TO INDIEX GIVING INDIEX                                      
024718      END-PERFORM                                                         
024719      MOVE 'N'   TO 3158-FLEXCDET                                         
024720      MOVE 'N'   TO 3158-FLEXCREP                                         
024721      MOVE 'N'   TO 3158-FLEXCRET                                         
024722      MOVE 'N'   TO 3158-FLFAKT                                           
024723      MOVE 'N'   TO 3158-FLRETREM                                         
024724      MOVE 5 TO     3158-KVVECKOR-BYFA                                    
024725      MOVE 5 TO     3158-KVVECKOR-BYRE                                    
024726      MOVE W-IDDISTR TO 3158-IDDISTR-BET                                  
024727      MOVE SPACE TO 3158-IDMAIL                                           
024728      MOVE 'N'   TO 3158-IDUSER                                           
024729      MOVE ZERO  TO 3158-SUPOINT-BAL                                      
024730      MOVE ZERO  TO 3158-SUPOINT-RIT                                      
024731      MOVE ZERO  TO 3158-SUPOINT-PP                                       
024732      MOVE SPACE TO 3158-FILLER                                           
024733      PERFORM  IMS-ISRT-WDGX3158                                          
024734* LÄGG UPP NY ACCOUNT FÖR DISTRIKTET**SLUT*****                           
024735     ELSE                                                                 
024736* OM SEGMENTET FINNS REDAN*********************                           
024737     IF MID-W3I12101 = ALL '+'                                            
024738       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024739       CALL WMEDKONV USING MED-WMEDAREA                                   
024740       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024741       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024742       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024743       MOVE NEJ TO INDATA-SW                                              
024744     ELSE                                                                 
024745***********************                                                   
024746       IF MID-IDDISTR-BET  NOT = ALL '+'                                  
024747       INSPECT MID-IDDISTR-BET REPLACING LEADING SPACE                    
024748                                         BY ZERO                          
024749       IF MID-IDDISTR-BET NOT NUMERIC                                     
024750          MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-BET-ATTR                  
024751          MOVE NEJ TO INDATA-SW                                           
024752       ELSE                                                               
024753          MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-BET-ATTR                
024754       END-IF                                                             
024755       END-IF                                                             
024756*************************                                                 
024757       IF MID-FLEXCRET NOT = ALL '+'                                      
024758       IF MID-FLEXCRET = 'Y' OR 'N' OR 'y' OR 'n'                         
024759         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCRET-ATTR                   
024760       ELSE                                                               
024761          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCRET-ATTR                    
024762          MOVE NEJ TO INDATA-SW                                           
024763       END-IF                                                             
024764       END-IF                                                             
024765***************************                                               
024766       IF MID-FLEXCREP NOT = ALL '+'                                      
024767       IF MID-FLEXCREP = 'Y' OR 'N' OR 'y' OR 'n'                         
024768         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCREP-ATTR                   
024769       ELSE                                                               
024770          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCREP-ATTR                    
024771          MOVE NEJ TO INDATA-SW                                           
024772       END-IF                                                             
024773       END-IF                                                             
024774***************************                                               
024775       IF MID-FLEXCDET NOT = ALL '+'                                      
024776       IF MID-FLEXCDET = 'Y' OR 'N' OR 'y' OR 'n'                         
024777         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLEXCDET-ATTR                   
024778       ELSE                                                               
024779          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLEXCDET-ATTR                    
024780          MOVE NEJ TO INDATA-SW                                           
024781       END-IF                                                             
024782       END-IF                                                             
024783***************************                                               
024784       IF MID-IDMAIL NOT = ALL '+'                                        
024785         IF MID-IDMAIL > SPACES                                           
024786           MOVE MID-IDMAIL           TO EMAD-IDMAIL                       
024787           CALL W009EMAD USING EMAD-W009EMAD                              
024788           MOVE EMAD-IDMAIL TO MID-IDMAIL 3158-IDMAIL MOD-IDMAIL          
024789           IF EMAD-KDSVAR > SPACE                                         
024790             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMAIL-ATTR                   
024791             MOVE NEJ                TO INDATA-SW                         
024792           ELSE                                                           
024794             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL-ATTR                 
024795           END-IF                                                         
024796         ELSE                                                             
024797           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL-ATTR                   
024798         END-IF                                                           
024799       END-IF                                                             
024800****************************                                              
024801       IF MID-FLFAKT   NOT = ALL '+'                                      
024802         IF MID-FLFAKT       = 'Y' OR 'N' OR 'y' OR 'n'                   
024803           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFAKT-ATTR                   
024804         ELSE                                                             
024805           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFAKT-ATTR                     
024806           MOVE NEJ TO INDATA-SW                                          
024807         END-IF                                                           
024808       END-IF                                                             
024809***************************                                               
024810       IF MID-FLRETREM NOT = ALL '+'                                      
024811         IF MID-FLRETREM     = 'Y' OR 'N' OR 'y' OR 'n'                   
024812           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRETREM-ATTR                 
024813         ELSE                                                             
024814           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRETREM-ATTR                   
024815           MOVE NEJ TO INDATA-SW                                          
024816         END-IF                                                           
024817       END-IF                                                             
024818***************************                                               
024819       IF MID-KVVECKOR-BYFA NOT = ALL '+'                                 
024820       INSPECT MID-KVVECKOR-BYFA REPLACING LEADING SPACE                  
024821                                         BY ZERO                          
024822         IF MID-KVVECKOR-BYFA NOT NUMERIC                                 
024823           MOVE MFS-NUM-FAELT-FEL TO MOD-KVVECKOR-BYFA-ATTR               
024824           MOVE NEJ TO INDATA-SW                                          
024825         ELSE                                                             
024826           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVVECKOR-BYFA-ATTR             
024827         END-IF                                                           
024828       END-IF                                                             
024829***************************                                               
024830       IF MID-KVVECKOR-BYRE NOT = ALL '+'                                 
024831       INSPECT MID-KVVECKOR-BYRE REPLACING LEADING SPACE                  
024832                                         BY ZERO                          
024833         IF MID-KVVECKOR-BYRE NOT NUMERIC                                 
024834           MOVE MFS-NUM-FAELT-FEL TO MOD-KVVECKOR-BYRE-ATTR               
024835           MOVE NEJ TO INDATA-SW                                          
024836         ELSE                                                             
024837           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVVECKOR-BYRE-ATTR             
024838         END-IF                                                           
024839       END-IF                                                             
024840*************************************                                     
024841****CHECK FGRPS******                                                     
024842        MOVE 1 TO INDIEX                                                  
024843        PERFORM UNTIL INDIEX = 17                                         
024844                                                                          
024845         IF MID-IDFKNGRP-FOM(INDIEX) NOT = ALL '+'                        
024846*** ERROR CORRECT FÖRSÖK                                                  
024847         MOVE MID-IDFKNGRP-FOM(INDIEX) TO X-FGRP                          
024848         PERFORM UNTIL X-FGRP(4:1) NOT = SPACE                            
024849           MOVE X-FGRP(1:3) TO X-FGR                                      
024850           MOVE X-FGR       TO X-FGRP(2:3)                                
024851           MOVE 0 TO X-FGRP(1:1)                                          
024852         END-PERFORM                                                      
024853         MOVE X-FGRP TO MID-IDFKNGRP-FOM(INDIEX)                          
024854*************************                                                 
024855         INSPECT MID-IDFKNGRP-FOM(INDIEX) REPLACING                       
024856                LEADING SPACE BY ZERO                                     
024857         IF MID-IDFKNGRP-FOM(INDIEX) NOT NUMERIC                          
024858         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR(INDIEX)          
024859         MOVE NEJ TO INDATA-SW                                            
024860         ELSE                                                             
024861         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-ATTR(INDIEX)        
024862         END-IF                                                           
024863         END-IF                                                           
024864                                                                          
024865         IF MID-IDFKNGRP-TOM(INDIEX) NOT = ALL '+'                        
024866*** ERROR CORRECT FÖRSÖK                                                  
024867         MOVE MID-IDFKNGRP-TOM(INDIEX) TO X-FGRP                          
024868         PERFORM UNTIL X-FGRP(4:1) NOT = SPACE                            
024869           MOVE X-FGRP(1:3) TO X-FGR                                      
024870           MOVE X-FGR       TO X-FGRP(2:3)                                
024871           MOVE 0 TO X-FGRP(1:1)                                          
024872         END-PERFORM                                                      
024873         MOVE X-FGRP TO MID-IDFKNGRP-TOM(INDIEX)                          
024874*************************                                                 
024875         INSPECT MID-IDFKNGRP-TOM(INDIEX) REPLACING                       
024876                LEADING SPACE BY ZERO                                     
024877         IF MID-IDFKNGRP-TOM(INDIEX) NOT NUMERIC                          
024878         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR(INDIEX)          
024879         MOVE NEJ TO INDATA-SW                                            
024880         ELSE                                                             
024881         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-TOM-ATTR(INDIEX)        
024882         END-IF                                                           
024883         END-IF                                                           
024884                                                                          
024885***************************************************************           
024886         IF MID-IDFKNGRP-FOM(INDIEX) NOT = ALL '+'   AND                  
024887            MID-IDFKNGRP-TOM(INDIEX) NOT = ALL '+'                        
024888         IF MID-IDFKNGRP-FOM(INDIEX)  >                                   
024889            MID-IDFKNGRP-TOM(INDIEX)                                      
024890         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR(INDIEX)          
024891         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR(INDIEX)          
024892         MOVE NEJ TO INDATA-SW                                            
024893         END-IF                                                           
024894         END-IF                                                           
024895**************************************************************            
024896**************************************************************            
024897         IF MID-IDFKNGRP-FOM(INDIEX) NOT = ALL '+'   AND                  
024898            MID-IDFKNGRP-TOM(INDIEX)     = ALL '+'                        
024899         MOVE 3158-IDFKNGRP-TOM(INDIEX) TO W-FGRP                         
024900         IF MID-IDFKNGRP-FOM(INDIEX)  >                                   
024901            W-FGRP                                                        
024902         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR(INDIEX)          
024903         MOVE NEJ TO INDATA-SW                                            
024904         END-IF                                                           
024905         END-IF                                                           
024906**************************************************************            
024907**************************************************************            
024908         IF MID-IDFKNGRP-FOM(INDIEX)     = ALL '+'   AND                  
024909            MID-IDFKNGRP-TOM(INDIEX) NOT = ALL '+'                        
024910            MOVE 3158-IDFKNGRP-FOM(INDIEX) TO W-FGRP                      
024911         IF MID-IDFKNGRP-TOM(INDIEX)  <                                   
024912            W-FGRP                                                        
024913         MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-TOM-ATTR(INDIEX)          
024914         MOVE NEJ TO INDATA-SW                                            
024915         END-IF                                                           
024916         END-IF                                                           
024917**************************************************************            
024918                                                                          
024919        ADD 1 TO INDIEX  GIVING INDIEX                                    
024920        END-PERFORM                                                       
024921******************************************                                
024922       IF INDATA-FEL                                                      
024923         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024924         CALL WMEDKONV USING MED-WMEDAREA                                 
024925         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024926         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024927         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024928       END-IF                                                             
024929     END-IF                                                               
024930     END-IF                                                               
024931     .                                                                    
024932     EJECT                                                                
024933 H-UPPDATERA SECTION.                                                     
024934                                                                          
024935     PERFORM IMS-GHU-WDGX3158                                             
024936********************'****************************                         
024937     IF SEGMENT-FINNS                                                     
024938********                                                                  
024939       IF MID-IDDISTR-BET NOT = ALL '+'                                   
024940         MOVE MID-IDDISTR-BET TO 3158-IDDISTR-BET  MOD-IDDISTR-BET        
024941         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDISTR-BET-ATTR               
024942       ELSE                                                               
024943         MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-BET                        
024944       END-IF                                                             
024945*************                                                             
024946       IF MID-FLEXCRET    NOT = ALL '+'                                   
024947         MOVE MID-FLEXCRET    TO 3158-FLEXCRET     MOD-FLEXCRET           
024948         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLEXCRET-ATTR                  
024949       ELSE                                                               
024950         MOVE MFS-ROER-EJ-FAELT TO MOD-FLEXCRET                           
024951       END-IF                                                             
024952******************                                                        
024953       IF MID-FLEXCREP    NOT = ALL '+'                                   
024954         MOVE MID-FLEXCREP    TO 3158-FLEXCREP     MOD-FLEXCREP           
024955         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLEXCREP-ATTR                  
024956       ELSE                                                               
024957         MOVE MFS-ROER-EJ-FAELT TO MOD-FLEXCREP                           
024958       END-IF                                                             
024959******************                                                        
024960       IF MID-FLEXCDET    NOT = ALL '+'                                   
024961         MOVE MID-FLEXCDET    TO 3158-FLEXCDET     MOD-FLEXCDET           
024962         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLEXCDET-ATTR                  
024963       ELSE                                                               
024964         MOVE MFS-ROER-EJ-FAELT TO MOD-FLEXCDET                           
024965       END-IF                                                             
024966*******                                                                   
024967       IF MID-IDMAIL      NOT = ALL '+'                                   
024968         MOVE MID-IDMAIL      TO 3158-IDMAIL       MOD-IDMAIL             
024969         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDMAIL-ATTR                    
024970       ELSE                                                               
024971         MOVE MFS-ROER-EJ-FAELT TO MOD-IDMAIL                             
024972       END-IF                                                             
024973*********                                                                 
024974       IF MID-FLFAKT      NOT = ALL '+'                                   
024975         MOVE MID-FLFAKT      TO 3158-FLFAKT       MOD-FLFAKT             
024976         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLFAKT-ATTR                    
024977       ELSE                                                               
024978         MOVE MFS-ROER-EJ-FAELT TO MOD-FLFAKT                             
024979       END-IF                                                             
024980*********                                                                 
024981       IF MID-FLRETREM    NOT = ALL '+'                                   
024982         MOVE MID-FLRETREM    TO 3158-FLRETREM     MOD-FLRETREM           
024983         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLRETREM-ATTR                  
024984       ELSE                                                               
024985         MOVE MFS-ROER-EJ-FAELT TO MOD-FLFAKT                             
024986       END-IF                                                             
024987*********                                                                 
024988       IF MID-KVVECKOR-BYFA NOT = ALL '+'                                 
024989         MOVE MID-KVVECKOR-BYFA TO 3158-KVVECKOR-BYFA                     
024990                                   MOD-KVVECKOR-BYFA                      
024991         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVECKOR-BYFA-ATTR             
024992       ELSE                                                               
024993         MOVE MFS-ROER-EJ-FAELT TO MOD-KVVECKOR-BYFA                      
024994       END-IF                                                             
024995**********                                                                
024996       IF MID-KVVECKOR-BYRE NOT = ALL '+'                                 
024997         MOVE MID-KVVECKOR-BYRE TO 3158-KVVECKOR-BYRE                     
024998                                   MOD-KVVECKOR-BYRE                      
024999         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVECKOR-BYRE-ATTR             
025000       ELSE                                                               
025001         MOVE MFS-ROER-EJ-FAELT TO MOD-KVVECKOR-BYRE                      
025002       END-IF                                                             
025003                                                                          
025004*****************************                                             
025005       MOVE 1 TO INDIEX                                                   
025006       PERFORM UNTIL INDIEX = 17                                          
025007                                                                          
025008       IF MID-IDFKNGRP-FOM(INDIEX) NOT = ALL '+'                          
025009         MOVE MID-IDFKNGRP-FOM(INDIEX) TO                                 
025010                              3158-IDFKNGRP-FOM(INDIEX)                   
025011                               MOD-IDFKNGRP-FOM(INDIEX)                   
025012         MOVE MFS-ADD-LYS-UPP-FAELT TO                                    
025013                          MOD-IDFKNGRP-FOM-ATTR(INDIEX)                   
025014       ELSE                                                               
025015         MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-FOM(INDIEX)               
025016       END-IF                                                             
025017                                                                          
025018       IF MID-IDFKNGRP-TOM(INDIEX) NOT = ALL '+'                          
025019         MOVE MID-IDFKNGRP-TOM(INDIEX) TO                                 
025020                              3158-IDFKNGRP-TOM(INDIEX)                   
025021                               MOD-IDFKNGRP-TOM(INDIEX)                   
025022         MOVE MFS-ADD-LYS-UPP-FAELT TO                                    
025023                          MOD-IDFKNGRP-TOM-ATTR(INDIEX)                   
025024       ELSE                                                               
025025         MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-TOM(INDIEX)               
025026       END-IF                                                             
025027                                                                          
025028       ADD 1 TO INDIEX GIVING INDIEX                                      
025029       END-PERFORM                                                        
025030*****************************                                             
025031       PERFORM IMS-REPL-WDGX3158                                          
025032                                                                          
025033       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
025034       CALL WMEDKONV USING MED-WMEDAREA                                   
025035       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
025036       PERFORM MFS-RENSA-FAELT-IN                                         
025037* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
025038     END-IF                                                               
025039     .                                                                    
025040     EJECT                                                                
025041 MFS-RENSA-FAELT-UT SECTION.                                              
025050                                                                          
025100*    --- ALLA UTDATA-FÄLT                                                 
025300     MOVE MFS-RENSA-FAELT TO                                              
025310                             MOD-SUPOINT-PP                               
025400                             MOD-SUPOINT-RIT                              
025410                             MOD-SUPOINT-BAL                              
025420                             MOD-KVVECKOR-BYRE                            
025430                             MOD-KVVECKOR-BYFA                            
025440                             MOD-FLRETREM                                 
025441                             MOD-FLFAKT                                   
025450                             MOD-IDMAIL                                   
025460                             MOD-FLEXCDET                                 
025470                             MOD-FLEXCREP                                 
025471                             MOD-FLEXCRET                                 
025480                             MOD-IDDISTR-BET                              
025481     MOVE 1 TO INDIEX                                                     
025482     PERFORM UNTIL INDIEX = 17                                            
025483      MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-FOM(INDIEX)                    
025484      MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-TOM(INDIEX)                    
025485      ADD 1 TO INDIEX GIVING INDIEX                                       
025486     END-PERFORM                                                          
025530     .                                                                    
025600                                                                          
025700     SKIP3                                                                
025800 MFS-RENSA-FAELT-IN SECTION.                                              
025900                                                                          
026000*    --- ALLA INDATA-FÄLT                                                 
026100     MOVE MFS-RENSA-FAELT TO                                              
026200                                                                          
026210                             MOD-KVVECKOR-BYRE                            
026220                             MOD-KVVECKOR-BYFA                            
026230                             MOD-FLRETREM                                 
026231                             MOD-FLFAKT                                   
026240                             MOD-IDMAIL                                   
026250                             MOD-FLEXCDET                                 
026260                             MOD-FLEXCREP                                 
026261                             MOD-FLEXCRET                                 
026270                             MOD-IDDISTR-BET                              
026280     MOVE 1 TO INDIEX                                                     
026290     PERFORM UNTIL INDIEX = 17                                            
026291      MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-FOM(INDIEX)                    
026292      MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-TOM(INDIEX)                    
026293      ADD 1 TO INDIEX GIVING INDIEX                                       
026294     END-PERFORM                                                          
026300     .                                                                    
026400     EJECT                                                                
026500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026600                                                                          
026700*    --- ALLA UTDATA-FÄLT                                                 
026900     MOVE MFS-ROER-EJ-FAELT TO                                            
026910                             MOD-SUPOINT-PP                               
026920                             MOD-SUPOINT-RIT                              
026930                             MOD-SUPOINT-BAL                              
026940                             MOD-KVVECKOR-BYRE                            
026950                             MOD-KVVECKOR-BYFA                            
026960                             MOD-FLRETREM                                 
026970                             MOD-FLFAKT                                   
026980                             MOD-IDMAIL                                   
026990                             MOD-FLEXCDET                                 
026991                             MOD-FLEXCREP                                 
026992                             MOD-FLEXCRET                                 
026993                             MOD-IDDISTR-BET                              
027000                                                                          
027191     MOVE 1 TO INDIEX                                                     
027192     PERFORM UNTIL INDIEX = 17                                            
027193      MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-FOM(INDIEX)                  
027194      MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-TOM(INDIEX)                  
027195      ADD 1 TO INDIEX GIVING INDIEX                                       
027196     END-PERFORM                                                          
027200     .                                                                    
027300     SKIP3                                                                
027400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027500                                                                          
027600*    --- ALLA INDATA-FÄLT                                                 
027700     MOVE MFS-ROER-EJ-FAELT TO                                            
027710                             MOD-KVVECKOR-BYRE                            
027720                             MOD-KVVECKOR-BYFA                            
027730                             MOD-FLRETREM                                 
027740                             MOD-FLFAKT                                   
027750                             MOD-IDMAIL                                   
027760                             MOD-FLEXCDET                                 
027770                             MOD-FLEXCREP                                 
027780                             MOD-FLEXCRET                                 
027790                             MOD-IDDISTR-BET                              
027800                                                                          
027880     MOVE 1 TO INDIEX                                                     
027890     PERFORM UNTIL INDIEX = 17                                            
027891      MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-FOM(INDIEX)                  
027892      MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP-TOM(INDIEX)                  
027893      ADD 1 TO INDIEX GIVING INDIEX                                       
027894     END-PERFORM                                                          
027900     .                                                                    
028000     EJECT                                                                
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
030700     IF MSGI-IDLAND-SPR = 'GB'                                            
030800       MOVE 'N' TO MFS-KDHUVOMR                                           
030900     END-IF                                                               
031000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031100     MOVE SPACE TO GODK-STATUSKODER                                       
031200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031601     EJECT                                                                
031602 IMS-GU-WDGX3158 SECTION.                                                 
031603                                                                          
031604     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
031605          DELIMITED BY SIZE INTO SSA1                                     
031606     STRING 'WDGX3158(KEY3158  =' W-KEY3158-X ')'                         
031607          DELIMITED BY SIZE INTO SSA2                                     
031608     MOVE '  GE' TO GODK-STATUSKODER                                      
031609     CALL CBLTDLI USING GU WDR1-PCB DLI-IO-WDGX3158 SSA1 SSA2             
031610     MOVE WDR1-STATUS-CODE TO STATUS-WS                                   
031611     PERFORM IMS-STATUSKONTROLL                                           
031612     .                                                                    
031613     SKIP3                                                                
031614 IMS-GHU-WDGX3158 SECTION.                                                
031615                                                                          
031616     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
031617          DELIMITED BY SIZE INTO SSA1                                     
031618     STRING 'WDGX3158(KEY3158  =' W-KEY3158-X ')'                         
031619          DELIMITED BY SIZE INTO SSA2                                     
031620     MOVE '  GE' TO GODK-STATUSKODER                                      
031621     CALL CBLTDLI USING GHU WDR1-PCB DLI-IO-WDGX3158 SSA1 SSA2            
031622     MOVE WDR1-STATUS-CODE TO STATUS-WS                                   
031623     PERFORM IMS-STATUSKONTROLL                                           
031624     .                                                                    
031625     SKIP3                                                                
031626 IMS-ISRT-WDGX3158 SECTION.                                               
031627                                                                          
031628     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
031629          DELIMITED BY SIZE INTO SSA1                                     
031630     MOVE 'WDGX3158 ' TO SSA2                                             
031632     MOVE '  II' TO GODK-STATUSKODER                                      
031633     CALL CBLTDLI USING ISRT WDR1-PCB DLI-IO-WDGX3158 SSA1 SSA2           
031634     MOVE WDR1-STATUS-CODE TO STATUS-WS                                   
031635     PERFORM IMS-STATUSKONTROLL                                           
031636     .                                                                    
031637     SKIP3                                                                
031647 IMS-REPL-WDGX3158 SECTION.                                               
031648                                                                          
031649     MOVE '  ' TO GODK-STATUSKODER                                        
031650     CALL CBLTDLI USING REPL WDR1-PCB DLI-IO-WDGX3158                     
031651     MOVE WDR1-STATUS-CODE TO STATUS-WS                                   
031652     PERFORM IMS-STATUSKONTROLL                                           
031653     .                                                                    
031654     SKIP3                                                                
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
