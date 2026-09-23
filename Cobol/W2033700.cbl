000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033700.                                                
000300 AUTHOR.         BO HAMMARIN, GDC-GROUP.                                  
000400 DATE-WRITTEN.   FEBR-1999.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMETS HUVUDFUNKTIONER ÄR FÖLJANDE:                         
000900*        -FRÅGA PÅ GRUPP OCH PÅ SÅ SÄTT FÅ UT                             
001000*         VILKA ARTIKLAR SOM FINNS I GRUPPEN                              
001100*        -GÅR ÄVEN ATT FYLLA I ARTIKELNR, PÅ SÅ VIS KAN MAN               
001200*         SE OM ARTIKELN FINNS UPPLAGD I NÅGON GRUPP                      
001300*                                                                         
001400*        -LÄGGA UPP NYA ARTIKLAR I GRUPP (N)                              
001500*        -ÄNDRA STARTDATUM FÖR BEFINTLIG ARTIKEL (E)                      
001600*        -TA BORT GAMLA ARTIKLAR I GRUPP (D)                              
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001900*        PROGRAMMET UPPDATERAR WLLEVF (WDF8)                              
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W2T337 W2T337U W2T337X                              
002300*        MID:         W2I33701                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W2O33701                                            
002700                                                                          
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003500 77  IDPGM                       PIC X(08)   VALUE 'W2033700'.            
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004110 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004120 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004200                                                                          
004300*    --- GENERELLA ARBETSFÄLT                                             
004400 01  DAGENS-DATUM                PIC 9(6).                                
004500                                                                          
005000 01  WS-TISTADAT                 PIC 9(6).                                
005100 01  WS-IDARTNR                  PIC 9(9).                                
005200 01  W-IDARTNR-KEY               PIC S9(9)   VALUE ZERO COMP-3.           
005300                                                                          
005400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005500 77  MFS-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
005510 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
005700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005800                                                                          
005900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006000     88  ALLT-OK                             VALUE 'J'.                   
006100                                                                          
006600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006700     88  INDATA-OK                           VALUE 'J'.                   
006800     88  INDATA-FEL                          VALUE 'N'.                   
006900                                                                          
007000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007100     88  NYCKLAR-OK                          VALUE 'J'.                   
007200     88  NYCKLAR-FEL                         VALUE 'N'.                   
007210                                                                          
007220 77  RAD-VALD-SW                 PIC X       VALUE 'N'.                   
007230     88  RAD-VALD                            VALUE 'J'.                   
007240     88  RAD-EJ-VALD                         VALUE 'N'.                   
007300                                                                          
007310 77  E-RAD-FEL-SW                PIC X       VALUE 'N'.                   
007320     88  E-RAD-FEL                           VALUE 'J'.                   
007330                                                                          
007340 77  E-ART-FEL-SW                PIC X       VALUE 'N'.                   
007350     88  E-ART-FEL                           VALUE 'J'.                   
007351 77  E-ART-TI-FEL-SW             PIC X       VALUE 'N'.                   
007352     88  E-ART-TI-FEL                        VALUE 'J'.                   
007353                                                                          
007355 77  E-URS-FEL-SW                PIC X       VALUE 'N'.                   
007356     88  E-URS-FEL                           VALUE 'J'.                   
007357 77  E-URS-TI-FEL-SW             PIC X       VALUE 'N'.                   
007358     88  E-URS-TI-FEL                        VALUE 'J'.                   
007359                                                                          
007360 77  E-PRODFOM-FEL-SW            PIC X       VALUE 'N'.                   
007361     88  E-PRODFOM-FEL                       VALUE 'J'.                   
007362 77  E-PRODTOM-FEL-SW            PIC X       VALUE 'N'.                   
007363     88  E-PRODTOM-FEL                       VALUE 'J'.                   
007364 77  E-PROD-TI-FEL-SW            PIC X       VALUE 'N'.                   
007365     88  E-PROD-TI-FEL                       VALUE 'J'.                   
007366                                                                          
007367 77  E-FKNFOM-FEL-SW             PIC X       VALUE 'N'.                   
007368     88  E-FKNFOM-FEL                        VALUE 'J'.                   
007369 77  E-FKNTOM-FEL-SW             PIC X       VALUE 'N'.                   
007370     88  E-FKNTOM-FEL                        VALUE 'J'.                   
007371 77  E-FKN-TI-FEL-SW             PIC X       VALUE 'N'.                   
007372     88  E-FKN-TI-FEL                        VALUE 'J'.                   
007380                                                                          
007398                                                                          
007400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007500     88  EGEN-MID                            VALUE '2337'.                
007600     88  GODK-MID                            VALUE '2331' '2332'          
007700                                                   '2333' '2334'          
007800                                                   '2335' '2336'          
007900                                                   '2337' '2338'          
008000                                                   '2339'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200                                                                          
008210                                                                          
008300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008400 01  GENERELLA-SUBPROGRAM.                                                
008500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000                                                                          
009100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009200*01 -COPY WMEDAREA                                                        
009300                                                                          
009400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
009500*01 -COPY WDATAREA                                                        
009600                                                                          
009700 01  MESSAGE-CODES.                                                       
009800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010500     03  GROUP-MISSING           PIC X(3)    VALUE '275'.                 
010600     03  PART-EXISTS             PIC X(3)    VALUE '278'.                 
010700     03  PART-MISSING            PIC X(3)    VALUE '279'.                 
010800     03  PART-MISSING-WDK6       PIC X(3)    VALUE '017'.                 
010900                                                                          
010910                                                                          
011000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011100                                                                          
011200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011300                                                                          
011400*01 -COPY WMSGINIT                                                        
011500                                                                          
011510                                                                          
011600*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
011700                                                                          
011800 01  SPAR-AREA.                                                           
011900     03  SPAR-IDTRANS            PIC X(4)    VALUE '2337'.                
012000     03  SPAR-IDARTNR-ENTER      PIC S9(9)   VALUE ZERO COMP-3.           
012010     03  SPAR-KDARTURS-ENTER     PIC X(2)    VALUE SPACE.                 
012020     03  SPAR-KDPRODSL-FOM-ENTER PIC S9(3)   VALUE ZERO COMP-3.           
012030     03  SPAR-KDPRODSL-TOM-ENTER PIC S9(3)   VALUE ZERO COMP-3.           
012040     03  SPAR-IDFKNGRP-FOM-ENTER PIC S9(5)   VALUE ZERO COMP-3.           
012050     03  SPAR-IDFKNGRP-TOM-ENTER PIC S9(5)   VALUE ZERO COMP-3.           
012060     03  SPAR-IDARTNR-NEXT       PIC S9(9)   VALUE ZERO COMP-3.           
012070     03  SPAR-KDARTURS-NEXT      PIC X(2)    VALUE SPACE.                 
012080     03  SPAR-KDPRODSL-FOM-NEXT  PIC S9(3)   VALUE ZERO COMP-3.           
012090     03  SPAR-KDPRODSL-TOM-NEXT  PIC S9(3)   VALUE ZERO COMP-3.           
012091     03  SPAR-IDFKNGRP-FOM-NEXT  PIC S9(5)   VALUE ZERO COMP-3.           
012092     03  SPAR-IDFKNGRP-TOM-NEXT  PIC S9(5)   VALUE ZERO COMP-3.           
012100                                                                          
012200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012300                                                                          
012400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012500                                                                          
012600*01  MID -COPY W2I33701                                                   
012700                                                                          
012710                                                                          
012800 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
012900                                                                          
013000*01  -COPY WMSGAREA                                                       
013100                                                                          
013110                                                                          
013200     03  MOD REDEFINES MSG-AREA.                                          
013300*      05  -COPY W2O33701                                                 
013400                                                                          
013410                                                                          
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013600                                                                          
013700*01  -COPY WMFSAREA                                                       
013800                                                                          
013810                                                                          
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300                                                                          
014500 01  FILLER              PIC X(16)  VALUE 'MSG-KOM-AREA'.                 
014600*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
014700*01  -COPY WMSGKOM                                                        
014800                                                                          
014900 01  NYCKLAR-TILL-DLI.                                                    
015000*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
015300                                                                          
015400     03  W-IDSPRGRP-X.                                                    
015600         05  W-IDSPRGRP          PIC X(10)   VALUE SPACE.                 
015700                                                                          
015800     03  W-IDARTNR-X.                                                     
015900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016000                                                                          
016010     03  W-IDARTNR-F8-X.                                                  
016020         05  W-IDARTNR-F8        PIC S9(9)   VALUE ZERO COMP-3.           
016030                                                                          
016100     03  W-KDARTURS-X.                                                    
016110         05  W-KDARTURS          PIC  X(2)   VALUE SPACE.                 
016200                                                                          
016201     03  W-KDARTURS-F8-X.                                                 
016202         05  W-KDARTURS-F8       PIC  X(2)   VALUE SPACE.                 
016203                                                                          
016204     03  W-KDPRODSL-X.                                                    
016205         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
016206                                                                          
016210     03  W-WDF814KY-X.                                                    
016220         05  W-KDPRODSL-FOM      PIC S9(3)   VALUE ZERO COMP-3.           
016221         05  W-KDPRODSL-TOM      PIC S9(3)   VALUE ZERO COMP-3.           
016222                                                                          
016223     03  W-WDF814KY-F8-X.                                                 
016224         05  W-KDPRODSL-FOM-F8   PIC S9(3)   VALUE ZERO COMP-3.           
016225         05  W-KDPRODSL-TOM-F8   PIC S9(3)   VALUE ZERO COMP-3.           
016226                                                                          
016227     03  W-IDFKNGRP-X.                                                    
016228         05  W-IDFKNGRP          PIC S9(5)   VALUE ZERO COMP-3.           
016230                                                                          
016240     03  W-WDF815KY-X.                                                    
016250         05  W-IDFKNGRP-FOM      PIC S9(5)   VALUE ZERO COMP-3.           
016260         05  W-IDFKNGRP-TOM      PIC S9(5)   VALUE ZERO COMP-3.           
016261                                                                          
016262     03  W-WDF815KY-F8-X.                                                 
016263         05  W-IDFKNGRP-FOM-F8   PIC S9(5)   VALUE ZERO COMP-3.           
016264         05  W-IDFKNGRP-TOM-F8   PIC S9(5)   VALUE ZERO COMP-3.           
016270                                                                          
016300                                                                          
016400*    --- STATUS-KOD FRÅN IMS                                              
016500 01  STATUS-WS                   PIC XX.                                  
016600     88  SEGMENT-FINNS                       VALUE '  '.                  
016700     88  ARTIKEL-FINNS                       VALUE 'NI'.                  
016800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000                                                                          
017100 01  GODK-STATUSKODER.                                                    
017200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017300                                                                          
017400 01  ALL-SSA.                                                             
017410     03 SSA1                     PIC X(64).                               
017500     03 SSA2                     PIC X(64).                               
017600                                                                          
017610                                                                          
017700*    --- IMS FUNKTIONSKODER                                               
017800*01  -COPY W0003                                                          
017900                                                                          
017910                                                                          
018000*    ---  DLI INPUT-OUTPUT AREA                                           
018100                                                                          
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
018300 01  DLI-IO-WDK601.                                                       
018400*    03  -COPY WDK601 -PRE WDK6                                           
018500                                                                          
018510                                                                          
018600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
018700 01  DLI-IO-WDK611.                                                       
018800*    03  -COPY WDK611 -PRE WDK6                                           
018900                                                                          
018910                                                                          
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF810'.                      
019100 01  DLI-IO-WDF801.                                                       
019200*    03  -COPY WDF801                                                     
019300                                                                          
019310                                                                          
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF812'.                      
019500 01  DLI-IO-WDF812.                                                       
019600*    03  -COPY WDF812                                                     
019610                                                                          
019620 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF813'.                      
019630 01  DLI-IO-WDF813.                                                       
019640*    03  -COPY WDF813                                                     
019650                                                                          
019660 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF814'.                      
019670 01  DLI-IO-WDF814.                                                       
019680*    03  -COPY WDF814                                                     
019700                                                                          
019701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF815'.                      
019702 01  DLI-IO-WDF815.                                                       
019703*    03  -COPY WDF815                                                     
019704                                                                          
019710                                                                          
019720                                                                          
019800 LINKAGE SECTION.                                                         
019900*01  -COPY W0009  -PRE MSG-                                               
020000                                                                          
020100*01  -COPY W0009  -PRE MSGKOM-                                            
020200                                                                          
020300*01  -COPY W0008  -PRE USEA-                                              
020400     05  FILLER                  PIC X.                                   
020500                                                                          
020600*01  -COPY W0008  -PRE WDK6-                                              
020700     05  FILLER                  PIC X.                                   
020800                                                                          
020900*01  -COPY W0008  -PRE WDF8-                                              
021000     05  FILLER                  PIC X.                                   
021010                                                                          
021020*01  -COPY W0008  -PRE WDF8B-                                             
021030     05  FILLER                  PIC X.                                   
021100                                                                          
021110                                                                          
021200 PROCEDURE DIVISION  USING MSG-PCB MSGKOM-PCB USEA-PCB                    
021300                           WDK6-PCB WDF8-PCB WDF8B-PCB.                   
021400 MAIN SECTION.                                                            
021500     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB USEA-PCB                    
021600                           WDK6-PCB WDF8-PCB WDF8B-PCB.                   
021700                                                                          
021800     PERFORM IMS-GET-MSG                                                  
021900     IF SEGMENT-FINNS                                                     
022000        PERFORM IMS-GET-WMSGKOM-MSG                                       
022100        PERFORM A-INIT                                                    
022200        PERFORM B-KOLLA-NYCKLAR                                           
022300        IF NYCKLAR-OK                                                     
022400           IF MFS-UPDATE OR MFS-UPD-X                                     
022500              PERFORM G-KOLLA-INPUT                                       
022600              IF INDATA-OK                                                
022700                 PERFORM H-UPPDATERA                                      
022800              END-IF                                                      
022900           ELSE                                                           
023000              IF MFS-FIRST                                                
023100                 PERFORM C-FOERSTA-SIDA                                   
023200              ELSE                                                        
023300                 IF MFS-NEXT                                              
023400                    PERFORM D-NAESTA-SIDA                                 
023500                 ELSE                                                     
023600                    PERFORM E-SAMMA-SIDA                                  
023700                 END-IF                                                   
023800              END-IF                                                      
023900           END-IF                                                         
024000           IF ALLT-OK                                                     
024100              PERFORM F-LAES-VISA-INFO                                    
024110           ELSE                                                           
024120              IF E-RAD-FEL                                                
024122                 PERFORM FE-E-RAD-TO-MOD                                  
024130              END-IF                                                      
024200           END-IF                                                         
024300        END-IF                                                            
024400        IF MFS-UPD-X                                                      
024500           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM + 17          
024600           PERFORM IMS-INSERT-WMSGKOM-MSG                                 
024700        ELSE                                                              
024800           COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33701 + 4                  
024900           PERFORM IMS-INSERT-MSG                                         
025000        END-IF                                                            
025100     END-IF                                                               
025200                                                                          
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     .                                                                    
025600                                                                          
025610                                                                          
025700 A-INIT SECTION.                                                          
025710     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
025800                                                                          
025900     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
026000                                                                          
026100     IF MSG-DUBBLA-TRANSKODER                                             
026200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33701                 
026300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
026400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026500     ELSE                                                                 
026600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I33701                  
026700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026900     END-IF                                                               
027000                                                                          
027100     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
027200     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
027300     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
027400                                                                          
027500     MOVE LOW-VALUE        TO MSG-AREA                                    
027600     MOVE 'W2O337N1'       TO MFS-IDMOD                                   
027700     MOVE '2337'           TO MOD-IDTRANS                                 
027710                                                                          
027800     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
027900                                                                          
028000     IF EGEN-MID OR HELP-MID                                              
028010       IF MID-IDSPRGRP-IN NOT = ALL '+'                                   
028020       OR MID-IDARTNR-IN  NOT = ALL '+'                                   
028021          MOVE SPACE       TO MFS-KDTRTYP                                 
028022          MOVE '7'         TO MFS-IDPFK                                   
028030       ELSE                                                               
028100          CONTINUE                                                        
028110       END-IF                                                             
028200     ELSE                                                                 
028300       MOVE SPACE          TO MFS-KDTRTYP                                 
028400       MOVE '7'            TO MFS-IDPFK                                   
028500     END-IF                                                               
028600     .                                                                    
028700                                                                          
028710                                                                          
028800 B-KOLLA-NYCKLAR SECTION.                                                 
028810     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
028900                                                                          
029000     MOVE ALL '+'            TO MSGI-WMSGINIT                             
029100     MOVE '001'              TO MSGI-KDCALL                               
029200     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
029300     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
029400     MOVE '2337'             TO MSGI-IDTRANS                              
029500                                                                          
029600     IF EGEN-MID                                                          
029700       MOVE MID-IDSPRGRP-IN  TO MSGI-IDDIRGRP                             
029800       IF MID-IDARTNR-IN NOT = ALL '+'                                    
029801         INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO           
029810         IF MID-IDARTNR-IN NUMERIC                                        
029900            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
029901         ELSE                                                             
029902            MOVE ALL '+'        TO MID-IDARTNR-IN                         
029910         END-IF                                                           
030000       ELSE                                                               
030100         MOVE MID-IDARTNR-UT TO MID-IDARTNR-IN                            
030200                                MSGI-IDARTNR                              
030300       END-IF                                                             
030310     ELSE                                                                 
030320       MOVE ALL '+'          TO MID-IDARTNR-IN                            
030400     END-IF                                                               
030500                                                                          
030600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030700     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
030800                                                                          
030810*VALIDATE SPAR AREA                                                       
030811     IF SPAR-IDARTNR-ENTER  NOT NUMERIC                                   
030812        MOVE ZERO            TO SPAR-IDARTNR-ENTER                        
030813     END-IF                                                               
030814     IF SPAR-KDPRODSL-FOM-ENTER NOT NUMERIC                               
030815        MOVE ZERO            TO SPAR-KDPRODSL-FOM-ENTER                   
030816     END-IF                                                               
030817     IF SPAR-KDPRODSL-TOM-ENTER NOT NUMERIC                               
030818        MOVE ZERO            TO SPAR-KDPRODSL-TOM-ENTER                   
030819     END-IF                                                               
030820     IF SPAR-IDFKNGRP-FOM-ENTER NOT NUMERIC                               
030830        MOVE ZERO            TO SPAR-IDFKNGRP-FOM-ENTER                   
030840     END-IF                                                               
030841     IF SPAR-IDFKNGRP-TOM-ENTER NOT NUMERIC                               
030842        MOVE ZERO            TO SPAR-IDFKNGRP-TOM-ENTER                   
030843     END-IF                                                               
030844     IF SPAR-IDARTNR-NEXT       NOT NUMERIC                               
030845        MOVE ZERO            TO SPAR-IDARTNR-NEXT                         
030846     END-IF                                                               
030847     IF SPAR-KDPRODSL-FOM-NEXT  NOT NUMERIC                               
030848        MOVE ZERO            TO SPAR-KDPRODSL-FOM-NEXT                    
030849     END-IF                                                               
030850     IF SPAR-KDPRODSL-TOM-NEXT  NOT NUMERIC                               
030851        MOVE ZERO            TO SPAR-KDPRODSL-TOM-NEXT                    
030852     END-IF                                                               
030853     IF SPAR-IDFKNGRP-FOM-NEXT  NOT NUMERIC                               
030854        MOVE ZERO            TO SPAR-IDFKNGRP-FOM-NEXT                    
030855     END-IF                                                               
030856     IF SPAR-IDFKNGRP-TOM-NEXT  NOT NUMERIC                               
030857        MOVE ZERO            TO SPAR-IDFKNGRP-TOM-NEXT                    
030858     END-IF                                                               
030860*                                                                         
030900     IF MSGI-IDLAND-SPR = 'GB'                                            
031000       MOVE 'GB'             TO MED-IDSKYLT                               
031100     ELSE                                                                 
031200       MOVE 'S'              TO MED-IDSKYLT                               
031300     END-IF                                                               
031400                                                                          
031500     MOVE JA                 TO ALLT-SW                                   
031600     MOVE JA                 TO NYCKLAR-SW                                
031700     MOVE ZERO               TO W-IDARTNR-KEY                             
031800     MOVE SPACE              TO MED-IDMFSFEL                              
031900                                MED-IDMFSINF                              
032000                                MSG-KOM-IDMFSMED                          
032100                                                                          
032400                                                                          
032500     MOVE MSGI-IDDIRGRP      TO W-IDSPRGRP                                
032900                                                                          
033000     IF MID-IDARTNR-IN NOT = ALL '+'                                      
033100        INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO            
033200        IF MID-IDARTNR-IN NUMERIC                                         
033300           MOVE MID-IDARTNR-IN TO WS-IDARTNR                              
033400           IF MID-IDARTNR-IN NOT = ZERO                                   
033500              MOVE WS-IDARTNR     TO W-IDARTNR-KEY                        
033600           END-IF                                                         
033700        ELSE                                                              
033800           MOVE NEJ            TO NYCKLAR-SW                              
033900        END-IF                                                            
034000        MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                          
034100        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
034101        MOVE MFS-RENSA-FAELT   TO MOD-IDSPRGRP-IN                         
034200     ELSE                                                                 
034300        MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                          
034400     END-IF                                                               
034500                                                                          
034600     IF GODK-MID OR NYCKLAR-OK                                            
034700        MOVE MSGI-IDDIRGRP    TO MOD-IDSPRGRP-UT                          
034800     ELSE                                                                 
034900        MOVE MFS-RENSA-FAELT  TO MOD-IDSPRGRP-UT                          
035000     END-IF                                                               
035100                                                                          
035200     IF NYCKLAR-FEL                                                       
035300        MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                             
035400                                 MSG-KOM-IDMFSMED                         
035500        CALL WMEDKONV USING MED-WMEDAREA                                  
035600        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
035700        PERFORM MFS-RENSA-FAELT-IN                                        
035800        PERFORM MFS-RENSA-FAELT-UT                                        
036100     END-IF                                                               
036200     .                                                                    
036300                                                                          
036310                                                                          
036400 C-FOERSTA-SIDA SECTION.                                                  
036410     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
036500                                                                          
036600     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
036700                            MSG-KOM-IDMFSMED                              
036800     CALL WMEDKONV USING MED-WMEDAREA                                     
036900     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
037000                                                                          
037100     PERFORM MFS-RENSA-FAELT-IN                                           
037200     .                                                                    
037300                                                                          
037310                                                                          
037400 D-NAESTA-SIDA SECTION.                                                   
037410     MOVE 'D-NAESTA-SIDA   ' TO CURRENT-SECTION                           
037420                                                                          
037500     IF SPAR-IDTRANS = '2337'                                             
037600        MOVE SPAR-IDARTNR-NEXT      TO W-IDARTNR                          
037610        MOVE SPAR-KDARTURS-NEXT     TO W-KDARTURS                         
037620        MOVE SPAR-KDPRODSL-FOM-NEXT TO W-KDPRODSL-FOM                     
037630        MOVE SPAR-KDPRODSL-TOM-NEXT TO W-KDPRODSL-TOM                     
037640        MOVE SPAR-IDFKNGRP-FOM-NEXT TO W-IDFKNGRP-FOM                     
037650        MOVE SPAR-IDFKNGRP-TOM-NEXT TO W-IDFKNGRP-TOM                     
037700     ELSE                                                                 
037800        PERFORM MFS-RENSA-FAELT-IN                                        
037900     END-IF                                                               
038000     .                                                                    
038100                                                                          
038110                                                                          
038200 E-SAMMA-SIDA SECTION.                                                    
038201     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
038203                                                                          
038204     IF SPAR-IDTRANS = '2337'                                             
038205        MOVE SPAR-IDARTNR-ENTER      TO W-IDARTNR                         
038206        MOVE SPAR-KDARTURS-ENTER     TO W-KDARTURS                        
038207        MOVE SPAR-KDPRODSL-FOM-ENTER TO W-KDPRODSL-FOM                    
038208        MOVE SPAR-KDPRODSL-TOM-ENTER TO W-KDPRODSL-TOM                    
038209        MOVE SPAR-IDFKNGRP-FOM-ENTER TO W-IDFKNGRP-FOM                    
038210        MOVE SPAR-IDFKNGRP-TOM-ENTER TO W-IDFKNGRP-TOM                    
038211     END-IF                                                               
038212     MOVE JA  TO INDATA-SW                                                
038213     MOVE NEJ TO RAD-VALD-SW                                              
038214                                                                          
038220     MOVE 1 TO INDX                                                       
038230     PERFORM UNTIL INDX > MAX-INDX                                        
038234        PERFORM EA-KOLLA-ARTIKEL                                          
038237        PERFORM EB-KOLLA-URSPRUNG                                         
038240        PERFORM EC-KOLLA-PRODUKTSLAG                                      
038243        PERFORM ED-KOLLA-FUNKTIONSGRUPP                                   
038301        ADD 1 TO INDX                                                     
038302     END-PERFORM                                                          
038303                                                                          
038310     IF INDATA-OK AND                                                     
038400        SPAR-IDTRANS = '2337' OR '0551'                                   
038500                                                                          
038801        IF RAD-VALD                                                       
038803           PERFORM MFS-LAES-IN-IGEN-E                                     
038804           MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                            
038805           CALL WMEDKONV USING MED-WMEDAREA                               
038806           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
038808           MOVE 1 TO INDX                                                 
038809           PERFORM UNTIL INDX > MAX-INDX                                  
038811              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-A-ATTR (INDX)            
038812                                       MOD-KDCMD-U-ATTR (INDX)            
038813                                       MOD-KDCMD-P-ATTR (INDX)            
038814                                       MOD-KDCMD-F-ATTR (INDX)            
038815              ADD +1 TO INDX                                              
038816           END-PERFORM                                                    
038869        ELSE                                                              
038870           PERFORM MFS-ROER-EJ-FAELT-UT                                   
038871           PERFORM MFS-ROER-EJ-FAELT-IN                                   
038872           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
038880           PERFORM MFS-LAES-IN-IGEN-E                                     
038900           MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                            
039100           CALL WMEDKONV USING MED-WMEDAREA                               
039200           MOVE MED-MFSFEL     TO MOD-TEMFSFEL                            
039410        END-IF                                                            
039411                                                                          
039420           IF MID-IDARTNR-E = SPACE                                       
039421           OR MID-IDARTNR-E = ALL '+'                                     
039422              CONTINUE                                                    
039423           ELSE                                                           
039430              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-E-ATTR                   
039440                                       MOD-IDARTNR-E-ATTR                 
039450                                       MOD-KDARTURS-E-ATTR                
039460                                       MOD-TISTADAT-U-E-ATTR              
039470                                       MOD-KDPRODSL-FOM-E-ATTR            
039480                                       MOD-KDPRODSL-TOM-E-ATTR            
039490                                       MOD-TISTADAT-P-E-ATTR              
039500                                       MOD-IDFKNGRP-FOM-E-ATTR            
039600                                       MOD-IDFKNGRP-TOM-E-ATTR            
039700                                       MOD-TISTADAT-F-E-ATTR              
039710           END-IF                                                         
039720           IF MID-KDARTURS-E = SPACE                                      
039721           OR MID-KDARTURS-E = ALL '+'                                    
039722              CONTINUE                                                    
039723           ELSE                                                           
039730              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-E-ATTR                   
039740                                       MOD-IDARTNR-E-ATTR                 
039750                                       MOD-TISTADAT-A-E-ATTR              
039760                                       MOD-KDARTURS-E-ATTR                
039770                                       MOD-KDPRODSL-FOM-E-ATTR            
039780                                       MOD-KDPRODSL-TOM-E-ATTR            
039790                                       MOD-TISTADAT-P-E-ATTR              
039791                                       MOD-IDFKNGRP-FOM-E-ATTR            
039792                                       MOD-IDFKNGRP-TOM-E-ATTR            
039793                                       MOD-TISTADAT-F-E-ATTR              
039794           END-IF                                                         
039795           IF MID-KDPRODSL-FOM-E = SPACE                                  
039796           OR MID-KDPRODSL-FOM-E = ALL '+'                                
039797              CONTINUE                                                    
039798           ELSE                                                           
039799              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-E-ATTR                   
039800                                       MOD-IDARTNR-E-ATTR                 
039801                                       MOD-TISTADAT-A-E-ATTR              
039802                                       MOD-KDARTURS-E-ATTR                
039803                                       MOD-TISTADAT-U-E-ATTR              
039804                                       MOD-KDPRODSL-FOM-E-ATTR            
039805                                       MOD-KDPRODSL-TOM-E-ATTR            
039806                                       MOD-IDFKNGRP-FOM-E-ATTR            
039807                                       MOD-IDFKNGRP-TOM-E-ATTR            
039808                                       MOD-TISTADAT-F-E-ATTR              
039809           END-IF                                                         
039810           IF MID-IDFKNGRP-FOM-E = SPACE                                  
039811           OR MID-IDFKNGRP-FOM-E = ALL '+'                                
039812              CONTINUE                                                    
039813           ELSE                                                           
039814              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-E-ATTR                   
039815                                       MOD-IDARTNR-E-ATTR                 
039816                                       MOD-TISTADAT-A-E-ATTR              
039817                                       MOD-KDARTURS-E-ATTR                
039818                                       MOD-TISTADAT-U-E-ATTR              
039819                                       MOD-KDPRODSL-FOM-E-ATTR            
039820                                       MOD-KDPRODSL-TOM-E-ATTR            
039821                                       MOD-TISTADAT-P-E-ATTR              
039822                                       MOD-IDFKNGRP-FOM-E-ATTR            
039823                                       MOD-IDFKNGRP-TOM-E-ATTR            
039824           END-IF                                                         
039830     END-IF                                                               
039900     .                                                                    
040000                                                                          
040010                                                                          
040020 EA-KOLLA-ARTIKEL   SECTION.                                              
040030     MOVE 'EA-KOLLA-ARTIKEL' TO CURRENT-SECTION                           
040031                                                                          
040032     IF MID-KDCMD-A (INDX) NOT = '+' AND                                  
040033        MID-KDCMD-A (INDX) NOT = ' '                                      
040034        IF MID-KDCMD-A (INDX) NOT = 'C' OR                                
040035           RAD-VALD                                                       
040036                                                                          
040037           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-A-ATTR (INDX)        
040038           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
040039                                                                          
040040           CALL WMEDKONV USING MED-WMEDAREA                               
040041           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
040043           MOVE NEJ TO INDATA-SW                                          
040044           PERFORM MFS-ROER-EJ-FAELT-UT                                   
040045           PERFORM MFS-ROER-EJ-FAELT-IN-ART                               
040046           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
040047        ELSE                                                              
040048           PERFORM EAA-FLYTTA-VALD-ARTIKEL                                
040049           MOVE JA TO RAD-VALD-SW                                         
040050        END-IF                                                            
040051     END-IF                                                               
040052     .                                                                    
040053                                                                          
040054                                                                          
040055 EAA-FLYTTA-VALD-ARTIKEL SECTION.                                         
040056     MOVE 'EAA-FLYTTA-ARTIK' TO CURRENT-SECTION                           
040057                                                                          
040058     MOVE MFS-RENSA-FAELT    TO MOD-KDCMD-A (INDX)                        
040059     INSPECT MID-IDARTNR (INDX) REPLACING LEADING SPACE BY ZERO           
040060     MOVE MID-IDARTNR (INDX) TO WS-IDARTNR                                
040061     MOVE WS-IDARTNR         TO W-IDARTNR-F8                              
040062                                                                          
040070     PERFORM IMS-GHU-WDF812M                                              
040073                                                                          
040080     MOVE 'C'                TO MOD-KDCMD-E                               
040081                                MID-KDCMD-E                               
040090     MOVE ASPR-IDARTNR          TO WS-IDARTNR                             
040092     MOVE WS-IDARTNR         TO MOD-IDARTNR-E                             
040093                                MID-IDARTNR-E                             
040094     MOVE ASPR-TISTADAT      TO WS-TISTADAT                               
040095     MOVE WS-TISTADAT        TO MOD-TISTADAT-A-E                          
040096                                MID-TISTADAT-A-E                          
040146     .                                                                    
040147                                                                          
040148                                                                          
040149 EB-KOLLA-URSPRUNG  SECTION.                                              
040150     MOVE 'EB-KOLLA-URSPRUN' TO CURRENT-SECTION                           
040151                                                                          
040152     IF MID-KDCMD-U (INDX) NOT = '+' AND                                  
040153        MID-KDCMD-U (INDX) NOT = ' '                                      
040154        IF MID-KDCMD-U (INDX) NOT = 'C' OR                                
040155           RAD-VALD                                                       
040156           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-U-ATTR (INDX)        
040157           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
040158                                                                          
040159           CALL WMEDKONV USING MED-WMEDAREA                               
040160           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
040161           MOVE NEJ TO INDATA-SW                                          
040162           PERFORM MFS-ROER-EJ-FAELT-UT                                   
040163           PERFORM MFS-ROER-EJ-FAELT-IN-URS                               
040164           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
040165        ELSE                                                              
040166           PERFORM EBA-FLYTTA-VALT-URSPRUNG                               
040167           MOVE JA TO RAD-VALD-SW                                         
040168        END-IF                                                            
040169     END-IF                                                               
040170     .                                                                    
040171                                                                          
040172                                                                          
040173 EBA-FLYTTA-VALT-URSPRUNG SECTION.                                        
040174     MOVE 'EBE-FLYTTA-URSPR' TO CURRENT-SECTION                           
040175                                                                          
040176     MOVE MFS-RENSA-FAELT     TO MOD-KDCMD-U (INDX)                       
040177     MOVE MID-KDARTURS (INDX) TO W-KDARTURS-F8                            
040178                                                                          
040179     PERFORM IMS-GHU-WDF813M                                              
040180                                                                          
040181     MOVE 'C'            TO MOD-KDCMD-E                                   
040182                            MID-KDCMD-E                                   
040183     MOVE USPR-KDARTURS  TO MOD-KDARTURS-E                                
040184                            MID-KDARTURS-E                                
040185     MOVE USPR-TISTADAT  TO WS-TISTADAT                                   
040186     MOVE WS-TISTADAT    TO MOD-TISTADAT-U-E                              
040187                            MID-TISTADAT-U-E                              
040200     .                                                                    
040201                                                                          
040202                                                                          
040203 EC-KOLLA-PRODUKTSLAG  SECTION.                                           
040204     MOVE 'EC-KOLLA-PRODSL ' TO CURRENT-SECTION                           
040205                                                                          
040206     IF MID-KDCMD-P (INDX) NOT = '+' AND                                  
040207        MID-KDCMD-P (INDX) NOT = ' '                                      
040208        IF MID-KDCMD-P (INDX) NOT = 'C' OR                                
040209           RAD-VALD                                                       
040210           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-P-ATTR (INDX)        
040211           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
040212                                                                          
040213           CALL WMEDKONV USING MED-WMEDAREA                               
040214           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
040215           MOVE NEJ TO INDATA-SW                                          
040216           PERFORM MFS-ROER-EJ-FAELT-UT                                   
040217           PERFORM MFS-ROER-EJ-FAELT-IN-PRO                               
040218           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
040219        ELSE                                                              
040220           PERFORM ECA-FLYTTA-VALT-PRODUKTSLAG                            
040221           MOVE JA TO RAD-VALD-SW                                         
040222        END-IF                                                            
040223     END-IF                                                               
040224     .                                                                    
040225                                                                          
040226                                                                          
040227 ECA-FLYTTA-VALT-PRODUKTSLAG SECTION.                                     
040228     MOVE 'ECA-FLYTTA-PRODS' TO CURRENT-SECTION                           
040229                                                                          
040230     MOVE MFS-RENSA-FAELT         TO MOD-KDCMD-P (INDX)                   
040231     MOVE MID-KDPRODSL-FOM (INDX) TO W-KDPRODSL-FOM-F8                    
040232     MOVE MID-KDPRODSL-TOM (INDX) TO W-KDPRODSL-TOM-F8                    
040233                                                                          
040234     PERFORM IMS-GHU-WDF814M                                              
040235                                                                          
040236     MOVE 'C'               TO MOD-KDCMD-E                                
040237                               MID-KDCMD-E                                
040238     MOVE PSPR-KDPRODSL-FOM TO MOD-KDPRODSL-FOM-E                         
040239                               MID-KDPRODSL-FOM-E                         
040240     MOVE PSPR-KDPRODSL-TOM TO MOD-KDPRODSL-TOM-E                         
040241                               MID-KDPRODSL-TOM-E                         
040242     MOVE PSPR-TISTADAT  TO WS-TISTADAT                                   
040243     MOVE WS-TISTADAT    TO MOD-TISTADAT-P-E                              
040244                            MID-TISTADAT-P-E                              
040257     .                                                                    
040258                                                                          
040259                                                                          
040260 ED-KOLLA-FUNKTIONSGRUPP  SECTION.                                        
040261     MOVE 'ED-KOLLA-FKNGRP ' TO CURRENT-SECTION                           
040262                                                                          
040263     IF MID-KDCMD-F (INDX) NOT = '+' AND                                  
040264        MID-KDCMD-F (INDX) NOT = ' '                                      
040265        IF MID-KDCMD-F (INDX) NOT = 'C' OR                                
040266           RAD-VALD                                                       
040267           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-F-ATTR (INDX)        
040268           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
040269                                                                          
040270           CALL WMEDKONV USING MED-WMEDAREA                               
040271           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
040272           MOVE NEJ TO INDATA-SW                                          
040273           PERFORM MFS-ROER-EJ-FAELT-UT                                   
040274           PERFORM MFS-ROER-EJ-FAELT-IN-FKN                               
040275           PERFORM MFS-ROER-EJ-FAELT-IN-E                                 
040276        ELSE                                                              
040277           PERFORM EDA-FLYTTA-VALD-FUNKTIONSGRUPP                         
040278           MOVE JA TO RAD-VALD-SW                                         
040279        END-IF                                                            
040280     END-IF                                                               
040281     .                                                                    
040282                                                                          
040283                                                                          
040284 EDA-FLYTTA-VALD-FUNKTIONSGRUPP SECTION.                                  
040285     MOVE 'EDA-FLYTTA-FKNGR' TO CURRENT-SECTION                           
040286                                                                          
040287     MOVE MFS-RENSA-FAELT         TO MOD-KDCMD-F (INDX)                   
040288     MOVE MID-IDFKNGRP-FOM (INDX) TO W-IDFKNGRP-FOM-F8                    
040289     MOVE MID-IDFKNGRP-TOM (INDX) TO W-IDFKNGRP-TOM-F8                    
040290                                                                          
040291     PERFORM IMS-GHU-WDF815M                                              
040292                                                                          
040293     MOVE 'C'               TO MOD-KDCMD-E                                
040294                               MID-KDCMD-E                                
040295     MOVE FSPR-IDFKNGRP-FOM TO MOD-IDFKNGRP-FOM-E                         
040296                               MID-IDFKNGRP-FOM-E                         
040297     MOVE FSPR-IDFKNGRP-TOM TO MOD-IDFKNGRP-TOM-E                         
040298                               MID-IDFKNGRP-TOM-E                         
040299     MOVE FSPR-TISTADAT  TO WS-TISTADAT                                   
040300     MOVE WS-TISTADAT    TO MOD-TISTADAT-F-E                              
040301                            MID-TISTADAT-F-E                              
040314     .                                                                    
040315                                                                          
040316                                                                          
040439 F-LAES-VISA-INFO SECTION.                                                
040440     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
040441                                                                          
040442     PERFORM IMS-GU-WDF801                                                
040443                                                                          
040444     IF SEGMENT-FINNS                                                     
040445        MOVE GSPR-TISTADAT     TO WS-TISTADAT                             
040446        MOVE WS-TISTADAT       TO MOD-TISTADAT-GRP                        
040447        IF GSPR-TISTADAT > DAGENS-DATUM                                   
040448           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-GRP-ATTR            
040449        END-IF                                                            
040450        IF GSPR-FLAUTUPD = 'N'                                            
040451           MOVE 'NO'           TO MOD-FLAUTUPD-GRP                        
040452           PERFORM FA-VISA-ARTIKEL                                        
040453           MOVE +1 TO INDX                                                
040454           PERFORM UNTIL INDX > MAX-INDX                                  
040455              MOVE MFS-RENSA-FAELT  TO MOD-KDARTURS     (INDX)            
040456                                       MOD-TISTADAT-U   (INDX)            
040457              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-U-ATTR (INDX)            
040458              MOVE MFS-RENSA-FAELT  TO MOD-KDPRODSL-FOM (INDX)            
040459                                       MOD-KDPRODSL-TOM (INDX)            
040460                                       MOD-TISTADAT-P   (INDX)            
040461              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-P-ATTR (INDX)            
040462              MOVE MFS-RENSA-FAELT  TO MOD-IDFKNGRP-FOM (INDX)            
040463                                       MOD-IDFKNGRP-TOM (INDX)            
040464                                       MOD-TISTADAT-F   (INDX)            
040465              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-F-ATTR (INDX)            
040466              ADD +1 TO INDX                                              
040467           END-PERFORM                                                    
040468           MOVE MFS-RENSA-FAELT     TO MOD-KDARTURS-E                     
040469                                    MOD-TISTADAT-U-E                      
040470           MOVE MFS-STAENG-FAELT TO MOD-KDARTURS-E-ATTR                   
040471                                    MOD-TISTADAT-U-E-ATTR                 
040472           MOVE MFS-RENSA-FAELT  TO MOD-KDPRODSL-FOM-E                    
040473                                    MOD-KDPRODSL-TOM-E                    
040474                                    MOD-TISTADAT-P-E                      
040475           MOVE MFS-STAENG-FAELT TO MOD-KDPRODSL-FOM-E-ATTR               
040476                                    MOD-KDPRODSL-TOM-E-ATTR               
040477                                    MOD-TISTADAT-P-E-ATTR                 
040478           MOVE MFS-RENSA-FAELT  TO MOD-IDFKNGRP-FOM-E                    
040479                                    MOD-IDFKNGRP-TOM-E                    
040480                                    MOD-TISTADAT-F-E                      
040481           MOVE MFS-STAENG-FAELT TO MOD-IDFKNGRP-FOM-E-ATTR               
040482                                    MOD-IDFKNGRP-TOM-E-ATTR               
040483                                    MOD-TISTADAT-F-E-ATTR                 
040484        ELSE                                                              
040485           MOVE 'YES'          TO MOD-FLAUTUPD-GRP                        
040486           PERFORM FB-VISA-URSPRUNG                                       
040487           PERFORM FC-VISA-PRODUKTSLAG                                    
040488           PERFORM FD-VISA-FUNKTIONSGRUPP                                 
040489           MOVE +1 TO INDX                                                
040490           PERFORM UNTIL INDX > MAX-INDX                                  
040491              MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR (INDX)                 
040492                                       MOD-TISTADAT-A (INDX)              
040493              MOVE MFS-STAENG-FAELT TO MOD-KDCMD-A-ATTR (INDX)            
040494              ADD +1 TO INDX                                              
040495           END-PERFORM                                                    
040496           MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-E                      
040497                                    MOD-TISTADAT-A-E                      
040498           MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-E-ATTR                    
040499                                    MOD-TISTADAT-A-E-ATTR                 
040500        END-IF                                                            
040501                                                                          
040502                                                                          
040503        IF E-RAD-FEL                                                      
040504           PERFORM FE-E-RAD-TO-MOD                                        
040505        END-IF                                                            
040506     ELSE                                                                 
040507        MOVE GROUP-MISSING     TO MED-IDMFSFEL                            
040508                                  MSG-KOM-IDMFSMED                        
040509        CALL WMEDKONV USING MED-WMEDAREA                                  
040510        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
040511        PERFORM MFS-RENSA-FAELT-UT                                        
040520     END-IF                                                               
040600                                                                          
045700                                                                          
045800     MOVE '002'                TO MSGI-KDCALL                             
045900     MOVE '2337'               TO SPAR-IDTRANS                            
046000     MOVE SPAR-AREA            TO MSGI-SPAR-AREA                          
046100     MOVE W-IDSPRGRP           TO MSGI-IDDIRGRP                           
046200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046300     .                                                                    
046400                                                                          
046410                                                                          
046500 FA-VISA-ARTIKEL SECTION.                                                 
046510     MOVE 'FA-VISA-ARTIKEL ' TO CURRENT-SECTION                           
046614                                                                          
046620     IF W-IDARTNR-KEY = ZERO                                              
046621        IF MFS-FIRST                                                      
046622           PERFORM IMS-GHNP-WDF812                                        
046624        ELSE                                                              
046625           PERFORM IMS-GHNP-WDF812-KEY                                    
046627        END-IF                                                            
046628     ELSE                                                                 
046629        MOVE W-IDARTNR-KEY TO W-IDARTNR                                   
046630        PERFORM IMS-GHNP-WDF812-KEY                                       
046633     END-IF                                                               
046634                                                                          
046635     MOVE +1 TO INDX                                                      
046636     IF SEGMENT-SAKNAS                                                    
046637        MOVE ZERO                     TO SPAR-IDARTNR-ENTER               
046638     ELSE                                                                 
046639        MOVE ASPR-IDARTNR             TO SPAR-IDARTNR-ENTER               
046640        PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                   
046641           IF SEGMENT-FINNS                                               
046643              MOVE ASPR-TISTADAT      TO WS-TISTADAT                      
046647              MOVE WS-TISTADAT        TO MOD-TISTADAT-A (INDX)            
046648        IF ASPR-TISTADAT > DAGENS-DATUM                                   
046649          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-A-ATTR(INDX)         
046650        END-IF                                                            
046651              MOVE ASPR-IDARTNR       TO WS-IDARTNR                       
046652              MOVE WS-IDARTNR         TO MOD-IDARTNR (INDX)               
046653              IF W-IDARTNR-KEY > ZERO                                     
046654                ADD +1 TO INDX                                            
046655                PERFORM UNTIL INDX > MAX-INDX                             
046656                  MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR      (INDX)        
046657                                           MOD-TISTADAT-A   (INDX)        
046658                  MOVE MFS-STAENG-FAELT TO MOD-KDCMD-A-ATTR (INDX)        
046659                  ADD +1 TO INDX                                          
046660                END-PERFORM                                               
046661              END-IF                                                      
046662           ELSE                                                           
046663              MOVE MFS-RENSA-FAELT TO MOD-IDARTNR    (INDX)               
046664                                      MOD-TISTADAT-A (INDX)               
046665           END-IF                                                         
046666           ADD +1 TO INDX                                                 
046667           PERFORM IMS-GHNP-WDF812                                        
046668        END-PERFORM                                                       
046669     END-IF                                                               
046670     PERFORM UNTIL INDX > MAX-INDX                                        
046671        MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR      (INDX)                  
046672                                 MOD-TISTADAT-A   (INDX)                  
046673        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-A-ATTR (INDX)                  
046674        ADD +1 TO INDX                                                    
046675     END-PERFORM                                                          
046676                                                                          
046677     IF SEGMENT-FINNS                                                     
046678        MOVE ASPR-IDARTNR         TO SPAR-IDARTNR-NEXT                    
046679        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
046680                                     MSG-KOM-IDMFSMED                     
046681        CALL WMEDKONV USING MED-WMEDAREA                                  
046682        MOVE MED-TEMFSINF         TO MOD-TEMFSINF                         
046683     ELSE                                                                 
046684        MOVE SPAR-IDARTNR-ENTER   TO SPAR-IDARTNR-NEXT                    
046685     END-IF                                                               
046686     .                                                                    
046687                                                                          
046688                                                                          
046689 FB-VISA-URSPRUNG SECTION.                                                
046690     MOVE 'FB-VISA-URSPRUNG' TO CURRENT-SECTION                           
046691                                                                          
046692     IF W-IDARTNR-KEY > ZERO                                              
046693        MOVE W-IDARTNR-KEY TO W-IDARTNR                                   
046694        PERFORM FBA-VISA-URSPRUNG-UNIKT                                   
046695     ELSE                                                                 
046696        IF MFS-FIRST                                                      
046697           PERFORM IMS-GHNP-WDF813                                        
046698        ELSE                                                              
046699           PERFORM IMS-GHNP-WDF813-KEY                                    
046700        END-IF                                                            
046701                                                                          
046702        MOVE +1 TO INDX                                                   
046703        IF SEGMENT-SAKNAS                                                 
046704           MOVE SPACE                 TO SPAR-KDARTURS-ENTER              
046705        ELSE                                                              
046706           MOVE USPR-KDARTURS         TO SPAR-KDARTURS-ENTER              
046707           PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                
046708              IF SEGMENT-FINNS                                            
046709                 MOVE USPR-TISTADAT   TO WS-TISTADAT                      
046710                 MOVE WS-TISTADAT     TO MOD-TISTADAT-U (INDX)            
046711       IF USPR-TISTADAT > DAGENS-DATUM                                    
046712          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-U-ATTR (INDX)        
046713       END-IF                                                             
046714                 MOVE USPR-KDARTURS   TO MOD-KDARTURS (INDX)              
046715              ELSE                                                        
046716                 MOVE MFS-RENSA-FAELT TO MOD-KDARTURS   (INDX)            
046717                                         MOD-TISTADAT-U (INDX)            
046718              END-IF                                                      
046719              ADD +1 TO INDX                                              
046720              PERFORM IMS-GHNP-WDF813                                     
046721           END-PERFORM                                                    
046722        END-IF                                                            
046723        PERFORM UNTIL INDX > MAX-INDX                                     
046724           MOVE MFS-RENSA-FAELT      TO MOD-KDARTURS   (INDX)             
046725                                        MOD-TISTADAT-U (INDX)             
046726           MOVE MFS-STAENG-FAELT     TO MOD-KDCMD-U-ATTR (INDX)           
046727           ADD +1 TO INDX                                                 
046728        END-PERFORM                                                       
046729                                                                          
046730        IF SEGMENT-FINNS                                                  
046731           MOVE USPR-KDARTURS        TO SPAR-KDARTURS-NEXT                
046732           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
046733                                        MSG-KOM-IDMFSMED                  
046734           CALL WMEDKONV USING MED-WMEDAREA                               
046735           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
046736        ELSE                                                              
046737           MOVE SPAR-KDARTURS-ENTER  TO SPAR-KDARTURS-NEXT                
046738        END-IF                                                            
046739     END-IF                                                               
046740     .                                                                    
046741                                                                          
046742                                                                          
046743 FBA-VISA-URSPRUNG-UNIKT SECTION.                                         
046744     MOVE 'FBA-VISA-URSP-U ' TO CURRENT-SECTION                           
046745                                                                          
046746     MOVE +1 TO INDX                                                      
046747                                                                          
046748     PERFORM IMS-GU-WDK601                                                
046749     IF SEGMENT-FINNS                                                     
046750        MOVE WDK6ART-KDPRODSL   TO MOD-KDPRODSL-ART                       
046751        MOVE WDK6ART-IDFKNGRP   TO MOD-IDFKNGRP-ART                       
046752        PERFORM IMS-GNP-WDK611                                            
046753        MOVE WDK6CLAG-KDARTURS  TO MOD-KDARTURS-ART                       
046754                                                                          
046755        MOVE WDK6CLAG-KDARTURS  TO W-KDARTURS                             
046756        PERFORM IMS-GHNP-WDF813-KEY                                       
046757                                                                          
046758        IF SEGMENT-SAKNAS                                                 
046759           MOVE SPACE           TO SPAR-KDARTURS-ENTER                    
046760        ELSE                                                              
046761           MOVE USPR-KDARTURS   TO SPAR-KDARTURS-ENTER                    
046762                                   SPAR-KDARTURS-NEXT                     
046763           MOVE USPR-TISTADAT   TO WS-TISTADAT                            
046764           MOVE WS-TISTADAT     TO MOD-TISTADAT-U (INDX)                  
046765     IF USPR-TISTADAT > DAGENS-DATUM                                      
046766        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-U-ATTR (INDX)          
046767     END-IF                                                               
046768           MOVE USPR-KDARTURS   TO MOD-KDARTURS (INDX)                    
046769           ADD +1 TO INDX                                                 
046770        END-IF                                                            
046771     END-IF                                                               
046772     PERFORM UNTIL INDX > MAX-INDX                                        
046773        MOVE MFS-RENSA-FAELT    TO MOD-KDARTURS   (INDX)                  
046774                                   MOD-TISTADAT-U (INDX)                  
046775        MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-U-ATTR (INDX)                
046776        ADD +1 TO INDX                                                    
046777     END-PERFORM                                                          
046778     .                                                                    
046779                                                                          
046780                                                                          
046781 FC-VISA-PRODUKTSLAG SECTION.                                             
046782     MOVE 'FC-VISA-PRODUKTS' TO CURRENT-SECTION                           
046783                                                                          
046786     IF W-IDARTNR-KEY > ZERO                                              
046787        MOVE W-IDARTNR-KEY TO W-IDARTNR                                   
046788        PERFORM FCA-VISA-PRODUKTSLAG-UNIKT                                
046789     ELSE                                                                 
046790        IF MFS-FIRST                                                      
046791           PERFORM IMS-GHNP-WDF814                                        
046793        ELSE                                                              
046794           PERFORM IMS-GHNP-WDF814-KEY                                    
046796        END-IF                                                            
046797                                                                          
046798        MOVE +1 TO INDX                                                   
046799        IF SEGMENT-SAKNAS                                                 
046800           MOVE ZERO               TO SPAR-KDPRODSL-FOM-ENTER             
046801           MOVE 99                 TO SPAR-KDPRODSL-TOM-ENTER             
046802        ELSE                                                              
046803           MOVE PSPR-KDPRODSL-FOM  TO SPAR-KDPRODSL-FOM-ENTER             
046804           MOVE PSPR-KDPRODSL-TOM  TO SPAR-KDPRODSL-TOM-ENTER             
046805           PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                
046806              IF SEGMENT-FINNS                                            
046807                MOVE PSPR-TISTADAT     TO WS-TISTADAT                     
046808                MOVE WS-TISTADAT       TO MOD-TISTADAT-P   (INDX)         
046809     IF PSPR-TISTADAT > DAGENS-DATUM                                      
046810        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-P-ATTR (INDX)          
046811     END-IF                                                               
046812                MOVE PSPR-KDPRODSL-FOM TO MOD-KDPRODSL-FOM (INDX)         
046813                MOVE PSPR-KDPRODSL-TOM TO MOD-KDPRODSL-TOM (INDX)         
046814              ELSE                                                        
046815                MOVE MFS-RENSA-FAELT   TO MOD-KDPRODSL-FOM (INDX)         
046816                                          MOD-KDPRODSL-TOM (INDX)         
046817                                          MOD-TISTADAT-P   (INDX)         
046818              END-IF                                                      
046819              ADD +1 TO INDX                                              
046820              PERFORM IMS-GHNP-WDF814                                     
046821           END-PERFORM                                                    
046822        END-IF                                                            
046823        PERFORM UNTIL INDX > MAX-INDX                                     
046824           MOVE MFS-RENSA-FAELT    TO MOD-KDPRODSL-FOM (INDX)             
046825                                      MOD-KDPRODSL-TOM (INDX)             
046826                                      MOD-TISTADAT-P   (INDX)             
046827           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-P-ATTR (INDX)             
046828           ADD +1 TO INDX                                                 
046829        END-PERFORM                                                       
046830                                                                          
046831        IF SEGMENT-FINNS                                                  
046832           MOVE PSPR-KDPRODSL-FOM    TO SPAR-KDPRODSL-FOM-NEXT            
046833           MOVE PSPR-KDPRODSL-TOM    TO SPAR-KDPRODSL-TOM-NEXT            
046834           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
046835           CALL WMEDKONV USING MED-WMEDAREA                               
046836           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
046837        ELSE                                                              
046838           MOVE SPAR-KDPRODSL-FOM-ENTER  TO SPAR-KDPRODSL-FOM-NEXT        
046839           MOVE SPAR-KDPRODSL-TOM-ENTER  TO SPAR-KDPRODSL-TOM-NEXT        
046840        END-IF                                                            
046841     END-IF                                                               
046842     .                                                                    
046843                                                                          
046844                                                                          
046845 FCA-VISA-PRODUKTSLAG-UNIKT SECTION.                                      
046846     MOVE 'FCA-VISA-PROD-U ' TO CURRENT-SECTION                           
046847                                                                          
046848     MOVE +1 TO INDX                                                      
046849                                                                          
046850     PERFORM IMS-GU-WDK601                                                
046851     IF SEGMENT-FINNS                                                     
046852                                                                          
046853        MOVE WDK6ART-KDPRODSL   TO W-KDPRODSL                             
046854        PERFORM IMS-GNP-WDF814-PROD                                       
046855                                                                          
046856        IF SEGMENT-SAKNAS                                                 
046857           MOVE ZERO               TO SPAR-KDPRODSL-FOM-ENTER             
046858           MOVE 99                 TO SPAR-KDPRODSL-TOM-ENTER             
046859        ELSE                                                              
046860           MOVE PSPR-KDPRODSL-FOM  TO SPAR-KDPRODSL-FOM-ENTER             
046861           MOVE PSPR-KDPRODSL-TOM  TO SPAR-KDPRODSL-TOM-ENTER             
046862           PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                
046863              IF SEGMENT-FINNS                                            
046864                MOVE PSPR-TISTADAT     TO WS-TISTADAT                     
046865                MOVE WS-TISTADAT       TO MOD-TISTADAT-P   (INDX)         
046866       IF PSPR-TISTADAT > DAGENS-DATUM                                    
046867         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-P-ATTR (INDX)         
046868       END-IF                                                             
046869                MOVE PSPR-KDPRODSL-FOM TO MOD-KDPRODSL-FOM (INDX)         
046870                MOVE PSPR-KDPRODSL-TOM TO MOD-KDPRODSL-TOM (INDX)         
046871              ELSE                                                        
046872                MOVE MFS-RENSA-FAELT   TO MOD-KDPRODSL-FOM (INDX)         
046873                                          MOD-KDPRODSL-TOM (INDX)         
046874                                          MOD-TISTADAT-P   (INDX)         
046875              END-IF                                                      
046876              ADD +1 TO INDX                                              
046877              PERFORM IMS-GNP-WDF814-PROD                                 
046878           END-PERFORM                                                    
046879        END-IF                                                            
046880        PERFORM UNTIL INDX > MAX-INDX                                     
046881           MOVE MFS-RENSA-FAELT    TO MOD-KDPRODSL-FOM (INDX)             
046882                                      MOD-KDPRODSL-TOM (INDX)             
046883                                      MOD-TISTADAT-P   (INDX)             
046884           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-P-ATTR (INDX)             
046885           ADD +1 TO INDX                                                 
046886        END-PERFORM                                                       
046887     END-IF                                                               
046888     .                                                                    
046889                                                                          
046890                                                                          
046891 FD-VISA-FUNKTIONSGRUPP SECTION.                                          
046892     MOVE 'FD-VISA-FKNGRP  ' TO CURRENT-SECTION                           
046893                                                                          
046894                                                                          
046895     IF W-IDARTNR-KEY > ZERO                                              
046896        MOVE W-IDARTNR-KEY TO W-IDARTNR                                   
046897        PERFORM FDA-VISA-FUNKTIONSGRUPP-UNIKT                             
046898     ELSE                                                                 
046899        IF MFS-FIRST                                                      
046900           PERFORM IMS-GHNP-WDF815                                        
046901        ELSE                                                              
046902           PERFORM IMS-GHNP-WDF815-KEY                                    
046903        END-IF                                                            
046904                                                                          
046905        MOVE +1 TO INDX                                                   
046906        IF SEGMENT-SAKNAS                                                 
046907           MOVE ZERO            TO SPAR-IDFKNGRP-FOM-ENTER                
046908           MOVE 9999            TO SPAR-IDFKNGRP-TOM-ENTER                
046909        ELSE                                                              
046910           MOVE FSPR-IDFKNGRP-FOM TO SPAR-IDFKNGRP-FOM-ENTER              
046911           MOVE FSPR-IDFKNGRP-TOM TO SPAR-IDFKNGRP-TOM-ENTER              
046912           PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                
046913              IF SEGMENT-FINNS                                            
046914                 MOVE FSPR-TISTADAT   TO WS-TISTADAT                      
046915                 MOVE WS-TISTADAT     TO MOD-TISTADAT-F   (INDX)          
046916       IF FSPR-TISTADAT > DAGENS-DATUM                                    
046917          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-F-ATTR (INDX)        
046918       END-IF                                                             
046919                 MOVE FSPR-IDFKNGRP-FOM TO MOD-IDFKNGRP-FOM (INDX)        
046920                 MOVE FSPR-IDFKNGRP-TOM TO MOD-IDFKNGRP-TOM (INDX)        
046921              ELSE                                                        
046922                 MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-FOM (INDX)          
046923                                         MOD-IDFKNGRP-TOM (INDX)          
046924                                         MOD-TISTADAT-F (INDX)            
046925              END-IF                                                      
046926              ADD +1 TO INDX                                              
046927              PERFORM IMS-GHNP-WDF815                                     
046928           END-PERFORM                                                    
046929        END-IF                                                            
046930        PERFORM UNTIL INDX > MAX-INDX                                     
046931           MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-FOM (INDX)                
046932                                      MOD-IDFKNGRP-TOM (INDX)             
046933                                      MOD-TISTADAT-F (INDX)               
046934           MOVE MFS-STAENG-FAELT TO MOD-KDCMD-F-ATTR (INDX)               
046935           ADD +1 TO INDX                                                 
046936        END-PERFORM                                                       
046937                                                                          
046938        IF SEGMENT-FINNS                                                  
046939           MOVE FSPR-IDFKNGRP-FOM    TO SPAR-IDFKNGRP-FOM-NEXT            
046940           MOVE FSPR-IDFKNGRP-TOM    TO SPAR-IDFKNGRP-TOM-NEXT            
046941           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
046942           CALL WMEDKONV USING MED-WMEDAREA                               
046943           MOVE MED-TEMFSINF         TO MOD-TEMFSINF                      
046944        ELSE                                                              
046945           MOVE SPAR-IDFKNGRP-FOM-ENTER TO SPAR-IDFKNGRP-FOM-NEXT         
046946           MOVE SPAR-IDFKNGRP-TOM-ENTER TO SPAR-IDFKNGRP-TOM-NEXT         
046947        END-IF                                                            
046948     END-IF                                                               
046949     .                                                                    
046950                                                                          
046951                                                                          
046952 FDA-VISA-FUNKTIONSGRUPP-UNIKT SECTION.                                   
046953     MOVE 'FDA-VISA-FUNK-U ' TO CURRENT-SECTION                           
046954                                                                          
046955     MOVE +1 TO INDX                                                      
046956                                                                          
046957     PERFORM IMS-GU-WDK601                                                
046958     IF SEGMENT-FINNS                                                     
046959                                                                          
046960        MOVE WDK6ART-IDFKNGRP   TO W-IDFKNGRP                             
046961        PERFORM IMS-GNP-WDF815-FKN                                        
046962                                                                          
046963        IF SEGMENT-SAKNAS                                                 
046964           MOVE ZERO               TO SPAR-IDFKNGRP-FOM-ENTER             
046965           MOVE 99                 TO SPAR-IDFKNGRP-TOM-ENTER             
046966        ELSE                                                              
046967           MOVE FSPR-IDFKNGRP-FOM  TO SPAR-IDFKNGRP-FOM-ENTER             
046968           MOVE FSPR-IDFKNGRP-TOM  TO SPAR-IDFKNGRP-TOM-ENTER             
046969           PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                
046970              IF SEGMENT-FINNS                                            
046971                MOVE FSPR-TISTADAT     TO WS-TISTADAT                     
046972                MOVE WS-TISTADAT       TO MOD-TISTADAT-F   (INDX)         
046973      IF FSPR-TISTADAT >  DAGENS-DATUM                                    
046974         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTADAT-F-ATTR (INDX)         
046975      END-IF                                                              
046976                MOVE FSPR-IDFKNGRP-FOM TO MOD-IDFKNGRP-FOM (INDX)         
046977                MOVE FSPR-IDFKNGRP-TOM TO MOD-IDFKNGRP-TOM (INDX)         
046978              ELSE                                                        
046979                MOVE MFS-RENSA-FAELT   TO MOD-IDFKNGRP-FOM (INDX)         
046980                                          MOD-IDFKNGRP-TOM (INDX)         
046981                                          MOD-TISTADAT-F   (INDX)         
046982              END-IF                                                      
046983              ADD +1 TO INDX                                              
046984              PERFORM IMS-GNP-WDF815-FKN                                  
046985           END-PERFORM                                                    
046986        END-IF                                                            
046987        PERFORM UNTIL INDX > MAX-INDX                                     
046988           MOVE MFS-RENSA-FAELT    TO MOD-IDFKNGRP-FOM (INDX)             
046989                                      MOD-IDFKNGRP-TOM (INDX)             
046990                                      MOD-TISTADAT-F   (INDX)             
046991           MOVE MFS-STAENG-FAELT   TO MOD-KDCMD-F-ATTR (INDX)             
046992           ADD +1 TO INDX                                                 
046993        END-PERFORM                                                       
046994     END-IF                                                               
046995     .                                                                    
046996                                                                          
046997                                                                          
046998 FE-E-RAD-TO-MOD SECTION.                                                 
046999     MOVE 'FE-E-RAD-TO-MOD ' TO CURRENT-SECTION                           
047000                                                                          
047001     IF E-ART-FEL OR E-ART-TI-FEL                                         
047002        IF E-ART-FEL                                                      
047003           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-E-ATTR                   
047004        END-IF                                                            
047005        IF E-ART-TI-FEL                                                   
047006           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-A-E-ATTR                
047007        END-IF                                                            
047008        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-E-ATTR                         
047009                                 MOD-IDARTNR-E-ATTR                       
047010                                 MOD-KDARTURS-E-ATTR                      
047011                                 MOD-TISTADAT-U-E-ATTR                    
047012                                 MOD-KDPRODSL-FOM-E-ATTR                  
047013                                 MOD-KDPRODSL-TOM-E-ATTR                  
047014                                 MOD-TISTADAT-P-E-ATTR                    
047015                                 MOD-IDFKNGRP-FOM-E-ATTR                  
047016                                 MOD-IDFKNGRP-TOM-E-ATTR                  
047017                                 MOD-TISTADAT-F-E-ATTR                    
047018     ELSE                                                                 
047019                                                                          
047020        IF E-URS-FEL                                                      
047021           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDARTURS-E-ATTR                 
047022        END-IF                                                            
047023        IF E-URS-TI-FEL                                                   
047024           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-U-E-ATTR                
047025        END-IF                                                            
047026                                                                          
047027        IF E-PRODFOM-FEL                                                  
047028           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-E-ATTR              
047029        END-IF                                                            
047030        IF E-PRODTOM-FEL                                                  
047031           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-TOM-E-ATTR              
047032        END-IF                                                            
047033        IF E-PROD-TI-FEL                                                  
047034           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-P-E-ATTR                
047035        END-IF                                                            
047036                                                                          
047037        IF E-FKNFOM-FEL                                                   
047038           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-E-ATTR              
047039        END-IF                                                            
047040        IF E-FKNTOM-FEL                                                   
047041           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-TOM-E-ATTR              
047042        END-IF                                                            
047043        IF E-FKN-TI-FEL                                                   
047044           MOVE MFS-NUM-FAELT-FEL TO MOD-TISTADAT-F-E-ATTR                
047045        END-IF                                                            
047046        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-E-ATTR                         
047047                                 MOD-IDARTNR-E-ATTR                       
047048                                 MOD-TISTADAT-A-E-ATTR                    
047049                                                                          
047057     END-IF                                                               
047058     MOVE +1 TO INDX                                                      
047059     PERFORM UNTIL INDX > MAX-INDX                                        
047060        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-A-ATTR (INDX)                  
047061        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-U-ATTR (INDX)                  
047062        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-P-ATTR (INDX)                  
047063        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-F-ATTR (INDX)                  
047064        ADD +1 TO INDX                                                    
047065     END-PERFORM                                                          
047066     .                                                                    
047067                                                                          
047068                                                                          
047069 G-KOLLA-INPUT SECTION.                                                   
047070     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
047071                                                                          
047073     MOVE JA  TO INDATA-SW                                                
047074     MOVE JA  TO ALLT-SW                                                  
047075     MOVE NEJ TO E-RAD-FEL-SW                                             
047076     MOVE NEJ TO RAD-VALD-SW                                              
047077     MOVE NEJ TO E-ART-FEL-SW                                             
047078     MOVE NEJ TO E-ART-TI-FEL-SW                                          
047079     MOVE NEJ TO E-URS-FEL-SW                                             
047080     MOVE NEJ TO E-URS-TI-FEL-SW                                          
047081     MOVE NEJ TO E-PRODFOM-FEL-SW                                         
047082     MOVE NEJ TO E-PRODTOM-FEL-SW                                         
047083     MOVE NEJ TO E-PROD-TI-FEL-SW                                         
047084     MOVE NEJ TO E-FKNFOM-FEL-SW                                          
047085     MOVE NEJ TO E-FKNTOM-FEL-SW                                          
047086     MOVE NEJ TO E-FKN-TI-FEL-SW                                          
047087                                                                          
047088     MOVE SPAR-IDARTNR-ENTER      TO W-IDARTNR                            
047089     MOVE SPAR-KDARTURS-ENTER     TO W-KDARTURS                           
047090     MOVE SPAR-KDPRODSL-FOM-ENTER TO W-KDPRODSL-FOM                       
047091     MOVE SPAR-KDPRODSL-TOM-ENTER TO W-KDPRODSL-TOM                       
047092     MOVE SPAR-IDFKNGRP-FOM-ENTER TO W-IDFKNGRP-FOM                       
047093     MOVE SPAR-IDFKNGRP-TOM-ENTER TO W-IDFKNGRP-TOM                       
047094                                                                          
047095     PERFORM IMS-GU-WDF801                                                
047096     IF SEGMENT-SAKNAS                                                    
047097        MOVE NEJ               TO INDATA-SW                               
047098        MOVE GROUP-MISSING     TO MED-IDMFSFEL                            
047099                                  MSG-KOM-IDMFSMED                        
047100        CALL WMEDKONV USING MED-WMEDAREA                                  
047101        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
047102        PERFORM MFS-RENSA-FAELT-UT                                        
047103     ELSE                                                                 
047104                                                                          
047105        PERFORM GA-KOLLA-ARTIKEL-DELETE                                   
047106        PERFORM GB-KOLLA-URSPRUNG-DELETE                                  
047107        PERFORM GC-KOLLA-PRODUKTSLAG-DELETE                               
047108        PERFORM GD-KOLLA-FUNKTIONSGRUPP-DELETE                            
047109                                                                          
047110        IF INDATA-OK                                                      
047111           IF (MID-KDCMD-E = ALL '+' OR MID-KDCMD-E = ALL ' ')            
047112           AND MID-IDARTNR-E = ALL '+'                                    
047113           AND MID-TISTADAT-A-E = ALL '+'                                 
047114           AND MID-KDARTURS-E = ALL '+'                                   
047115           AND MID-TISTADAT-U-E = ALL '+'                                 
047116           AND MID-KDPRODSL-FOM-E = ALL '+'                               
047117           AND MID-KDPRODSL-TOM-E = ALL '+'                               
047118           AND MID-TISTADAT-P-E = ALL '+'                                 
047119           AND MID-IDFKNGRP-FOM-E = ALL '+'                               
047120           AND MID-IDFKNGRP-TOM-E = ALL '+'                               
047121           AND MID-TISTADAT-F-E = ALL '+'                                 
047122           IF RAD-EJ-VALD                                                 
047123              MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                   
047124              CALL WMEDKONV USING MED-WMEDAREA                            
047130              MOVE MED-MFSFEL        TO MOD-TEMFSFEL                      
047200              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
047300              PERFORM MFS-ROER-EJ-FAELT-UT                                
047400              MOVE NEJ TO INDATA-SW                                       
047500           END-IF                                                         
047600        END-IF                                                            
047610                                                                          
047700                                                                          
047800        IF INDATA-OK                                                      
047810           IF MID-KDCMD-E = 'N' OR MID-KDCMD-E = 'C'                      
047820              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-E-ATTR               
047821              PERFORM GE-KOLLA-ARTIKEL-CHNG                               
047822              PERFORM GF-KOLLA-URSPRUNG-CHNG                              
047823              PERFORM GG-KOLLA-PRODUKTSLAG-CHNG                           
047824              PERFORM GH-KOLLA-FUNKTIONSGRUPP-CHNG                        
047825              IF INDATA-FEL                                               
047827                 PERFORM MFS-ROER-EJ-FAELT-UT                             
047828                 PERFORM MFS-ROER-EJ-FAELT-IN                             
047829                 PERFORM MFS-ROER-EJ-FAELT-IN-E                           
047830                 PERFORM MFS-LAES-IN-IGEN-E                               
047831                 CALL WMEDKONV USING MED-WMEDAREA                         
047832                 MOVE MED-MFSFEL     TO MOD-TEMFSFEL                      
047833              END-IF                                                      
047834           ELSE                                                           
047835              IF RAD-EJ-VALD                                              
047876                 PERFORM MFS-ROER-EJ-FAELT-UT                             
047877                 PERFORM MFS-ROER-EJ-FAELT-IN                             
047878                 PERFORM MFS-ROER-EJ-FAELT-IN-E                           
047879                 PERFORM MFS-LAES-IN-IGEN-E                               
047883                 MOVE JA TO E-RAD-FEL-SW                                  
047885                                                                          
047886                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
047887                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-E-ATTR              
047888                 CALL WMEDKONV USING MED-WMEDAREA                         
047889                 MOVE MED-MFSFEL        TO MOD-TEMFSFEL                   
047890              END-IF                                                      
047900           END-IF                                                         
048200        END-IF                                                            
048300     END-IF                                                               
060600     .                                                                    
060700                                                                          
060710                                                                          
060800 GA-KOLLA-ARTIKEL-DELETE SECTION.                                         
060810     MOVE 'GA-KOLLA-ART-DEL'   TO CURRENT-SECTION                         
060820                                                                          
060822     MOVE 1 TO INDX                                                       
060823     PERFORM UNTIL INDX > MAX-INDX                                        
060824        IF MID-KDCMD-A(INDX) NOT = '+' AND ' '                            
060825           IF MID-KDCMD-A(INDX) = 'D'                                     
060826              MOVE JA                   TO RAD-VALD-SW                    
060827              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-A-ATTR (INDX)        
060828           ELSE                                                           
060829              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-A-ATTR (INDX)        
060830              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
060831              CALL WMEDKONV USING MED-WMEDAREA                            
060832              MOVE MED-MFSFEL           TO MOD-TEMFSFEL                   
060834              PERFORM MFS-ROER-EJ-FAELT-UT                                
060835              PERFORM MFS-ROER-EJ-FAELT-IN-ART                            
060836              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
060837              MOVE NEJ TO INDATA-SW                                       
060838           END-IF                                                         
060839        END-IF                                                            
060840        ADD 1 TO INDX                                                     
060841     END-PERFORM                                                          
060850                                                                          
060974     .                                                                    
060975                                                                          
060976                                                                          
060977 GB-KOLLA-URSPRUNG-DELETE  SECTION.                                       
060978     MOVE 'GB-KOLLA-URS-DEL'   TO CURRENT-SECTION                         
060979                                                                          
060981     MOVE 1 TO INDX                                                       
060982     PERFORM UNTIL INDX > MAX-INDX                                        
060983        IF MID-KDCMD-U(INDX) NOT = '+' AND ' '                            
060984           IF MID-KDCMD-U(INDX) = 'D'                                     
060985              MOVE JA                   TO RAD-VALD-SW                    
060986              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-U-ATTR (INDX)        
060987           ELSE                                                           
060988              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-U-ATTR (INDX)        
060989              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
060990              CALL WMEDKONV USING MED-WMEDAREA                            
060991              MOVE MED-MFSFEL           TO MOD-TEMFSFEL                   
060993              PERFORM MFS-ROER-EJ-FAELT-UT                                
060994              PERFORM MFS-ROER-EJ-FAELT-IN-URS                            
060995              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
060996              MOVE NEJ TO INDATA-SW                                       
060997           END-IF                                                         
060998        END-IF                                                            
060999        ADD 1 TO INDX                                                     
061000     END-PERFORM                                                          
061001     .                                                                    
061002                                                                          
061003                                                                          
061004 GC-KOLLA-PRODUKTSLAG-DELETE  SECTION.                                    
061005     MOVE 'GC-KOLLA-PRO-DEL'   TO CURRENT-SECTION                         
061006                                                                          
061007     MOVE 1 TO INDX                                                       
061008     PERFORM UNTIL INDX > MAX-INDX                                        
061009        IF MID-KDCMD-P(INDX) NOT = '+' AND ' '                            
061010           IF MID-KDCMD-P(INDX) = 'D'                                     
061011              MOVE JA                   TO RAD-VALD-SW                    
061012              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-P-ATTR (INDX)        
061013           ELSE                                                           
061014              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-P-ATTR (INDX)        
061015              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061016              CALL WMEDKONV USING MED-WMEDAREA                            
061017              MOVE MED-MFSFEL           TO MOD-TEMFSFEL                   
061019              PERFORM MFS-ROER-EJ-FAELT-UT                                
061020              PERFORM MFS-ROER-EJ-FAELT-IN-PRO                            
061021              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
061022              MOVE NEJ TO INDATA-SW                                       
061023           END-IF                                                         
061024        END-IF                                                            
061025        ADD 1 TO INDX                                                     
061026     END-PERFORM                                                          
061027     .                                                                    
061028                                                                          
061029                                                                          
061030 GD-KOLLA-FUNKTIONSGRUPP-DELETE SECTION.                                  
061031     MOVE 'GD-KOLLA-FKN-DEL'   TO CURRENT-SECTION                         
061032                                                                          
061033     MOVE 1 TO INDX                                                       
061034     PERFORM UNTIL INDX > MAX-INDX                                        
061035        IF MID-KDCMD-F(INDX) NOT = '+' AND ' '                            
061036           IF MID-KDCMD-F(INDX) = 'D'                                     
061037              MOVE JA                   TO RAD-VALD-SW                    
061038              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-F-ATTR (INDX)        
061039           ELSE                                                           
061040              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-F-ATTR (INDX)        
061041              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061042              CALL WMEDKONV USING MED-WMEDAREA                            
061043              MOVE MED-MFSFEL           TO MOD-TEMFSFEL                   
061045              PERFORM MFS-ROER-EJ-FAELT-UT                                
061046              PERFORM MFS-ROER-EJ-FAELT-IN-FKN                            
061047              PERFORM MFS-ROER-EJ-FAELT-IN-E                              
061048              MOVE NEJ TO INDATA-SW                                       
061049           END-IF                                                         
061050        END-IF                                                            
061051        ADD 1 TO INDX                                                     
061052     END-PERFORM                                                          
061053     .                                                                    
061054                                                                          
061055                                                                          
061056 GE-KOLLA-ARTIKEL-CHNG   SECTION.                                         
061057     MOVE 'GE-KOLLA-ART-CHG'   TO CURRENT-SECTION                         
061058                                                                          
061060     IF MID-IDARTNR-E = ALL '+' OR SPACE                                  
061063        CONTINUE                                                          
061064     ELSE                                                                 
061065                                                                          
061066        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-E-ATTR                   
061068                                                                          
061070        IF MID-TISTADAT-A-E = ALL '+' OR ALL '0'                          
061071*---OM MAN EJ MATAT IN DATUM TAS DAGENS DATUM ELLER GRUPPENS              
061072           IF DAGENS-DATUM < GSPR-TISTADAT                                
061073              MOVE GSPR-TISTADAT     TO MID-TISTADAT-A-E                  
061074           ELSE                                                           
061075              MOVE DAGENS-DATUM      TO MID-TISTADAT-A-E                  
061076           END-IF                                                         
061078        END-IF                                                            
061079                                                                          
061080*---VALIDERING AV DATUM                                                   
061081        MOVE 'AAMMDD'                TO DAT-KDDATFORM                     
061082        MOVE MID-TISTADAT-A-E        TO DAT-I-TIDATUM                     
061084                                                                          
061085        CALL WDATKONV USING DAT-KDDATFORM                                 
061086                            DAT-I-TIDATUM                                 
061087                            DAT-O-TIDATUM                                 
061088                            DAT-KDSVAR                                    
061089        IF DAT-KDSVAR-FEL                                                 
061090           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
061091           MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-A-E-ATTR             
061092           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-E-ATTR                
061093           MOVE NEJ                  TO INDATA-SW                         
061094           MOVE NEJ                  TO ALLT-SW                           
061095           MOVE JA                   TO E-RAD-FEL-SW                      
061097           MOVE JA                   TO E-ART-TI-FEL-SW                   
061098        ELSE                                                              
061099           MOVE MID-TISTADAT-A-E     TO WS-TISTADAT                       
061100        END-IF                                                            
061101                                                                          
061102        IF ALLT-OK                                                        
061103*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
061107           IF  WS-TISTADAT >= DAGENS-DATUM                                
061108           AND WS-TISTADAT >= GSPR-TISTADAT                               
061109              MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISTADAT-A-E-ATTR          
061110           ELSE                                                           
061111              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061112              MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-A-E-ATTR          
061113              MOVE NEJ                  TO INDATA-SW                      
061114              MOVE NEJ                  TO ALLT-SW                        
061115              MOVE JA                   TO E-RAD-FEL-SW                   
061117              MOVE JA                   TO E-ART-TI-FEL-SW                
061118           END-IF                                                         
061119        END-IF                                                            
061120        IF ALLT-OK                                                        
061121           IF MID-KDCMD-E = 'N'                                           
061122*---KOLLA SÅ ATT ARTIKEL FINNS PÅ ARTIKELBASEN                            
061123              MOVE MID-IDARTNR-E          TO WS-IDARTNR                   
061124              MOVE WS-IDARTNR             TO W-IDARTNR                    
061125              PERFORM IMS-GHU-WDK611                                      
061127              IF SEGMENT-SAKNAS                                           
061128                 MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-E-ATTR           
061129                 MOVE NEJ                 TO INDATA-SW                    
061130                 MOVE NEJ                 TO ALLT-SW                      
061131                 MOVE PART-MISSING-WDK6   TO MED-IDMFSFEL                 
061132                 CALL WMEDKONV USING MED-WMEDAREA                         
061133                 MOVE MED-MFSFEL          TO MOD-TEMFSFEL                 
061134                 MOVE JA                  TO E-RAD-FEL-SW                 
061136                 MOVE JA                  TO E-ART-FEL-SW                 
061137              ELSE                                                        
061138                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-E-ATTR           
061139              END-IF                                                      
061140                                                                          
061141*---KOLLA SÅ ATT SEGMENT INTE REDAN FINNS PÅ BAS                          
061142              PERFORM IMS-GHU-WDF812                                      
061144              IF SEGMENT-FINNS                                            
061145                 MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-E-ATTR          
061146                 MOVE NEJ                  TO INDATA-SW                   
061147                 MOVE NEJ                  TO ALLT-SW                     
061148                 MOVE PART-EXISTS          TO MED-IDMFSFEL                
061149                 MOVE JA                   TO E-RAD-FEL-SW                
061151                 MOVE JA                   TO E-ART-FEL-SW                
061152              ELSE                                                        
061153                 MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-E-ATTR          
061154              END-IF                                                      
061155                                                                          
061156           ELSE                                                           
061157              IF MID-KDCMD-E = 'C'                                        
061158                                                                          
061159*---KOLLA SÅ ATT SEGMENT FINNS PÅ BAS                                     
061160                MOVE MID-IDARTNR-E        TO WS-IDARTNR                   
061161                MOVE WS-IDARTNR           TO W-IDARTNR                    
061162                PERFORM IMS-GHU-WDF812                                    
061163                IF SEGMENT-SAKNAS                                         
061164                   MOVE MFS-NUM-FAELT-FEL  TO MOD-IDARTNR-E-ATTR          
061165                   MOVE NEJ                TO INDATA-SW                   
061166                   MOVE NEJ                TO ALLT-SW                     
061167                   MOVE PART-MISSING       TO MED-IDMFSFEL                
061168                   MOVE JA                 TO E-RAD-FEL-SW                
061170                   MOVE JA                 TO E-ART-FEL-SW                
061171                ELSE                                                      
061172                   MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-E-ATTR         
061173                END-IF                                                    
061174             END-IF                                                       
061175           END-IF                                                         
061176        END-IF                                                            
061178        IF NOT ALLT-OK                                                    
061180           MOVE MFS-STAENG-FAELT TO MOD-KDCMD-E-ATTR                      
061181                                    MOD-IDARTNR-E-ATTR                    
061182                                    MOD-KDARTURS-E-ATTR                   
061183                                    MOD-TISTADAT-U-E-ATTR                 
061184                                    MOD-KDPRODSL-FOM-E-ATTR               
061185                                    MOD-KDPRODSL-TOM-E-ATTR               
061186                                    MOD-TISTADAT-P-E-ATTR                 
061187                                    MOD-IDFKNGRP-FOM-E-ATTR               
061188                                    MOD-IDFKNGRP-TOM-E-ATTR               
061189                                    MOD-TISTADAT-F-E-ATTR                 
061190        END-IF                                                            
061191     END-IF                                                               
061192     .                                                                    
061193                                                                          
061194                                                                          
061195 GF-KOLLA-URSPRUNG-CHNG   SECTION.                                        
061196     MOVE 'GF-KOLLA-URS-CHG'   TO CURRENT-SECTION                         
061197                                                                          
061198     IF MID-KDARTURS-E = ALL '+'                                          
061199     OR MID-KDARTURS-E = SPACE                                            
061200        CONTINUE                                                          
061201     ELSE                                                                 
061202                                                                          
061203        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARTURS-E-ATTR                  
061204                                                                          
061205        IF MID-TISTADAT-U-E = ALL '+' OR ALL '0'                          
061207*---OM MAN EJ MATAT IN DATUM TAS DAGENS DATUM ELLER GRUPPENS              
061208           IF DAGENS-DATUM < GSPR-TISTADAT                                
061209              MOVE GSPR-TISTADAT     TO MID-TISTADAT-U-E                  
061210           ELSE                                                           
061211              MOVE DAGENS-DATUM      TO MID-TISTADAT-U-E                  
061212           END-IF                                                         
061214        END-IF                                                            
061215                                                                          
061216*---VALIDERING AV DATUM                                                   
061217        MOVE 'AAMMDD'             TO DAT-KDDATFORM                        
061218        MOVE MID-TISTADAT-U-E     TO DAT-I-TIDATUM                        
061219                                                                          
061220        CALL WDATKONV USING DAT-KDDATFORM                                 
061221                            DAT-I-TIDATUM                                 
061222                            DAT-O-TIDATUM                                 
061223                            DAT-KDSVAR                                    
061224        IF DAT-KDSVAR-FEL                                                 
061225           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
061226                                        MSG-KOM-IDMFSMED                  
061227           MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-U-E-ATTR             
061228           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-E-ATTR                
061229           MOVE NEJ                  TO INDATA-SW                         
061230           MOVE NEJ                  TO ALLT-SW                           
061231           MOVE JA                   TO E-RAD-FEL-SW                      
061233           MOVE JA                   TO E-URS-TI-FEL-SW                   
061234        ELSE                                                              
061235           MOVE MID-TISTADAT-U-E     TO WS-TISTADAT                       
061236        END-IF                                                            
061237                                                                          
061238        IF ALLT-OK                                                        
061239*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
061240           IF  WS-TISTADAT >= DAGENS-DATUM                                
061241           AND WS-TISTADAT >= GSPR-TISTADAT                               
061242              MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISTADAT-U-E-ATTR          
061243           ELSE                                                           
061244              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061245              MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-U-E-ATTR          
061246              MOVE NEJ                  TO INDATA-SW                      
061247              MOVE NEJ                  TO ALLT-SW                        
061248              MOVE JA                   TO E-RAD-FEL-SW                   
061250              MOVE JA                   TO E-URS-TI-FEL-SW                
061251           END-IF                                                         
061252        END-IF                                                            
061253        IF ALLT-OK                                                        
061254           MOVE MID-KDARTURS-E          TO W-KDARTURS                     
061255           PERFORM IMS-GHU-WDF813                                         
061256           IF MID-KDCMD-E = 'N'                                           
061257*---KOLLA SÅ ATT SEGMENT INTE REDAN FINNS PÅ BAS                          
061258              IF SEGMENT-FINNS                                            
061259                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARTURS-E-ATTR         
061260                 MOVE NEJ                  TO INDATA-SW                   
061261                 MOVE PART-EXISTS          TO MED-IDMFSFEL                
061262                 MOVE JA                   TO E-RAD-FEL-SW                
061264                 MOVE JA                   TO E-URS-FEL-SW                
061265              ELSE                                                        
061266                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARTURS-E-ATTR         
061267              END-IF                                                      
061268                                                                          
061269           ELSE                                                           
061270                                                                          
061271*---KOLLA SÅ ATT SEGMENT FINNS PÅ BAS                                     
061272              IF SEGMENT-SAKNAS                                           
061273                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDARTURS-E-ATTR         
061274                 MOVE NEJ                  TO INDATA-SW                   
061275                 MOVE PART-EXISTS          TO MED-IDMFSFEL                
061276                 MOVE JA                   TO E-RAD-FEL-SW                
061278                 MOVE JA                   TO E-URS-FEL-SW                
061279              ELSE                                                        
061280                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARTURS-E-ATTR         
061281              END-IF                                                      
061282           END-IF                                                         
061283        END-IF                                                            
061284     END-IF                                                               
061285     .                                                                    
061286                                                                          
061287                                                                          
061288 GG-KOLLA-PRODUKTSLAG-CHNG   SECTION.                                     
061289     MOVE 'GG-KOLLA-PRO-CHG'   TO CURRENT-SECTION                         
061290                                                                          
061291     IF MID-KDPRODSL-FOM-E = ALL '+'                                      
061292     OR MID-KDPRODSL-FOM-E = SPACE                                        
061293        CONTINUE                                                          
061294     ELSE                                                                 
061295                                                                          
061296        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRODSL-FOM-E-ATTR              
061297                                     MOD-KDPRODSL-TOM-E-ATTR              
061298                                                                          
061299        IF MID-TISTADAT-P-E = ALL '+' OR ALL '0' OR SPACE                 
061301*---OM MAN EJ MATAT IN DATUM TAS DAGENS DATUM ELLER GRUPPENS              
061302           IF DAGENS-DATUM < GSPR-TISTADAT                                
061303              MOVE GSPR-TISTADAT     TO MID-TISTADAT-P-E                  
061304           ELSE                                                           
061305              MOVE DAGENS-DATUM      TO MID-TISTADAT-P-E                  
061306           END-IF                                                         
061308        END-IF                                                            
061309                                                                          
061310*---VALIDERING AV DATUM                                                   
061311        MOVE 'AAMMDD'             TO DAT-KDDATFORM                        
061312        MOVE MID-TISTADAT-P-E     TO DAT-I-TIDATUM                        
061313                                                                          
061314        CALL WDATKONV USING DAT-KDDATFORM                                 
061315                            DAT-I-TIDATUM                                 
061316                            DAT-O-TIDATUM                                 
061317                            DAT-KDSVAR                                    
061318        IF DAT-KDSVAR-FEL                                                 
061319           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
061320                                        MSG-KOM-IDMFSMED                  
061321           MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-P-E-ATTR             
061322           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-E-ATTR                
061323           MOVE NEJ                  TO INDATA-SW                         
061324           MOVE NEJ                  TO ALLT-SW                           
061325           MOVE JA                   TO E-RAD-FEL-SW                      
061327           MOVE JA                   TO E-PROD-TI-FEL-SW                  
061328        ELSE                                                              
061329           MOVE MID-TISTADAT-P-E     TO WS-TISTADAT                       
061330        END-IF                                                            
061331                                                                          
061332        IF ALLT-OK                                                        
061333*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
061334           IF  WS-TISTADAT >= DAGENS-DATUM                                
061335           AND WS-TISTADAT >= GSPR-TISTADAT                               
061336              MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISTADAT-P-E-ATTR          
061337           ELSE                                                           
061338              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061339              MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-P-E-ATTR          
061340              MOVE NEJ                  TO INDATA-SW                      
061341              MOVE NEJ                  TO ALLT-SW                        
061342              MOVE JA                   TO E-RAD-FEL-SW                   
061344              MOVE JA                   TO E-PROD-TI-FEL-SW               
061345           END-IF                                                         
061346        END-IF                                                            
061347        IF ALLT-OK                                                        
061348        INSPECT MID-KDPRODSL-FOM-E REPLACING LEADING SPACE BY ZERO        
061349           MOVE MID-KDPRODSL-FOM-E      TO W-KDPRODSL-FOM                 
061350           IF MID-KDPRODSL-TOM-E = ALL '+'                                
061351              MOVE MID-KDPRODSL-FOM-E   TO W-KDPRODSL-TOM                 
061352           ELSE                                                           
061353        INSPECT MID-KDPRODSL-TOM-E REPLACING LEADING SPACE BY ZERO        
061354              MOVE MID-KDPRODSL-TOM-E   TO W-KDPRODSL-TOM                 
061355           END-IF                                                         
061356           PERFORM IMS-GHU-WDF814                                         
061357           IF MID-KDCMD-E = 'N'                                           
061358*---KOLLA SÅ ATT SEGMENT INTE REDAN FINNS PÅ BAS                          
061359              IF SEGMENT-FINNS                                            
061360                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-E-ATTR        
061361                                           MOD-KDPRODSL-TOM-E-ATTR        
061362                 MOVE NEJ               TO INDATA-SW                      
061363                 MOVE PART-EXISTS       TO MED-IDMFSFEL                   
061364                 MOVE JA                TO E-RAD-FEL-SW                   
061366                 MOVE JA                TO E-PRODFOM-FEL-SW               
061367              ELSE                                                        
061368               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-FOM-E-ATTR        
061369                                           MOD-KDPRODSL-TOM-E-ATTR        
061370              END-IF                                                      
061371                                                                          
061372           ELSE                                                           
061373                                                                          
061374*---KOLLA SÅ ATT SEGMENT FINNS PÅ BAS                                     
061375              IF SEGMENT-SAKNAS                                           
061376                 MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-E-ATTR        
061377                                           MOD-KDPRODSL-TOM-E-ATTR        
061378                 MOVE NEJ               TO INDATA-SW                      
061379                 MOVE PART-MISSING      TO MED-IDMFSFEL                   
061380                 MOVE JA                TO E-RAD-FEL-SW                   
061382                 MOVE JA                TO E-PRODFOM-FEL-SW               
061383              ELSE                                                        
061384               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-FOM-E-ATTR        
061385                                           MOD-KDPRODSL-TOM-E-ATTR        
061386              END-IF                                                      
061387           END-IF                                                         
061388        END-IF                                                            
061389     END-IF                                                               
061390     .                                                                    
061391                                                                          
061392                                                                          
061393 GH-KOLLA-FUNKTIONSGRUPP-CHNG   SECTION.                                  
061394     MOVE 'GH-KOLLA-FKN-CHG'   TO CURRENT-SECTION                         
061395                                                                          
061396     IF MID-IDFKNGRP-FOM-E = ALL '+'                                      
061397     OR MID-IDFKNGRP-FOM-E = SPACE                                        
061398        CONTINUE                                                          
061399     ELSE                                                                 
061400                                                                          
061401        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDFKNGRP-FOM-E-ATTR              
061402                                     MOD-IDFKNGRP-TOM-E-ATTR              
061403                                                                          
061404        IF MID-TISTADAT-F-E = ALL '+' OR ALL '0'                          
061406*---OM MAN EJ MATAT IN DATUM TAS DAGENS DATUM ELLER GRUPPENS              
061407           IF DAGENS-DATUM < GSPR-TISTADAT                                
061408              MOVE GSPR-TISTADAT     TO MID-TISTADAT-F-E                  
061409           ELSE                                                           
061410              MOVE DAGENS-DATUM      TO MID-TISTADAT-F-E                  
061411           END-IF                                                         
061413        END-IF                                                            
061414                                                                          
061415*---VALIDERING AV DATUM                                                   
061416        MOVE 'AAMMDD'             TO DAT-KDDATFORM                        
061417        MOVE MID-TISTADAT-F-E     TO DAT-I-TIDATUM                        
061418                                                                          
061419        CALL WDATKONV USING DAT-KDDATFORM                                 
061420                            DAT-I-TIDATUM                                 
061421                            DAT-O-TIDATUM                                 
061422                            DAT-KDSVAR                                    
061423        IF DAT-KDSVAR-FEL                                                 
061424           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
061425                                        MSG-KOM-IDMFSMED                  
061426           MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-F-E-ATTR             
061427           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-E-ATTR                
061428           MOVE NEJ                  TO INDATA-SW                         
061429           MOVE NEJ                  TO ALLT-SW                           
061430           MOVE JA                   TO E-RAD-FEL-SW                      
061432           MOVE JA                   TO E-FKN-TI-FEL-SW                   
061433        ELSE                                                              
061434           MOVE MID-TISTADAT-F-E     TO WS-TISTADAT                       
061435        END-IF                                                            
061436                                                                          
061437        IF ALLT-OK                                                        
061438*---KOLLAR SÅ ATT DATUM ÄR STÖRRE ELLER LIKA MED DAGENS DATUM             
061439           IF  WS-TISTADAT >= DAGENS-DATUM                                
061440           AND WS-TISTADAT >= GSPR-TISTADAT                               
061441              MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISTADAT-F-E-ATTR          
061442           ELSE                                                           
061443              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061444              MOVE MFS-NUM-FAELT-FEL    TO MOD-TISTADAT-F-E-ATTR          
061445              MOVE NEJ                  TO INDATA-SW                      
061446              MOVE NEJ                  TO ALLT-SW                        
061447              MOVE JA                   TO E-RAD-FEL-SW                   
061449              MOVE JA                   TO E-FKN-TI-FEL-SW                
061450           END-IF                                                         
061451        END-IF                                                            
061452        IF ALLT-OK                                                        
061453        INSPECT MID-IDFKNGRP-FOM-E REPLACING LEADING SPACE BY ZERO        
061454           MOVE MID-IDFKNGRP-FOM-E      TO W-IDFKNGRP-FOM                 
061455        INSPECT MID-IDFKNGRP-TOM-E REPLACING LEADING SPACE BY ZERO        
061456           IF MID-IDFKNGRP-TOM-E = ALL '+'                                
061457              MOVE MID-IDFKNGRP-FOM-E   TO W-IDFKNGRP-TOM                 
061458           ELSE                                                           
061459              MOVE MID-IDFKNGRP-TOM-E   TO W-IDFKNGRP-TOM                 
061460           END-IF                                                         
061461           PERFORM IMS-GHU-WDF815                                         
061462           IF MID-KDCMD-E = 'N'                                           
061463*---KOLLA SÅ ATT SEGMENT INTE REDAN FINNS PÅ BAS                          
061464              IF SEGMENT-FINNS                                            
061465                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-E-ATTR        
061466                                           MOD-IDFKNGRP-TOM-E-ATTR        
061467                 MOVE NEJ               TO INDATA-SW                      
061468                 MOVE PART-EXISTS       TO MED-IDMFSFEL                   
061469                 MOVE JA                TO E-RAD-FEL-SW                   
061471                 MOVE JA                TO E-FKNFOM-FEL-SW                
061472              ELSE                                                        
061473               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-E-ATTR        
061474                                           MOD-IDFKNGRP-TOM-E-ATTR        
061475              END-IF                                                      
061476                                                                          
061477           ELSE                                                           
061478                                                                          
061479*---KOLLA SÅ ATT SEGMENT FINNS PÅ BAS                                     
061480              IF SEGMENT-SAKNAS                                           
061481                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-E-ATTR        
061482                                           MOD-IDFKNGRP-TOM-E-ATTR        
061483                 MOVE NEJ               TO INDATA-SW                      
061484                 MOVE PART-MISSING      TO MED-IDMFSFEL                   
061485                 MOVE JA                TO E-RAD-FEL-SW                   
061487                 MOVE JA                TO E-FKNFOM-FEL-SW                
061488              ELSE                                                        
061489               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-E-ATTR        
061490                                           MOD-IDFKNGRP-TOM-E-ATTR        
061491              END-IF                                                      
061492           END-IF                                                         
061493        END-IF                                                            
061494     END-IF                                                               
061495     .                                                                    
061496                                                                          
061497                                                                          
061498 H-UPPDATERA SECTION.                                                     
061499     MOVE 'H-UPPDATERA     '   TO CURRENT-SECTION                         
061500                                                                          
061501     PERFORM HA-DELETE-ARTIKEL                                            
061502     PERFORM HB-DELETE-URSPRUNG                                           
061503     PERFORM HC-DELETE-PRODUKTSLAG                                        
061504     PERFORM HD-DELETE-FUNKTIONSGRUPP                                     
061505                                                                          
061506     PERFORM HE-UPPDATERA-ARTIKEL                                         
061507     PERFORM HF-UPPDATERA-URSPRUNG                                        
061508     PERFORM HG-UPPDATERA-PRODUKTSLAG                                     
061509     PERFORM HH-UPPDATERA-FUNKTIONSGRUPP                                  
061510                                                                          
061511     IF INDATA-OK                                                         
061512        MOVE INF-UPDATE-DONE   TO MED-IDMFSINF                            
061513                                  MSG-KOM-IDMFSMED                        
061514        CALL WMEDKONV USING MED-WMEDAREA                                  
061515        MOVE MED-MFSINF        TO MOD-TEMFSINF                            
061516        PERFORM MFS-RENSA-FAELT-IN                                        
061520     END-IF                                                               
061600                                                                          
061700     PERFORM HI-SPARA-ENTER-NYCKLAR                                       
066700     .                                                                    
066800                                                                          
066810                                                                          
066900 HA-DELETE-ARTIKEL SECTION.                                               
066910     MOVE 'HA-DEL-ARTIKEL '   TO CURRENT-SECTION                          
067000                                                                          
067001     MOVE 1 TO INDX                                                       
067002     PERFORM UNTIL INDX > MAX-INDX                                        
067003        IF MID-KDCMD-A (INDX) = 'D'                                       
067004           MOVE MID-IDARTNR (INDX) TO WS-IDARTNR                          
067005           MOVE WS-IDARTNR         TO W-IDARTNR                           
067006           PERFORM IMS-GHU-WDF812                                         
067007           IF SEGMENT-FINNS                                               
067008              PERFORM IMS-DLET-WDF812                                     
067009           END-IF                                                         
067010        END-IF                                                            
067011        ADD 1 TO INDX                                                     
067012     END-PERFORM                                                          
067015     .                                                                    
067020                                                                          
067030                                                                          
067031 HB-DELETE-URSPRUNG SECTION.                                              
067032     MOVE 'HB-DEL-URSPRUNG '   TO CURRENT-SECTION                         
067033                                                                          
067034     MOVE 1 TO INDX                                                       
067035     PERFORM UNTIL INDX > MAX-INDX                                        
067036        IF MID-KDCMD-U (INDX) = 'D'                                       
067037           MOVE MID-KDARTURS (INDX) TO W-KDARTURS                         
067039           PERFORM IMS-GHU-WDF813                                         
067040           IF SEGMENT-FINNS                                               
067041              PERFORM IMS-DLET-WDF813                                     
067042           END-IF                                                         
067043        END-IF                                                            
067044        ADD 1 TO INDX                                                     
067045     END-PERFORM                                                          
067046     .                                                                    
067047                                                                          
067048                                                                          
067049 HC-DELETE-PRODUKTSLAG SECTION.                                           
067050     MOVE 'HC-DEL-PRODUKTSL'   TO CURRENT-SECTION                         
067051                                                                          
067052     MOVE 1 TO INDX                                                       
067053     PERFORM UNTIL INDX > MAX-INDX                                        
067054        IF MID-KDCMD-P (INDX) = 'D'                                       
067055           MOVE MID-KDPRODSL-FOM (INDX) TO W-KDPRODSL-FOM                 
067056           IF MID-KDPRODSL-FOM (INDX) = ALL '+'                           
067057              MOVE MID-KDPRODSL-FOM (INDX)                                
067058                                        TO MID-KDPRODSL-TOM (INDX)        
067059           END-IF                                                         
067060           MOVE MID-KDPRODSL-TOM (INDX) TO W-KDPRODSL-TOM                 
067061           PERFORM IMS-GHU-WDF814                                         
067062           IF SEGMENT-FINNS                                               
067063              PERFORM IMS-DLET-WDF814                                     
067064           END-IF                                                         
067065        END-IF                                                            
067066        ADD 1 TO INDX                                                     
067067     END-PERFORM                                                          
067068     .                                                                    
067069                                                                          
067070                                                                          
067071 HD-DELETE-FUNKTIONSGRUPP SECTION.                                        
067072     MOVE 'HD-DEL-FKNGRP   '   TO CURRENT-SECTION                         
067073                                                                          
067074     MOVE 1 TO INDX                                                       
067075     PERFORM UNTIL INDX > MAX-INDX                                        
067076        IF MID-KDCMD-F (INDX) = 'D'                                       
067077           MOVE MID-IDFKNGRP-FOM (INDX) TO W-IDFKNGRP-FOM                 
067078           IF MID-IDFKNGRP-FOM (INDX) = ALL '+'                           
067079              MOVE MID-IDFKNGRP-FOM (INDX)                                
067080                                        TO MID-IDFKNGRP-TOM (INDX)        
067081           END-IF                                                         
067082           MOVE MID-IDFKNGRP-TOM (INDX) TO W-IDFKNGRP-TOM                 
067083           PERFORM IMS-GHU-WDF815                                         
067084           IF SEGMENT-FINNS                                               
067085              PERFORM IMS-DLET-WDF815                                     
067086           END-IF                                                         
067087        END-IF                                                            
067088        ADD 1 TO INDX                                                     
067089     END-PERFORM                                                          
067090     .                                                                    
067091                                                                          
067092                                                                          
067093 HE-UPPDATERA-ARTIKEL SECTION.                                            
067094     MOVE 'HE-UPD-ARTIKEL  '   TO CURRENT-SECTION                         
067095                                                                          
067096     IF MID-IDARTNR-E NOT = ALL '+'                                       
067097        IF MID-KDCMD-E = 'C'                                              
067098           MOVE MID-IDARTNR-E     TO WS-IDARTNR                           
067099           MOVE WS-IDARTNR        TO W-IDARTNR                            
067100           PERFORM IMS-GHU-WDF812                                         
067101                                                                          
067102           IF SEGMENT-FINNS                                               
067103              MOVE WS-TISTADAT TO ASPR-TISTADAT                           
067104              PERFORM IMS-REPL-WDF812                                     
067105           END-IF                                                         
067106        ELSE                                                              
067107           IF MID-KDCMD-E = 'N'                                           
067108              MOVE MID-IDARTNR-E  TO WS-IDARTNR                           
067109              MOVE WS-IDARTNR     TO ASPR-IDARTNR                         
067110                                         W-IDARTNR                        
067111              MOVE MID-TISTADAT-A-E TO WS-TISTADAT                        
067112              MOVE WS-TISTADAT    TO ASPR-TISTADAT                        
067113              PERFORM IMS-ISRT-WDF812                                     
067114                                                                          
067115              PERFORM IMS-GHU-WDK611                                      
067116              MOVE JA       TO WDK6CLAG-FLMARKSP                          
067117              PERFORM IMS-REPL-WDK611                                     
067118           END-IF                                                         
067119        END-IF                                                            
067120     END-IF                                                               
067123     .                                                                    
067124                                                                          
067125                                                                          
067126 HF-UPPDATERA-URSPRUNG SECTION.                                           
067127     MOVE 'HE-UPD-URSPRUNG '   TO CURRENT-SECTION                         
067128                                                                          
067129     IF MID-KDARTURS-E = ALL '+' OR SPACE                                 
067130        CONTINUE                                                          
067131     ELSE                                                                 
067132     IF MID-KDCMD-E = 'C'                                                 
067133        MOVE MID-KDARTURS-E       TO W-KDARTURS                           
067134        PERFORM IMS-GHU-WDF813                                            
067135                                                                          
067136        IF SEGMENT-FINNS                                                  
067137           MOVE WS-TISTADAT TO USPR-TISTADAT                              
067138           PERFORM IMS-REPL-WDF813                                        
067139        END-IF                                                            
067140     ELSE                                                                 
067141        IF MID-KDCMD-E = 'N'                                              
067142           MOVE MID-KDARTURS-E    TO W-KDARTURS                           
067143           MOVE W-KDARTURS        TO USPR-KDARTURS                        
067144           MOVE MID-TISTADAT-U-E  TO WS-TISTADAT                          
067145           MOVE WS-TISTADAT       TO USPR-TISTADAT                        
067146           PERFORM IMS-ISRT-WDF813                                        
067147*                                                                         
067148*          PERFORM IMS-GHU-WDK611                                         
067149*          MOVE JA          TO WDK6CLAG-FLMARKSP                          
067150*          PERFORM IMS-REPL-WDK611                                        
067151        END-IF                                                            
067152     END-IF                                                               
067153     END-IF                                                               
067154     .                                                                    
067155                                                                          
067156                                                                          
067157 HG-UPPDATERA-PRODUKTSLAG SECTION.                                        
067158     MOVE 'HG-UPD-PRODUKTSL'   TO CURRENT-SECTION                         
067159                                                                          
067160     IF MID-KDPRODSL-FOM-E = ALL '+' OR SPACE                             
067161        CONTINUE                                                          
067162     ELSE                                                                 
067163     IF MID-KDCMD-E = 'C'                                                 
067164        INSPECT MID-KDPRODSL-FOM-E REPLACING LEADING SPACE BY ZERO        
067165        MOVE MID-KDPRODSL-FOM-E   TO W-KDPRODSL-FOM                       
067166        INSPECT MID-KDPRODSL-TOM-E REPLACING LEADING SPACE BY ZERO        
067167        IF MID-KDPRODSL-FOM-E = ALL '+'                                   
067168           MOVE MID-KDPRODSL-FOM-E                                        
067169                                  TO MID-KDPRODSL-TOM-E                   
067170        END-IF                                                            
067171        MOVE MID-KDPRODSL-TOM-E   TO W-KDPRODSL-TOM                       
067172        PERFORM IMS-GHU-WDF814                                            
067173                                                                          
067174        IF SEGMENT-FINNS                                                  
067175           MOVE WS-TISTADAT TO PSPR-TISTADAT                              
067176           PERFORM IMS-REPL-WDF814                                        
067177        END-IF                                                            
067178     ELSE                                                                 
067179        IF MID-KDCMD-E = 'N'                                              
067180        INSPECT MID-KDPRODSL-FOM-E REPLACING LEADING SPACE BY ZERO        
067181           MOVE MID-KDPRODSL-FOM-E TO W-KDPRODSL-FOM                      
067182           MOVE W-KDPRODSL-FOM     TO PSPR-KDPRODSL-FOM                   
067183           IF MID-KDPRODSL-TOM-E = ALL '+'                                
067184              MOVE MID-KDPRODSL-FOM-E                                     
067185                                   TO MID-KDPRODSL-TOM-E                  
067186           END-IF                                                         
067187        INSPECT MID-KDPRODSL-TOM-E REPLACING LEADING SPACE BY ZERO        
067188           MOVE MID-KDPRODSL-TOM-E TO W-KDPRODSL-TOM                      
067189           MOVE W-KDPRODSL-TOM     TO PSPR-KDPRODSL-TOM                   
067190           MOVE MID-TISTADAT-P-E   TO WS-TISTADAT                         
067191           MOVE WS-TISTADAT        TO PSPR-TISTADAT                       
067192           PERFORM IMS-ISRT-WDF814                                        
067193*                                                                         
067194*          PERFORM IMS-GHU-WDK611                                         
067195*          MOVE JA          TO WDK6CLAG-FLMARKSP                          
067196*          PERFORM IMS-REPL-WDK611                                        
067197        END-IF                                                            
067198     END-IF                                                               
067199     END-IF                                                               
067200     .                                                                    
067201                                                                          
067202                                                                          
067203 HH-UPPDATERA-FUNKTIONSGRUPP SECTION.                                     
067204     MOVE 'HH-UPD-FKNGRP   '   TO CURRENT-SECTION                         
067205                                                                          
067206     IF MID-IDFKNGRP-FOM-E = ALL '+' OR SPACE                             
067207        CONTINUE                                                          
067208     ELSE                                                                 
067209     IF MID-KDCMD-E = 'C'                                                 
067210        INSPECT MID-IDFKNGRP-FOM-E REPLACING LEADING SPACE BY ZERO        
067211        MOVE MID-IDFKNGRP-FOM-E   TO W-IDFKNGRP-FOM                       
067212        IF MID-IDFKNGRP-FOM-E = ALL '+'                                   
067213           MOVE MID-IDFKNGRP-FOM-E                                        
067214                                  TO MID-IDFKNGRP-TOM-E                   
067215        END-IF                                                            
067216        INSPECT MID-IDFKNGRP-TOM-E REPLACING LEADING SPACE BY ZERO        
067217        MOVE MID-IDFKNGRP-TOM-E   TO W-IDFKNGRP-TOM                       
067218        PERFORM IMS-GHU-WDF815                                            
067219                                                                          
067220        IF SEGMENT-FINNS                                                  
067221           MOVE WS-TISTADAT TO FSPR-TISTADAT                              
067222           PERFORM IMS-REPL-WDF815                                        
067223        END-IF                                                            
067224     ELSE                                                                 
067225        IF MID-KDCMD-E = 'N'                                              
067226        INSPECT MID-IDFKNGRP-FOM-E REPLACING LEADING SPACE BY ZERO        
067227           MOVE MID-IDFKNGRP-FOM-E TO W-IDFKNGRP-FOM                      
067228           MOVE W-IDFKNGRP-FOM     TO FSPR-IDFKNGRP-FOM                   
067229           IF MID-IDFKNGRP-TOM-E = ALL '+'                                
067230              MOVE MID-IDFKNGRP-FOM-E                                     
067231                                   TO MID-IDFKNGRP-TOM-E                  
067232           END-IF                                                         
067233        INSPECT MID-IDFKNGRP-TOM-E REPLACING LEADING SPACE BY ZERO        
067234           MOVE MID-IDFKNGRP-TOM-E TO W-IDFKNGRP-TOM                      
067235           MOVE W-IDFKNGRP-TOM     TO FSPR-IDFKNGRP-TOM                   
067236           MOVE MID-TISTADAT-F-E   TO WS-TISTADAT                         
067237           MOVE WS-TISTADAT        TO FSPR-TISTADAT                       
067238           PERFORM IMS-ISRT-WDF815                                        
067239*                                                                         
067240*          PERFORM IMS-GHU-WDK611                                         
067241*          MOVE JA          TO WDK6CLAG-FLMARKSP                          
067242*          PERFORM IMS-REPL-WDK611                                        
067243        END-IF                                                            
067244     END-IF                                                               
067245     END-IF                                                               
067246     .                                                                    
067247                                                                          
067248                                                                          
067254 HI-SPARA-ENTER-NYCKLAR SECTION.                                          
067255     MOVE 'HI-SPARA-ENTER  '   TO CURRENT-SECTION                         
067256                                                                          
067258     MOVE SPAR-IDARTNR-ENTER      TO W-IDARTNR                            
067259     MOVE SPAR-KDARTURS-ENTER     TO W-KDARTURS                           
067260     MOVE SPAR-KDPRODSL-FOM-ENTER TO W-KDPRODSL-FOM                       
067261     MOVE SPAR-KDPRODSL-TOM-ENTER TO W-KDPRODSL-TOM                       
067264     MOVE SPAR-IDFKNGRP-FOM-ENTER TO W-IDFKNGRP-FOM                       
067265     MOVE SPAR-IDFKNGRP-TOM-ENTER TO W-IDFKNGRP-TOM                       
067266                                                                          
067267     MOVE SPACE       TO MFS-KDTRTYP                                      
067268     MOVE '7'         TO MFS-IDPFK                                        
067269     .                                                                    
067270                                                                          
067271                                                                          
067272 MFS-RENSA-FAELT-UT SECTION.                                              
067273                                                                          
067274*    --- ALLA UTDATA-FÄLT                                                 
067280*    --- INKL. BLÄDDRINGSNYCKLAR                                          
067300     MOVE MFS-RENSA-FAELT TO MOD-TISTADAT-GRP                             
067310                             MOD-KDCMD-E                                  
067400                             MOD-IDARTNR-E                                
067500                             MOD-TISTADAT-A-E                             
067600                             MOD-KDARTURS-E                               
067610                             MOD-TISTADAT-U-E                             
067620                             MOD-KDPRODSL-FOM-E                           
067621                             MOD-KDPRODSL-TOM-E                           
067630                             MOD-TISTADAT-P-E                             
067640                             MOD-IDFKNGRP-FOM-E                           
067650                             MOD-IDFKNGRP-TOM-E                           
067660                             MOD-TISTADAT-F-E                             
067700     MOVE +1 TO MFS-INDX                                                  
067800     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
067900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
068000       ADD +1 TO MFS-INDX                                                 
068100     END-PERFORM                                                          
068200     .                                                                    
068300                                                                          
068400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
068500                                                                          
068600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
068700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR      (MFS-INDX)                  
068800                             MOD-TISTADAT-A   (MFS-INDX)                  
068810                             MOD-KDARTURS     (MFS-INDX)                  
068820                             MOD-TISTADAT-U   (MFS-INDX)                  
068830                             MOD-KDPRODSL-FOM (MFS-INDX)                  
068831                             MOD-KDPRODSL-TOM (MFS-INDX)                  
068840                             MOD-TISTADAT-P   (MFS-INDX)                  
068850                             MOD-IDFKNGRP-FOM (MFS-INDX)                  
068860                             MOD-IDFKNGRP-TOM (MFS-INDX)                  
068870                             MOD-TISTADAT-F   (MFS-INDX)                  
068900     .                                                                    
069000                                                                          
069100 MFS-RENSA-FAELT-IN SECTION.                                              
069200                                                                          
069300*    --- ALLA INDATA-FÄLT                                                 
069400     MOVE MFS-RENSA-FAELT TO MOD-IDSPRGRP-IN                              
069500                             MOD-IDARTNR-IN                               
069600                             MOD-KDCMD-E                                  
069700                             MOD-IDARTNR-E                                
069800                             MOD-TISTADAT-A-E                             
069801                             MOD-KDARTURS-E                               
069802                             MOD-TISTADAT-U-E                             
069803                             MOD-KDPRODSL-FOM-E                           
069804                             MOD-KDPRODSL-TOM-E                           
069805                             MOD-TISTADAT-P-E                             
069806                             MOD-IDFKNGRP-FOM-E                           
069807                             MOD-IDFKNGRP-TOM-E                           
069808                             MOD-TISTADAT-F-E                             
069810     MOVE +1 TO MFS-INDX                                                  
069820     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
069830       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
069840       ADD +1 TO MFS-INDX                                                 
069850     END-PERFORM                                                          
069900     .                                                                    
069910 MFS-RENSA-RAD-FAELT-IN  SECTION.                                         
069920                                                                          
069940     MOVE MFS-RENSA-FAELT   TO MOD-KDCMD-A      (MFS-INDX)                
069950                               MOD-IDARTNR      (MFS-INDX)                
069951                               MOD-KDCMD-U      (MFS-INDX)                
069952                               MOD-KDARTURS     (MFS-INDX)                
069953                               MOD-KDPRODSL-FOM (MFS-INDX)                
069954                               MOD-KDPRODSL-TOM (MFS-INDX)                
069955                               MOD-IDFKNGRP-FOM (MFS-INDX)                
069956                               MOD-IDFKNGRP-TOM (MFS-INDX)                
069960     .                                                                    
070000                                                                          
070010                                                                          
070100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
070200                                                                          
070300*    --- ALLA UTDATA-FÄLT                                                 
070400*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
070500     MOVE MFS-ROER-EJ-FAELT TO MOD-TISTADAT-GRP                           
070510                               MOD-FLAUTUPD-GRP                           
070511                               MOD-KDARTURS-ART                           
070512                               MOD-KDPRODSL-ART                           
070513                               MOD-IDFKNGRP-ART                           
070520                               MOD-KDCMD-E                                
070600                               MOD-IDARTNR-E                              
070700                               MOD-TISTADAT-A-E                           
070800                               MOD-KDARTURS-E                             
070810                               MOD-TISTADAT-U-E                           
070820                               MOD-KDPRODSL-FOM-E                         
070821                               MOD-KDPRODSL-TOM-E                         
070830                               MOD-TISTADAT-P-E                           
070840                               MOD-IDFKNGRP-FOM-E                         
070850                               MOD-IDFKNGRP-TOM-E                         
070860                               MOD-TISTADAT-F-E                           
070900     MOVE +1 TO MFS-INDX                                                  
071000     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
071100       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
071200       ADD +1 TO MFS-INDX                                                 
071300     END-PERFORM                                                          
071500     .                                                                    
071510                                                                          
071600 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
071700                                                                          
071800*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
071900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR      (MFS-INDX)                
072000                               MOD-TISTADAT-A   (MFS-INDX)                
072010                               MOD-KDARTURS     (MFS-INDX)                
072020                               MOD-TISTADAT-U   (MFS-INDX)                
072030                               MOD-KDPRODSL-FOM (MFS-INDX)                
072031                               MOD-KDPRODSL-TOM (MFS-INDX)                
072040                               MOD-TISTADAT-P   (MFS-INDX)                
072050                               MOD-IDFKNGRP-FOM (MFS-INDX)                
072060                               MOD-IDFKNGRP-TOM (MFS-INDX)                
072070                               MOD-TISTADAT-F   (MFS-INDX)                
072100     .                                                                    
072200                                                                          
072300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
072400                                                                          
072500*    --- ALLA INDATA-FÄLT                                                 
072600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDSPRGRP-IN                            
072700                               MOD-IDARTNR-IN                             
072800     MOVE +1 TO MFS-INDX                                                  
072900     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
073000       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-A      (MFS-INDX)             
073001                                  MOD-IDARTNR      (MFS-INDX)             
073002                                  MOD-KDCMD-U      (MFS-INDX)             
073003                                  MOD-KDARTURS     (MFS-INDX)             
073004                                  MOD-KDPRODSL-FOM (MFS-INDX)             
073005                                  MOD-KDPRODSL-TOM (MFS-INDX)             
073006                                  MOD-TISTADAT-P   (MFS-INDX)             
073007                                  MOD-IDFKNGRP-FOM (MFS-INDX)             
073008                                  MOD-IDFKNGRP-TOM (MFS-INDX)             
073009                                  MOD-TISTADAT-F   (MFS-INDX)             
073010       ADD +1 TO MFS-INDX                                                 
073020     END-PERFORM                                                          
073100     .                                                                    
073200                                                                          
073210                                                                          
073211 MFS-ROER-EJ-FAELT-IN-ART  SECTION.                                       
073212                                                                          
073216     MOVE +1 TO MFS-INDX                                                  
073217     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
073218       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-A  (MFS-INDX)                 
073219                                  MOD-IDARTNR  (MFS-INDX)                 
073222       ADD +1 TO MFS-INDX                                                 
073223     END-PERFORM                                                          
073224     .                                                                    
073225                                                                          
073226                                                                          
073227 MFS-ROER-EJ-FAELT-IN-URS  SECTION.                                       
073228                                                                          
073232     MOVE +1 TO MFS-INDX                                                  
073233     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
073234       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-U  (MFS-INDX)                 
073235                                  MOD-KDARTURS (MFS-INDX)                 
073236       ADD +1 TO MFS-INDX                                                 
073237     END-PERFORM                                                          
073238     .                                                                    
073239                                                                          
073240                                                                          
073241 MFS-ROER-EJ-FAELT-IN-PRO  SECTION.                                       
073242                                                                          
073246     MOVE +1 TO MFS-INDX                                                  
073247     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
073248       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-P      (MFS-INDX)             
073249                                  MOD-KDPRODSL-FOM (MFS-INDX)             
073250                                  MOD-KDPRODSL-TOM (MFS-INDX)             
073251       ADD +1 TO MFS-INDX                                                 
073252     END-PERFORM                                                          
073253     .                                                                    
073254                                                                          
073255                                                                          
073256 MFS-ROER-EJ-FAELT-IN-FKN  SECTION.                                       
073257                                                                          
073261     MOVE +1 TO MFS-INDX                                                  
073262     PERFORM UNTIL MFS-INDX > MAX-INDX                                    
073263       MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMD-F      (MFS-INDX)             
073264                                  MOD-IDFKNGRP-FOM (MFS-INDX)             
073265                                  MOD-IDFKNGRP-TOM (MFS-INDX)             
073266       ADD +1 TO MFS-INDX                                                 
073267     END-PERFORM                                                          
073268     .                                                                    
073269                                                                          
073270                                                                          
073271 MFS-ROER-EJ-FAELT-IN-E  SECTION.                                         
073272                                                                          
073273*    --- ALLA INDATA-FÄLT                                                 
073274                                                                          
073275     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-E                                
073280                               MOD-IDARTNR-E                              
073290                               MOD-TISTADAT-A-E                           
073291                               MOD-KDARTURS-E                             
073292                               MOD-TISTADAT-U-E                           
073293                               MOD-KDPRODSL-FOM-E                         
073294                               MOD-KDPRODSL-TOM-E                         
073295                               MOD-TISTADAT-P-E                           
073296                               MOD-IDFKNGRP-FOM-E                         
073297                               MOD-IDFKNGRP-TOM-E                         
073298                               MOD-TISTADAT-F-E                           
073299     .                                                                    
073300                                                                          
073301                                                                          
073310 MFS-FORM-ATTR SECTION.                                                   
073400                                                                          
073500*    --- ALLA INDATA-FÄLT                                                 
073600     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-E-ATTR                          
073700                                MOD-IDARTNR-E-ATTR                        
073800                                MOD-TISTADAT-A-E-ATTR                     
073810                                MOD-KDARTURS-E-ATTR                       
073820                                MOD-TISTADAT-U-E-ATTR                     
073830                                MOD-KDPRODSL-FOM-E-ATTR                   
073831                                MOD-KDPRODSL-TOM-E-ATTR                   
073840                                MOD-TISTADAT-P-E-ATTR                     
073850                                MOD-IDFKNGRP-FOM-E-ATTR                   
073860                                MOD-IDFKNGRP-TOM-E-ATTR                   
073870                                MOD-TISTADAT-F-E-ATTR                     
073900     .                                                                    
074000                                                                          
074100 MFS-LAES-IN-IGEN-E SECTION.                                              
074200                                                                          
074300*    --- ALLA INDATA-FÄLT                                                 
074400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-E-ATTR                       
074500                                   MOD-IDARTNR-E-ATTR                     
074600                                   MOD-TISTADAT-A-E-ATTR                  
074610                                   MOD-KDARTURS-E-ATTR                    
074620                                   MOD-TISTADAT-U-E-ATTR                  
074630                                   MOD-KDPRODSL-FOM-E-ATTR                
074631                                   MOD-KDPRODSL-TOM-E-ATTR                
074640                                   MOD-TISTADAT-P-E-ATTR                  
074650                                   MOD-IDFKNGRP-FOM-E-ATTR                
074660                                   MOD-IDFKNGRP-TOM-E-ATTR                
074670                                   MOD-TISTADAT-F-E-ATTR                  
074700     .                                                                    
074800                                                                          
074810                                                                          
074900* --- IMS SEKTIONER ---                                                   
075000                                                                          
075100 IMS-GET-MSG SECTION.                                                     
075200                                                                          
075300     MOVE '  QC' TO GODK-STATUSKODER                                      
075400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075600     PERFORM IMS-STATUSKONTROLL                                           
075700     .                                                                    
075800                                                                          
075810                                                                          
075900 IMS-INSERT-MSG SECTION.                                                  
076000                                                                          
076100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
076200     MOVE SPACE TO GODK-STATUSKODER                                       
076300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700                                                                          
076800 IMS-GET-WMSGKOM-MSG SECTION.                                             
076900                                                                          
077000     MOVE '  QD'   TO GODK-STATUSKODER                                    
077100     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
077200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077300     PERFORM IMS-STATUSKONTROLL                                           
077400     .                                                                    
077500                                                                          
077600                                                                          
077700 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
077800                                                                          
077900     MOVE '  '  TO GODK-STATUSKODER                                       
078000     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
078100     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
078200     PERFORM IMS-STATUSKONTROLL                                           
078300     .                                                                    
078400                                                                          
078500                                                                          
078600                                                                          
079500 IMS-GU-WDK601 SECTION.                                                   
079510     MOVE 'IMS-GU-WDK601   '   TO CURRENT-IMS-SECTION                     
079600                                                                          
079610     MOVE SPACE               TO ALL-SSA                                  
079620                                                                          
079700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
079800          DELIMITED BY SIZE INTO SSA1                                     
080100     MOVE '  GE' TO GODK-STATUSKODER                                      
080200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
080300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
080400     PERFORM IMS-STATUSKONTROLL                                           
080500     .                                                                    
080600                                                                          
080610                                                                          
080620 IMS-GNP-WDK611 SECTION.                                                  
080630     MOVE 'IMS-GNP-WDK611  '   TO CURRENT-IMS-SECTION                     
080640                                                                          
080650     MOVE SPACE               TO ALL-SSA                                  
080660                                                                          
080690     MOVE 'WDK611   '         TO SSA1                                     
080700     MOVE '  GE' TO GODK-STATUSKODER                                      
080710     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
080720     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
080730     PERFORM IMS-STATUSKONTROLL                                           
080731     .                                                                    
080732                                                                          
080733                                                                          
080734 IMS-GHU-WDK611 SECTION.                                                  
080735     MOVE 'IMS-GHU-WDK611  '   TO CURRENT-IMS-SECTION                     
080736                                                                          
080737     MOVE SPACE               TO ALL-SSA                                  
080738                                                                          
080739     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
080740          DELIMITED BY SIZE INTO SSA1                                     
080741     MOVE 'WDK611   '         TO SSA2                                     
080742     MOVE '  GE' TO GODK-STATUSKODER                                      
080743     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
080744     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
080745     PERFORM IMS-STATUSKONTROLL                                           
080746     .                                                                    
080750                                                                          
080760                                                                          
080800 IMS-REPL-WDK611 SECTION.                                                 
080810     MOVE 'IMS-REPL-WDK611 '   TO CURRENT-IMS-SECTION                     
080820                                                                          
080830     MOVE SPACE               TO ALL-SSA                                  
080900                                                                          
081000     MOVE '    ' TO GODK-STATUSKODER                                      
081100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
081200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
081300     PERFORM IMS-STATUSKONTROLL                                           
081400     .                                                                    
081500                                                                          
081510                                                                          
081600 IMS-GHU-LEV SECTION.                                                     
081610     MOVE 'IMS-GHU-LEV     '   TO CURRENT-IMS-SECTION                     
081620                                                                          
081630     MOVE SPACE               TO ALL-SSA                                  
081700                                                                          
081800     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
081900          DELIMITED BY SIZE INTO SSA1                                     
082000     MOVE '  '             TO GODK-STATUSKODER                            
082100     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF801 SSA1                   
082200     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
082300     PERFORM IMS-STATUSKONTROLL                                           
082400     .                                                                    
082500                                                                          
082510                                                                          
082600 IMS-GU-WDF801 SECTION.                                                   
082610     MOVE 'IMS-GU-WDF801   '   TO CURRENT-IMS-SECTION                     
082620                                                                          
082630     MOVE SPACE               TO ALL-SSA                                  
082700                                                                          
082800     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
082900          DELIMITED BY SIZE INTO SSA1                                     
083000     MOVE '  GE' TO GODK-STATUSKODER                                      
083100     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF801 SSA1                    
083200     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500                                                                          
083510                                                                          
083600 IMS-GHU-WDF812 SECTION.                                                  
083610     MOVE 'IMS-GHU-WDF812  '   TO CURRENT-IMS-SECTION                     
083620                                                                          
083630     MOVE SPACE                TO ALL-SSA                                 
083700                                                                          
083800     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
083900           DELIMITED BY SIZE INTO SSA1                                    
084000     STRING 'WDF812  (IDARTNR = ' W-IDARTNR-X ')'                         
084100           DELIMITED BY SIZE INTO SSA2                                    
084200     MOVE '  GE'               TO GODK-STATUSKODER                        
084300     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF812 SSA1 SSA2              
084400     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
084500     PERFORM IMS-STATUSKONTROLL                                           
084600     .                                                                    
084700                                                                          
084710                                                                          
084720 IMS-GHU-WDF812M SECTION.                                                 
084730     MOVE 'IMS-GHU-WDF812M '   TO CURRENT-IMS-SECTION                     
084740                                                                          
084750     MOVE SPACE                TO ALL-SSA                                 
084760                                                                          
084770     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
084780           DELIMITED BY SIZE INTO SSA1                                    
084790     STRING 'WDF812  (IDARTNR = ' W-IDARTNR-F8-X ')'                      
084800           DELIMITED BY SIZE INTO SSA2                                    
084900     MOVE '  GE'               TO GODK-STATUSKODER                        
085000     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF812 SSA1 SSA2              
085100     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
085200     PERFORM IMS-STATUSKONTROLL                                           
085300     .                                                                    
085400                                                                          
085500                                                                          
086000 IMS-GHNP-WDF812 SECTION.                                                 
086010     MOVE 'IMS-GHNP-WDF812 '   TO CURRENT-IMS-SECTION                     
086020                                                                          
086030     MOVE SPACE                TO ALL-SSA                                 
086100                                                                          
086400     MOVE 'WDF812   '          TO SSA1                                    
086500     MOVE '  GE'               TO GODK-STATUSKODER                        
086600     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF812 SSA1                  
086700     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
086800     PERFORM IMS-STATUSKONTROLL                                           
086900     .                                                                    
087000                                                                          
087010                                                                          
087020 IMS-GHNP-WDF812-KEY SECTION.                                             
087030     MOVE 'IMS-GHNP-WDF812K'   TO CURRENT-IMS-SECTION                     
087040                                                                          
087050     MOVE SPACE                TO ALL-SSA                                 
087060                                                                          
087071     STRING 'WDF812  (IDARTNR = ' W-IDARTNR-X ')'                         
087072           DELIMITED BY SIZE INTO SSA1                                    
087080     MOVE '  GE'               TO GODK-STATUSKODER                        
087090     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF812 SSA1                  
087091     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
087092     PERFORM IMS-STATUSKONTROLL                                           
087093     .                                                                    
087094                                                                          
087095                                                                          
087100 IMS-ISRT-WDF812 SECTION.                                                 
087110     MOVE 'IMS-ISRT-WDF812 '  TO CURRENT-IMS-SECTION                      
087120                                                                          
087130     MOVE SPACE               TO ALL-SSA                                  
087200                                                                          
087210     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
087220          DELIMITED BY SIZE INTO SSA1                                     
087300     MOVE 'WDF812   '         TO SSA2                                     
087400     MOVE '  '                TO GODK-STATUSKODER                         
087500     CALL CBLTDLI USING ISRT WDF8-PCB DLI-IO-WDF812 SSA1 SSA2             
087600     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     .                                                                    
087900                                                                          
087910                                                                          
088000 IMS-REPL-WDF812 SECTION.                                                 
088010     MOVE 'IMS-REPL-WDF812 '   TO CURRENT-IMS-SECTION                     
088020                                                                          
088030     MOVE SPACE                TO ALL-SSA                                 
088100                                                                          
088200     MOVE '  '                 TO GODK-STATUSKODER                        
088300     CALL CBLTDLI USING REPL WDF8-PCB DLI-IO-WDF812                       
088400     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
088500     PERFORM IMS-STATUSKONTROLL                                           
088600     .                                                                    
088700                                                                          
088710                                                                          
088800 IMS-DLET-WDF812 SECTION.                                                 
088810     MOVE 'IMS-DLET-WDF812 '  TO CURRENT-IMS-SECTION                      
088820                                                                          
088830     MOVE SPACE               TO ALL-SSA                                  
088900                                                                          
089000     MOVE '  '                TO GODK-STATUSKODER                         
089100     CALL CBLTDLI USING DLET WDF8-PCB DLI-IO-WDF812                       
089200     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     .                                                                    
089500                                                                          
089510                                                                          
089520 IMS-GHU-WDF813 SECTION.                                                  
089530     MOVE 'IMS-GHU-WDF813  '   TO CURRENT-IMS-SECTION                     
089540                                                                          
089550     MOVE SPACE                TO ALL-SSA                                 
089560                                                                          
089570     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089580           DELIMITED BY SIZE INTO SSA1                                    
089590     STRING 'WDF813  (KDARTURS =' W-KDARTURS-X ')'                        
089591           DELIMITED BY SIZE INTO SSA2                                    
089592     MOVE '  GE'               TO GODK-STATUSKODER                        
089593     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF813 SSA1 SSA2              
089594     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089595     PERFORM IMS-STATUSKONTROLL                                           
089596     .                                                                    
089597                                                                          
089598                                                                          
089599 IMS-GHU-WDF813M SECTION.                                                 
089600     MOVE 'IMS-GHU-WDF813M '   TO CURRENT-IMS-SECTION                     
089601                                                                          
089602     MOVE SPACE                TO ALL-SSA                                 
089603                                                                          
089604     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089605           DELIMITED BY SIZE INTO SSA1                                    
089606     STRING 'WDF813  (KDARTURS =' W-KDARTURS-F8-X ')'                     
089607           DELIMITED BY SIZE INTO SSA2                                    
089608     MOVE '  GE'               TO GODK-STATUSKODER                        
089609     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF813 SSA1 SSA2              
089610     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089611     PERFORM IMS-STATUSKONTROLL                                           
089612     .                                                                    
089613                                                                          
089614                                                                          
089615 IMS-GHNP-WDF813 SECTION.                                                 
089616     MOVE 'IMS-GHNP-WDF813 '   TO CURRENT-IMS-SECTION                     
089617                                                                          
089618     MOVE SPACE                TO ALL-SSA                                 
089619                                                                          
089620     MOVE 'WDF813   '          TO SSA1                                    
089621     MOVE '  GE'               TO GODK-STATUSKODER                        
089622     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF813 SSA1                  
089623     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089624     PERFORM IMS-STATUSKONTROLL                                           
089625     .                                                                    
089626                                                                          
089627                                                                          
089628 IMS-GHNP-WDF813-KEY SECTION.                                             
089629     MOVE 'IMS-GHNP-WDF813K'   TO CURRENT-IMS-SECTION                     
089630                                                                          
089631     MOVE SPACE                TO ALL-SSA                                 
089632                                                                          
089633     STRING 'WDF813  (KDARTURS =' W-KDARTURS-X ')'                        
089634           DELIMITED BY SIZE INTO SSA1                                    
089635     MOVE '  GE'               TO GODK-STATUSKODER                        
089636     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF813 SSA1                  
089637     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089638     PERFORM IMS-STATUSKONTROLL                                           
089639     .                                                                    
089640                                                                          
089641                                                                          
089642 IMS-ISRT-WDF813 SECTION.                                                 
089643     MOVE 'IMS-ISRT-WDF813 '  TO CURRENT-IMS-SECTION                      
089644                                                                          
089645     MOVE SPACE               TO ALL-SSA                                  
089646                                                                          
089647     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089648          DELIMITED BY SIZE INTO SSA1                                     
089649     MOVE 'WDF813   '         TO SSA2                                     
089650     MOVE '  '                TO GODK-STATUSKODER                         
089651     CALL CBLTDLI USING ISRT WDF8-PCB DLI-IO-WDF813 SSA1 SSA2             
089652     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
089653     PERFORM IMS-STATUSKONTROLL                                           
089654     .                                                                    
089655                                                                          
089656                                                                          
089657 IMS-REPL-WDF813 SECTION.                                                 
089658     MOVE 'IMS-REPL-WDF813 '   TO CURRENT-IMS-SECTION                     
089659                                                                          
089660     MOVE SPACE                TO ALL-SSA                                 
089661                                                                          
089662     MOVE '  '                 TO GODK-STATUSKODER                        
089663     CALL CBLTDLI USING REPL WDF8-PCB DLI-IO-WDF813                       
089664     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089665     PERFORM IMS-STATUSKONTROLL                                           
089666     .                                                                    
089667                                                                          
089668                                                                          
089669 IMS-DLET-WDF813 SECTION.                                                 
089670     MOVE 'IMS-DLET-WDF813 '  TO CURRENT-IMS-SECTION                      
089671                                                                          
089672     MOVE SPACE               TO ALL-SSA                                  
089673                                                                          
089674     MOVE '  '                TO GODK-STATUSKODER                         
089675     CALL CBLTDLI USING DLET WDF8-PCB DLI-IO-WDF813                       
089676     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
089677     PERFORM IMS-STATUSKONTROLL                                           
089678     .                                                                    
089679                                                                          
089680                                                                          
089681 IMS-GHU-WDF814 SECTION.                                                  
089682     MOVE 'IMS-GHU-WDF814  '   TO CURRENT-IMS-SECTION                     
089683                                                                          
089684     MOVE SPACE                TO ALL-SSA                                 
089685                                                                          
089686     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089687           DELIMITED BY SIZE INTO SSA1                                    
089688     STRING 'WDF814  (WDF814KY =' W-WDF814KY-X ')'                        
089689           DELIMITED BY SIZE INTO SSA2                                    
089690     MOVE '  GE'               TO GODK-STATUSKODER                        
089691     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF814 SSA1 SSA2              
089692     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089693     PERFORM IMS-STATUSKONTROLL                                           
089694     .                                                                    
089695                                                                          
089696                                                                          
089697 IMS-GHU-WDF814M SECTION.                                                 
089698     MOVE 'IMS-GHU-WDF814M '   TO CURRENT-IMS-SECTION                     
089699                                                                          
089700     MOVE SPACE                TO ALL-SSA                                 
089701                                                                          
089702     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089703           DELIMITED BY SIZE INTO SSA1                                    
089704     STRING 'WDF814  (WDF814KY =' W-WDF814KY-F8-X ')'                     
089705           DELIMITED BY SIZE INTO SSA2                                    
089706     MOVE '  GE'               TO GODK-STATUSKODER                        
089707     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF814 SSA1 SSA2              
089708     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089709     PERFORM IMS-STATUSKONTROLL                                           
089710     .                                                                    
089711                                                                          
089712                                                                          
089713 IMS-GHNP-WDF814 SECTION.                                                 
089714     MOVE 'IMS-GHNP-WDF814 '   TO CURRENT-IMS-SECTION                     
089715                                                                          
089716     MOVE SPACE                TO ALL-SSA                                 
089717                                                                          
089718     MOVE 'WDF814   '          TO SSA1                                    
089719     MOVE '  GE'               TO GODK-STATUSKODER                        
089720     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF814 SSA1                  
089721     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089722     PERFORM IMS-STATUSKONTROLL                                           
089723     .                                                                    
089724                                                                          
089725                                                                          
089726 IMS-GNP-WDF814-PROD SECTION.                                             
089727     MOVE 'IMS-GNP-WDF814P '   TO CURRENT-IMS-SECTION                     
089728                                                                          
089729     MOVE SPACE                TO ALL-SSA                                 
089730                                                                          
089731     STRING 'WDF814  (KDPRODSF<=' W-KDPRODSL-X                            
089732                    '&KDPRODST>=' W-KDPRODSL-X ')'                        
089733           DELIMITED BY SIZE INTO SSA1                                    
089734     MOVE '  GE'               TO GODK-STATUSKODER                        
089735     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF814 SSA1                  
089736     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089737     PERFORM IMS-STATUSKONTROLL                                           
089738     .                                                                    
089739                                                                          
089740                                                                          
089741 IMS-GHNP-WDF814-KEY SECTION.                                             
089742     MOVE 'IMS-GHNP-WDF814K'   TO CURRENT-IMS-SECTION                     
089743                                                                          
089744     MOVE SPACE                TO ALL-SSA                                 
089745                                                                          
089746     STRING 'WDF814  (WDF814KY =' W-WDF814KY-X ')'                        
089747           DELIMITED BY SIZE INTO SSA1                                    
089748     MOVE '  GE'               TO GODK-STATUSKODER                        
089749     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF814 SSA1                  
089750     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089751     PERFORM IMS-STATUSKONTROLL                                           
089752     .                                                                    
089753                                                                          
089754                                                                          
089755 IMS-ISRT-WDF814 SECTION.                                                 
089756     MOVE 'IMS-ISRT-WDF814 '  TO CURRENT-IMS-SECTION                      
089757                                                                          
089758     MOVE SPACE               TO ALL-SSA                                  
089759                                                                          
089760     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089761          DELIMITED BY SIZE INTO SSA1                                     
089762     MOVE 'WDF814   '         TO SSA2                                     
089763     MOVE '  '                TO GODK-STATUSKODER                         
089764     CALL CBLTDLI USING ISRT WDF8-PCB DLI-IO-WDF814 SSA1 SSA2             
089765     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
089766     PERFORM IMS-STATUSKONTROLL                                           
089767     .                                                                    
089768                                                                          
089769                                                                          
089770 IMS-REPL-WDF814 SECTION.                                                 
089771     MOVE 'IMS-REPL-WDF814 '   TO CURRENT-IMS-SECTION                     
089772                                                                          
089773     MOVE SPACE                TO ALL-SSA                                 
089774                                                                          
089775     MOVE '  '                 TO GODK-STATUSKODER                        
089776     CALL CBLTDLI USING REPL WDF8-PCB DLI-IO-WDF814                       
089777     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089778     PERFORM IMS-STATUSKONTROLL                                           
089779     .                                                                    
089780                                                                          
089781                                                                          
089782 IMS-DLET-WDF814 SECTION.                                                 
089783     MOVE 'IMS-DLET-WDF814 '  TO CURRENT-IMS-SECTION                      
089784                                                                          
089785     MOVE SPACE               TO ALL-SSA                                  
089786                                                                          
089787     MOVE '  '                TO GODK-STATUSKODER                         
089788     CALL CBLTDLI USING DLET WDF8-PCB DLI-IO-WDF814                       
089789     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
089790     PERFORM IMS-STATUSKONTROLL                                           
089791     .                                                                    
089792                                                                          
089793                                                                          
089794 IMS-GHU-WDF815 SECTION.                                                  
089795     MOVE 'IMS-GHU-WDF815  '   TO CURRENT-IMS-SECTION                     
089796                                                                          
089797     MOVE SPACE                TO ALL-SSA                                 
089798                                                                          
089799     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089800           DELIMITED BY SIZE INTO SSA1                                    
089801     STRING 'WDF815  (WDF815KY =' W-WDF815KY-X ')'                        
089802           DELIMITED BY SIZE INTO SSA2                                    
089803     MOVE '  GE'               TO GODK-STATUSKODER                        
089804     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF815 SSA1 SSA2              
089805     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089806     PERFORM IMS-STATUSKONTROLL                                           
089807     .                                                                    
089808                                                                          
089809                                                                          
089810 IMS-GHU-WDF815M SECTION.                                                 
089811     MOVE 'IMS-GHU-WDF815M '   TO CURRENT-IMS-SECTION                     
089812                                                                          
089813     MOVE SPACE                TO ALL-SSA                                 
089814                                                                          
089815     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089816           DELIMITED BY SIZE INTO SSA1                                    
089817     STRING 'WDF815  (WDF815KY =' W-WDF815KY-F8-X ')'                     
089818           DELIMITED BY SIZE INTO SSA2                                    
089819     MOVE '  GE'               TO GODK-STATUSKODER                        
089820     CALL CBLTDLI USING GHU WDF8-PCB DLI-IO-WDF815 SSA1 SSA2              
089821     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089822     PERFORM IMS-STATUSKONTROLL                                           
089823     .                                                                    
089824                                                                          
089825                                                                          
089826 IMS-GHNP-WDF815 SECTION.                                                 
089827     MOVE 'IMS-GHNP-WDF815 '   TO CURRENT-IMS-SECTION                     
089828                                                                          
089829     MOVE SPACE                TO ALL-SSA                                 
089830                                                                          
089831     MOVE 'WDF815   '          TO SSA1                                    
089832     MOVE '  GE'               TO GODK-STATUSKODER                        
089833     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF815 SSA1                  
089834     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089835     PERFORM IMS-STATUSKONTROLL                                           
089836     .                                                                    
089837                                                                          
089838                                                                          
089839 IMS-GNP-WDF815-FKN SECTION.                                              
089840     MOVE 'IMS-GNP-WDF815F '   TO CURRENT-IMS-SECTION                     
089841                                                                          
089842     MOVE SPACE                TO ALL-SSA                                 
089843                                                                          
089844     STRING 'WDF815  (IDFKNGRF<=' W-IDFKNGRP-X                            
089845                    '&IDFKNGRT>=' W-IDFKNGRP-X ')'                        
089846           DELIMITED BY SIZE INTO SSA1                                    
089847     MOVE '  GE'               TO GODK-STATUSKODER                        
089848     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF815 SSA1                  
089849     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089850     PERFORM IMS-STATUSKONTROLL                                           
089851     .                                                                    
089852                                                                          
089853                                                                          
089854 IMS-GHNP-WDF815-KEY SECTION.                                             
089855     MOVE 'IMS-GHNP-WDF815K'   TO CURRENT-IMS-SECTION                     
089856                                                                          
089857     MOVE SPACE                TO ALL-SSA                                 
089858                                                                          
089859     STRING 'WDF815  (WDF815KY =' W-WDF815KY-X ')'                        
089860           DELIMITED BY SIZE INTO SSA1                                    
089861     MOVE '  GE'               TO GODK-STATUSKODER                        
089862     CALL CBLTDLI USING GHNP WDF8-PCB DLI-IO-WDF815 SSA1                  
089863     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089864     PERFORM IMS-STATUSKONTROLL                                           
089865     .                                                                    
089866                                                                          
089867                                                                          
089868 IMS-ISRT-WDF815 SECTION.                                                 
089869     MOVE 'IMS-ISRT-WDF815 '  TO CURRENT-IMS-SECTION                      
089870                                                                          
089871     MOVE SPACE               TO ALL-SSA                                  
089872                                                                          
089873     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
089874          DELIMITED BY SIZE INTO SSA1                                     
089875     MOVE 'WDF815   '         TO SSA2                                     
089876     MOVE '  '                TO GODK-STATUSKODER                         
089877     CALL CBLTDLI USING ISRT WDF8-PCB DLI-IO-WDF815 SSA1 SSA2             
089878     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
089879     PERFORM IMS-STATUSKONTROLL                                           
089880     .                                                                    
089881                                                                          
089882                                                                          
089883 IMS-REPL-WDF815 SECTION.                                                 
089884     MOVE 'IMS-REPL-WDF815 '   TO CURRENT-IMS-SECTION                     
089885                                                                          
089886     MOVE SPACE                TO ALL-SSA                                 
089887                                                                          
089888     MOVE '  '                 TO GODK-STATUSKODER                        
089889     CALL CBLTDLI USING REPL WDF8-PCB DLI-IO-WDF815                       
089890     MOVE WDF8-STATUS-CODE     TO STATUS-WS                               
089891     PERFORM IMS-STATUSKONTROLL                                           
089892     .                                                                    
089893                                                                          
089894                                                                          
089895 IMS-DLET-WDF815 SECTION.                                                 
089896     MOVE 'IMS-DLET-WDF815 '  TO CURRENT-IMS-SECTION                      
089897                                                                          
089898     MOVE SPACE               TO ALL-SSA                                  
089899                                                                          
089900     MOVE '  '                TO GODK-STATUSKODER                         
089901     CALL CBLTDLI USING DLET WDF8-PCB DLI-IO-WDF815                       
089902     MOVE WDF8-STATUS-CODE    TO STATUS-WS                                
089903     PERFORM IMS-STATUSKONTROLL                                           
089904     .                                                                    
089905                                                                          
089906                                                                          
089907 IMS-STATUSKONTROLL SECTION.                                              
089908                                                                          
089909     SET STATUS-IX TO 1                                                   
089910     SEARCH GODK-STATUS                                                   
090000       AT END                                                             
090100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
090200         DELIMITED BY SIZE INTO FELTEXT                                   
090300         CALL FELLOG                                                      
090400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
090500         CONTINUE                                                         
090600     END-SEARCH                                                           
090700     .                                                                    
