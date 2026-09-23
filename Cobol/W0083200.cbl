000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0083200.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   91/08/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        MPP-PROGRAM FÖR DATAREGISTRERING ANSKAFFNING.                    
001000*        PROGRAMMET UPPDATERAR WDG901 (WLZZAD)                            
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        FRÅN GRUNDBILD                                                   
001500*                                                                         
001600*              TRANSAKTION: W0T832                                        
001700*              MID:         W0I83201                                      
001800*                                                                         
001900*        FRÅN STANSBILDER                                                 
002000*                                                                         
002100*              TRANSAKTION: W0T83B                                        
002200*              MID:         W0IR0101                                      
002500*                           W0IR1701                                      
002600*                           W0IR2201                                      
002700*                           W0IR2301                                      
002800*                                                                         
002900*    UTDATA.                                                              
003000*        TILL GRUNDBILD                                                   
003100*                                                                         
003200*              MOD:         W0O83201                                      
003300*                                                                         
003400*                                                                         
003500*                                                                         
003600*        TILL STANSBILDER                                                 
003700*                                                                         
003800*              MOD:         W0OR0101                                      
004100*                           W0OR1701                                      
004200*                           W0OR2201                                      
004300*                           W0OR2301                                      
004400*                                                                         
004500*                                                                         
004510* 2017-10-18  E'TRACKER 10302687 LOCAL SOURCING USA.                      
004520*             REMOVE R02, R05 AND R85 IN PROGRAM W0083200.                
004530*             R02, R05 AND R85 IS NOT REMOVED IN TRATTEN.                 
      *                                                                         
      * 2021-08-06  STORY 2230520 / CHECK MULTIPLE ENTRIES FOR R01              
      *                             RECORDS IN EVENT DATABASE                   
004600                                                                          
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(08)   VALUE 'W0083200'.            
005500                                                                          
005600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005800                                                                          
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100                                                                          
006200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +160  COMP SYNC.        
006400                                                                          
006500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006600 77  RADIX                       PIC S9(2)   VALUE +0 COMP SYNC.          
006610 77  MAX-RADIX                   PIC S9(2)   VALUE +4 COMP SYNC.          
006700                                                                          
006800 77  WS-FELTYP                   PIC S9      VALUE ZERO.                  
006900                                                                          
007000 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO.                  
007100 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
007110 77  WS-IDLEVNR-SHIP             PIC X(5)    VALUE SPACE.                 
007200 77  WS-KDBEHX                   PIC X(1)    VALUE SPACE.                 
007300 77  WS-KVVECKOR-LT              PIC S9(3)   VALUE ZERO.                  
007400 77  WS-KVVECKOR-AT              PIC S9(3)   VALUE ZERO.                  
007500 77  WS-KVDAGAR-TTC1             PIC S9(3)   VALUE ZERO.                  
007600 77  WS-KVDAGAR-TTC2             PIC S9(3)   VALUE ZERO.                  
007700 77  WS-KDLEVTYP                 PIC S9(1)   VALUE ZERO.                  
007800 77  WS-KDGK                     PIC S9(1)   VALUE ZERO.                  
007900 77  WS-IDLPKOLL                 PIC S9(1)   VALUE ZERO.                  
008000 77  WS-FLRSADR                  PIC X(1)    VALUE SPACE.                 
008100 77  WS-FLEMBPOL                 PIC X(1)    VALUE SPACE.                 
008200 77  WS-KDSPRAK                  PIC S9(1)   VALUE ZERO.                  
008300 77  WS-TIBEST                   PIC S9(7)   VALUE ZERO.                  
008400 77  WS-KVBEST                   PIC S9(7)   VALUE ZERO.                  
008500 77  WS-KDBEH-BEST               PIC S9(1)   VALUE ZERO.                  
008600 77  WS-IDBEST-PACKAT            PIC S9(13)  VALUE ZERO COMP-3.           
008700 77  WS-TIAVTAL                  PIC S9(7)   VALUE ZERO.                  
008800 77  WS-KVAVTANT                 PIC S9(7)   VALUE ZERO.                  
008900 77  WS-IDAVTAL-PACKAT           PIC S9(13)  VALUE ZERO COMP-3.           
009000 77  WS-TIAAP-AVBOK              PIC S9(3)   VALUE ZERO.                  
009100 77  WS-KVOI                     PIC S9(7)   VALUE ZERO.                  
009200 77  WS-IDFTG                    PIC  9(2)   VALUE ZERO.                  
009300 01  WS-IDLKTO-OLD               PIC  9(7)   VALUE ZERO.                  
009400 01  FILLER   REDEFINES WS-IDLKTO-OLD.                                    
009500     03  WS-IDLKTO-OLD-POS2      PIC 9(2).                                
009600 01  WS-IDLKTO-NEW               PIC  9(7)   VALUE ZERO.                  
009700 01  FILLER   REDEFINES WS-IDLKTO-NEW.                                    
009800     03  WS-IDLKTO-NEW-POS2      PIC 9(2).                                
009900 01  WS-IDARTNR-X                PIC X(8).                                
010000 01  WS-IDARTNR-8      REDEFINES WS-IDARTNR-X                             
010100                                 PIC 9(8).                                
       01  WS-EVENT-FOUND              PIC X(1)    VALUE SPACE.                 
010200                                                                          
010300 77  PG-IX                       PIC S9(1)   VALUE ZERO.                  
010400                                                                          
010500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010600     88  INDATA-OK                           VALUE 'J'.                   
010700     88  INDATA-FEL                          VALUE 'N'.                   
010800                                                                          
010900 77  0832-VAL-SW                 PIC X       VALUE 'J'.                   
011000     88  0832-VAL-OK                         VALUE 'J'.                   
011100     88  0832-VAL-FEL                        VALUE 'N'.                   
011200                                                                          
011300 77  0832-VAL-HITTAT-SW          PIC X       VALUE 'N'.                   
011400     88  0832-VAL-HITTAT                     VALUE 'J'.                   
011500     88  0832-VAL-EJ-HITTAT                  VALUE 'N'.                   
011600                                                                          
011700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011800     88  EGEN-MID                            VALUE '0832'.                
011900     88  EGEN-STANS-MID                      VALUE '083B'.                
012000     88  GODK-MID                            VALUE '0831'                 
012100                                                   '0833' '0834'          
012200                                                   '0835' '0836'          
012300                                                   '0837' '0838'          
012400                                                   '0839'.                
012500     EJECT                                                                
013200* R22                                                                     
013300 01  WS-BESTALLNINGS-ID.                                                  
013400     03 WS-IDBEST-NUM             PIC 9(12) VALUE ZERO.                   
013500     03 WS-IDBEST REDEFINES WS-IDBEST-NUM.                                
013600        05 WS-IDBEST-1            PIC X(3).                               
013700        05 WS-IDBEST-2            PIC X(6).                               
013800        05 WS-IDBEST-3            PIC X(3).                               
013900                                                                          
014000* R23                                                                     
014100 01  WS-AVTALS-ID.                                                        
014200     03 WS-IDAVTAL-NUM            PIC 9(12) VALUE ZERO.                   
014300     03 WS-IDAVTAL REDEFINES WS-IDAVTAL-NUM.                              
014400        05 WS-IDAVTAL-1           PIC X(3).                               
014500        05 WS-IDAVTAL-2           PIC X(6).                               
014600        05 WS-IDAVTAL-3           PIC X(3).                               
014700                                                                          
014800 01  083B-MODNAMN.                                                        
014900     03 FILLER                  PIC X(3)    VALUE 'W0O'.                  
015000     03 WS-IDPTYP               PIC X(3).                                 
015100     03 FILLER                  PIC X(2)    VALUE '01' .                  
015200     EJECT                                                                
015300 01  BAS-R01-REGPOST.                                                     
015400                                                                          
015500     03      -COPY W213R01   -PRE BAS-R01-                                
015600     03 BAS-R01-REST            PIC X(73).                                
015700     EJECT                                                                
016800 01  BAS-R17-REGPOST.                                                     
016900*    03      -COPY W213R17T  -PRE BAS-R17-                                
017000     03 BAS-R17-REST            PIC X(67).                                
017100     EJECT                                                                
017200 01  BAS-R22-REGPOST.                                                     
017300*    03      -COPY W212R22   -PRE BAS-R22-                                
017400     03 BAS-R22-REST            PIC X(41).                                
017500     EJECT                                                                
017600 01  BAS-R23-REGPOST.                                                     
017700*    03      -COPY W212R23   -PRE BAS-R23-                                
017800     03 BAS-R23-REST            PIC X(41).                                
017900     EJECT                                                                
018000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
018100 01  GENERELLA-SUBPROGRAM.                                                
018200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
018300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018600     03  WKPSKONV                PIC X(8)    VALUE 'WKPSKONV'.            
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL SUBPROGRAM WKPSKONV                              
018900*01 -COPY WKPSAREA                                                        
019000     EJECT                                                                
019100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
019200*01 -COPY WMEDAREA                                                        
019300     EJECT                                                                
019400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
019500*01 -COPY WDATAREA                                                        
019600     SKIP3                                                                
019700 01  MESSAGE-CODES.                                                       
019800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
019900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
020100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
           03  INF-R01-DUPLICATE       PIC X(50)   VALUE                        
               'DUPLICATE ENTRY,CHECK 2131 FOR SUPPLIER INFO.'.                 
020300     EJECT                                                                
020400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020700     SKIP3                                                                
020800 01  MID-AREA                    PIC X(1920).                             
020900*01  MID-0832-AREA  -COPY  W0I83201.                                      
021000     EJECT                                                                
021100 01  MID-083B-STANSVAL-AREA.                                              
021200     03  MID-083B-STANSVAL-IDPTYP PIC X(3).                               
021300     03  FILLER                  PIC X(1917).                             
021400*01  MID-R01-AREA   -COPY W0IR0101.                                       
021500     EJECT                                                                
022000*01  MID-R17-AREA   -COPY W0IR1701.                                       
022100     EJECT                                                                
022200*01  MID-R22-AREA   -COPY W0IR2201.                                       
022300     EJECT                                                                
022400*01  MID-R23-AREA   -COPY W0IR2301.                                       
022500     EJECT                                                                
022600 01  FILLER                    PIC X(16)  VALUE 'MSG/MOD-AREA'.           
022700     SKIP3                                                                
022800*01  -COPY WMSGAREA                                                       
022900     EJECT                                                                
023000     03  W0O832-MOD REDEFINES MSG-AREA.                                   
023100*        05        -COPY W0O83201                                         
023200     EJECT                                                                
023300     03  W0OR01-MOD REDEFINES MSG-AREA.                                   
023400*        05        -COPY W0OR0101                                         
023500     EJECT                                                                
024200     03  W0OR17-MOD REDEFINES MSG-AREA.                                   
024300*        05        -COPY W0OR1701                                         
024400     EJECT                                                                
024500     03  W0OR22-MOD REDEFINES MSG-AREA.                                   
024600*        05        -COPY W0OR2201                                         
024700     EJECT                                                                
024800     03  W0OR23-MOD REDEFINES MSG-AREA.                                   
024900*        05        -COPY W0OR2301                                         
025000     EJECT                                                                
025100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025200     SKIP3                                                                
025300*01  -COPY WMFSAREA                                                       
025400     EJECT                                                                
025500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025600*                                                                         
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025900     SKIP3                                                                
026000 01  NYCKLAR-TILL-DLI.                                                    
026100                                                                          
026200     03  W-WDG901KY-X.                                                    
026300         05  W-TIREGDAT          PIC S9(07)   VALUE ZERO COMP-3.          
026400         05  W-TIKLOCK           PIC S9(09)   VALUE ZERO COMP-3.          
026500                                                                          
026600     SKIP2                                                                
026700*    --- STATUS-KOD FRÅN IMS                                              
026800 01  STATUS-WS                   PIC XX.                                  
026900     88  SEGMENT-FINNS                       VALUE '  '.                  
027000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027100     88  IMS-END-OF-DB                       VALUE 'GB'.                  
027200     SKIP2                                                                
027300 01  GODK-STATUSKODER.                                                    
027400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027500     SKIP3                                                                
027600 01  SSA1                        PIC X(64).                               
027700 01  SSA2                        PIC X(64).                               
027800     EJECT                                                                
027900*    --- IMS FUNKTIONSKODER                                               
028000*01  -COPY W0003                                                          
028100     EJECT                                                                
028200*    ---  DLI INPUT-OUTPUT AREA                                           
028300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
028400     SKIP3                                                                
028500 01  DLI-IO-AREA.                                                         
028600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
028700     SKIP3                                                                
028800     03  WLZZAD01 REDEFINES IO-AREA.                                      
028900*        05  -COPY WDG901                                                 
029000     EJECT                                                                
029100 LINKAGE SECTION.                                                         
029200                                                                          
029300*01  -COPY W0009   -PRE MSG-                                              
029400     EJECT                                                                
029500*01  -COPY W0008  -PRE ZZAD-                                              
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800 PROCEDURE DIVISION  USING MSG-PCB ZZAD-PCB.                              
029900     ENTRY 'DLITCBL' USING MSG-PCB ZZAD-PCB.                              
030000                                                                          
030100     PERFORM IMS-GET-MSG                                                  
030200     IF SEGMENT-FINNS                                                     
030300       PERFORM A-INIT                                                     
030400       IF MFS-IDTRANS = '0832'                                            
030500          IF MFS-SPLIT                                                    
030600             PERFORM B-VAL-AV-STANSBILD                                   
030700             IF 0832-VAL-OK                                               
030800                PERFORM C-LAGG-UT-STANSBILD                               
030900             ELSE                                                         
031000               PERFORM D-VISA-0832-FEL                                    
031100             END-IF                                                       
031200          ELSE                                                            
031300             IF MFS-FIRST                                                 
031400                PERFORM E-0832-FORSTA-SIDA                                
031500             ELSE                                                         
031600                IF MFS-NEXT                                               
031700                   PERFORM F-0832-NAESTA-SIDA                             
031800                ELSE                                                      
031900                   PERFORM G-0832-SAMMA-SIDA                              
032000                END-IF                                                    
032100             END-IF                                                       
032200          END-IF                                                          
032300       END-IF                                                             
032400       IF MFS-IDTRANS = '083B'                                            
032500          IF MFS-FIRST                                                    
032600             PERFORM H-TILLBAKA-TILL-VALBILD-0832                         
032700          ELSE                                                            
032800             PERFORM I-VILKEN-STANSBILD                                   
032900*                                                                         
033000*                   UPPDATERING, TOM STANSBILD                            
033100*                   FELBILD                                               
033200*                   NÄSTA SIDA                                            
033300*                                                                         
033400          END-IF                                                          
033500       END-IF                                                             
033600       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
033700       PERFORM IMS-INSERT-MSG                                             
033800     END-IF                                                               
033900                                                                          
034000     MOVE ZERO TO RETURN-CODE                                             
034100     GOBACK                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 A-INIT SECTION.                                                          
034500                                                                          
034600     IF MSG-DUBBLA-TRANSKODER                                             
034700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-AREA                     
034800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
034900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035000     ELSE                                                                 
035100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-AREA                      
035200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
035300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035400     END-IF                                                               
035500                                                                          
035600     IF MFS-IDTRANS = '0832'                                              
035700        MOVE MID-AREA TO MID-0832-AREA                                    
035800     END-IF                                                               
035900                                                                          
036000     IF MFS-IDTRANS = '083B'                                              
036100        MOVE MID-AREA TO MID-083B-STANSVAL-AREA                           
036200     END-IF                                                               
036300                                                                          
036400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
036500     MOVE MSG-IDPFK TO MFS-IDPFK                                          
036600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
036700                                                                          
036800     MOVE LOW-VALUE TO MSG-AREA                                           
036900                                                                          
037000     IF EGEN-STANS-MID                                                    
037100        IF MFS-SPLIT                                                      
037200           MOVE '7' TO MFS-IDPFK                                          
037300        END-IF                                                            
037400     END-IF                                                               
037500     IF NOT EGEN-MID AND NOT EGEN-STANS-MID                               
037600        MOVE SPACE TO MFS-KDTRTYP                                         
037700        MOVE '7' TO MFS-IDPFK                                             
037800        MOVE 'W0O83201' TO MFS-IDMOD                                      
037900        MOVE '0832'     TO MOD-0832-IDTRANS                               
038000     END-IF                                                               
038100     IF ENGLISH-TEXT                                                      
038200       MOVE +2 TO SPRAK-IX                                                
038300       MOVE 'GB ' TO MED-IDSKYLT                                          
038400     ELSE                                                                 
038500       MOVE +1 TO SPRAK-IX                                                
038600       MOVE 'S  ' TO MED-IDSKYLT                                          
038700     END-IF                                                               
038800     .                                                                    
038900     EJECT                                                                
039000                                                                          
039100                                                                          
039200 B-VAL-AV-STANSBILD SECTION.                                              
039300                                                                          
039400     MOVE +1               TO RADIX                                       
039500     PERFORM UNTIL RADIX > MAX-RADIX OR 0832-VAL-HITTAT                   
039600        IF MID-0832-KDSVAR (RADIX) =  'S'                                 
039700           MOVE JA              TO 0832-VAL-HITTAT-SW                     
039800           IF MID-0832-IDPTYP(RADIX) = 'R01' OR                           
040100              MID-0832-IDPTYP(RADIX) = 'R17' OR                           
040200              MID-0832-IDPTYP(RADIX) = 'R22' OR                           
040300              MID-0832-IDPTYP(RADIX) = 'R23'                              
040400                 MOVE MID-0832-IDPTYP (RADIX) TO WS-IDPTYP                
040500                 MOVE JA             TO 0832-VAL-SW                       
040600           ELSE                                                           
040700              MOVE JA           TO 0832-VAL-HITTAT-SW                     
040800              MOVE NEJ          TO 0832-VAL-SW                            
040900           END-IF                                                         
041000        ELSE                                                              
041100           MOVE NEJ             TO 0832-VAL-SW                            
041200        END-IF                                                            
041300        ADD +1                  TO RADIX                                  
041400     END-PERFORM                                                          
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800                                                                          
041900 C-LAGG-UT-STANSBILD SECTION.                                             
042000                                                                          
042100     IF WS-IDPTYP = 'R01'                                                 
042200        MOVE MFS-RENSA-FAELT    TO MOD-R01-IDARTNR                        
042300                                   MOD-R01-IDLEVNR                        
042310                                   MOD-R01-IDLEVNR-SHIP                   
042400        MOVE 083B-MODNAMN          TO MFS-IDMOD                           
042500        MOVE '0832'                TO MOD-R01-IDTRANS                     
042600     END-IF                                                               
042700                                                                          
044900     IF WS-IDPTYP = 'R17'                                                 
045000        MOVE MFS-RENSA-FAELT    TO MOD-R17-IDLEVNR                        
045100                                   MOD-R17-KDBEHX                         
045200                                   MOD-R17-KVVECKOR-LT                    
045300                                   MOD-R17-KVVECKOR-AT                    
045400                                   MOD-R17-KVDAGAR-TTC1                   
045500                                   MOD-R17-KVDAGAR-TTC2                   
045600                                   MOD-R17-KDLEVTYP                       
045700                                   MOD-R17-KDGK                           
045800                                   MOD-R17-IDLPKOLL                       
045900                                   MOD-R17-FLRSADR                        
046000                                   MOD-R17-FLEMBPOL                       
046100                                   MOD-R17-KDSPRAK                        
046200     END-IF                                                               
046300                                                                          
046400     MOVE 083B-MODNAMN    TO MFS-IDMOD                                    
046500     MOVE '0832'          TO MOD-R17-IDTRANS                              
046600                                                                          
046700     IF WS-IDPTYP = 'R22'                                                 
046800        MOVE MFS-RENSA-FAELT    TO MOD-R22-IDARTNR                        
046900                                   MOD-R22-IDBEST-1                       
047000                                   MOD-R22-IDBEST-2                       
047100                                   MOD-R22-IDBEST-3                       
047200                                   MOD-R22-IDLEVNR-BEST                   
047400                                   MOD-R22-TIBEST                         
047500                                   MOD-R22-KVBEST                         
047600                                   MOD-R22-KDBEH-BEST                     
047700        MOVE 083B-MODNAMN          TO MFS-IDMOD                           
047800        MOVE '0832'                TO MOD-R22-IDTRANS                     
047900     END-IF                                                               
048000                                                                          
048100     IF WS-IDPTYP = 'R23'                                                 
048200        MOVE MFS-RENSA-FAELT    TO MOD-R23-IDARTNR                        
048300                                   MOD-R23-IDAVTAL-1                      
048400                                   MOD-R23-IDAVTAL-2                      
048500                                   MOD-R23-IDAVTAL-3                      
048600                                   MOD-R23-IDLEVNR-AVT                    
048610                                   MOD-R23-IDLEVNR-SHIP                   
048700                                   MOD-R23-TIAVTAL                        
048800                                   MOD-R23-KVAVTANT                       
048900        MOVE 083B-MODNAMN          TO MFS-IDMOD                           
049000        MOVE '0832'                TO MOD-R23-IDTRANS                     
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400                                                                          
049500                                                                          
049600 D-VISA-0832-FEL SECTION.                                                 
049700     IF 0832-VAL-HITTAT                                                   
049800        MOVE 'STANSBILDEN UNDER ARBETE' TO MOD-0832-TEMFSINF              
049900        MOVE +1 TO RADIX                                                  
050000        PERFORM UNTIL RADIX > MAX-RADIX                                   
050100           MOVE MFS-ROER-EJ-FAELT   TO MOD-0832-KDSVAR(RADIX)             
050200           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
050300                             MOD-0832-KDSVAR-ATTR(RADIX)                  
050400           ADD +1                   TO RADIX                              
050500        END-PERFORM                                                       
050600     END-IF                                                               
050700     MOVE 'W0O83201'      TO MFS-IDMOD                                    
050800     MOVE '0832'          TO MOD-0832-IDTRANS                             
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200                                                                          
051300 E-0832-FORSTA-SIDA SECTION.                                              
051400                                                                          
051500     MOVE 'W0O83201'      TO MFS-IDMOD                                    
051600     MOVE '0832'          TO MOD-0832-IDTRANS                             
051700     .                                                                    
051800     EJECT                                                                
051900                                                                          
052000                                                                          
052100 F-0832-NAESTA-SIDA SECTION.                                              
052200                                                                          
052300     MOVE '106'                    TO MED-MFSMED                          
052400     CALL WMEDKONV USING MED-WMEDAREA                                     
052500     MOVE MED-MFSMED               TO MOD-0832-TEMFSINF                   
052600     MOVE 'W0O83201'      TO MFS-IDMOD                                    
052700     MOVE '0832'          TO MOD-0832-IDTRANS                             
052800     .                                                                    
052900     EJECT                                                                
053000                                                                          
053100                                                                          
053200 G-0832-SAMMA-SIDA SECTION.                                               
053300                                                                          
053400     MOVE 'W0O83201'      TO MFS-IDMOD                                    
053500     MOVE '0832'          TO MOD-0832-IDTRANS                             
053600     .                                                                    
053700     EJECT                                                                
053800                                                                          
053900                                                                          
054000 H-TILLBAKA-TILL-VALBILD-0832 SECTION.                                    
054100                                                                          
054200     MOVE +1              TO RADIX                                        
054300     PERFORM UNTIL RADIX > MAX-RADIX                                      
054400        MOVE MFS-RENSA-FAELT TO MOD-0832-KDSVAR (RADIX)                   
054500        ADD +1            TO RADIX                                        
054600     END-PERFORM                                                          
054700     MOVE 'W0O83201'      TO MFS-IDMOD                                    
054800     MOVE '0832'          TO MOD-0832-IDTRANS                             
054900     .                                                                    
055000     EJECT                                                                
055100                                                                          
055200 I-VILKEN-STANSBILD SECTION.                                              
055300*    VILKEN STANSBILD ?                                                   
055400                                                                          
055500     EVALUATE MID-083B-STANSVAL-IDPTYP                                    
055600        WHEN 'R01'                                                        
055700           PERFORM IA-R01-STYR                                            
056200        WHEN 'R17'                                                        
056300           PERFORM ID-R17-STYR                                            
056400        WHEN 'R22'                                                        
056500           PERFORM IE-R22-STYR                                            
056600        WHEN 'R23'                                                        
056700           PERFORM IF-R23-STYR                                            
056800     END-EVALUATE                                                         
056900     .                                                                    
057000     EJECT                                                                
057100                                                                          
057200 IA-R01-STYR SECTION.                                                     
057300                                                                          
057400     MOVE MID-083B-STANSVAL-AREA TO MID-R01-AREA                          
057500      IF MFS-UPDATE                                                       
057600         PERFORM IAA-R01-INDATAKONTROLL-1                                 
057700         IF INDATA-OK                                                     
057800            PERFORM IAB-R01-INDATAKONTROLL-2                              
057900            IF INDATA-OK                                                  
                     PERFORM IAH-R01-CHK-DUPLICATES                             
                  END-IF                                                        
057900            IF INDATA-OK                                                  
058000               PERFORM IAC-R01-SAMLA-UTDATA                               
058100               PERFORM IMS-ISRT-WDG901                                    
058200               PERFORM IAD-R01-TOM-BILD                                   
058300            ELSE                                                          
058400               PERFORM IAE-R01-FELBILD                                    
058500            END-IF                                                        
058600         ELSE                                                             
058700            PERFORM IAE-R01-FELBILD                                       
058800         END-IF                                                           
058900      ELSE                                                                
059000         IF MFS-NEXT                                                      
059100            PERFORM IAF-R01-NASTA-SIDA                                    
059200         ELSE                                                             
059300            IF MFS-ENTER                                                  
059400               PERFORM IAG-R01-ENTER                                      
059500            END-IF                                                        
059600         END-IF                                                           
059700      END-IF                                                              
059800      .                                                                   
059900      EJECT                                                               
060000                                                                          
060100  IAA-R01-INDATAKONTROLL-1 SECTION.                                       
060200                                                                          
060300     IF MID-R01-IDARTNR = ALL '+' AND                                     
060400      ( MID-R01-IDLEVNR = ALL '+' OR SPACE ) AND                          
060410      ( MID-R01-IDLEVNR-SHIP = ALL '+' OR SPACE )                         
060500        MOVE NEJ                   TO INDATA-SW                           
060600        MOVE MFS-RENSA-FAELT       TO MOD-R01-IDARTNR                     
060700                                      MOD-R01-IDLEVNR                     
060710                                      MOD-R01-IDLEVNR-SHIP                
060800        MOVE 1                     TO WS-FELTYP                           
060900     END-IF                                                               
061000     IF MID-R01-IDARTNR = ALL '+'                                         
061100        MOVE NEJ                   TO INDATA-SW                           
061200        MOVE MFS-RENSA-FAELT       TO MOD-R01-IDARTNR                     
061300        MOVE MFS-NUM-FAELT-FEL     TO MOD-R01-IDARTNR-ATTR                
061400        MOVE 2                     TO WS-FELTYP                           
061500     ELSE                                                                 
061600        IF MID-R01-IDARTNR NUMERIC                                        
061700           MOVE MFS-NUM-FAELT-RAETT  TO MOD-R01-IDARTNR-ATTR              
061800           MOVE MID-R01-IDARTNR      TO WS-IDARTNR                        
061900        ELSE                                                              
062000           MOVE NEJ                  TO INDATA-SW                         
062100           MOVE MFS-NUM-FAELT-FEL    TO MOD-R01-IDARTNR-ATTR              
062200           MOVE 2                    TO WS-FELTYP                         
062300        END-IF                                                            
062400        MOVE MFS-ROER-EJ-FAELT       TO MOD-R01-IDARTNR                   
062500     END-IF                                                               
062600                                                                          
062700     IF MID-R01-IDLEVNR = ALL '+' OR SPACE                                
062800        MOVE NEJ                   TO INDATA-SW                           
062900        MOVE MFS-RENSA-FAELT       TO MOD-R01-IDLEVNR                     
063000        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R01-IDLEVNR-ATTR                
063100        MOVE 2                     TO WS-FELTYP                           
063200     ELSE                                                                 
063300        IF MID-R01-IDLEVNR(1:1) NOT = ' ' AND '+'                         
063400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-R01-IDLEVNR-ATTR              
063500           MOVE MID-R01-IDLEVNR      TO WS-IDLEVNR                        
063600        ELSE                                                              
063700           MOVE NEJ TO INDATA-SW                                          
063800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-R01-IDLEVNR-ATTR              
063900           MOVE 2                    TO WS-FELTYP                         
064000        END-IF                                                            
064100        MOVE MFS-ROER-EJ-FAELT       TO MOD-R01-IDLEVNR                   
064200     END-IF                                                               
064201                                                                          
064210     IF MID-R01-IDLEVNR-SHIP = ALL '+' OR SPACE                           
064220        MOVE NEJ                   TO INDATA-SW                           
064230        MOVE MFS-RENSA-FAELT       TO MOD-R01-IDLEVNR-SHIP                
064240        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R01-IDLEVNR-SHIP-ATTR           
064250        MOVE 2                     TO WS-FELTYP                           
064260     ELSE                                                                 
064270        IF MID-R01-IDLEVNR-SHIP(1:1) NOT = ' ' AND '+'                    
064280           MOVE MFS-ALFA-FAELT-RAETT TO MOD-R01-IDLEVNR-SHIP-ATTR         
064290           MOVE MID-R01-IDLEVNR-SHIP      TO WS-IDLEVNR-SHIP              
064291        ELSE                                                              
064292           MOVE NEJ TO INDATA-SW                                          
064293           MOVE MFS-ALFA-FAELT-FEL   TO                                   
064294                                     MOD-R01-IDLEVNR-SHIP-ATTR            
064295           MOVE 2                    TO WS-FELTYP                         
064296        END-IF                                                            
064297        MOVE MFS-ROER-EJ-FAELT       TO MOD-R01-IDLEVNR-SHIP              
064298     END-IF                                                               
064300     .                                                                    
064400     EJECT                                                                
064500                                                                          
064600                                                                          
064700 IAB-R01-INDATAKONTROLL-2 SECTION.                                        
064800                                                                          
064900     IF WS-IDARTNR > 0                                                    
065000        CONTINUE                                                          
065100     ELSE                                                                 
065200        MOVE MFS-NUM-FAELT-FEL  TO MOD-R01-IDARTNR-ATTR                   
065300        MOVE NEJ                TO INDATA-SW                              
065400        MOVE 2                  TO WS-FELTYP                              
065500     END-IF                                                               
065600                                                                          
065700     IF WS-IDLEVNR(1:1) NOT = ' ' AND '+'                                 
065800        CONTINUE                                                          
065900     ELSE                                                                 
066000        MOVE MFS-ALFA-FAELT-FEL TO MOD-R01-IDLEVNR-ATTR                   
066100        MOVE NEJ                TO INDATA-SW                              
066200        MOVE 2                  TO WS-FELTYP                              
066300     END-IF                                                               
066310                                                                          
066320     IF WS-IDLEVNR-SHIP(1:1) NOT = ' ' AND '+'                            
066330        CONTINUE                                                          
066340     ELSE                                                                 
066350        MOVE MFS-ALFA-FAELT-FEL TO MOD-R01-IDLEVNR-SHIP-ATTR              
066360        MOVE NEJ                TO INDATA-SW                              
066370        MOVE 2                  TO WS-FELTYP                              
066380     END-IF                                                               
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700                                                                          
066800 IAC-R01-SAMLA-UTDATA SECTION.                                            
066900                                                                          
067000     MOVE 'R01'                TO BAS-R01-IDPTYP                          
067100     MOVE WS-IDARTNR           TO BAS-R01-IDARTNR                         
067200     MOVE WS-IDLEVNR           TO BAS-R01-IDLEVNR                         
067210     MOVE WS-IDLEVNR-SHIP      TO BAS-R01-IDLEVNR-SHIP                    
067300     MOVE SPACE                TO BAS-R01-IDSYSTEM                        
067400                                  BAS-R01-REST                            
067500                                                                          
067600                                                                          
067700     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
067800     MOVE MSG-TIME-OF-DAY      TO POST-TIKLOCK W-TIKLOCK                  
067900     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
068000     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
068100     MOVE ZERO                 TO POST-TIBORT                             
068200                                                                          
068300     MOVE BAS-R01-REGPOST      TO POST-REGPOST                            
068400                                                                          
068500     .                                                                    
068600     EJECT                                                                
068700                                                                          
068800                                                                          
068900 IAD-R01-TOM-BILD SECTION.                                                
069000                                                                          
069100     MOVE 'W0OR0101'               TO MFS-IDMOD                           
069200     MOVE '0832'                   TO MOD-R01-IDTRANS                     
069300     MOVE MFS-ADD-SAETT-CURSOR     TO MOD-R01-IDARTNR-ATTR                
069400     MOVE MFS-ROER-EJ-FAELT        TO MOD-R01-IDARTNR                     
069500                                      MOD-R01-IDLEVNR                     
069510                                      MOD-R01-IDLEVNR-SHIP                
069600     MOVE INF-UPDATE-DONE          TO MED-MFSMED                          
069700     CALL WMEDKONV USING MED-WMEDAREA                                     
069800     MOVE MED-MFSMED               TO MOD-R01-TEMFSINF                    
069900     .                                                                    
070000     EJECT                                                                
070100                                                                          
070200                                                                          
070300 IAE-R01-FELBILD SECTION.                                                 
070400                                                                          
070500     MOVE 'W0OR0101'            TO MFS-IDMOD                              
070600     MOVE '0832'                TO MOD-R01-IDTRANS                        
070700     IF WS-FELTYP = 1                                                     
070800        MOVE ERR-PF11-AND-NO-DATA  TO MED-MFSFEL                          
070900        CALL WMEDKONV USING MED-WMEDAREA                                  
071000        MOVE MED-MFSFEL            TO MOD-R01-TEMFSFEL                    
071100     ELSE                                                                 
071200        IF WS-FELTYP = 2                                                  
071300           MOVE ERR-CORR-HILITE-FLDS  TO MED-MFSFEL                       
071400           CALL WMEDKONV USING MED-WMEDAREA                               
071500           MOVE MED-MFSFEL            TO MOD-R01-TEMFSFEL                 
071600        END-IF                                                            
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000                                                                          
072100 IAF-R01-NASTA-SIDA SECTION.                                              
072200                                                                          
072300     MOVE '106'                 TO MED-MFSMED                             
072400     CALL WMEDKONV USING MED-WMEDAREA                                     
072500     MOVE MED-MFSMED               TO MOD-R01-TEMFSINF                    
072600     MOVE 'W0OR0101'            TO MFS-IDMOD                              
072700     MOVE '0832'                TO MOD-R01-IDTRANS                        
072800     .                                                                    
072900     EJECT                                                                
073000                                                                          
073100 IAG-R01-ENTER SECTION.                                                   
073200                                                                          
073300     MOVE MFS-ADD-SAETT-CURSOR  TO MOD-R01-IDLEVNR-ATTR                   
073400     MOVE MFS-ROER-EJ-FAELT     TO MOD-R01-IDARTNR                        
073500                                   MOD-R01-IDLEVNR                        
073510                                   MOD-R01-IDLEVNR-SHIP                   
073600     MOVE '003'                 TO MED-MFSFEL                             
073700     CALL WMEDKONV USING MED-WMEDAREA                                     
073800     MOVE MED-MFSFEL               TO MOD-R01-TEMFSFEL                    
073900     MOVE 'W0OR0101'            TO MFS-IDMOD                              
074000     MOVE '0832'                TO MOD-R01-IDTRANS                        
074100     .                                                                    
074200     EJECT                                                                
074300                                                                          
073100 IAH-R01-CHK-DUPLICATES SECTION.                                          
073200                                                                          
           PERFORM IMS-GN-WDG901                                                
           MOVE NEJ                  TO WS-EVENT-FOUND                          
           MOVE POST-REGPOST         TO BAS-R01-REGPOST                         
                                                                                
           PERFORM UNTIL IMS-END-OF-DB OR                                       
                         WS-EVENT-FOUND = JA                                    
              IF BAS-R01-IDPTYP  = 'R01' AND                                    
                 BAS-R01-IDARTNR = WS-IDARTNR                                   
                  MOVE JA            TO WS-EVENT-FOUND                          
              END-IF                                                            
              PERFORM IMS-GN-WDG901                                             
              MOVE POST-REGPOST      TO BAS-R01-REGPOST                         
           END-PERFORM                                                          
                                                                                
           IF WS-EVENT-FOUND = JA                                               
               MOVE NEJ               TO INDATA-SW                              
               MOVE INF-R01-DUPLICATE TO MOD-R01-TEMFSINF                       
           END-IF                                                               
074100     .                                                                    
074200     EJECT                                                                
074300                                                                          
097400                                                                          
124400***********************************************R17*********               
124500 ID-R17-STYR SECTION.                                                     
124600                                                                          
124700     MOVE MID-083B-STANSVAL-AREA TO MID-R17-AREA                          
124800      IF MFS-UPD-V                                                        
124900         PERFORM IDA-R17-FINNS-INDATA                                     
125000         IF INDATA-OK                                                     
125100            PERFORM IDB-R17-KOLLA-LEVNR-KDBEHX                            
125200            IF INDATA-OK                                                  
125300               IF MID-R17-KDBEHX = 'Ä'                                    
125400                  PERFORM IDBBA-R17-REPL-FINNS-ANDR                       
125500                  PERFORM IDBBB-R17-REPL-INDATAKOLL-1                     
125600                  IF INDATA-OK                                            
125700                     PERFORM IDBBC-R17-REPL-SAMLA-UTDATA                  
125800                  END-IF                                                  
125900               ELSE                                                       
126000                  IF MID-R17-KDBEHX = 'B'                                 
126100                     PERFORM IDBCA-R17-BORT-INDATAKOLL-1                  
126200                     IF INDATA-OK                                         
126300                        PERFORM IDBCB-R17-BORT-SAMLA-UTDATA               
126400                     END-IF                                               
126500                  END-IF                                                  
126600               END-IF                                                     
126700            END-IF                                                        
126800         END-IF                                                           
126900         IF INDATA-OK                                                     
127000            PERFORM IMS-ISRT-WDG901                                       
127100            PERFORM IDC-R17-TOM-BILD                                      
127200         ELSE                                                             
127300            PERFORM IDD-R17-FELBILD                                       
127400         END-IF                                                           
127500      ELSE                                                                
127600         IF MFS-NEXT                                                      
127700            PERFORM IDE-R17-NASTA-SIDA                                    
127800         ELSE                                                             
127900            IF MFS-ENTER                                                  
128000               PERFORM IDF-R17-ENTER                                      
128100            END-IF                                                        
128200         END-IF                                                           
128300      END-IF                                                              
128400      .                                                                   
128500      EJECT                                                               
128600                                                                          
128700 IDA-R17-FINNS-INDATA SECTION.                                            
128800                                                                          
128900     IF MID-R17-IDLEVNR            = ALL '+' AND                          
129000        MID-R17-KDBEHX             = ALL '+' AND                          
129100        MID-R17-KVVECKOR-LT        = ALL '+' AND                          
129200        MID-R17-KVVECKOR-AT        = ALL '+' AND                          
129300        MID-R17-KVDAGAR-TTC1       = ALL '+' AND                          
129400        MID-R17-KVDAGAR-TTC2       = ALL '+' AND                          
129500        MID-R17-KDLEVTYP           = ALL '+' AND                          
129600        MID-R17-KDGK               = ALL '+' AND                          
129700        MID-R17-IDLPKOLL           = ALL '+' AND                          
129800        MID-R17-FLRSADR            = ALL '+' AND                          
129900        MID-R17-FLEMBPOL           = ALL '+' AND                          
130000        MID-R17-KDSPRAK            = ALL '+'                              
130100        MOVE NEJ                   TO INDATA-SW                           
130200        MOVE MFS-RENSA-FAELT       TO MOD-R17-IDLEVNR                     
130300                                      MOD-R17-KDBEHX                      
130400                                      MOD-R17-KVVECKOR-LT                 
130500                                      MOD-R17-KVVECKOR-AT                 
130600                                      MOD-R17-KVDAGAR-TTC1                
130700                                      MOD-R17-KVDAGAR-TTC2                
130800                                      MOD-R17-KDLEVTYP                    
130900                                      MOD-R17-KDGK                        
131000                                      MOD-R17-IDLPKOLL                    
131100                                      MOD-R17-FLRSADR                     
131200                                      MOD-R17-FLEMBPOL                    
131300                                      MOD-R17-KDSPRAK                     
131400        MOVE 1                     TO WS-FELTYP                           
131500     END-IF                                                               
131600     .                                                                    
131700     EJECT                                                                
131800                                                                          
131900                                                                          
132000 IDB-R17-KOLLA-LEVNR-KDBEHX SECTION.                                      
132100                                                                          
132200*IDLEVNR FORMELL                                                          
132300     IF MID-R17-IDLEVNR = ALL '+'                                         
132400        MOVE NEJ                   TO INDATA-SW                           
132500        MOVE MFS-RENSA-FAELT       TO MOD-R17-IDLEVNR                     
132600        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R17-IDLEVNR-ATTR                
132700        MOVE 2                     TO WS-FELTYP                           
132800     ELSE                                                                 
132900        IF MID-R17-IDLEVNR (1:1) NOT = ' ' AND '+'                        
133000           MOVE MID-R17-IDLEVNR      TO WS-IDLEVNR                        
133100           IF WS-IDLEVNR NOT = SPACE                                      
133200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-R17-IDLEVNR-ATTR           
133300           ELSE                                                           
133400              MOVE NEJ                  TO INDATA-SW                      
133500              MOVE MFS-ALFA-FAELT-FEL   TO MOD-R17-IDLEVNR-ATTR           
133600              MOVE 2                    TO WS-FELTYP                      
133700           END-IF                                                         
133800        ELSE                                                              
133900           MOVE MFS-ALFA-FAELT-FEL TO MOD-R17-IDLEVNR-ATTR                
134000           MOVE MFS-ROER-EJ-FAELT  TO MOD-R17-IDLEVNR                     
134100           MOVE NEJ                TO INDATA-SW                           
134200           MOVE 2                  TO WS-FELTYP                           
134300        END-IF                                                            
134400        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-IDLEVNR                   
134500     END-IF                                                               
134600*KDBEHX FORMELL/INNEHÅLL                                                  
134700     IF MID-R17-KDBEHX = ALL '+'                                          
134800        MOVE NEJ                   TO INDATA-SW                           
134900        MOVE MFS-RENSA-FAELT       TO MOD-R17-KDBEHX                      
135000        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R17-KDBEHX-ATTR                 
135100        MOVE 2                     TO WS-FELTYP                           
135200     ELSE                                                                 
135300        IF MID-R17-KDBEHX = 'Ä' OR 'R' OR                                 
135400           MID-R17-KDBEHX = 'B'                                           
135500           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-R17-KDBEHX-ATTR              
135600           MOVE MID-R17-KDBEHX        TO WS-KDBEHX                        
135700        ELSE                                                              
135800           MOVE NEJ                   TO INDATA-SW                        
135900           MOVE MFS-ALFA-FAELT-FEL    TO MOD-R17-KDBEHX-ATTR              
136000           MOVE 2                     TO WS-FELTYP                        
136100        END-IF                                                            
136200        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KDBEHX                    
136300     END-IF                                                               
136400     IF INDATA-OK                                                         
136500        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KVVECKOR-LT                 
136600                                      MOD-R17-KVVECKOR-AT                 
136700                                      MOD-R17-KVDAGAR-TTC1                
136800                                      MOD-R17-KVDAGAR-TTC2                
136900                                      MOD-R17-KDLEVTYP                    
137000                                      MOD-R17-KDGK                        
137100                                      MOD-R17-IDLPKOLL                    
137200                                      MOD-R17-FLRSADR                     
137300                                      MOD-R17-FLEMBPOL                    
137400                                      MOD-R17-KDSPRAK                     
137500     END-IF                                                               
137600     .                                                                    
137700     EJECT                                                                
137800                                                                          
137900                                                                          
138000* REPLACE                                                                 
138100                                                                          
138200  IDBBA-R17-REPL-FINNS-ANDR SECTION.                                      
138300     IF MID-R17-KVVECKOR-LT        = ALL '+' AND                          
138400        MID-R17-KVVECKOR-AT        = ALL '+' AND                          
138500        MID-R17-KVDAGAR-TTC1       = ALL '+' AND                          
138600        MID-R17-KVDAGAR-TTC2       = ALL '+' AND                          
138700        MID-R17-KDLEVTYP           = ALL '+' AND                          
138800        MID-R17-KDGK               = ALL '+' AND                          
138900        MID-R17-IDLPKOLL           = ALL '+' AND                          
139000        MID-R17-FLRSADR            = ALL '+' AND                          
139100        MID-R17-FLEMBPOL           = ALL '+' AND                          
139200        MID-R17-KDSPRAK            = ALL '+'                              
139300        MOVE NEJ                   TO INDATA-SW                           
139400        MOVE MFS-RENSA-FAELT       TO MOD-R17-KVVECKOR-LT                 
139500                                      MOD-R17-KVVECKOR-AT                 
139600                                      MOD-R17-KVDAGAR-TTC1                
139700                                      MOD-R17-KVDAGAR-TTC2                
139800                                      MOD-R17-KDLEVTYP                    
139900                                      MOD-R17-KDGK                        
140000                                      MOD-R17-IDLPKOLL                    
140100                                      MOD-R17-FLRSADR                     
140200                                      MOD-R17-FLEMBPOL                    
140300                                      MOD-R17-KDSPRAK                     
140400        MOVE 1                     TO WS-FELTYP                           
140500     END-IF                                                               
140600     .                                                                    
140700     EJECT                                                                
140800                                                                          
140900  IDBBB-R17-REPL-INDATAKOLL-1 SECTION.                                    
141000                                                                          
141100*KVVECKOR-LT                                                              
141200     IF MID-R17-KVVECKOR-LT = ALL '+'                                     
141300        MOVE MFS-RENSA-FAELT       TO MOD-R17-KVVECKOR-LT                 
141400        MOVE MFS-NUM-FAELT-RAETT   TO MOD-R17-KVVECKOR-LT-ATTR            
141500     ELSE                                                                 
141600        IF MID-R17-KVVECKOR-LT NUMERIC                                    
141700           MOVE MID-R17-KVVECKOR-LT TO WS-KVVECKOR-LT                     
141800           IF WS-KVVECKOR-LT > 0                                          
141900              MOVE MFS-NUM-FAELT-RAETT                                    
142000                             TO MOD-R17-KVVECKOR-LT-ATTR                  
142100           ELSE                                                           
142200              MOVE MFS-NUM-FAELT-FEL TO                                   
142300                                 MOD-R17-KVVECKOR-LT-ATTR                 
142400              MOVE NEJ                TO INDATA-SW                        
142500              MOVE 2                  TO WS-FELTYP                        
142600           END-IF                                                         
142700        ELSE                                                              
142800           MOVE NEJ                  TO INDATA-SW                         
142900           MOVE MFS-NUM-FAELT-FEL                                         
143000                          TO MOD-R17-KVVECKOR-LT-ATTR                     
143100           MOVE 2                    TO WS-FELTYP                         
143200        END-IF                                                            
143300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KVVECKOR-LT               
143400     END-IF                                                               
143500                                                                          
143600                                                                          
143700*KVVECKOR-AT                                                              
143800     IF MID-R17-KVVECKOR-AT = ALL '+'                                     
143900        MOVE MFS-RENSA-FAELT       TO MOD-R17-KVVECKOR-AT                 
144000        MOVE MFS-NUM-FAELT-RAETT   TO MOD-R17-KVVECKOR-AT-ATTR            
144100     ELSE                                                                 
144200        IF MID-R17-KVVECKOR-AT NUMERIC                                    
144300           MOVE MID-R17-KVVECKOR-AT                                       
144400                          TO WS-KVVECKOR-AT                               
144500           IF WS-KVVECKOR-AT > 0                                          
144600              MOVE MFS-NUM-FAELT-RAETT                                    
144700                             TO MOD-R17-KVVECKOR-AT-ATTR                  
144800           ELSE                                                           
144900              MOVE MFS-NUM-FAELT-FEL  TO                                  
145000                               MOD-R17-KVVECKOR-AT-ATTR                   
145100              MOVE NEJ                TO INDATA-SW                        
145200              MOVE 2                  TO WS-FELTYP                        
145300           END-IF                                                         
145400        ELSE                                                              
145500           MOVE NEJ                  TO INDATA-SW                         
145600           MOVE MFS-NUM-FAELT-FEL                                         
145700                          TO MOD-R17-KVVECKOR-AT-ATTR                     
145800           MOVE 2                    TO WS-FELTYP                         
145900        END-IF                                                            
146000        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KVVECKOR-AT               
146100     END-IF                                                               
146200                                                                          
146300*KVDAGAR-TTC1                                                             
146400     IF MID-R17-KVDAGAR-TTC1 = ALL '+'                                    
146500        MOVE MFS-RENSA-FAELT       TO MOD-R17-KVDAGAR-TTC1                
146600        MOVE MFS-NUM-FAELT-RAETT                                          
146700                          TO MOD-R17-KVDAGAR-TTC1-ATTR                    
146800     ELSE                                                                 
146900        IF MID-R17-KVDAGAR-TTC1 NUMERIC                                   
147000           MOVE MID-R17-KVDAGAR-TTC1 TO WS-KVDAGAR-TTC1                   
147100           IF WS-KVDAGAR-TTC1 > 0 OR WS-KVDAGAR-TTC1 = 0                  
147200              MOVE MFS-NUM-FAELT-RAETT                                    
147300                             TO MOD-R17-KVDAGAR-TTC1-ATTR                 
147400              MOVE MID-R17-KVDAGAR-TTC1 TO WS-KVDAGAR-TTC1                
147500           ELSE                                                           
147600              MOVE MFS-NUM-FAELT-FEL  TO                                  
147700                                 MOD-R17-KVDAGAR-TTC1-ATTR                
147800              MOVE NEJ                TO INDATA-SW                        
147900              MOVE 2                  TO WS-FELTYP                        
148000           END-IF                                                         
148100        ELSE                                                              
148200           MOVE NEJ                  TO INDATA-SW                         
148300           MOVE MFS-NUM-FAELT-FEL                                         
148400                          TO MOD-R17-KVDAGAR-TTC1-ATTR                    
148500           MOVE 2                    TO WS-FELTYP                         
148600        END-IF                                                            
148700        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KVDAGAR-TTC1              
148800     END-IF                                                               
148900                                                                          
149000*KVDAGAR-TTC2                                                             
149100     IF MID-R17-KVDAGAR-TTC2 = ALL '+'                                    
149200        MOVE MFS-RENSA-FAELT       TO MOD-R17-KVDAGAR-TTC2                
149300        MOVE MFS-NUM-FAELT-RAETT                                          
149400                          TO MOD-R17-KVDAGAR-TTC2-ATTR                    
149500     ELSE                                                                 
149600        IF MID-R17-KVDAGAR-TTC2 NUMERIC                                   
149700           MOVE MID-R17-KVDAGAR-TTC2                                      
149800                          TO WS-KVDAGAR-TTC2                              
149900           IF WS-KVDAGAR-TTC2 > 0 OR WS-KVDAGAR-TTC2 = 0                  
150000              MOVE MFS-NUM-FAELT-RAETT                                    
150100                             TO MOD-R17-KVDAGAR-TTC2-ATTR                 
150200              MOVE MID-R17-KVDAGAR-TTC2                                   
150300                             TO WS-KVDAGAR-TTC2                           
150400           ELSE                                                           
150500              MOVE MFS-NUM-FAELT-FEL  TO                                  
150600                             MOD-R17-KVDAGAR-TTC2-ATTR                    
150700              MOVE NEJ                TO INDATA-SW                        
150800              MOVE 2                  TO WS-FELTYP                        
150900           END-IF                                                         
151000        ELSE                                                              
151100           MOVE NEJ                  TO INDATA-SW                         
151200           MOVE MFS-NUM-FAELT-FEL                                         
151300                          TO MOD-R17-KVDAGAR-TTC2-ATTR                    
151400           MOVE 2                    TO WS-FELTYP                         
151500        END-IF                                                            
151600        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KVDAGAR-TTC2              
151700     END-IF                                                               
151800                                                                          
151900*KDLEVTYP                                                                 
152000     IF MID-R17-KDLEVTYP = ALL '+'                                        
152100        MOVE MFS-RENSA-FAELT       TO MOD-R17-KDLEVTYP                    
152200        MOVE MFS-NUM-FAELT-RAETT                                          
152300                                   TO MOD-R17-KDLEVTYP-ATTR               
152400     ELSE                                                                 
152500        IF MID-R17-KDLEVTYP NUMERIC                                       
152600           MOVE MID-R17-KDLEVTYP   TO WS-KDLEVTYP                         
152700           IF WS-KDLEVTYP = 0 OR WS-KDLEVTYP = 3                          
152800              MOVE MFS-NUM-FAELT-RAETT                                    
152900                             TO MOD-R17-KDLEVTYP-ATTR                     
153000              MOVE MID-R17-KDLEVTYP                                       
153100                         TO WS-KDLEVTYP                                   
153200           ELSE                                                           
153300              MOVE MFS-NUM-FAELT-FEL  TO MOD-R17-KDLEVTYP-ATTR            
153400              MOVE NEJ                TO INDATA-SW                        
153500              MOVE 2                  TO WS-FELTYP                        
153600           END-IF                                                         
153700        ELSE                                                              
153800           MOVE NEJ                  TO INDATA-SW                         
153900           MOVE MFS-NUM-FAELT-FEL                                         
154000                          TO MOD-R17-KDLEVTYP-ATTR                        
154100           MOVE 2                    TO WS-FELTYP                         
154200        END-IF                                                            
154300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KDLEVTYP                  
154400     END-IF                                                               
154500                                                                          
154600*KDGK                                                                     
154700     IF MID-R17-KDGK = ALL '+'                                            
154800        MOVE MFS-RENSA-FAELT       TO MOD-R17-KDGK                        
154900        MOVE MFS-NUM-FAELT-RAETT                                          
155000                          TO MOD-R17-KDGK-ATTR                            
155100     ELSE                                                                 
155200        IF MID-R17-KDGK NUMERIC                                           
155300           MOVE MID-R17-KDGK   TO WS-KDGK                                 
155400           IF WS-KDGK > 0 AND WS-KDGK < 2                                 
155500              MOVE MFS-NUM-FAELT-RAETT                                    
155600                             TO MOD-R17-KDGK-ATTR                         
155700           ELSE                                                           
155800              MOVE MFS-NUM-FAELT-FEL  TO MOD-R17-KDGK-ATTR                
155900              MOVE NEJ                TO INDATA-SW                        
156000              MOVE 2                  TO WS-FELTYP                        
156100           END-IF                                                         
156200                                                                          
156300        ELSE                                                              
156400           MOVE NEJ                  TO INDATA-SW                         
156500           MOVE MFS-NUM-FAELT-FEL                                         
156600                          TO MOD-R17-KDGK-ATTR                            
156700           MOVE 2                    TO WS-FELTYP                         
156800        END-IF                                                            
156900        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KDGK                      
157000     END-IF                                                               
157100*IDLPKOLL                                                                 
157200     IF MID-R17-IDLPKOLL = ALL '+'                                        
157300        MOVE MFS-RENSA-FAELT       TO MOD-R17-IDLPKOLL                    
157400        MOVE MFS-NUM-FAELT-RAETT                                          
157500                          TO MOD-R17-IDLPKOLL-ATTR                        
157600     ELSE                                                                 
157700        IF MID-R17-IDLPKOLL NUMERIC                                       
157800           MOVE MID-R17-IDLPKOLL TO WS-IDLPKOLL                           
157900           IF WS-IDLPKOLL > -1 AND WS-IDLPKOLL < 5                        
158000              MOVE MFS-NUM-FAELT-RAETT                                    
158100                             TO MOD-R17-IDLPKOLL-ATTR                     
158200           ELSE                                                           
158300              MOVE MFS-NUM-FAELT-FEL  TO MOD-R17-IDLPKOLL-ATTR            
158400              MOVE NEJ                TO INDATA-SW                        
158500              MOVE 2                  TO WS-FELTYP                        
158600           END-IF                                                         
158700        ELSE                                                              
158800           MOVE NEJ                  TO INDATA-SW                         
158900           MOVE MFS-NUM-FAELT-FEL                                         
159000                          TO MOD-R17-IDLPKOLL-ATTR                        
159100           MOVE 2                    TO WS-FELTYP                         
159200        END-IF                                                            
159300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-IDLPKOLL                  
159400     END-IF                                                               
159500*FLRSADR                                                                  
159600     IF MID-R17-FLRSADR = ALL '+'                                         
159700        MOVE MFS-RENSA-FAELT       TO MOD-R17-FLRSADR                     
159800        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-R17-FLRSADR-ATTR                
159900     ELSE                                                                 
160000        IF MID-R17-FLRSADR = 'J' OR                                       
160100           MID-R17-FLRSADR = 'N'                                          
160200           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-R17-FLRSADR-ATTR             
160300           MOVE MID-R17-FLRSADR       TO WS-FLRSADR                       
160400        ELSE                                                              
160500           MOVE NEJ                   TO INDATA-SW                        
160600           MOVE MFS-ALFA-FAELT-FEL    TO MOD-R17-FLRSADR-ATTR             
160700           MOVE 2                     TO WS-FELTYP                        
160800        END-IF                                                            
160900        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-FLRSADR                   
161000     END-IF                                                               
161100                                                                          
161200                                                                          
161300*FLEMBPOL                                                                 
161400     IF MID-R17-FLEMBPOL = ALL '+'                                        
161500        MOVE MFS-RENSA-FAELT       TO MOD-R17-FLEMBPOL                    
161600        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-R17-FLEMBPOL-ATTR               
161700     ELSE                                                                 
161800        IF MID-R17-FLEMBPOL = 'J' OR                                      
161900           MID-R17-FLEMBPOL = 'Y' OR                                      
162000           MID-R17-FLEMBPOL = 'N'                                         
162100           MOVE MFS-ALFA-FAELT-RAETT                                      
162200                       TO MOD-R17-FLEMBPOL-ATTR                           
162300           MOVE MID-R17-FLEMBPOL      TO WS-FLEMBPOL                      
162400        ELSE                                                              
162500           MOVE NEJ                   TO INDATA-SW                        
162600           MOVE MFS-ALFA-FAELT-FEL                                        
162700                          TO MOD-R17-FLEMBPOL-ATTR                        
162800           MOVE 2                     TO WS-FELTYP                        
162900        END-IF                                                            
163000        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-FLEMBPOL                  
163100     END-IF                                                               
163200                                                                          
163300                                                                          
163400*KDSPRAK                                                                  
163500     IF MID-R17-KDSPRAK = ALL '+'                                         
163600        MOVE MFS-RENSA-FAELT       TO MOD-R17-KDSPRAK                     
163700        MOVE MFS-NUM-FAELT-RAETT                                          
163800                          TO MOD-R17-KDSPRAK-ATTR                         
163900     ELSE                                                                 
164000        IF MID-R17-KDSPRAK NUMERIC                                        
164100           MOVE MID-R17-KDSPRAK     TO WS-KDSPRAK                         
164200           IF WS-KDSPRAK > -1 AND WS-KDSPRAK < 5                          
164300              MOVE MFS-NUM-FAELT-RAETT                                    
164400                             TO MOD-R17-KDSPRAK-ATTR                      
164500           ELSE                                                           
164600              MOVE MFS-NUM-FAELT-FEL  TO MOD-R17-KDSPRAK-ATTR             
164700              MOVE NEJ                TO INDATA-SW                        
164800              MOVE 2                  TO WS-FELTYP                        
164900           END-IF                                                         
165000        ELSE                                                              
165100           MOVE NEJ                  TO INDATA-SW                         
165200           MOVE MFS-NUM-FAELT-FEL                                         
165300                          TO MOD-R17-KDSPRAK-ATTR                         
165400           MOVE 2                    TO WS-FELTYP                         
165500        END-IF                                                            
165600        MOVE MFS-ROER-EJ-FAELT       TO MOD-R17-KDSPRAK                   
165700     END-IF                                                               
165800     .                                                                    
165900     EJECT                                                                
166000                                                                          
166100                                                                          
166200                                                                          
166300                                                                          
166400                                                                          
166500 IDBBC-R17-REPL-SAMLA-UTDATA SECTION.                                     
166600                                                                          
166700     MOVE 'R17'                TO BAS-R17-IDPTYP                          
166800     MOVE WS-IDLEVNR           TO BAS-R17-IDLEVNR                         
166900     MOVE WS-KDBEHX            TO BAS-R17-KDBEHX                          
167000     MOVE SPACE                TO BAS-R17-DATA-LEVNR-ARTNR                
167100                                  BAS-R17-DATA-ENDAST-LEVNR               
167200     IF MID-R17-KVVECKOR-LT NOT = ALL '+'                                 
167300        MOVE WS-KVVECKOR-LT    TO BAS-R17-KVVECKOR-LT                     
167400     END-IF                                                               
167500     IF MID-R17-KVVECKOR-AT NOT = ALL '+'                                 
167600        MOVE WS-KVVECKOR-AT       TO BAS-R17-KVVECKOR-AT                  
167700     END-IF                                                               
167800     IF MID-R17-KVDAGAR-TTC1 NOT = ALL '+'                                
167900        MOVE WS-KVDAGAR-TTC1      TO BAS-R17-KVDAGAR-TTC1                 
168000     END-IF                                                               
168100     IF MID-R17-KVDAGAR-TTC2 NOT = ALL '+'                                
168200        MOVE WS-KVDAGAR-TTC2      TO BAS-R17-KVDAGAR-TTC2                 
168300     END-IF                                                               
168400     IF MID-R17-KDLEVTYP NOT = ALL '+'                                    
168500        MOVE WS-KDLEVTYP          TO BAS-R17-KDLEVTYP                     
168600     END-IF                                                               
168700     IF MID-R17-KDGK NOT = ALL '+'                                        
168800        MOVE WS-KDGK              TO BAS-R17-KDGK                         
168900     END-IF                                                               
169000     IF MID-R17-IDLPKOLL NOT = ALL '+'                                    
169100        MOVE WS-IDLPKOLL          TO BAS-R17-IDLPKOLL                     
169200     END-IF                                                               
169300     IF MID-R17-FLRSADR NOT = ALL '+'                                     
169400        MOVE WS-FLRSADR           TO BAS-R17-FLRSADR                      
169500     END-IF                                                               
169600     IF MID-R17-FLEMBPOL NOT = ALL '+'                                    
169700        MOVE WS-FLEMBPOL          TO BAS-R17-FLEMBPOL                     
169800     END-IF                                                               
169900     IF MID-R17-KDSPRAK NOT = ALL '+'                                     
170000        MOVE WS-KDSPRAK           TO BAS-R17-KDSPRAK                      
170100     END-IF                                                               
170200     MOVE SPACE                TO BAS-R17-REST                            
170300                                                                          
170400                                                                          
170500     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
170600     MOVE MSG-TIME-OF-DAY      TO POST-TIKLOCK W-TIKLOCK                  
170700     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
170800     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
170900     MOVE ZERO                 TO POST-TIBORT                             
171000                                                                          
171100     MOVE BAS-R17-REGPOST      TO POST-REGPOST                            
171200                                                                          
171300     .                                                                    
171400     EJECT                                                                
171500                                                                          
171600                                                                          
171700 IDBCA-R17-BORT-INDATAKOLL-1  SECTION.                                    
171800                                                                          
171900*KVVECKOR-LT                                                              
172000     IF MID-R17-KVVECKOR-LT NOT = ALL '+'                                 
172100        MOVE NEJ                   TO INDATA-SW                           
172200        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KVVECKOR-LT                 
172300        MOVE MFS-NUM-FAELT-FEL     TO MOD-R17-KVVECKOR-LT-ATTR            
172400        MOVE 2                     TO WS-FELTYP                           
172500     END-IF                                                               
172600                                                                          
172700                                                                          
172800*KVVECKOR-AT                                                              
172900     IF MID-R17-KVVECKOR-AT NOT = ALL '+'                                 
173000        MOVE NEJ                   TO INDATA-SW                           
173100        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KVVECKOR-AT                 
173200        MOVE MFS-NUM-FAELT-FEL     TO MOD-R17-KVVECKOR-AT-ATTR            
173300        MOVE 2                     TO WS-FELTYP                           
173400     END-IF                                                               
173500                                                                          
173600*KVDAGAR-TTC1                                                             
173700     IF MID-R17-KVDAGAR-TTC1 NOT = ALL '+'                                
173800        MOVE NEJ                   TO INDATA-SW                           
173900        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KVDAGAR-TTC1                
174000        MOVE MFS-NUM-FAELT-FEL                                            
174100                          TO MOD-R17-KVDAGAR-TTC1-ATTR                    
174200        MOVE 2                     TO WS-FELTYP                           
174300     END-IF                                                               
174400                                                                          
174500*KVDAGAR-TTC2                                                             
174600     IF MID-R17-KVDAGAR-TTC2 NOT = ALL '+'                                
174700        MOVE NEJ                   TO INDATA-SW                           
174800        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KVDAGAR-TTC2                
174900        MOVE MFS-NUM-FAELT-FEL                                            
175000                          TO MOD-R17-KVDAGAR-TTC2-ATTR                    
175100        MOVE 2                     TO WS-FELTYP                           
175200     END-IF                                                               
175300                                                                          
175400*KDLEVTYP                                                                 
175500     IF MID-R17-KDLEVTYP NOT = ALL '+'                                    
175600        MOVE NEJ                   TO INDATA-SW                           
175700        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KDLEVTYP                    
175800        MOVE MFS-NUM-FAELT-FEL                                            
175900                          TO MOD-R17-KDLEVTYP-ATTR                        
176000        MOVE 2                     TO WS-FELTYP                           
176100     END-IF                                                               
176200                                                                          
176300*KDGK                                                                     
176400     IF MID-R17-KDGK NOT = ALL '+'                                        
176500        MOVE NEJ                   TO INDATA-SW                           
176600        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KDGK                        
176700        MOVE MFS-NUM-FAELT-FEL                                            
176800                          TO MOD-R17-KDGK-ATTR                            
176900        MOVE 2                     TO WS-FELTYP                           
177000     END-IF                                                               
177100                                                                          
177200*IDLPKOLL                                                                 
177300     IF MID-R17-IDLPKOLL NOT = ALL '+'                                    
177400        MOVE NEJ                   TO INDATA-SW                           
177500        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-IDLPKOLL                    
177600        MOVE MFS-NUM-FAELT-FEL                                            
177700                          TO MOD-R17-IDLPKOLL-ATTR                        
177800        MOVE 2                     TO WS-FELTYP                           
177900     END-IF                                                               
178000                                                                          
178100*FLRSADR                                                                  
178200     IF MID-R17-FLRSADR NOT = ALL '+'                                     
178300        MOVE NEJ                   TO INDATA-SW                           
178400        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-FLRSADR                     
178500        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R17-FLRSADR-ATTR                
178600        MOVE 2                     TO WS-FELTYP                           
178700     END-IF                                                               
178800                                                                          
178900*FLEMBPOL                                                                 
179000     IF MID-R17-FLEMBPOL NOT = ALL '+'                                    
179100        MOVE NEJ                   TO INDATA-SW                           
179200        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-FLEMBPOL                    
179300        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R17-FLEMBPOL-ATTR               
179400        MOVE 2                     TO WS-FELTYP                           
179500     END-IF                                                               
179600                                                                          
179700*KDSPRAK                                                                  
179800     IF MID-R17-KDSPRAK NOT = ALL '+'                                     
179900        MOVE NEJ                   TO INDATA-SW                           
180000        MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-KDSPRAK                     
180100        MOVE MFS-NUM-FAELT-FEL                                            
180200                          TO MOD-R17-KDSPRAK-ATTR                         
180300        MOVE 2                     TO WS-FELTYP                           
180400     END-IF                                                               
180500     .                                                                    
180600     EJECT                                                                
180700                                                                          
180800                                                                          
180900 IDBCB-R17-BORT-SAMLA-UTDATA SECTION.                                     
181000                                                                          
181100     MOVE 'R17'                TO BAS-R17-IDPTYP                          
181200     MOVE WS-IDLEVNR           TO BAS-R17-IDLEVNR                         
181300     MOVE WS-KDBEHX            TO BAS-R17-KDBEHX                          
181400     MOVE SPACE                TO BAS-R17-DATA-LEVNR-ARTNR                
181500                                  BAS-R17-DATA-ENDAST-LEVNR               
181600                                  BAS-R17-REST                            
181700                                                                          
181800                                                                          
181900     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
182000     MOVE MSG-TIME-OF-DAY      TO POST-TIKLOCK W-TIKLOCK                  
182100     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
182200     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
182300     MOVE ZERO                 TO POST-TIBORT                             
182400                                                                          
182500     MOVE BAS-R17-REGPOST      TO POST-REGPOST                            
182600                                                                          
182700     .                                                                    
182800     EJECT                                                                
182900                                                                          
183000                                                                          
183100 IDC-R17-TOM-BILD SECTION.                                                
183200                                                                          
183300     MOVE 'W0OR1701'               TO MFS-IDMOD                           
183400     MOVE '0832'                   TO MOD-R17-IDTRANS                     
183500     MOVE MFS-ADD-SAETT-CURSOR     TO MOD-R17-IDLEVNR-ATTR                
183600     MOVE MFS-ROER-EJ-FAELT        TO MOD-R17-IDLEVNR                     
183700                                      MOD-R17-KDBEHX                      
183800                                      MOD-R17-KVVECKOR-LT                 
183900                                      MOD-R17-KVVECKOR-AT                 
184000                                      MOD-R17-KVDAGAR-TTC1                
184100                                      MOD-R17-KVDAGAR-TTC2                
184200                                      MOD-R17-KDLEVTYP                    
184300                                      MOD-R17-KDGK                        
184400                                      MOD-R17-IDLPKOLL                    
184500                                      MOD-R17-FLRSADR                     
184600                                      MOD-R17-FLEMBPOL                    
184700                                      MOD-R17-KDSPRAK                     
184800     MOVE INF-UPDATE-DONE          TO MED-MFSMED                          
184900     CALL WMEDKONV USING MED-WMEDAREA                                     
185000     MOVE MED-MFSMED               TO MOD-R17-TEMFSINF                    
185100                                                                          
185200                                                                          
185300     .                                                                    
185400     EJECT                                                                
185500                                                                          
185600 IDD-R17-FELBILD SECTION.                                                 
185700                                                                          
185800     MOVE 'W0OR1701'            TO MFS-IDMOD                              
185900     MOVE '0832'                TO MOD-R17-IDTRANS                        
186000     IF WS-FELTYP = 1                                                     
186100        MOVE ERR-PF11-AND-NO-DATA  TO MED-MFSFEL                          
186200        CALL WMEDKONV USING MED-WMEDAREA                                  
186300        MOVE MED-MFSFEL            TO MOD-R17-TEMFSFEL                    
186400     ELSE                                                                 
186500        IF WS-FELTYP = 2                                                  
186600           MOVE ERR-CORR-HILITE-FLDS  TO MED-MFSFEL                       
186700           CALL WMEDKONV USING MED-WMEDAREA                               
186800           MOVE MED-MFSFEL            TO MOD-R17-TEMFSFEL                 
186900        END-IF                                                            
187000     END-IF                                                               
187100     .                                                                    
187200     EJECT                                                                
187300                                                                          
187400 IDE-R17-NASTA-SIDA SECTION.                                              
187500                                                                          
187600     MOVE '106'                 TO MED-MFSMED                             
187700     CALL WMEDKONV USING MED-WMEDAREA                                     
187800     MOVE MED-MFSMED            TO MOD-R17-TEMFSINF                       
187900     MOVE 'W0OR1701'            TO MFS-IDMOD                              
188000     MOVE '0832'                TO MOD-R17-IDTRANS                        
188100     .                                                                    
188200     EJECT                                                                
188300                                                                          
188400 IDF-R17-ENTER SECTION.                                                   
188500                                                                          
188600     MOVE MFS-ROER-EJ-FAELT     TO MOD-R17-IDLEVNR                        
188700                                   MOD-R17-KDBEHX                         
188800                                   MOD-R17-KVVECKOR-LT                    
188900                                   MOD-R17-KVVECKOR-AT                    
189000                                   MOD-R17-KVDAGAR-TTC1                   
189100                                   MOD-R17-KVDAGAR-TTC2                   
189200                                   MOD-R17-KDLEVTYP                       
189300                                   MOD-R17-KDGK                           
189400                                   MOD-R17-IDLPKOLL                       
189500                                   MOD-R17-FLRSADR                        
189600                                   MOD-R17-FLEMBPOL                       
189700                                   MOD-R17-KDSPRAK                        
189800     MOVE '003'                 TO MED-MFSFEL                             
189900     CALL WMEDKONV USING MED-WMEDAREA                                     
190000     MOVE MED-MFSFEL               TO MOD-R17-TEMFSFEL                    
190100     MOVE 'W0OR1701'            TO MFS-IDMOD                              
190200     MOVE '0832'                TO MOD-R17-IDTRANS                        
190300     .                                                                    
190400     EJECT                                                                
190500                                                                          
190600                                                                          
190700*************   R22   *********************************                   
190800                                                                          
190900 IE-R22-STYR SECTION.                                                     
191000                                                                          
191100     MOVE MID-083B-STANSVAL-AREA TO MID-R22-AREA                          
191200      IF MFS-UPDATE                                                       
191300         PERFORM IEA-R22-INDATAKONTROLL-1                                 
191400         IF INDATA-OK                                                     
191500            PERFORM IEB-R22-INDATAKONTROLL-2                              
191600            IF INDATA-OK                                                  
191700               PERFORM IEC-R22-SAMLA-UTDATA                               
191800               PERFORM IMS-ISRT-WDG901                                    
191900               PERFORM IED-R22-TOM-BILD                                   
192000            ELSE                                                          
192100               PERFORM IEE-R22-FELBILD                                    
192200            END-IF                                                        
192300         ELSE                                                             
192400            PERFORM IEE-R22-FELBILD                                       
192500         END-IF                                                           
192600      ELSE                                                                
192700         IF MFS-NEXT                                                      
192800            PERFORM IEF-R22-NASTA-SIDA                                    
192900         ELSE                                                             
193000            IF MFS-ENTER                                                  
193100               PERFORM IEG-R22-ENTER                                      
193200            END-IF                                                        
193300         END-IF                                                           
193400      END-IF                                                              
193500      .                                                                   
193600      EJECT                                                               
193700                                                                          
193800  IEA-R22-INDATAKONTROLL-1 SECTION.                                       
193900                                                                          
194000     IF MID-R22-IDARTNR       = ALL '+' AND                               
194100        MID-R22-IDBEST-1      = ALL '+' AND                               
194200        MID-R22-IDBEST-2      = ALL '+' AND                               
194300        MID-R22-IDBEST-3      = ALL '+' AND                               
194400        MID-R22-IDLEVNR-BEST  = ALL '+' AND                               
194500        MID-R22-TIBEST        = ALL '+' AND                               
194600        MID-R22-KVBEST        = ALL '+' AND                               
194700        MID-R22-KDBEH-BEST    = ALL '+'                                   
194800        MOVE NEJ                   TO INDATA-SW                           
194900        MOVE 1                     TO WS-FELTYP                           
195000        MOVE MFS-RENSA-FAELT       TO MOD-R22-IDARTNR                     
195100                                      MOD-R22-IDBEST-1                    
195200                                      MOD-R22-IDBEST-2                    
195300                                      MOD-R22-IDBEST-3                    
195400                                      MOD-R22-IDLEVNR-BEST                
195500                                      MOD-R22-TIBEST                      
195600                                      MOD-R22-KVBEST                      
195700                                      MOD-R22-KDBEH-BEST                  
195800        MOVE MFS-NUM-FAELT-FEL     TO MOD-R22-IDARTNR-ATTR                
195900                                      MOD-R22-IDBEST-1-ATTR               
196000                                      MOD-R22-IDBEST-2-ATTR               
196100                                      MOD-R22-IDBEST-3-ATTR               
196200                                      MOD-R22-TIBEST-ATTR                 
196300                                      MOD-R22-KVBEST-ATTR                 
196400                                      MOD-R22-KDBEH-BEST-ATTR             
196500        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R22-IDLEVNR-BEST-ATTR           
196600     END-IF                                                               
196700      .                                                                   
196800      EJECT                                                               
196900                                                                          
197000                                                                          
197100  IEB-R22-INDATAKONTROLL-2 SECTION.                                       
197200                                                                          
197300*IDARTNR                                                                  
197400                                                                          
197500     IF MID-R22-IDARTNR = ALL '+'                                         
197600        MOVE NEJ                   TO INDATA-SW                           
197700        MOVE 2                     TO WS-FELTYP                           
197800        MOVE MFS-RENSA-FAELT       TO MOD-R22-IDARTNR                     
197900        MOVE MFS-NUM-FAELT-FEL                                            
198000                          TO MOD-R22-IDARTNR-ATTR                         
198100     ELSE                                                                 
198200        IF MID-R22-IDARTNR NUMERIC                                        
198300           MOVE MID-R22-IDARTNR TO WS-IDARTNR                             
198400           IF WS-IDARTNR > 0                                              
198500              MOVE MFS-NUM-FAELT-RAETT                                    
198600                             TO MOD-R22-IDARTNR-ATTR                      
198700           ELSE                                                           
198800              MOVE MFS-NUM-FAELT-FEL  TO MOD-R22-IDARTNR-ATTR             
198900              MOVE NEJ                TO INDATA-SW                        
199000              MOVE 2                  TO WS-FELTYP                        
199100           END-IF                                                         
199200                                                                          
199300        ELSE                                                              
199400           MOVE NEJ                  TO INDATA-SW                         
199500           MOVE MFS-NUM-FAELT-FEL                                         
199600                          TO MOD-R22-IDARTNR-ATTR                         
199700           MOVE 2                    TO WS-FELTYP                         
199800        END-IF                                                            
199900        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-IDARTNR                   
200000     END-IF                                                               
200100                                                                          
200200                                                                          
200300*IDBEST-1                                                                 
200400                                                                          
200500     IF MID-R22-IDBEST-1 = ALL '+'                                        
200600        MOVE NEJ                   TO INDATA-SW                           
200700        MOVE 2                     TO WS-FELTYP                           
200800        MOVE MFS-RENSA-FAELT       TO MOD-R22-IDBEST-1                    
200900        MOVE MFS-NUM-FAELT-FEL                                            
201000                          TO MOD-R22-IDBEST-1-ATTR                        
201100     ELSE                                                                 
201200        IF MID-R22-IDBEST-1 NUMERIC                                       
201300           MOVE MID-R22-IDBEST-1 TO WS-IDBEST-1                           
201400        ELSE                                                              
201500           MOVE NEJ                  TO INDATA-SW                         
201600           MOVE MFS-NUM-FAELT-FEL                                         
201700                          TO MOD-R22-IDBEST-1-ATTR                        
201800           MOVE 2                    TO WS-FELTYP                         
201900        END-IF                                                            
202000        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-IDBEST-1                  
202100     END-IF                                                               
202200                                                                          
202300*IDBEST-2                                                                 
202400                                                                          
202500     IF MID-R22-IDBEST-2 = ALL '+'                                        
202600        MOVE NEJ                   TO INDATA-SW                           
202700        MOVE 2                     TO WS-FELTYP                           
202800        MOVE MFS-RENSA-FAELT       TO MOD-R22-IDBEST-2                    
202900        MOVE MFS-NUM-FAELT-FEL                                            
203000                          TO MOD-R22-IDBEST-2-ATTR                        
203100     ELSE                                                                 
203200        IF MID-R22-IDBEST-2 NUMERIC                                       
203300           MOVE MID-R22-IDBEST-2 TO WS-IDBEST-2                           
203400        ELSE                                                              
203500           MOVE NEJ                  TO INDATA-SW                         
203600           MOVE MFS-NUM-FAELT-FEL                                         
203700                          TO MOD-R22-IDBEST-2-ATTR                        
203800           MOVE 2                    TO WS-FELTYP                         
203900        END-IF                                                            
204000        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-IDBEST-2                  
204100     END-IF                                                               
204200                                                                          
204300*IDBEST-3                                                                 
204400                                                                          
204500     IF MID-R22-IDBEST-3 = ALL '+'                                        
204600        MOVE NEJ                   TO INDATA-SW                           
204700        MOVE 2                     TO WS-FELTYP                           
204800        MOVE MFS-RENSA-FAELT       TO MOD-R22-IDBEST-3                    
204900        MOVE MFS-NUM-FAELT-FEL                                            
205000                          TO MOD-R22-IDBEST-3-ATTR                        
205100     ELSE                                                                 
205200        IF MID-R22-IDBEST-3 NUMERIC                                       
205300           MOVE MID-R22-IDBEST-3 TO WS-IDBEST-3                           
205400        ELSE                                                              
205500           MOVE NEJ                  TO INDATA-SW                         
205600           MOVE MFS-NUM-FAELT-FEL                                         
205700                          TO MOD-R22-IDBEST-3-ATTR                        
205800           MOVE 2                    TO WS-FELTYP                         
205900        END-IF                                                            
206000        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-IDBEST-3                  
206100     END-IF                                                               
206200                                                                          
206300*IDLEVNR                                                                  
206400                                                                          
206500     IF MID-R22-IDLEVNR-BEST = ALL '+' OR SPACE                           
206600        MOVE NEJ                   TO INDATA-SW                           
206700        MOVE 2                     TO WS-FELTYP                           
206800        MOVE MFS-RENSA-FAELT       TO MOD-R22-IDLEVNR-BEST                
206900        MOVE MFS-ALFA-FAELT-FEL                                           
207000                          TO MOD-R22-IDLEVNR-BEST-ATTR                    
207100     ELSE                                                                 
207200        IF MID-R22-IDLEVNR-BEST(1:1) NOT = ' ' AND '+'                    
207300           MOVE MID-R22-IDLEVNR-BEST TO WS-IDLEVNR                        
207400        ELSE                                                              
207500           MOVE NEJ                  TO INDATA-SW                         
207600           MOVE MFS-ALFA-FAELT-FEL                                        
207700                          TO MOD-R22-IDLEVNR-BEST-ATTR                    
207800           MOVE 2                    TO WS-FELTYP                         
207900        END-IF                                                            
208000        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-IDLEVNR-BEST              
208100     END-IF                                                               
208200                                                                          
208210*IDLEVNR-SHIP                                                             
208220                                                                          
208310*TIBEST                                                                   
208400                                                                          
208500     IF MID-R22-TIBEST = ALL '+'                                          
208600        MOVE NEJ                   TO INDATA-SW                           
208700        MOVE 2                     TO WS-FELTYP                           
208800        MOVE MFS-RENSA-FAELT       TO MOD-R22-TIBEST                      
208900        MOVE MFS-NUM-FAELT-FEL                                            
209000                          TO MOD-R22-TIBEST-ATTR                          
209100     ELSE                                                                 
209200        IF MID-R22-TIBEST NUMERIC                                         
209300           MOVE MID-R22-TIBEST       TO WS-TIBEST                         
209400           MOVE WS-TIBEST           TO DAT-I-TIDATUM                      
209500           MOVE 'AAMMDD'            TO DAT-KDDATFORM                      
209600           CALL WDATKONV USING DAT-KDDATFORM                              
209700                               DAT-I-TIDATUM                              
209800                               DAT-O-TIDATUM                              
209900                               DAT-KDSVAR                                 
210000           IF DAT-KDSVAR-OK                                               
210100              CONTINUE                                                    
210200           ELSE                                                           
210300              MOVE NEJ                  TO INDATA-SW                      
210400              MOVE MFS-NUM-FAELT-FEL                                      
210500                             TO MOD-R22-TIBEST-ATTR                       
210600              MOVE 2                    TO WS-FELTYP                      
210700           END-IF                                                         
210800        ELSE                                                              
210900           MOVE NEJ                  TO INDATA-SW                         
211000           MOVE MFS-NUM-FAELT-FEL                                         
211100                          TO MOD-R22-TIBEST-ATTR                          
211200           MOVE 2                    TO WS-FELTYP                         
211300        END-IF                                                            
211400        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-TIBEST                    
211500     END-IF                                                               
211600                                                                          
211700*KVBEST                                                                   
211800                                                                          
211900     IF MID-R22-KVBEST = ALL '+'                                          
212000        MOVE NEJ                   TO INDATA-SW                           
212100        MOVE 2                     TO WS-FELTYP                           
212200        MOVE MFS-RENSA-FAELT       TO MOD-R22-KVBEST                      
212300        MOVE MFS-NUM-FAELT-FEL                                            
212400                          TO MOD-R22-KVBEST-ATTR                          
212500     ELSE                                                                 
212600        IF MID-R22-KVBEST NUMERIC                                         
212700           MOVE MID-R22-KVBEST TO WS-KVBEST                               
212800        ELSE                                                              
212900           MOVE NEJ                  TO INDATA-SW                         
213000           MOVE MFS-NUM-FAELT-FEL                                         
213100                          TO MOD-R22-KVBEST-ATTR                          
213200           MOVE 2                    TO WS-FELTYP                         
213300        END-IF                                                            
213400        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-KVBEST                    
213500     END-IF                                                               
213600                                                                          
213700*KDBEH-BEST                                                               
213800                                                                          
213900     IF MID-R22-KDBEH-BEST = ALL '+'                                      
214000        MOVE NEJ                   TO INDATA-SW                           
214100        MOVE 2                     TO WS-FELTYP                           
214200        MOVE MFS-RENSA-FAELT       TO MOD-R22-KDBEH-BEST                  
214300        MOVE MFS-NUM-FAELT-FEL                                            
214400                          TO MOD-R22-KDBEH-BEST-ATTR                      
214500     ELSE                                                                 
214600        IF MID-R22-KDBEH-BEST NUMERIC                                     
214700           MOVE MID-R22-KDBEH-BEST TO WS-KDBEH-BEST                       
214800           IF WS-KDBEH-BEST = 1 OR WS-KDBEH-BEST = 5                      
214900              MOVE MFS-NUM-FAELT-RAETT                                    
215000                             TO MOD-R22-KDBEH-BEST-ATTR                   
215100           ELSE                                                           
215200              MOVE MFS-NUM-FAELT-FEL  TO MOD-R22-KDBEH-BEST-ATTR          
215300              MOVE NEJ                TO INDATA-SW                        
215400              MOVE 2                  TO WS-FELTYP                        
215500           END-IF                                                         
215600                                                                          
215700        ELSE                                                              
215800           MOVE NEJ                  TO INDATA-SW                         
215900           MOVE MFS-NUM-FAELT-FEL                                         
216000                          TO MOD-R22-KDBEH-BEST-ATTR                      
216100           MOVE 2                    TO WS-FELTYP                         
216200        END-IF                                                            
216300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R22-KDBEH-BEST                
216400     END-IF                                                               
216500                                                                          
216600     .                                                                    
216700     EJECT                                                                
216800                                                                          
216900                                                                          
217000 IEC-R22-SAMLA-UTDATA SECTION.                                            
217100                                                                          
217200     MOVE 'R22'                TO BAS-R22-IDPTYP                          
217300     MOVE WS-IDARTNR           TO BAS-R22-IDARTNR                         
217400     MOVE WS-IDBEST-NUM        TO BAS-R22-IDBEST                          
217500     MOVE WS-IDLEVNR           TO BAS-R22-IDLEVNR-BEST                    
217510     MOVE SPACES               TO BAS-R22-IDLEVNR-SHIP                    
217600     MOVE WS-TIBEST            TO BAS-R22-TIBEST                          
217700     MOVE WS-KVBEST            TO BAS-R22-KVBEST                          
217800     MOVE WS-KDBEH-BEST        TO BAS-R22-KDBEH-BEST                      
217900     MOVE SPACE                TO BAS-R22-TENOT-BESTPRIS                  
218000     MOVE SPACE                TO BAS-R22-REST                            
218100                                                                          
218200                                                                          
218300     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
218400     MOVE MSG-TIME-OF-DAY      TO POST-TIKLOCK W-TIKLOCK                  
218500     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
218600     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
218700     MOVE ZERO                 TO POST-TIBORT                             
218800                                                                          
218900     MOVE BAS-R22-REGPOST      TO POST-REGPOST                            
219000     .                                                                    
219100     EJECT                                                                
219200                                                                          
219300                                                                          
219400 IED-R22-TOM-BILD SECTION.                                                
219500                                                                          
219600     MOVE 'W0OR2201'               TO MFS-IDMOD                           
219700     MOVE '0832'                   TO MOD-R22-IDTRANS                     
219800     MOVE MFS-ADD-SAETT-CURSOR     TO MOD-R22-IDARTNR-ATTR                
219900     MOVE MFS-ROER-EJ-FAELT        TO MOD-R22-IDARTNR                     
220000                                      MOD-R22-IDBEST-1                    
220100                                      MOD-R22-IDBEST-2                    
220200                                      MOD-R22-IDBEST-3                    
220300                                      MOD-R22-IDLEVNR-BEST                
220400                                      MOD-R22-TIBEST                      
220500                                      MOD-R22-KVBEST                      
220600                                      MOD-R22-KDBEH-BEST                  
220700     MOVE INF-UPDATE-DONE          TO MED-MFSMED                          
220800     CALL WMEDKONV USING MED-WMEDAREA                                     
220900     MOVE MED-MFSMED               TO MOD-R22-TEMFSINF                    
221000                                                                          
221100                                                                          
221200     .                                                                    
221300     EJECT                                                                
221400                                                                          
221500 IEE-R22-FELBILD SECTION.                                                 
221600                                                                          
221700     MOVE 'W0OR2201'            TO MFS-IDMOD                              
221800     MOVE '0832'                TO MOD-R22-IDTRANS                        
221900     IF WS-FELTYP = 1                                                     
222000        MOVE ERR-PF11-AND-NO-DATA  TO MED-MFSFEL                          
222100        CALL WMEDKONV USING MED-WMEDAREA                                  
222200        MOVE MED-MFSFEL            TO MOD-R22-TEMFSFEL                    
222300     ELSE                                                                 
222400        IF WS-FELTYP = 2                                                  
222500           MOVE ERR-CORR-HILITE-FLDS  TO MED-MFSFEL                       
222600           CALL WMEDKONV USING MED-WMEDAREA                               
222700           MOVE MED-MFSFEL            TO MOD-R22-TEMFSFEL                 
222800        END-IF                                                            
222900     END-IF                                                               
223000     .                                                                    
223100     EJECT                                                                
223200                                                                          
223300 IEF-R22-NASTA-SIDA SECTION.                                              
223400                                                                          
223500     MOVE '106'                 TO MED-MFSMED                             
223600     CALL WMEDKONV USING MED-WMEDAREA                                     
223700     MOVE MED-MFSMED            TO MOD-R22-TEMFSINF                       
223800     MOVE 'W0OR2201'            TO MFS-IDMOD                              
223900     MOVE '0832'                TO MOD-R22-IDTRANS                        
224000     .                                                                    
224100     EJECT                                                                
224200                                                                          
224300                                                                          
224400 IEG-R22-ENTER SECTION.                                                   
224500                                                                          
224600     MOVE MFS-ADD-SAETT-CURSOR     TO MOD-R22-IDARTNR-ATTR                
224700     MOVE MFS-ROER-EJ-FAELT        TO MOD-R22-IDARTNR                     
224800                                      MOD-R22-IDBEST-1                    
224900                                      MOD-R22-IDBEST-2                    
225000                                      MOD-R22-IDBEST-3                    
225100                                      MOD-R22-IDLEVNR-BEST                
225200                                      MOD-R22-TIBEST                      
225300                                      MOD-R22-KVBEST                      
225400                                      MOD-R22-KDBEH-BEST                  
225500     MOVE '003'                 TO MED-MFSFEL                             
225600     CALL WMEDKONV USING MED-WMEDAREA                                     
225700     MOVE MED-MFSFEL               TO MOD-R22-TEMFSFEL                    
225800     MOVE 'W0OR2201'            TO MFS-IDMOD                              
225900     MOVE '0832'                TO MOD-R22-IDTRANS                        
226000     .                                                                    
226100     EJECT                                                                
226200                                                                          
226300*************   R23   *********************************                   
226400                                                                          
226500 IF-R23-STYR SECTION.                                                     
226600                                                                          
226700     MOVE MID-083B-STANSVAL-AREA TO MID-R23-AREA                          
226800      IF MFS-UPDATE                                                       
226900         PERFORM IFA-R23-INDATAKONTROLL-1                                 
227000         IF INDATA-OK                                                     
227100            PERFORM IFB-R23-INDATAKONTROLL-2                              
227200            IF INDATA-OK                                                  
227300               PERFORM IFC-R23-SAMLA-UTDATA                               
227400               PERFORM IMS-ISRT-WDG901                                    
227500               PERFORM IFD-R23-TOM-BILD                                   
227600            ELSE                                                          
227700               PERFORM IFE-R23-FELBILD                                    
227800            END-IF                                                        
227900         ELSE                                                             
228000            PERFORM IFE-R23-FELBILD                                       
228100         END-IF                                                           
228200      ELSE                                                                
228300         IF MFS-NEXT                                                      
228400            PERFORM IFF-R23-NASTA-SIDA                                    
228500         ELSE                                                             
228600            IF MFS-ENTER                                                  
228700               PERFORM IFG-R23-ENTER                                      
228800            END-IF                                                        
228900         END-IF                                                           
229000      END-IF                                                              
229100      .                                                                   
229200      EJECT                                                               
229300                                                                          
229400  IFA-R23-INDATAKONTROLL-1 SECTION.                                       
229500                                                                          
229600     IF MID-R23-IDARTNR       = ALL '+' AND                               
229700        MID-R23-IDAVTAL-1     = ALL '+' AND                               
229800        MID-R23-IDAVTAL-2     = ALL '+' AND                               
229900        MID-R23-IDAVTAL-3     = ALL '+' AND                               
230000        MID-R23-IDLEVNR-AVT   = ALL '+' AND                               
230010        MID-R23-IDLEVNR-SHIP  = ALL '+' AND                               
230100        MID-R23-TIAVTAL       = ALL '+' AND                               
230200        MID-R23-KVAVTANT      = ALL '+'                                   
230300        MOVE NEJ                   TO INDATA-SW                           
230400        MOVE 1                     TO WS-FELTYP                           
230500        MOVE MFS-RENSA-FAELT       TO MOD-R23-IDARTNR                     
230600                                      MOD-R23-IDAVTAL-1                   
230700                                      MOD-R23-IDAVTAL-2                   
230800                                      MOD-R23-IDAVTAL-3                   
230900                                      MOD-R23-IDLEVNR-AVT                 
230910                                      MOD-R23-IDLEVNR-SHIP                
231000                                      MOD-R23-TIAVTAL                     
231100                                      MOD-R23-KVAVTANT                    
231200        MOVE MFS-NUM-FAELT-FEL     TO MOD-R23-IDARTNR-ATTR                
231300                                      MOD-R23-IDAVTAL-1-ATTR              
231400                                      MOD-R23-IDAVTAL-2-ATTR              
231500                                      MOD-R23-IDAVTAL-3-ATTR              
231600                                      MOD-R23-TIAVTAL-ATTR                
231700                                      MOD-R23-KVAVTANT-ATTR               
231800        MOVE MFS-ALFA-FAELT-FEL    TO MOD-R23-IDLEVNR-AVT-ATTR            
231810                                      MOD-R23-IDLEVNR-SHIP-ATTR           
231900     END-IF                                                               
232000      .                                                                   
232100      EJECT                                                               
232200                                                                          
232300                                                                          
232400  IFB-R23-INDATAKONTROLL-2 SECTION.                                       
232500                                                                          
232600*IDARTNR                                                                  
232700                                                                          
232800     IF MID-R23-IDARTNR = ALL '+'                                         
232900        MOVE NEJ                   TO INDATA-SW                           
233000        MOVE 2                     TO WS-FELTYP                           
233100        MOVE MFS-RENSA-FAELT       TO MOD-R23-IDARTNR                     
233200        MOVE MFS-NUM-FAELT-FEL                                            
233300                          TO MOD-R23-IDARTNR-ATTR                         
233400     ELSE                                                                 
233500        IF MID-R23-IDARTNR NUMERIC                                        
233600           MOVE MID-R23-IDARTNR TO WS-IDARTNR                             
233700           IF WS-IDARTNR > 0                                              
233800              MOVE MFS-NUM-FAELT-RAETT                                    
233900                             TO MOD-R23-IDARTNR-ATTR                      
234000           ELSE                                                           
234100              MOVE MFS-NUM-FAELT-FEL  TO MOD-R23-IDARTNR-ATTR             
234200              MOVE NEJ                TO INDATA-SW                        
234300              MOVE 2                  TO WS-FELTYP                        
234400           END-IF                                                         
234500                                                                          
234600        ELSE                                                              
234700           MOVE NEJ                  TO INDATA-SW                         
234800           MOVE MFS-NUM-FAELT-FEL                                         
234900                          TO MOD-R23-IDARTNR-ATTR                         
235000           MOVE 2                    TO WS-FELTYP                         
235100        END-IF                                                            
235200        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-IDARTNR                   
235300     END-IF                                                               
235400                                                                          
235500                                                                          
235600*IDAVTAL-1                                                                
235700                                                                          
235800     IF MID-R23-IDAVTAL-1 = ALL '+'                                       
235900        MOVE NEJ                   TO INDATA-SW                           
236000        MOVE 2                     TO WS-FELTYP                           
236100        MOVE MFS-RENSA-FAELT       TO MOD-R23-IDAVTAL-1                   
236200        MOVE MFS-NUM-FAELT-FEL                                            
236300                          TO MOD-R23-IDAVTAL-1-ATTR                       
236400     ELSE                                                                 
236500        IF MID-R23-IDAVTAL-1 NUMERIC                                      
236600           MOVE MID-R23-IDAVTAL-1 TO WS-IDAVTAL-1                         
236700        ELSE                                                              
236800           MOVE NEJ                  TO INDATA-SW                         
236900           MOVE MFS-NUM-FAELT-FEL                                         
237000                          TO MOD-R23-IDAVTAL-1-ATTR                       
237100           MOVE 2                    TO WS-FELTYP                         
237200        END-IF                                                            
237300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-IDAVTAL-1                 
237400     END-IF                                                               
237500                                                                          
237600*IDAVTAL-2                                                                
237700                                                                          
237800     IF MID-R23-IDAVTAL-2 = ALL '+'                                       
237900        MOVE NEJ                   TO INDATA-SW                           
238000        MOVE 2                     TO WS-FELTYP                           
238100        MOVE MFS-RENSA-FAELT       TO MOD-R23-IDAVTAL-2                   
238200        MOVE MFS-NUM-FAELT-FEL                                            
238300                          TO MOD-R23-IDAVTAL-2-ATTR                       
238400     ELSE                                                                 
238500        IF MID-R23-IDAVTAL-2 NUMERIC                                      
238600           MOVE MID-R23-IDAVTAL-2 TO WS-IDAVTAL-2                         
238700        ELSE                                                              
238800           MOVE NEJ                  TO INDATA-SW                         
238900           MOVE MFS-NUM-FAELT-FEL                                         
239000                          TO MOD-R23-IDAVTAL-2-ATTR                       
239100           MOVE 2                    TO WS-FELTYP                         
239200        END-IF                                                            
239300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-IDAVTAL-2                 
239400     END-IF                                                               
239500                                                                          
239600*IDAVTAL-3                                                                
239700                                                                          
239800     IF MID-R23-IDAVTAL-3 = ALL '+'                                       
239900        MOVE NEJ                   TO INDATA-SW                           
240000        MOVE 2                     TO WS-FELTYP                           
240100        MOVE MFS-RENSA-FAELT       TO MOD-R23-IDAVTAL-3                   
240200        MOVE MFS-NUM-FAELT-FEL                                            
240300                          TO MOD-R23-IDAVTAL-3-ATTR                       
240400     ELSE                                                                 
240500        IF MID-R23-IDAVTAL-3 NUMERIC                                      
240600           MOVE MID-R23-IDAVTAL-3 TO WS-IDAVTAL-3                         
240700        ELSE                                                              
240800           MOVE NEJ                  TO INDATA-SW                         
240900           MOVE MFS-NUM-FAELT-FEL                                         
241000                          TO MOD-R23-IDAVTAL-3-ATTR                       
241100           MOVE 2                    TO WS-FELTYP                         
241200        END-IF                                                            
241300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-IDAVTAL-3                 
241400     END-IF                                                               
241500                                                                          
241600*IDLEVNR                                                                  
241700                                                                          
241800     IF MID-R23-IDLEVNR-AVT = ALL '+' OR SPACE                            
241900        MOVE NEJ                   TO INDATA-SW                           
242000        MOVE 2                     TO WS-FELTYP                           
242100        MOVE MFS-RENSA-FAELT       TO MOD-R23-IDLEVNR-AVT                 
242200        MOVE MFS-ALFA-FAELT-FEL                                           
242300                          TO MOD-R23-IDLEVNR-AVT-ATTR                     
242400     ELSE                                                                 
242500        IF MID-R23-IDLEVNR-AVT(1:1) NOT = ' ' AND '+'                     
242600           MOVE MID-R23-IDLEVNR-AVT TO WS-IDLEVNR                         
242700        ELSE                                                              
242800           MOVE NEJ                  TO INDATA-SW                         
242900           MOVE MFS-ALFA-FAELT-FEL                                        
243000                          TO MOD-R23-IDLEVNR-AVT-ATTR                     
243100           MOVE 2                    TO WS-FELTYP                         
243200        END-IF                                                            
243300        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-IDLEVNR-AVT               
243400     END-IF                                                               
243500                                                                          
243510*IDLEVNR-SHIP                                                             
243520                                                                          
243530     IF MID-R23-IDLEVNR-SHIP = ALL '+' OR SPACE                           
243540        MOVE NEJ                   TO INDATA-SW                           
243550        MOVE 2                     TO WS-FELTYP                           
243560        MOVE MFS-RENSA-FAELT       TO MOD-R23-IDLEVNR-SHIP                
243570        MOVE MFS-ALFA-FAELT-FEL                                           
243580                          TO MOD-R23-IDLEVNR-SHIP-ATTR                    
243590     ELSE                                                                 
243591        IF MID-R23-IDLEVNR-SHIP(1:1) NOT = ' ' AND '+'                    
243592           MOVE MID-R23-IDLEVNR-SHIP TO WS-IDLEVNR-SHIP                   
243593        ELSE                                                              
243594           MOVE NEJ                  TO INDATA-SW                         
243595           MOVE MFS-ALFA-FAELT-FEL                                        
243596                          TO MOD-R23-IDLEVNR-SHIP-ATTR                    
243597           MOVE 2                    TO WS-FELTYP                         
243598        END-IF                                                            
243599        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-IDLEVNR-SHIP              
243600     END-IF                                                               
243601                                                                          
243610*TIAVTAL                                                                  
243700                                                                          
243800     IF MID-R23-TIAVTAL = ALL '+'                                         
243900        MOVE NEJ                   TO INDATA-SW                           
244000        MOVE 2                     TO WS-FELTYP                           
244100        MOVE MFS-RENSA-FAELT       TO MOD-R23-TIAVTAL                     
244200        MOVE MFS-NUM-FAELT-FEL                                            
244300                          TO MOD-R23-TIAVTAL-ATTR                         
244400     ELSE                                                                 
244500        IF MID-R23-TIAVTAL NUMERIC                                        
244600           MOVE MID-R23-TIAVTAL      TO WS-TIAVTAL                        
244700           MOVE WS-TIAVTAL          TO DAT-I-TIDATUM                      
244800           MOVE 'AAMMDD'            TO DAT-KDDATFORM                      
244900           CALL WDATKONV USING DAT-KDDATFORM                              
245000                               DAT-I-TIDATUM                              
245100                               DAT-O-TIDATUM                              
245200                               DAT-KDSVAR                                 
245300           IF DAT-KDSVAR-OK                                               
245400              CONTINUE                                                    
245500           ELSE                                                           
245600              MOVE NEJ                  TO INDATA-SW                      
245700              MOVE MFS-NUM-FAELT-FEL                                      
245800                             TO MOD-R23-TIAVTAL-ATTR                      
245900              MOVE 2                    TO WS-FELTYP                      
246000           END-IF                                                         
246100        ELSE                                                              
246200           MOVE NEJ                  TO INDATA-SW                         
246300           MOVE MFS-NUM-FAELT-FEL                                         
246400                          TO MOD-R23-TIAVTAL-ATTR                         
246500           MOVE 2                    TO WS-FELTYP                         
246600        END-IF                                                            
246700        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-TIAVTAL                   
246800     END-IF                                                               
246900                                                                          
247000*KVAVTANT                                                                 
247100                                                                          
247200     IF MID-R23-KVAVTANT = ALL '+'                                        
247300        MOVE ALL ZERO              TO WS-KVAVTANT                         
247400        MOVE MFS-RENSA-FAELT       TO MOD-R23-KVAVTANT                    
247500        MOVE MFS-NUM-FAELT-RAETT                                          
247600                          TO MOD-R23-KVAVTANT-ATTR                        
247700     ELSE                                                                 
247800        IF MID-R23-KVAVTANT NUMERIC                                       
247900           MOVE MID-R23-KVAVTANT TO WS-KVAVTANT                           
248000        ELSE                                                              
248100           MOVE NEJ                  TO INDATA-SW                         
248200           MOVE MFS-NUM-FAELT-FEL                                         
248300                          TO MOD-R23-KVAVTANT-ATTR                        
248400           MOVE 2                    TO WS-FELTYP                         
248500        END-IF                                                            
248600        MOVE MFS-ROER-EJ-FAELT       TO MOD-R23-KVAVTANT                  
248700     END-IF                                                               
248800                                                                          
248900     .                                                                    
249000     EJECT                                                                
249100                                                                          
249200                                                                          
249300 IFC-R23-SAMLA-UTDATA SECTION.                                            
249400                                                                          
249500     MOVE 'R23'                TO BAS-R23-IDPTYP                          
249600     MOVE WS-IDARTNR           TO BAS-R23-IDARTNR                         
249700     MOVE WS-IDAVTAL-NUM       TO BAS-R23-IDAVTAL                         
249800     MOVE WS-IDLEVNR           TO BAS-R23-IDLEVNR-AVT                     
249810     MOVE WS-IDLEVNR-SHIP      TO BAS-R23-IDLEVNR-SHIP                    
249900     MOVE WS-TIAVTAL           TO BAS-R23-TIAVTAL                         
250000     MOVE WS-KVAVTANT          TO BAS-R23-KVAVTANT                        
250100     MOVE 1                    TO BAS-R23-KDBEH-AVT                       
250200     MOVE SPACE                TO BAS-R23-TENOT-AVTPRIS                   
250300     MOVE SPACE                TO BAS-R23-REST                            
250400                                                                          
250500                                                                          
250600     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
250700     MOVE MSG-TIME-OF-DAY      TO POST-TIKLOCK W-TIKLOCK                  
250800     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
250900     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
251000     MOVE ZERO                 TO POST-TIBORT                             
251100                                                                          
251200     MOVE BAS-R23-REGPOST      TO POST-REGPOST                            
251300     .                                                                    
251400     EJECT                                                                
251500                                                                          
251600                                                                          
251700 IFD-R23-TOM-BILD SECTION.                                                
251800                                                                          
251900     MOVE 'W0OR2301'               TO MFS-IDMOD                           
252000     MOVE '0832'                   TO MOD-R23-IDTRANS                     
252100     MOVE MFS-ADD-SAETT-CURSOR     TO MOD-R23-IDARTNR-ATTR                
252200     MOVE MFS-ROER-EJ-FAELT        TO MOD-R23-IDARTNR                     
252300                                      MOD-R23-IDAVTAL-1                   
252400                                      MOD-R23-IDAVTAL-2                   
252500                                      MOD-R23-IDAVTAL-3                   
252600                                      MOD-R23-IDLEVNR-AVT                 
252610                                      MOD-R23-IDLEVNR-SHIP                
252700                                      MOD-R23-TIAVTAL                     
252800                                      MOD-R23-KVAVTANT                    
252900     MOVE INF-UPDATE-DONE          TO MED-MFSMED                          
253000     CALL WMEDKONV USING MED-WMEDAREA                                     
253100     MOVE MED-MFSMED               TO MOD-R23-TEMFSINF                    
253200                                                                          
253300                                                                          
253400     .                                                                    
253500     EJECT                                                                
253600                                                                          
253700 IFE-R23-FELBILD SECTION.                                                 
253800                                                                          
253900     MOVE 'W0OR2301'            TO MFS-IDMOD                              
254000     MOVE '0832'                TO MOD-R23-IDTRANS                        
254100     IF WS-FELTYP = 1                                                     
254200        MOVE ERR-PF11-AND-NO-DATA  TO MED-MFSFEL                          
254300        CALL WMEDKONV USING MED-WMEDAREA                                  
254400        MOVE MED-MFSFEL            TO MOD-R23-TEMFSFEL                    
254500     ELSE                                                                 
254600        IF WS-FELTYP = 2                                                  
254700           MOVE ERR-CORR-HILITE-FLDS  TO MED-MFSFEL                       
254800           CALL WMEDKONV USING MED-WMEDAREA                               
254900           MOVE MED-MFSFEL            TO MOD-R23-TEMFSFEL                 
255000        END-IF                                                            
255100     END-IF                                                               
255200     .                                                                    
255300     EJECT                                                                
255400                                                                          
255500 IFF-R23-NASTA-SIDA SECTION.                                              
255600                                                                          
255700     MOVE '106'                 TO MED-MFSMED                             
255800     CALL WMEDKONV USING MED-WMEDAREA                                     
255900     MOVE MED-MFSMED            TO MOD-R23-TEMFSINF                       
256000     MOVE 'W0OR2301'            TO MFS-IDMOD                              
256100     MOVE '0832'                TO MOD-R23-IDTRANS                        
256200     .                                                                    
256300     EJECT                                                                
256400                                                                          
256500                                                                          
256600 IFG-R23-ENTER SECTION.                                                   
256700                                                                          
256800     MOVE MFS-ADD-SAETT-CURSOR     TO MOD-R23-IDARTNR-ATTR                
256900     MOVE MFS-ROER-EJ-FAELT        TO MOD-R23-IDARTNR                     
257000                                      MOD-R23-IDAVTAL-1                   
257100                                      MOD-R23-IDAVTAL-2                   
257200                                      MOD-R23-IDAVTAL-3                   
257300                                      MOD-R23-IDLEVNR-AVT                 
257310                                      MOD-R23-IDLEVNR-SHIP                
257400                                      MOD-R23-TIAVTAL                     
257500                                      MOD-R23-KVAVTANT                    
257600     MOVE '003'                 TO MED-MFSFEL                             
257700     CALL WMEDKONV USING MED-WMEDAREA                                     
257800     MOVE MED-MFSFEL               TO MOD-R23-TEMFSFEL                    
257900     MOVE 'W0OR2301'            TO MFS-IDMOD                              
258000     MOVE '0832'                TO MOD-R23-IDTRANS                        
258100     .                                                                    
258200     EJECT                                                                
258300                                                                          
258400* --- IMS SEKTIONER ---                                                   
258500     SKIP3                                                                
258600 IMS-GET-MSG SECTION.                                                     
258700                                                                          
258800     MOVE '  QC' TO GODK-STATUSKODER                                      
258900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
259000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
259100     PERFORM IMS-STATUSKONTROLL                                           
259200     .                                                                    
259300     SKIP3                                                                
259400 IMS-INSERT-MSG SECTION.                                                  
259500                                                                          
259900     MOVE '0' TO MFS-KDHUVOMR                                             
260100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
260200     MOVE SPACE TO GODK-STATUSKODER                                       
260300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
260400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
260500     PERFORM IMS-STATUSKONTROLL                                           
260600     .                                                                    
260700     EJECT                                                                
260800 IMS-ISRT-WDG901 SECTION.                                                 
260900                                                                          
261000     MOVE 'WLZZAD01 ' TO SSA1                                             
261100     MOVE '  II' TO GODK-STATUSKODER                                      
261200     CALL CBLTDLI USING ISRT ZZAD-PCB DLI-IO-AREA SSA1                    
261300     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
261400     PERFORM IMS-STATUSKONTROLL                                           
261500     .                                                                    
261600     EJECT                                                                
260800 IMS-GN-WDG901 SECTION.                                                   
260900                                                                          
261000     MOVE 'WLZZAD01 ' TO SSA1                                             
261100     MOVE '  GB' TO GODK-STATUSKODER                                      
261200     CALL CBLTDLI USING GN ZZAD-PCB DLI-IO-AREA SSA1                      
261300     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
261400     PERFORM IMS-STATUSKONTROLL                                           
261500     .                                                                    
261600     EJECT                                                                
261700 IMS-STATUSKONTROLL SECTION.                                              
261800                                                                          
261900     SET STATUS-IX TO 1                                                   
262000     SEARCH GODK-STATUS                                                   
262100       AT END                                                             
262200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
262300         DELIMITED BY SIZE INTO FELTEXT                                   
262400         CALL FELLOG                                                      
262500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
262600         CONTINUE                                                         
262700     END-SEARCH                                                           
262800     .                                                                    
