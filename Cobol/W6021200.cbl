001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6021200.                                                
001300*AUTHOR.         INGER NILSSON.                                           
001400*DATE-WRITTEN.   92/07/14.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        UPPDATERING AV ARTIKELHISTORIK                                   
002000*                                                                         
002110*        PROGRAMMET UPPDATERAR W6KVAH (W6D2)                              
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W6T212                                              
002500*        MID:         W6I21201                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W6O21201                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003401*    -- CHECKED BY WY2000                                                 
003402     SKIP3                                                                
003500 77  IDPGM                       PIC X(08)   VALUE 'W6021200'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003824 01  W1-DAREGDAT                 PIC 9(08).                               
003850 77  W1-TIKLOCK                  PIC 9(09).                               
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004010 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004301*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004302 77  IX1                         PIC S9(4)  VALUE +0    COMP SYNC.        
004303 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004310 77  MAX-INDX                    PIC S9(4)  VALUE +7    COMP SYNC.        
004400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800                                                                          
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005010 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005011 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005020 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
005030 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
005031 77  WS-KDKVAINF                 PIC X(1)    VALUE SPACE.                 
005032 77  WS-BEINIT                   PIC X(3)    VALUE SPACE.                 
005033 77  FL-KEY                      PIC X       VALUE 'J'.                   
005100                                                                          
005201 77  IDARTNR-SW                  PIC X       VALUE 'J'.                   
005202     88  IDARTNR-OK                          VALUE 'J'.                   
005210     88  IDARTNR-FEL                         VALUE 'N'.                   
005300                                                                          
005301 77  NYTT-ARTNR-SW               PIC X       VALUE 'N'.                   
005302     88  NYTT-ARTNR                          VALUE 'J'.                   
005303     88  GAMMALT-ARTNR                       VALUE 'N'.                   
005304                                                                          
005310 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005320     88  INDATA-OK                           VALUE 'J'.                   
005330     88  INDATA-FEL                          VALUE 'N'.                   
005340                                                                          
005350 77  KKOD-SW                     PIC X       VALUE 'J'.                   
005360     88  KKOD-OK                             VALUE 'J'.                   
005370     88  KKOD-FEL                            VALUE 'N'.                   
005380                                                                          
005390 77  SPEC-SW                     PIC X       VALUE 'J'.                   
005391     88  SPEC-OK                             VALUE 'J'.                   
005392     88  SPEC-FEL                            VALUE 'N'.                   
005393                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '6212'.                
005910     88  HELP-MID                            VALUE '0551'.                
006000     88  GODK-MID                            VALUE '6211' '6212'          
006100                                                   '6213' '6214'          
006200                                                   '6215' '6216'          
006300                                                   '6217' '6218'          
006400                                                   '6219'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007500*01 -COPY WMSGINIT                                                        
007510     EJECT                                                                
007520*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007530*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007801     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007802     03  ERR-RENSA-ARTIKEL-INFO  PIC X(3)    VALUE '177'.                 
007807     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007808     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007810     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007901     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007910     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  INSPECT-CODE-MISSING    PIC X(3)    VALUE '234'.                 
008110     03  KEYS-MISSING            PIC X(3)    VALUE '005'.                 
008120     03  SPEC-CONTROL            PIC X(3)    VALUE '342'.                 
008200     EJECT                                                                
008300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008600     SKIP3                                                                
008700*01  MID -COPY W6I21201                                                   
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009000     SKIP3                                                                
009100*01  -COPY WMSGAREA                                                       
009200     EJECT                                                                
009300     03  MOD REDEFINES MSG-AREA.                                          
009400*      05  -COPY W6O21201                                                 
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009700     SKIP3                                                                
009800*01  -COPY WMFSAREA                                                       
009900     EJECT                                                                
010000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010100*                                                                         
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010601     03  W-IDARTNR-X.                                                     
010602         05  W-IDARTNR           PIC S9(09)   VALUE ZERO COMP-3.          
010603     03  W1-IDKVAINF-X.                                                   
010604         05  W1-IDKVAINF         PIC  9(02)   VALUE ZERO.                 
010605     03  W-IDKVAINF-X.                                                    
010606         05  W-IDKVAINF          PIC  9(02)   VALUE ZERO.                 
010607     03  W-IDKVAINF-MIN-X.                                                
010608         05  W-IDKVAINF-MIN      PIC  9(02)   VALUE ZERO.                 
010609     03  W-IDKVAINF-MAX-X.                                                
010610         05  W-IDKVAINF-MAX      PIC  9(02)   VALUE ZERO.                 
010611     03  W-KDKVAINF-X.                                                    
010612         05  W-KDKVAINF          PIC  X(01)   VALUE SPACE.                
010613     03  W-KDKVAINF-MIN-X.                                                
010614         05  W-KDKVAINF-MIN      PIC  X(01)   VALUE LOW-VALUE.            
010615     03  W-KDKVAINF-MAX-X.                                                
010616         05  W-KDKVAINF-MAX      PIC  X(01)   VALUE HIGH-VALUE.           
010617     03  W-W6D211KY-X.                                                    
010618         05  W-DAREGDAT-9KOMPL   PIC 9(08)   VALUE ZERO.                  
010620         05  W-TIKLOCK-9KOMPL    PIC S9(09)   VALUE ZERO COMP-3.          
010650     03  W-KDARBTYP-X.                                                    
010670         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
010690     03  W-IDPERSON-X.                                                    
010691         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(128).                              
011800 01  SSA2                        PIC X(64).                               
011810 01  SSA3                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012300     EJECT                                                                
012400*    ---  DLI INPUT-OUTPUT AREA                                           
013201 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013202     SKIP3                                                                
013203 01  DLI-IO-AREA.                                                         
013204     03  IO-AREA                 PIC X(1500)  VALUE SPACE.                
013205     SKIP3                                                                
013206     03  W6KVAH01 REDEFINES IO-AREA.                                      
013207*        05  -COPY W6D201                                                 
013208     EJECT                                                                
013210     03  W6KVAH11 REDEFINES IO-AREA.                                      
013220*        05  -COPY W6D211                                                 
013230     EJECT                                                                
013240*    ---  DLI INPUT-OUTPUT AREA2                                          
013250 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
013260     SKIP3                                                                
013270 01  DLI-IO-AREA2.                                                        
013280     03  IO-AREA2                PIC X(50)   VALUE SPACE.                 
013296     EJECT                                                                
013297 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P311'.         
013298                                                                          
013299 01  DLI-IO-P311.                                                         
013302*    03  -COPY WDP311                                                     
013303     EJECT                                                                
013310 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013601     EJECT                                                                
013602*01  -COPY W0008  -PRE USEA-                                              
013610     05  FILLER                  PIC X.                                   
013620     EJECT                                                                
013630*01  -COPY W0008  -PRE KVAH1-                                             
013640     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013710*01  -COPY W0008  -PRE KVAH2-                                             
013720     05  KEYFB-IDARTNR           PIC S9(09) COMP-3.                       
013721     05  KEYFB-IDLEVNR           PIC  X(05).                              
013730     EJECT                                                                
013800*01  -COPY W0008  -PRE ARTC-                                              
013801     05  FILLER                  PIC X.                                   
013802     EJECT                                                                
013803*01  -COPY W0008  -PRE WDP3-                                              
013804     05  FILLER                  PIC X.                                   
013805     EJECT                                                                
013806 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
013807                           KVAH1-PCB KVAH2-PCB ARTC-PCB WDP3-PCB.         
013810     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
013820                           KVAH1-PCB KVAH2-PCB ARTC-PCB WDP3-PCB.         
013900                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014300       PERFORM A-INIT                                                     
014400       PERFORM B-KOLLA-NYCKLAR                                            
014500       IF NYCKLAR-OK                                                      
014601         IF MFS-UPDATE                                                    
014602           PERFORM G-KOLLA-INPUT                                          
014603           IF INDATA-OK                                                   
014604             PERFORM H-UPPDATERA                                          
014606           END-IF                                                         
014610         ELSE                                                             
014701           IF MFS-FIRST                                                   
014702             PERFORM C-FOERSTA-SIDA                                       
014703           ELSE                                                           
014704             IF MFS-NEXT                                                  
014705               PERFORM D-NAESTA-SIDA                                      
014706             ELSE                                                         
014707               PERFORM E-SAMMA-SIDA                                       
014708             END-IF                                                       
014710           END-IF                                                         
015010         END-IF                                                           
015011         IF INDATA-OK                                                     
015020           PERFORM F-LAES-VISA-INFO                                       
015030         END-IF                                                           
015100       END-IF                                                             
015101       MOVE LENGTH OF MOD-W6O21201 TO MSG-KVLL                            
015110       ADD                    +4   TO MSG-KVLL                            
015300       PERFORM IMS-INSERT-MSG                                             
015400     END-IF                                                               
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200                                                                          
016300     IF MSG-DUBBLA-TRANSKODER                                             
016400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I21201                 
016500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
016600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016700     ELSE                                                                 
016800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I21201                  
016900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
017000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017100     END-IF                                                               
017200                                                                          
017300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
017500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017600                                                                          
017700     MOVE LOW-VALUE TO MSG-AREA                                           
017800     MOVE 'W6O212N1' TO MFS-IDMOD                                         
017900     MOVE '6212' TO MOD-IDTRANS                                           
018000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018100                                                                          
018200     IF EGEN-MID OR HELP-MID                                              
018300       CONTINUE                                                           
018400     ELSE                                                                 
018500       MOVE SPACE TO MFS-KDTRTYP                                          
018600       MOVE '7' TO MFS-IDPFK                                              
018700     END-IF                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 B-KOLLA-NYCKLAR SECTION.                                                 
020100                                                                          
020111     IF GODK-MID OR HELP-MID                                              
020112        CONTINUE                                                          
020113     ELSE                                                                 
020120        MOVE ALL '+'        TO MID-IDKVAINF-IN                            
020121                               MID-TIREGDAT-IN                            
020131        MOVE SPACE          TO MID-IDKVAINF-UT                            
020132                               MID-TIREGDAT-UT                            
020140     END-IF                                                               
020180                                                                          
020200     MOVE JA TO NYCKLAR-SW                                                
020301                                                                          
020305*    -- KONTROLL AV IDARTNR                                               
020306     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-IN                             
020307                                                                          
020309     MOVE ALL '+' TO MSGI-WMSGINIT                                        
020310     MOVE '001'             TO MSGI-KDCALL                                
020311     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020312     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020313     MOVE '6212'            TO MSGI-IDTRANS                               
020314                                                                          
020315     IF MFS-IDTRANS = '6212'                                              
020316     OR (MID-IDARTNR-IN NUMERIC                                           
020317     AND MID-IDARTNR-IN > ZERO)                                           
020318         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
020319     END-IF                                                               
020320     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020321                                                                          
020322     IF MSGI-IDLAND-SPR = 'GB'                                            
020323       MOVE +2 TO SPRAK-IX                                                
020324       MOVE 'GB ' TO MED-IDSKYLT                                          
020325     ELSE                                                                 
020326       MOVE +1 TO SPRAK-IX                                                
020327       MOVE 'S  ' TO MED-IDSKYLT                                          
020328     END-IF                                                               
020329                                                                          
020330     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
020331     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
020332                                                                          
020333     IF MID-IDARTNR-IN = ALL '+'                                          
020334       CONTINUE                                                           
020335     ELSE                                                                 
020336       MOVE SPACE TO MFS-KDTRTYP                                          
020337       MOVE '7' TO MFS-IDPFK                                              
020338       MOVE JA TO NYTT-ARTNR-SW                                           
020339       PERFORM MFS-RENSA-FAELT-IN                                         
020340     END-IF                                                               
020341                                                                          
020359                                                                          
020360*    -- KONTROLL AV IDKVAINF                                              
020362                                                                          
020363     MOVE MFS-RENSA-FAELT     TO MOD-IDKVAINF-IN                          
020364                                                                          
020365     IF MID-IDKVAINF-IN = ALL '+'                                         
020366       IF NYTT-ARTNR OR MFS-FIRST OR MFS-NEXT                             
020367         MOVE ZERO            TO WS-IDKVAINF                              
020368       ELSE                                                               
020369         MOVE MID-IDKVAINF-UT TO WS-IDKVAINF                              
020370       END-IF                                                             
020372     ELSE                                                                 
020373       IF MID-IDKVAINF-IN = '99'                                          
020374         MOVE NEJ             TO NYCKLAR-SW                               
020375       ELSE                                                               
020376         MOVE MID-IDKVAINF-IN TO WS-IDKVAINF                              
020377       END-IF                                                             
020382     END-IF                                                               
020383     INSPECT WS-IDKVAINF REPLACING LEADING SPACE BY ZERO                  
020384                                                                          
020385*    -- KONTROLL AV TIREGDAT                                              
020386     MOVE MFS-RENSA-FAELT     TO MOD-TIREGDAT-IN                          
020387                                                                          
020388     IF MID-TIREGDAT-IN = ALL '+'                                         
020389       IF NYTT-ARTNR                                                      
020390         MOVE ZERO            TO WS-TIREGDAT                              
020391       ELSE                                                               
020392         IF MID-IDKVAINF-IN = ALL '+'                                     
020393           MOVE MID-TIREGDAT-UT TO WS-TIREGDAT                            
020394         ELSE                                                             
020395           MOVE ZERO            TO WS-TIREGDAT                            
020396         END-IF                                                           
020397       END-IF                                                             
020398     ELSE                                                                 
020399       MOVE MID-TIREGDAT-IN   TO WS-TIREGDAT                              
020400     END-IF                                                               
020401     INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO                  
020416                                                                          
020417     IF  WS-IDARTNR  NUMERIC                                              
020418     AND WS-IDKVAINF NUMERIC                                              
020419     AND WS-TIREGDAT NUMERIC                                              
020420       IF WS-IDARTNR = ZERO                                               
020421          MOVE NEJ          TO NYCKLAR-SW                                 
020422       ELSE                                                               
020423          MOVE WS-IDARTNR   TO W-IDARTNR                                  
020424          MOVE WS-IDKVAINF  TO W-IDKVAINF                                 
020425          MOVE WS-TIREGDAT  TO W1-DAREGDAT                                
020426          IF WS-TIREGDAT NOT = ZERO                                       
020427            IF WS-TIREGDAT < 500000                                       
020428              MOVE 20       TO W1-DAREGDAT (1:2)                          
020429            ELSE                                                          
020430              IF WS-TIREGDAT < 999999                                     
020432                MOVE 19     TO W1-DAREGDAT (1:2)                          
020433              ELSE                                                        
020434                MOVE 99999999 TO W1-DAREGDAT                              
020435              END-IF                                                      
020436            END-IF                                                        
020437          END-IF                                                          
020439          COMPUTE W-DAREGDAT-9KOMPL = 99999999 - W1-DAREGDAT              
020440          MOVE WS-KDKVAINF  TO W-KDKVAINF                                 
020441       END-IF                                                             
020442     ELSE                                                                 
020443       MOVE NEJ             TO NYCKLAR-SW                                 
020444     END-IF                                                               
020445                                                                          
020446     IF GODK-MID OR NYCKLAR-OK                                            
020447       MOVE WS-IDARTNR      TO MOD-IDARTNR-UT                             
020448       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
020451       MOVE WS-IDKVAINF     TO MOD-IDKVAINF-UT                            
020452       INSPECT MOD-IDKVAINF-UT REPLACING LEADING ZERO BY SPACE            
020453       MOVE WS-TIREGDAT     TO MOD-TIREGDAT-UT                            
020454       IF MOD-TIREGDAT-UT = ZERO                                          
020455          INSPECT MOD-TIREGDAT-UT REPLACING LEADING ZERO BY SPACE         
020456       END-IF                                                             
020458     ELSE                                                                 
020459       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
020461                               MOD-IDKVAINF-UT                            
020462                               MOD-TIREGDAT-UT                            
020470     END-IF                                                               
020500                                                                          
020600     IF NYCKLAR-FEL                                                       
020700       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
020800       CALL WMEDKONV     USING MED-WMEDAREA                               
020900       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
021000       PERFORM MFS-RENSA-FAELT-IN                                         
021100       PERFORM MFS-RENSA-FAELT-UT                                         
021200     END-IF                                                               
021300     .                                                                    
021401     EJECT                                                                
021402 C-FOERSTA-SIDA SECTION.                                                  
021403                                                                          
021404     MOVE INF-FIRST-PAGE           TO MED-IDMFSINF                        
021405     CALL WMEDKONV              USING MED-WMEDAREA                        
021406     MOVE MED-MFSINF               TO MOD-TEMFSINF                        
021407                                                                          
021408*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
021411     MOVE ZERO                     TO MOD-TIREGDAT-9KOMPL-ENTER           
021413     MOVE ZERO                     TO MOD-TIKLOCK-9KOMPL-ENTER            
021415     MOVE ZERO                     TO MOD-TIREGDAT-9KOMPL-NEXT            
021417     MOVE ZERO                     TO MOD-TIKLOCK-9KOMPL-NEXT             
021418                                                                          
021440     PERFORM MFS-RENSA-FAELT-IN                                           
021452                                                                          
021453     IF MID-TIREGDAT-IN = ALL '+'                                         
021458       MOVE 0                         TO W-DAREGDAT-9KOMPL (1:1)          
021460       MOVE MID-TIKLOCK-9KOMPL-ENTER  TO W-TIKLOCK-9KOMPL                 
021461     END-IF                                                               
021462     .                                                                    
021463     EJECT                                                                
021464 D-NAESTA-SIDA SECTION.                                                   
021465                                                                          
021466     MOVE MID-TIREGDAT-9KOMPL-NEXT TO W-DAREGDAT-9KOMPL                   
021467     IF MID-TIREGDAT-9KOMPL-NEXT (1:1) = 0                                
021468       MOVE 80                     TO W-DAREGDAT-9KOMPL (1:2)             
021469     ELSE                                                                 
021470       MOVE 79                     TO W-DAREGDAT-9KOMPL (1:2)             
021471     END-IF                                                               
021472     MOVE MID-TIKLOCK-9KOMPL-NEXT  TO W-TIKLOCK-9KOMPL                    
021473                                                                          
021474     PERFORM MFS-RENSA-FAELT-IN                                           
021475     .                                                                    
021476     EJECT                                                                
021477 E-SAMMA-SIDA SECTION.                                                    
021478                                                                          
021479     IF EGEN-MID OR HELP-MID                                              
021480       IF MID-INPUT = ALL '+'                                             
021481         PERFORM MFS-RENSA-FAELT-IN                                       
021482       ELSE                                                               
021483         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
021484         CALL WMEDKONV    USING MED-WMEDAREA                              
021485         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
021486         PERFORM EA-MID-INDATA-TILL-MOD                                   
021487       END-IF                                                             
021488     ELSE                                                                 
021489       PERFORM MFS-RENSA-FAELT-IN                                         
021490     END-IF                                                               
021491                                                                          
021492     IF MID-IDKVAINF-IN NOT = ALL '+'                                     
021493        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
021494        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
021495        MOVE SPACE                 TO MOD-TIREGDAT-UT                     
021496     END-IF                                                               
021497                                                                          
021498     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021499        MOVE ZERO                  TO MID-TIREGDAT-9KOMPL-ENTER           
021500        MOVE ZERO                  TO MID-TIKLOCK-9KOMPL-ENTER            
021501     END-IF                                                               
021502                                                                          
021503     IF MID-TIREGDAT-IN = ALL '+'                                         
021504        MOVE MID-TIREGDAT-9KOMPL-ENTER TO W-DAREGDAT-9KOMPL               
021505        IF MID-TIREGDAT-9KOMPL-ENTER  (1:1) = 0                           
021506          MOVE 80                      TO W-DAREGDAT-9KOMPL (1:2)         
021507        ELSE                                                              
021508          MOVE 79                      TO W-DAREGDAT-9KOMPL (1:2)         
021509        END-IF                                                            
021510        MOVE MID-TIKLOCK-9KOMPL-ENTER  TO W-TIKLOCK-9KOMPL                
021511     END-IF                                                               
021512     .                                                                    
021513     EJECT                                                                
021514 EA-MID-INDATA-TILL-MOD SECTION.                                          
021515                                                                          
021537     IF MID-FLNYSEG NOT = ALL '+'                                         
021538        MOVE MID-FLNYSEG           TO MOD-FLNYSEG                         
021539        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNYSEG-ATTR                    
021540     ELSE                                                                 
021541        MOVE MFS-RENSA-FAELT       TO MOD-FLNYSEG                         
021542     END-IF                                                               
021550                                                                          
021551     IF MID-TEKVAINP NOT = ALL '+'                                        
021552        MOVE MID-TEKVAINP          TO MOD-TEKVAINP                        
021553        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINP-ATTR                   
021554     ELSE                                                                 
021555        MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINP                        
021556     END-IF                                                               
021557                                                                          
021558     IF MID-TIKLAR-QUAL NOT = ALL '+'                                     
021559        MOVE MID-TIKLAR-QUAL       TO MOD-TIKLAR-QUAL                     
021560        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIKLAR-QUAL-ATTR                
021561     ELSE                                                                 
021562        MOVE MFS-RENSA-FAELT       TO MOD-TIKLAR-QUAL                     
021563     END-IF                                                               
021564                                                                          
021565     IF MID-FLQPA NOT = ALL '+'                                           
021566        MOVE MID-FLQPA             TO MOD-FLQPA                           
021567        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLQPA-ATTR                      
021568     ELSE                                                                 
021569        MOVE MFS-RENSA-FAELT       TO MOD-FLQPA                           
021570     END-IF                                                               
021571                                                                          
021572     IF MID-FLTABORT NOT = ALL '+'                                        
021573        MOVE MID-FLTABORT          TO MOD-FLTABORT                        
021574        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTABORT-ATTR                   
021575     ELSE                                                                 
021576        MOVE MFS-RENSA-FAELT       TO MOD-FLTABORT                        
021577     END-IF                                                               
021578                                                                          
021579     IF MID-TEKVAINF-INT-RAD1 NOT = ALL '+'                               
021580       MOVE MID-TEKVAINF-INT-RAD1  TO MOD-TEKVAINF-INT-RAD1               
021581       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-INT-ATTR1               
021582     ELSE                                                                 
021583       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-INT-RAD1                
021584     END-IF                                                               
021585                                                                          
021586     IF MID-TEKVAINF-INT-RAD2 NOT = ALL '+'                               
021587       MOVE MID-TEKVAINF-INT-RAD2  TO MOD-TEKVAINF-INT-RAD2               
021588       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-INT-ATTR2               
021589     ELSE                                                                 
021590       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-INT-RAD2                
021591     END-IF                                                               
021592                                                                          
021593     IF MID-TEKVAINF-INT-RAD3 NOT = ALL '+'                               
021594       MOVE MID-TEKVAINF-INT-RAD3  TO MOD-TEKVAINF-INT-RAD3               
021595       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-INT-ATTR3               
021596     ELSE                                                                 
021597       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-INT-RAD3                
021598     END-IF                                                               
021599                                                                          
021600     IF MID-TEKVAINF-INT-RAD4 NOT = ALL '+'                               
021601       MOVE MID-TEKVAINF-INT-RAD4  TO MOD-TEKVAINF-INT-RAD4               
021602       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-INT-ATTR4               
021603     ELSE                                                                 
021604       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-INT-RAD4                
021605     END-IF                                                               
021606                                                                          
021607     IF MID-TEKVAINF-INT-RAD5 NOT = ALL '+'                               
021608       MOVE MID-TEKVAINF-INT-RAD5  TO MOD-TEKVAINF-INT-RAD5               
021609       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-INT-ATTR5               
021610     ELSE                                                                 
021611       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-INT-RAD5                
021612     END-IF                                                               
021613                                                                          
021614     IF MID-TEKVAINF-INT-RAD6 NOT = ALL '+'                               
021615       MOVE MID-TEKVAINF-INT-RAD6  TO MOD-TEKVAINF-INT-RAD6               
021616       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-INT-ATTR6               
021617     ELSE                                                                 
021618       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-INT-RAD6                
021619     END-IF                                                               
021620                                                                          
021621     IF MID-TEKVAINF-INT-RAD7 NOT = ALL '+'                               
021622       MOVE MID-TEKVAINF-INT-RAD7  TO MOD-TEKVAINF-INT-RAD7               
021623       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-INT-ATTR7               
021624     ELSE                                                                 
021625       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-INT-RAD7                
021626     END-IF                                                               
021627                                                                          
021628     IF MID-KDKVAINF NOT = ALL '+'                                        
021629        MOVE MID-KDKVAINF          TO MOD-KDKVAINF                        
021630        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDKVAINF-ATTR                   
021631     ELSE                                                                 
021632        MOVE MFS-RENSA-FAELT       TO MOD-KDKVAINF                        
021633     END-IF                                                               
021634                                                                          
021635     IF MID-TIKLAR-LEV NOT = ALL '+'                                      
021636        MOVE MID-TIKLAR-LEV        TO MOD-TIKLAR-LEV                      
021637        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TIKLAR-LEV-ATTR                 
021638     ELSE                                                                 
021639        MOVE MFS-RENSA-FAELT       TO MOD-TIKLAR-LEV                      
021640     END-IF                                                               
021641                                                                          
021642     IF MID-FLSTOCH NOT = ALL '+'                                         
021643        MOVE MID-FLSTOCH           TO MOD-FLSTOCH                         
021644        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSTOCH-ATTR                    
021645     ELSE                                                                 
021646        MOVE MFS-RENSA-FAELT       TO MOD-FLSTOCH                         
021647     END-IF                                                               
021648                                                                          
021649     IF MID-TEKVAINF-EXT-RAD1 NOT = ALL '+'                               
021650       MOVE MID-TEKVAINF-EXT-RAD1 TO MOD-TEKVAINF-EXT-RAD1                
021651       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-EXT-ATTR1               
021652     ELSE                                                                 
021653       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-EXT-RAD1                
021654     END-IF                                                               
021655                                                                          
021656     IF MID-TEKVAINF-EXT-RAD2 NOT = ALL '+'                               
021657       MOVE MID-TEKVAINF-EXT-RAD2 TO MOD-TEKVAINF-EXT-RAD2                
021658       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-EXT-ATTR2               
021659     ELSE                                                                 
021660       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-EXT-RAD2                
021661     END-IF                                                               
021662                                                                          
021663     IF MID-TEKVAINF-EXT-RAD3 NOT = ALL '+'                               
021664       MOVE MID-TEKVAINF-EXT-RAD3 TO MOD-TEKVAINF-EXT-RAD3                
021665       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-EXT-ATTR3               
021666     ELSE                                                                 
021667       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-EXT-RAD3                
021668     END-IF                                                               
021669                                                                          
021670     IF MID-TEKVAINF-EXT-RAD4 NOT = ALL '+'                               
021671       MOVE MID-TEKVAINF-EXT-RAD4 TO MOD-TEKVAINF-EXT-RAD4                
021672       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-EXT-ATTR4               
021673     ELSE                                                                 
021674       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-EXT-RAD4                
021675     END-IF                                                               
021676                                                                          
021677     IF MID-TEKVAINF-EXT-RAD5 NOT = ALL '+'                               
021678       MOVE MID-TEKVAINF-EXT-RAD5 TO MOD-TEKVAINF-EXT-RAD5                
021679       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-EXT-ATTR5               
021680     ELSE                                                                 
021681       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-EXT-RAD5                
021682     END-IF                                                               
021683                                                                          
021684     IF MID-TEKVAINF-EXT-RAD6 NOT = ALL '+'                               
021685       MOVE MID-TEKVAINF-EXT-RAD6 TO MOD-TEKVAINF-EXT-RAD6                
021686       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-EXT-ATTR6               
021687     ELSE                                                                 
021688       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-EXT-RAD6                
021689     END-IF                                                               
021690                                                                          
021691     IF MID-TEKVAINF-EXT-RAD7 NOT = ALL '+'                               
021692       MOVE MID-TEKVAINF-EXT-RAD7 TO MOD-TEKVAINF-EXT-RAD7                
021693       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEKVAINF-EXT-ATTR7               
021694     ELSE                                                                 
021695       MOVE MFS-RENSA-FAELT       TO MOD-TEKVAINF-EXT-RAD7                
021696     END-IF                                                               
021697     .                                                                    
021698     EJECT                                                                
021700 F-LAES-VISA-INFO SECTION.                                                
021800                                                                          
021910     PERFORM IMS-GU-KVAH-01                                               
022000                                                                          
022100     IF SEGMENT-SAKNAS                                                    
022210       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
022300       CALL WMEDKONV          USING MED-WMEDAREA                          
022400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
022500       PERFORM MFS-RENSA-FAELT-UT                                         
022600     ELSE                                                                 
022735       MOVE LOW-VALUE            TO W-IDKVAINF-MIN-X                      
022736       MOVE HIGH-VALUE           TO W-IDKVAINF-MAX-X                      
022737       IF WS-IDKVAINF > ZERO                                              
022738          MOVE W-IDKVAINF        TO W-IDKVAINF-MIN                        
022739                                    W-IDKVAINF-MAX                        
022740       END-IF                                                             
022749                                                                          
022750       MOVE LOW-VALUE            TO W-KDKVAINF-MIN-X                      
022751       MOVE HIGH-VALUE           TO W-KDKVAINF-MAX-X                      
022752                                                                          
022753       PERFORM IMS-GNP-KVAH-11                                            
022754                                                                          
022755       IF SEGMENT-FINNS                                                   
022756         IF INFO-IDKVAINF = 99                                            
022757           PERFORM IMS-GNP-KVAH-11                                        
022758         END-IF                                                           
022759       END-IF                                                             
022760       IF SEGMENT-FINNS                                                   
022761         MOVE INFO-DAREGDAT-9KOMPL(2:7) TO                                
022762                                    MOD-TIREGDAT-9KOMPL-ENTER             
022763         MOVE INFO-TIKLOCK-9KOMPL TO                                      
022764                                    MOD-TIKLOCK-9KOMPL-ENTER              
022765                                                                          
022770         COMPUTE W1-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL            
022777         MOVE W1-DAREGDAT (3:6)  TO MOD-TIREGDAT                          
022778                                    MOD-TIREGDAT-UT                       
022779         MOVE INFO-KDKVAINF      TO MOD-KDKVAINF                          
022780                                                                          
022781         MOVE INFO-KDPERSON      TO W-IDPERSON                            
022782         MOVE 'QUAL    '         TO W-KDARBTYP                            
022783         PERFORM IMS-GU-WDP311                                            
022784         IF SEGMENT-FINNS                                                 
022785           MOVE PERS-BEINIT(1:3) TO MOD-BEINIT                            
022786         ELSE                                                             
022787           MOVE INFO-KDPERSON    TO MOD-BEINIT                            
022788         END-IF                                                           
022789                                                                          
022790         MOVE INFO-TEKVAINP      TO MOD-TEKVAINP                          
022791         MOVE INFO-TIKLAR-QUAL   TO MOD-TIKLAR-QUAL                       
022792         MOVE INFO-TIKLAR-LEV    TO MOD-TIKLAR-LEV                        
022793         MOVE INFO-FLQPA         TO MOD-FLQPA                             
022794         MOVE INFO-FLSTOCH       TO MOD-FLSTOCH                           
022795         IF SPRAK-IX = 2                                                  
022796           IF INFO-FLQPA = JA                                             
022797             MOVE YES            TO MOD-FLQPA                             
022798           END-IF                                                         
022799           IF INFO-FLSTOCH = JA                                           
022800             MOVE YES            TO MOD-FLSTOCH                           
022801           END-IF                                                         
022802         END-IF                                                           
022803                                                                          
022804         MOVE INFO-TEKVAINF-INT(1)(1:63) TO MOD-TEKVAINF-INT-RAD1         
022805         MOVE INFO-TEKVAINF-INT(2)       TO MOD-TEKVAINF-INT-RAD2         
022806         MOVE INFO-TEKVAINF-INT(3)       TO MOD-TEKVAINF-INT-RAD3         
022807         MOVE INFO-TEKVAINF-INT(4)       TO MOD-TEKVAINF-INT-RAD4         
022808         MOVE INFO-TEKVAINF-INT(5)       TO MOD-TEKVAINF-INT-RAD5         
022809         MOVE INFO-TEKVAINF-INT(6)       TO MOD-TEKVAINF-INT-RAD6         
022810         MOVE INFO-TEKVAINF-INT(7)       TO MOD-TEKVAINF-INT-RAD7         
022811         MOVE INFO-TEKVAINF-EXT(1)(1:63) TO MOD-TEKVAINF-EXT-RAD1         
022812         MOVE INFO-TEKVAINF-EXT(2)       TO MOD-TEKVAINF-EXT-RAD2         
022813         MOVE INFO-TEKVAINF-EXT(3)       TO MOD-TEKVAINF-EXT-RAD3         
022814         MOVE INFO-TEKVAINF-EXT(4)       TO MOD-TEKVAINF-EXT-RAD4         
022815         MOVE INFO-TEKVAINF-EXT(5)       TO MOD-TEKVAINF-EXT-RAD5         
022816         MOVE INFO-TEKVAINF-EXT(6)       TO MOD-TEKVAINF-EXT-RAD6         
022817         MOVE INFO-TEKVAINF-EXT(7)       TO MOD-TEKVAINF-EXT-RAD7         
022818                                                                          
022819         MOVE INFO-IDKVAINF           TO W1-IDKVAINF                      
022820                                         MOD-IDKVAINF-VIEW                
022821                                         MOD-IDKVAINF-UT                  
022822         PERFORM IMS-GU-KVAH-22                                           
022823         IF SEGMENT-FINNS                                                 
022824            MOVE KEYFB-IDLEVNR        TO MOD-IDLEVNR                      
022825         ELSE                                                             
022826            MOVE ZERO                 TO MOD-IDLEVNR                      
022827         END-IF                                                           
022828                                                                          
022829         MOVE LOW-VALUE            TO W-IDKVAINF-MIN-X                    
022830         PERFORM IMS-GNP-KVAH-11                                          
022831       ELSE                                                               
022832         MOVE MFS-RENSA-FAELT         TO MOD-FLNYSEG                      
022833                                         MOD-IDKVAINF-VIEW                
022834                                         MOD-TIREGDAT                     
022835                                         MOD-KDPERSON                     
022836                                         MOD-BEINIT                       
022837                                         MOD-TEKVAINP                     
022838                                         MOD-TIKLAR-QUAL                  
022839                                         MOD-KDKVAINF                     
022840                                         MOD-TIKLAR-LEV                   
022841                                         MOD-FLSTOCH                      
022842                                         MOD-IDLEVNR                      
022843                                         MOD-FLQPA                        
022844                                         MOD-FLTABORT                     
022845                                         MOD-TEKVAINF-INT-RAD1            
022846                                         MOD-TEKVAINF-INT-RAD2            
022847                                         MOD-TEKVAINF-INT-RAD3            
022848                                         MOD-TEKVAINF-INT-RAD4            
022849                                         MOD-TEKVAINF-INT-RAD5            
022850                                         MOD-TEKVAINF-INT-RAD6            
022851                                         MOD-TEKVAINF-INT-RAD7            
022852                                         MOD-TEKVAINF-EXT-RAD1            
022853                                         MOD-TEKVAINF-EXT-RAD2            
022854                                         MOD-TEKVAINF-EXT-RAD3            
022855                                         MOD-TEKVAINF-EXT-RAD4            
022856                                         MOD-TEKVAINF-EXT-RAD5            
022857                                         MOD-TEKVAINF-EXT-RAD6            
022858                                         MOD-TEKVAINF-EXT-RAD7            
022859       END-IF                                                             
022860                                                                          
022861       IF SEGMENT-FINNS                                                   
022862         MOVE INFO-DAREGDAT-9KOMPL (2:7)                                  
022863                                       TO MOD-TIREGDAT-9KOMPL-NEXT        
022864         MOVE INFO-TIKLOCK-9KOMPL       TO MOD-TIKLOCK-9KOMPL-NEXT        
022865         IF MFS-UPDATE                                                    
022866           CONTINUE                                                       
022867         ELSE                                                             
022868           MOVE INF-MORE-INFO-EXISTS      TO MED-IDMFSINF                 
022869           CALL WMEDKONV               USING MED-WMEDAREA                 
022870           MOVE MED-MFSINF                TO MOD-TEMFSINF                 
022871         END-IF                                                           
022872       ELSE                                                               
022873         MOVE MOD-TIREGDAT-9KOMPL-ENTER                                   
022874                                       TO MOD-TIREGDAT-9KOMPL-NEXT        
022875         MOVE MOD-TIKLOCK-9KOMPL-ENTER TO MOD-TIKLOCK-9KOMPL-NEXT         
022876       END-IF                                                             
022880     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023902 G-KOLLA-INPUT SECTION.                                                   
023917                                                                          
023918     MOVE JA  TO INDATA-SW                                                
023919     IF MID-INPUT = ALL '+' AND MID-FLNYSEG = ALL '+'                     
023920       MOVE ERR-PF11-AND-NO-DATA         TO MED-IDMFSFEL                  
023921       CALL WMEDKONV USING MED-WMEDAREA                                   
023922       MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                  
023923       PERFORM MFS-ROER-EJ-FAELT-IN                                       
023924       PERFORM MFS-ROER-EJ-FAELT-UT                                       
023925       MOVE NEJ                          TO INDATA-SW                     
023927     ELSE                                                                 
023928       PERFORM IMS-GU-ARTC-01                                             
023929       IF SEGMENT-SAKNAS                                                  
023930         MOVE NEJ                       TO IDARTNR-SW                     
023931         MOVE NEJ                       TO INDATA-SW                      
023933       ELSE                                                               
023939                                                                          
023940         PERFORM IMS-GU-KVAH-01                                           
023941         IF SEGMENT-SAKNAS  OR (SEGMENT-FINNS AND                         
023942                               ART-KDKVATYP = 0)                          
023943           MOVE NEJ                         TO KKOD-SW                    
023944                                               INDATA-SW                  
023945         END-IF                                                           
023946                                                                          
023947         IF MID-KDPERSON NOT = ALL '+'                                    
023971           IF MID-KDPERSON NUMERIC                                        
023972             MOVE MID-KDPERSON            TO W-IDPERSON                   
023973             MOVE 'QUAL    '              TO W-KDARBTYP                   
023974             PERFORM IMS-GU-WDP311                                        
023975             IF SEGMENT-FINNS                                             
023976               MOVE PERS-BEINIT(1:3)      TO WS-BEINIT                    
023977               MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDPERSON-ATTR            
023978             ELSE                                                         
023979               MOVE NEJ                   TO INDATA-SW                    
023980               MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDPERSON-ATTR            
023981             END-IF                                                       
023982           ELSE                                                           
023983             MOVE NEJ                     TO INDATA-SW                    
023984             MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDPERSON-ATTR            
023985           END-IF                                                         
023986         ELSE                                                             
023987           IF MID-FLNYSEG = JA OR YES                                     
023988             MOVE NEJ                     TO INDATA-SW                    
023989             MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDPERSON-ATTR            
023990           END-IF                                                         
023991         END-IF                                                           
023992                                                                          
024000         IF MID-FLNYSEG NOT = ALL '+'                                     
024007           IF MID-FLNYSEG = JA OR YES                                     
024008             IF  (MID-FLTABORT = JA OR YES)                               
024010               MOVE NEJ TO INDATA-SW                                      
024011             ELSE                                                         
024012               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNYSEG-ATTR              
024013             END-IF                                                       
024025           ELSE                                                           
024032             MOVE NEJ TO INDATA-SW                                        
024033             MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLNYSEG-ATTR               
024034           END-IF                                                         
024035         END-IF                                                           
024036                                                                          
024037         IF MID-FLTABORT NOT = ALL '+'                                    
024038           IF MID-FLTABORT = JA OR YES                                    
024039             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTABORT-ATTR               
024041           ELSE                                                           
024042             MOVE NEJ                  TO INDATA-SW                       
024043             MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTABORT-ATTR               
024044           END-IF                                                         
024045         END-IF                                                           
024046                                                                          
024047         IF MID-KDKVAINF NOT = ALL '+'                                    
024048           IF MID-KDKVAINF = 'R' OR ' '                                   
024049             MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KDKVAINF-ATTR             
024050           ELSE                                                           
024051             MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDKVAINF-ATTR             
024052             MOVE NEJ                    TO INDATA-SW                     
024053           END-IF                                                         
024054         END-IF                                                           
024055                                                                          
024056         IF MID-TIKLAR-QUAL NOT = ALL '+'                                 
024057           IF MID-TIKLAR-QUAL NUMERIC                                     
024058             MOVE MFS-NUM-FAELT-RAETT    TO MOD-TIKLAR-QUAL-ATTR          
024060           ELSE                                                           
024061             MOVE MFS-NUM-FAELT-FEL      TO MOD-TIKLAR-QUAL-ATTR          
024062             MOVE NEJ                    TO INDATA-SW                     
024063           END-IF                                                         
024064         END-IF                                                           
024065                                                                          
024066         IF MID-TIKLAR-LEV NOT = ALL '+'                                  
024067           IF MID-TIKLAR-LEV NUMERIC                                      
024068             MOVE MFS-NUM-FAELT-RAETT    TO MOD-TIKLAR-LEV-ATTR           
024072           ELSE                                                           
024073             MOVE MFS-NUM-FAELT-FEL      TO MOD-TIKLAR-LEV-ATTR           
024074             MOVE NEJ                    TO INDATA-SW                     
024075           END-IF                                                         
024076         END-IF                                                           
024077                                                                          
024078         IF MID-FLSTOCH NOT = ALL '+'                                     
024079           IF MID-FLSTOCH = JA OR YES OR NEJ                              
024080             MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLSTOCH-ATTR              
024081           ELSE                                                           
024082             MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLSTOCH-ATTR             
024083             MOVE NEJ                    TO INDATA-SW                     
024084           END-IF                                                         
024085         END-IF                                                           
024086                                                                          
024087         IF MID-FLQPA NOT = ALL '+'                                       
024088           IF MID-FLQPA = JA OR YES                                       
024089             MOVE MFS-ALFA-FAELT-RAETT   TO MOD-FLQPA-ATTR                
024090           ELSE                                                           
024091             MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLQPA-ATTR                
024092             MOVE NEJ                    TO INDATA-SW                     
024093           END-IF                                                         
024094         END-IF                                                           
024095                                                                          
024096         IF INDATA-OK                                                     
024097           PERFORM IMS-GHU-KVAH-11                                        
024098           IF SEGMENT-FINNS                                               
024099             MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL               
024100             MOVE INFO-DAREGDAT-9KOMPL(2:7)                               
024101                                      TO MOD-TIREGDAT-9KOMPL-ENTER        
024102             MOVE INFO-TIKLOCK-9KOMPL TO W-TIKLOCK-9KOMPL                 
024103                                      MOD-TIKLOCK-9KOMPL-ENTER            
024104                                                                          
024105             IF MID-FLTABORT = JA OR YES                                  
024106               MOVE INFO-IDKVAINF          TO W1-IDKVAINF                 
024108               PERFORM IMS-GU-KVAH-22                                     
024109               IF SEGMENT-FINNS                                           
024110** FÅR INTE TA BORT EN NOT OM DEN ÄR KOPPLAD TILL SPECIALKONTROLL         
024111                 MOVE NEJ                  TO INDATA-SW                   
024112                 MOVE NEJ                  TO SPEC-SW                     
024113                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTABORT-ATTR           
024114               END-IF                                                     
024115             END-IF                                                       
024116                                                                          
024117           ELSE                                                           
024118             IF MID-FLNYSEG = JA OR YES                                   
024119               CONTINUE                                                   
024120             ELSE                                                         
024121               MOVE NEJ          TO INDATA-SW                             
024122                                    FL-KEY                                
024123             END-IF                                                       
024124           END-IF                                                         
024125         END-IF                                                           
024126       END-IF                                                             
024127                                                                          
024128       IF INDATA-FEL                                                      
024129         IF IDARTNR-FEL                                                   
024130            MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                  
024131         ELSE                                                             
024132          IF KKOD-FEL                                                     
024133            MOVE INSPECT-CODE-MISSING    TO MED-IDMFSFEL                  
024134          ELSE                                                            
024135            IF FL-KEY = NEJ                                               
024136              MOVE KEYS-MISSING          TO MED-IDMFSFEL                  
024137            ELSE                                                          
024138              IF SPEC-FEL                                                 
024139                MOVE SPEC-CONTROL         TO MED-IDMFSFEL                 
024140              ELSE                                                        
024141                MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                 
024142              END-IF                                                      
024143            END-IF                                                        
024144          END-IF                                                          
024145         END-IF                                                           
024146         CALL WMEDKONV                USING MED-WMEDAREA                  
024147         MOVE MED-MFSFEL                 TO MOD-TEMFSFEL                  
024148         PERFORM GA-MID-INDATA-TILL-MOD                                   
024149         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024150         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024151       END-IF                                                             
024152     END-IF                                                               
024153     .                                                                    
024154     EJECT                                                                
024155 GA-MID-INDATA-TILL-MOD SECTION.                                          
024156                                                                          
024157     IF MID-FLNYSEG NOT = ALL '+'                                         
024158        MOVE MID-FLNYSEG           TO MOD-FLNYSEG                         
024159     END-IF                                                               
024160                                                                          
024161     IF MID-TEKVAINP NOT = ALL '+'                                        
024162        MOVE MID-TEKVAINP          TO MOD-TEKVAINP                        
024163        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINP-ATTR                   
024164     END-IF                                                               
024165                                                                          
024166     IF MID-TIKLAR-QUAL NOT = ALL '+'                                     
024167        MOVE MID-TIKLAR-QUAL       TO MOD-TIKLAR-QUAL                     
024168     END-IF                                                               
024169                                                                          
024170     IF MID-FLQPA NOT = ALL '+'                                           
024171        MOVE MID-FLQPA             TO MOD-FLQPA                           
024172     END-IF                                                               
024173                                                                          
024174     IF MID-FLTABORT NOT = ALL '+'                                        
024175        MOVE MID-FLTABORT          TO MOD-FLTABORT                        
024176     END-IF                                                               
024177                                                                          
024178     IF MID-TEKVAINF-INT-RAD1 NOT = ALL '+'                               
024179       MOVE MID-TEKVAINF-INT-RAD1  TO MOD-TEKVAINF-INT-RAD1               
024180       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEKVAINF-INT-ATTR1              
024181     END-IF                                                               
024182                                                                          
024183     IF MID-TEKVAINF-INT-RAD2 NOT = ALL '+'                               
024184       MOVE MID-TEKVAINF-INT-RAD2  TO MOD-TEKVAINF-INT-RAD2               
024185       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEKVAINF-INT-ATTR2              
024186     END-IF                                                               
024187                                                                          
024188     IF MID-TEKVAINF-INT-RAD3 NOT = ALL '+'                               
024189       MOVE MID-TEKVAINF-INT-RAD3  TO MOD-TEKVAINF-INT-RAD3               
024190       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEKVAINF-INT-ATTR3              
024191     END-IF                                                               
024192                                                                          
024193     IF MID-TEKVAINF-INT-RAD4 NOT = ALL '+'                               
024194       MOVE MID-TEKVAINF-INT-RAD4  TO MOD-TEKVAINF-INT-RAD4               
024195       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEKVAINF-INT-ATTR4              
024196     END-IF                                                               
024197                                                                          
024198     IF MID-TEKVAINF-INT-RAD5 NOT = ALL '+'                               
024199       MOVE MID-TEKVAINF-INT-RAD5  TO MOD-TEKVAINF-INT-RAD5               
024200       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEKVAINF-INT-ATTR5              
024201     END-IF                                                               
024202                                                                          
024203     IF MID-TEKVAINF-INT-RAD6 NOT = ALL '+'                               
024204       MOVE MID-TEKVAINF-INT-RAD6  TO MOD-TEKVAINF-INT-RAD6               
024205       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEKVAINF-INT-ATTR6              
024206     END-IF                                                               
024207                                                                          
024208     IF MID-TEKVAINF-INT-RAD7 NOT = ALL '+'                               
024209       MOVE MID-TEKVAINF-INT-RAD7  TO MOD-TEKVAINF-INT-RAD7               
024210       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEKVAINF-INT-ATTR7              
024211     END-IF                                                               
024212                                                                          
024213     IF MID-KDKVAINF NOT = ALL '+'                                        
024214        MOVE MID-KDKVAINF          TO MOD-KDKVAINF                        
024215     END-IF                                                               
024216                                                                          
024217     IF MID-TIKLAR-LEV NOT = ALL '+'                                      
024218        MOVE MID-TIKLAR-LEV        TO MOD-TIKLAR-LEV                      
024219     END-IF                                                               
024220                                                                          
024221     IF MID-FLSTOCH NOT = ALL '+'                                         
024222        MOVE MID-FLSTOCH           TO MOD-FLSTOCH                         
024225     END-IF                                                               
024226                                                                          
024227     IF MID-TEKVAINF-EXT-RAD1 NOT = ALL '+'                               
024228       MOVE MID-TEKVAINF-EXT-RAD1 TO MOD-TEKVAINF-EXT-RAD1                
024229       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-EXT-ATTR1               
024232     END-IF                                                               
024233                                                                          
024234     IF MID-TEKVAINF-EXT-RAD2 NOT = ALL '+'                               
024235       MOVE MID-TEKVAINF-EXT-RAD2 TO MOD-TEKVAINF-EXT-RAD2                
024236       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-EXT-ATTR2               
024239     END-IF                                                               
024240                                                                          
024241     IF MID-TEKVAINF-EXT-RAD3 NOT = ALL '+'                               
024242       MOVE MID-TEKVAINF-EXT-RAD3 TO MOD-TEKVAINF-EXT-RAD3                
024243       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-EXT-ATTR3               
024246     END-IF                                                               
024247                                                                          
024248     IF MID-TEKVAINF-EXT-RAD4 NOT = ALL '+'                               
024249       MOVE MID-TEKVAINF-EXT-RAD4 TO MOD-TEKVAINF-EXT-RAD4                
024250       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-EXT-ATTR4               
024253     END-IF                                                               
024254                                                                          
024255     IF MID-TEKVAINF-EXT-RAD5 NOT = ALL '+'                               
024256       MOVE MID-TEKVAINF-EXT-RAD5 TO MOD-TEKVAINF-EXT-RAD5                
024257       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-EXT-ATTR5               
024260     END-IF                                                               
024261                                                                          
024262     IF MID-TEKVAINF-EXT-RAD6 NOT = ALL '+'                               
024263       MOVE MID-TEKVAINF-EXT-RAD6 TO MOD-TEKVAINF-EXT-RAD6                
024264       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-EXT-ATTR6               
024267     END-IF                                                               
024268                                                                          
024269     IF MID-TEKVAINF-EXT-RAD7 NOT = ALL '+'                               
024270       MOVE MID-TEKVAINF-EXT-RAD7 TO MOD-TEKVAINF-EXT-RAD7                
024271       MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEKVAINF-EXT-ATTR7               
024274     END-IF                                                               
024275     .                                                                    
024276     EJECT                                                                
024277 H-UPPDATERA SECTION.                                                     
024278                                                                          
024279     IF MID-FLTABORT = JA OR YES                                          
024280       PERFORM IMS-DLET-KVAH                                              
024281     ELSE                                                                 
024282                                                                          
024283       IF MID-FLNYSEG = JA OR YES                                         
024284         PERFORM HA-NYUPPLAGG-KVAH-11                                     
024285       ELSE                                                               
024286         IF MID-KDPERSON NOT = ALL '+'                                    
024287           MOVE W-IDPERSON              TO INFO-KDPERSON                  
024288         END-IF                                                           
024289                                                                          
024290         IF MID-KDKVAINF NOT = ALL '+'                                    
024291           MOVE MID-KDKVAINF          TO INFO-KDKVAINF                    
024292         END-IF                                                           
024293                                                                          
024294         IF MID-TEKVAINP NOT = ALL '+'                                    
024295           MOVE MID-TEKVAINP          TO INFO-TEKVAINP                    
024296         END-IF                                                           
024297                                                                          
024298         IF MID-TIKLAR-QUAL NOT = ALL '+'                                 
024299           MOVE MID-TIKLAR-QUAL       TO INFO-TIKLAR-QUAL                 
024300         END-IF                                                           
024301                                                                          
024302         IF MID-FLQPA NOT = ALL '+'                                       
024303           IF MID-FLQPA = YES                                             
024304             MOVE JA                  TO INFO-FLQPA                       
024305           ELSE                                                           
024306             MOVE MID-FLQPA           TO INFO-FLQPA                       
024307           END-IF                                                         
024308         END-IF                                                           
024309                                                                          
024310         IF MID-TIKLAR-LEV NOT = ALL '+'                                  
024311           MOVE MID-TIKLAR-LEV        TO INFO-TIKLAR-LEV                  
024312         END-IF                                                           
024313                                                                          
024314         IF MID-FLSTOCH NOT = ALL '+'                                     
024315           IF MID-FLSTOCH = YES                                           
024316             MOVE JA                    TO INFO-FLSTOCH                   
024317           ELSE                                                           
024318             MOVE MID-FLSTOCH           TO INFO-FLSTOCH                   
024319           END-IF                                                         
024320         END-IF                                                           
024321                                                                          
024322         IF MID-TEKVAINF-INT-RAD1 NOT = ALL '+'                           
024323           MOVE MID-TEKVAINF-INT-RAD1  TO INFO-TEKVAINF-INT(1)            
024324         END-IF                                                           
024325         IF MID-TEKVAINF-INT-RAD2 NOT = ALL '+'                           
024326           MOVE MID-TEKVAINF-INT-RAD2  TO INFO-TEKVAINF-INT(2)            
024327         END-IF                                                           
024328         IF MID-TEKVAINF-INT-RAD3 NOT = ALL '+'                           
024329           MOVE MID-TEKVAINF-INT-RAD3  TO INFO-TEKVAINF-INT(3)            
024330         END-IF                                                           
024331         IF MID-TEKVAINF-INT-RAD4 NOT = ALL '+'                           
024332           MOVE MID-TEKVAINF-INT-RAD4  TO INFO-TEKVAINF-INT(4)            
024333         END-IF                                                           
024334         IF MID-TEKVAINF-INT-RAD5 NOT = ALL '+'                           
024335           MOVE MID-TEKVAINF-INT-RAD5  TO INFO-TEKVAINF-INT(5)            
024336         END-IF                                                           
024337         IF MID-TEKVAINF-INT-RAD6 NOT = ALL '+'                           
024338           MOVE MID-TEKVAINF-INT-RAD6  TO INFO-TEKVAINF-INT(6)            
024339         END-IF                                                           
024340         IF MID-TEKVAINF-INT-RAD7 NOT = ALL '+'                           
024341           MOVE MID-TEKVAINF-INT-RAD7  TO INFO-TEKVAINF-INT(7)            
024342         END-IF                                                           
024343         IF MID-TEKVAINF-EXT-RAD1 NOT = ALL '+'                           
024344           MOVE MID-TEKVAINF-EXT-RAD1  TO INFO-TEKVAINF-EXT(1)            
024345         END-IF                                                           
024346         IF MID-TEKVAINF-EXT-RAD2 NOT = ALL '+'                           
024347           MOVE MID-TEKVAINF-EXT-RAD2  TO INFO-TEKVAINF-EXT(2)            
024348         END-IF                                                           
024349         IF MID-TEKVAINF-EXT-RAD3 NOT = ALL '+'                           
024350           MOVE MID-TEKVAINF-EXT-RAD3  TO INFO-TEKVAINF-EXT(3)            
024351         END-IF                                                           
024352         IF MID-TEKVAINF-EXT-RAD4 NOT = ALL '+'                           
024353           MOVE MID-TEKVAINF-EXT-RAD4  TO INFO-TEKVAINF-EXT(4)            
024354         END-IF                                                           
024355         IF MID-TEKVAINF-EXT-RAD5 NOT = ALL '+'                           
024356           MOVE MID-TEKVAINF-EXT-RAD5  TO INFO-TEKVAINF-EXT(5)            
024357         END-IF                                                           
024358         IF MID-TEKVAINF-EXT-RAD6 NOT = ALL '+'                           
024359           MOVE MID-TEKVAINF-EXT-RAD6  TO INFO-TEKVAINF-EXT(6)            
024360         END-IF                                                           
024361         IF MID-TEKVAINF-EXT-RAD7 NOT = ALL '+'                           
024362           MOVE MID-TEKVAINF-EXT-RAD7  TO INFO-TEKVAINF-EXT(7)            
024363         END-IF                                                           
024364                                                                          
024365         PERFORM IMS-REPL-KVAH                                            
024366       END-IF                                                             
024367                                                                          
024368       MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                     
024369       MOVE INFO-DAREGDAT-9KOMPL (2:7)                                    
024370                                 TO MOD-TIREGDAT-9KOMPL-ENTER             
024371       MOVE INFO-TIKLOCK-9KOMPL  TO MOD-TIKLOCK-9KOMPL-ENTER              
024372                                         W-TIKLOCK-9KOMPL                 
024373     END-IF                                                               
024374                                                                          
024375     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
024376     CALL WMEDKONV             USING MED-WMEDAREA                         
024377     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
024378     PERFORM MFS-FORM-ATTR                                                
024379     PERFORM MFS-RENSA-FAELT-IN                                           
024380     .                                                                    
024381     EJECT                                                                
024382 HA-NYUPPLAGG-KVAH-11 SECTION.                                            
024383                                                                          
024384     PERFORM IMS-GNP-KVAH-11-FIRST                                        
024385                                                                          
024386     IF SEGMENT-FINNS                                                     
024387       IF INFO-IDKVAINF = 99                                              
024388         PERFORM IMS-GNP-KVAH-11-NEXT                                     
024389       END-IF                                                             
024390       IF SEGMENT-FINNS                                                   
024391         MOVE INFO-IDKVAINF     TO W-IDKVAINF                             
024392         ADD  +1                TO W-IDKVAINF                             
024393       ELSE                                                               
024394         MOVE +1                TO W-IDKVAINF                             
024395       END-IF                                                             
024396     ELSE                                                                 
024397       MOVE +1                  TO W-IDKVAINF                             
024398     END-IF                                                               
024399                                                                          
024400     PERFORM IMS-GHU-KVAH-11                                              
024401                                                                          
024402     IF SEGMENT-FINNS                                                     
024403        MOVE +1                  TO IX1                                   
024404        PERFORM UNTIL IX1 = +98                                           
024405          ADD +1                 TO W-IDKVAINF                            
024406          PERFORM IMS-GHU-KVAH-11                                         
024407          IF SEGMENT-FINNS                                                
024408            ADD +1               TO IX1                                   
024409          ELSE                                                            
024410            MOVE +99             TO IX1                                   
024411          END-IF                                                          
024412        END-PERFORM                                                       
024413     END-IF                                                               
024414                                                                          
024415     IF IX1 = +100                                                        
024416        MOVE ERR-RENSA-ARTIKEL-INFO TO MED-IDMFSFEL                       
024417        CALL WMEDKONV            USING MED-WMEDAREA                       
024418        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
024419        PERFORM MFS-ROER-EJ-FAELT-UT                                      
024420        PERFORM MFS-ROER-EJ-FAELT-IN                                      
024421     ELSE                                                                 
024422                                                                          
024423        MOVE FUNCTION CURRENT-DATE (1:8) TO  W1-DAREGDAT                  
024425        COMPUTE INFO-DAREGDAT-9KOMPL = 99999999 -                         
024426                                            W1-DAREGDAT                   
024427                                                                          
024428        ACCEPT W1-TIKLOCK FROM TIME                                       
024429               COMPUTE INFO-TIKLOCK-9KOMPL =                              
024430                      +999999999 - W1-TIKLOCK                             
024431                                                                          
024432        MOVE W1-DAREGDAT         TO INFO-TIREGDAT                         
024433        MOVE W-IDKVAINF          TO INFO-IDKVAINF                         
024434        MOVE W-IDPERSON          TO INFO-KDPERSON                         
024435                                                                          
024436        MOVE ZERO  TO INFO-TIKLAR-QUAL                                    
024437                      INFO-TIKLAR-LEV                                     
024438        MOVE SPACE TO INFO-KDKVAINF                                       
024439                      INFO-TEKVAINP                                       
024440                      INFO-FLQPA                                          
024441                      INFO-FLSTOCH                                        
024442                      INFO-TEKVAINF-INT(1)                                
024443                      INFO-TEKVAINF-INT(2)                                
024444                      INFO-TEKVAINF-INT(3)                                
024445                      INFO-TEKVAINF-INT(4)                                
024446                      INFO-TEKVAINF-INT(5)                                
024447                      INFO-TEKVAINF-INT(6)                                
024448                      INFO-TEKVAINF-INT(7)                                
024449                      INFO-TEKVAINF-EXT(1)                                
024450                      INFO-TEKVAINF-EXT(2)                                
024451                      INFO-TEKVAINF-EXT(3)                                
024452                      INFO-TEKVAINF-EXT(4)                                
024453                      INFO-TEKVAINF-EXT(5)                                
024454                      INFO-TEKVAINF-EXT(6)                                
024455                      INFO-TEKVAINF-EXT(7)                                
024456                                                                          
024457        PERFORM IMS-ISRT-KVAH-11                                          
024458                                                                          
024459     END-IF                                                               
024460     .                                                                    
024461     EJECT                                                                
024462 MFS-RENSA-FAELT-UT SECTION.                                              
024463                                                                          
024464*    --- ALLA UTDATA-FÄLT                                                 
024470*    --- INKL. BLÄDDRINGSNYCKLAR                                          
024716     MOVE MFS-RENSA-FAELT TO MOD-FLNYSEG                                  
024717                             MOD-IDKVAINF-VIEW                            
024718                             MOD-TIREGDAT                                 
024719                             MOD-KDPERSON                                 
024720                             MOD-BEINIT                                   
024722                             MOD-TEKVAINP                                 
024723                             MOD-TIKLAR-QUAL                              
024724                             MOD-KDKVAINF                                 
024725                             MOD-TIKLAR-LEV                               
024726                             MOD-FLSTOCH                                  
024728                             MOD-IDLEVNR                                  
024729                             MOD-FLQPA                                    
024730                             MOD-FLTABORT                                 
024731                             MOD-TIREGDAT-9KOMPL-ENTER                    
024732                             MOD-TIKLOCK-9KOMPL-ENTER                     
024733                             MOD-TIREGDAT-9KOMPL-NEXT                     
024734                             MOD-TIKLOCK-9KOMPL-NEXT                      
024735                             MOD-TEKVAINF-INT-RAD1                        
024736                             MOD-TEKVAINF-INT-RAD2                        
024737                             MOD-TEKVAINF-INT-RAD3                        
024738                             MOD-TEKVAINF-INT-RAD4                        
024739                             MOD-TEKVAINF-INT-RAD5                        
024740                             MOD-TEKVAINF-INT-RAD6                        
024741                             MOD-TEKVAINF-INT-RAD7                        
024742                             MOD-TEKVAINF-EXT-RAD1                        
024743                             MOD-TEKVAINF-EXT-RAD2                        
024750                             MOD-TEKVAINF-EXT-RAD3                        
024760                             MOD-TEKVAINF-EXT-RAD4                        
024770                             MOD-TEKVAINF-EXT-RAD5                        
024780                             MOD-TEKVAINF-EXT-RAD6                        
024790                             MOD-TEKVAINF-EXT-RAD7                        
024800     .                                                                    
024900     EJECT                                                                
025100 MFS-RENSA-FAELT-IN SECTION.                                              
025200                                                                          
025300*    --- ALLA INDATA-FÄLT                                                 
025400     MOVE MFS-RENSA-FAELT TO MOD-FLNYSEG                                  
025500                             MOD-KDPERSON                                 
025520                             MOD-TEKVAINP                                 
025530                             MOD-TIKLAR-QUAL                              
025531                             MOD-KDKVAINF                                 
025532                             MOD-TIKLAR-LEV                               
025533                             MOD-FLSTOCH                                  
025540                             MOD-FLQPA                                    
025550                             MOD-FLTABORT                                 
025570                             MOD-TEKVAINF-INT-RAD1                        
025571                             MOD-TEKVAINF-INT-RAD2                        
025572                             MOD-TEKVAINF-INT-RAD3                        
025573                             MOD-TEKVAINF-INT-RAD4                        
025574                             MOD-TEKVAINF-INT-RAD5                        
025575                             MOD-TEKVAINF-INT-RAD6                        
025576                             MOD-TEKVAINF-INT-RAD7                        
025580                             MOD-TEKVAINF-EXT-RAD1                        
025590                             MOD-TEKVAINF-EXT-RAD2                        
025591                             MOD-TEKVAINF-EXT-RAD3                        
025592                             MOD-TEKVAINF-EXT-RAD4                        
025593                             MOD-TEKVAINF-EXT-RAD5                        
025594                             MOD-TEKVAINF-EXT-RAD6                        
025595                             MOD-TEKVAINF-EXT-RAD7                        
025600     .                                                                    
025700     EJECT                                                                
025800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
025900                                                                          
026000*    --- ALLA UTDATA-FÄLT                                                 
026110*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
026111     MOVE MFS-ROER-EJ-FAELT TO MOD-FLNYSEG                                
026112                               MOD-IDARTNR-UT                             
026113                               MOD-IDKVAINF-UT                            
026114                               MOD-TIREGDAT-UT                            
026120                               MOD-IDKVAINF-VIEW                          
026130                               MOD-TIREGDAT                               
026140                               MOD-BEINIT                                 
026150                               MOD-TEKVAINP                               
026160                               MOD-IDLEVNR                                
026170                               MOD-TIKLAR-QUAL                            
026180                               MOD-FLQPA                                  
026181                               MOD-FLTABORT                               
026190                               MOD-KDKVAINF                               
026200                               MOD-TIKLAR-LEV                             
026300                               MOD-FLSTOCH                                
026400                               MOD-TIREGDAT-9KOMPL-ENTER                  
026500                               MOD-TIKLOCK-9KOMPL-ENTER                   
026510                               MOD-TIREGDAT-9KOMPL-NEXT                   
026511                               MOD-TIKLOCK-9KOMPL-NEXT                    
026514                               MOD-TEKVAINF-INT-RAD1                      
026515                               MOD-TEKVAINF-INT-RAD2                      
026516                               MOD-TEKVAINF-INT-RAD3                      
026517                               MOD-TEKVAINF-INT-RAD4                      
026518                               MOD-TEKVAINF-INT-RAD5                      
026519                               MOD-TEKVAINF-INT-RAD6                      
026520                               MOD-TEKVAINF-INT-RAD7                      
026521                               MOD-TEKVAINF-EXT-RAD1                      
026522                               MOD-TEKVAINF-EXT-RAD2                      
026523                               MOD-TEKVAINF-EXT-RAD3                      
026524                               MOD-TEKVAINF-EXT-RAD4                      
026525                               MOD-TEKVAINF-EXT-RAD5                      
026526                               MOD-TEKVAINF-EXT-RAD6                      
026527                               MOD-TEKVAINF-EXT-RAD7                      
026528     .                                                                    
026530     EJECT                                                                
026800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
026900                                                                          
027000*    --- ALLA INDATA-FÄLT                                                 
027010     MOVE MFS-ROER-EJ-FAELT TO MOD-FLNYSEG                                
027020                               MOD-KDPERSON                               
027030                               MOD-TEKVAINP                               
027040                               MOD-TIKLAR-QUAL                            
027050                               MOD-FLQPA                                  
027051                               MOD-FLTABORT                               
027060                               MOD-KDKVAINF                               
027070                               MOD-TIKLAR-LEV                             
027080                               MOD-FLSTOCH                                
027092                               MOD-TEKVAINF-INT-RAD1                      
027093                               MOD-TEKVAINF-INT-RAD2                      
027094                               MOD-TEKVAINF-INT-RAD3                      
027095                               MOD-TEKVAINF-INT-RAD4                      
027096                               MOD-TEKVAINF-INT-RAD5                      
027097                               MOD-TEKVAINF-INT-RAD6                      
027098                               MOD-TEKVAINF-INT-RAD7                      
027099                               MOD-TEKVAINF-EXT-RAD1                      
027100                               MOD-TEKVAINF-EXT-RAD2                      
027200                               MOD-TEKVAINF-EXT-RAD3                      
027210                               MOD-TEKVAINF-EXT-RAD4                      
027220                               MOD-TEKVAINF-EXT-RAD5                      
027230                               MOD-TEKVAINF-EXT-RAD6                      
027240                               MOD-TEKVAINF-EXT-RAD7                      
027300     .                                                                    
027400     SKIP3                                                                
027500 MFS-FORM-ATTR SECTION.                                                   
027600                                                                          
027700*    --- ALLA INDATA-FÄLT                                                 
027800     MOVE MFS-FORMATETS-ATTR TO MOD-KDPERSON-ATTR                         
027900                                MOD-TEKVAINP-ATTR                         
027910                                MOD-TIKLAR-QUAL-ATTR                      
027920                                MOD-FLQPA-ATTR                            
027930                                MOD-KDKVAINF-ATTR                         
027940                                MOD-TIKLAR-LEV-ATTR                       
027950                                MOD-FLSTOCH-ATTR                          
027980                                MOD-TEKVAINF-INT-ATTR1                    
027981                                MOD-TEKVAINF-INT-ATTR2                    
027982                                MOD-TEKVAINF-INT-ATTR3                    
027983                                MOD-TEKVAINF-INT-ATTR4                    
027984                                MOD-TEKVAINF-INT-ATTR5                    
027985                                MOD-TEKVAINF-INT-ATTR6                    
027986                                MOD-TEKVAINF-INT-ATTR7                    
027990                                MOD-TEKVAINF-EXT-ATTR1                    
027991                                MOD-TEKVAINF-EXT-ATTR2                    
027992                                MOD-TEKVAINF-EXT-ATTR3                    
027993                                MOD-TEKVAINF-EXT-ATTR4                    
027994                                MOD-TEKVAINF-EXT-ATTR5                    
027995                                MOD-TEKVAINF-EXT-ATTR6                    
027996                                MOD-TEKVAINF-EXT-ATTR7                    
028000     .                                                                    
028800     EJECT                                                                
028900* --- IMS SEKTIONER ---                                                   
029000     SKIP3                                                                
029100 IMS-GET-MSG SECTION.                                                     
029200                                                                          
029300     MOVE '  QC' TO GODK-STATUSKODER                                      
029400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
029500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029600     PERFORM IMS-STATUSKONTROLL                                           
029700     .                                                                    
029800     SKIP3                                                                
029900 IMS-INSERT-MSG SECTION.                                                  
030000                                                                          
030400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
030500     MOVE SPACE TO GODK-STATUSKODER                                       
030600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
030700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031001     EJECT                                                                
031002 IMS-GU-ARTC-01 SECTION.                                                  
031003     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031004          DELIMITED BY SIZE INTO SSA1                                     
031005     MOVE '  GE' TO GODK-STATUSKODER                                      
031006     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
031007     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031008     PERFORM IMS-STATUSKONTROLL                                           
031009     .                                                                    
031010     SKIP2                                                                
031011 IMS-GU-KVAH-01 SECTION.                                                  
031012     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
031013          DELIMITED BY SIZE INTO SSA1                                     
031014     MOVE '  GE' TO GODK-STATUSKODER                                      
031015     CALL CBLTDLI USING GU KVAH1-PCB DLI-IO-AREA SSA1                     
031016     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031017     PERFORM IMS-STATUSKONTROLL                                           
031018     .                                                                    
031019     SKIP2                                                                
031038 IMS-GNP-KVAH-11-FIRST SECTION.                                           
031039     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
031040          DELIMITED BY SIZE INTO SSA1                                     
031041     MOVE 'W6KVAH11*F'        TO SSA2                                     
031042     MOVE '  GE' TO GODK-STATUSKODER                                      
031043     CALL CBLTDLI USING GN KVAH1-PCB DLI-IO-AREA SSA1 SSA2                
031044     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031045     PERFORM IMS-STATUSKONTROLL                                           
031046     .                                                                    
031047     SKIP3                                                                
031048 IMS-GNP-KVAH-11-NEXT SECTION.                                            
031051     MOVE 'W6KVAH11  '        TO SSA1                                     
031052     MOVE '  GE' TO GODK-STATUSKODER                                      
031053     CALL CBLTDLI USING GN KVAH1-PCB DLI-IO-AREA SSA1                     
031054     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031055     PERFORM IMS-STATUSKONTROLL                                           
031056     .                                                                    
031057     SKIP3                                                                
031058 IMS-GNP-KVAH-11 SECTION.                                                 
031059     STRING 'W6KVAH11(W6D211KY=>' W-W6D211KY-X                            
031060                    '&IDKVAINF=>' W-IDKVAINF-MIN-X                        
031061                    '&IDKVAINF=<' W-IDKVAINF-MAX-X                        
031062                    '&KDKVAINF=>' W-KDKVAINF-MIN-X                        
031063                    '&KDKVAINF=<' W-KDKVAINF-MAX-X ')'                    
031064          DELIMITED BY SIZE INTO SSA1                                     
031065     MOVE '  GE' TO GODK-STATUSKODER                                      
031066     CALL CBLTDLI USING GNP KVAH1-PCB DLI-IO-AREA SSA1                    
031067     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031068     PERFORM IMS-STATUSKONTROLL                                           
031069     .                                                                    
031070     EJECT                                                                
031101 IMS-GHU-KVAH-11 SECTION.                                                 
031102     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
031103          DELIMITED BY SIZE INTO SSA1                                     
031104     STRING 'W6KVAH11(IDKVAINF =' W-IDKVAINF-X ')'                        
031105          DELIMITED BY SIZE INTO SSA2                                     
031106     MOVE '  GE' TO GODK-STATUSKODER                                      
031107     CALL CBLTDLI USING GHU KVAH1-PCB DLI-IO-AREA SSA1 SSA2               
031108     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031109     PERFORM IMS-STATUSKONTROLL                                           
031110     .                                                                    
031120     SKIP3                                                                
031127 IMS-ISRT-KVAH-11 SECTION.                                                
031128                                                                          
031129     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
031130          DELIMITED BY SIZE INTO SSA1                                     
031131     MOVE 'W6KVAH11 '         TO SSA2                                     
031132     MOVE '  ' TO GODK-STATUSKODER                                        
031133     CALL CBLTDLI USING ISRT KVAH1-PCB DLI-IO-AREA SSA1 SSA2              
031134     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031135     PERFORM IMS-STATUSKONTROLL                                           
031136     .                                                                    
031137     EJECT                                                                
031138 IMS-GU-KVAH-22 SECTION.                                                  
031139     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
031140          DELIMITED BY SIZE INTO SSA1                                     
031141     MOVE 'W6KVAH12 '         TO SSA2                                     
031142     STRING 'W6KVAH22(IDKVAINF =' W1-IDKVAINF-X ')'                       
031143          DELIMITED BY SIZE INTO SSA3                                     
031144     MOVE '  GE' TO GODK-STATUSKODER                                      
031145     CALL CBLTDLI USING GU KVAH2-PCB DLI-IO-AREA2 SSA1 SSA2 SSA3          
031146     MOVE KVAH2-STATUS-CODE TO STATUS-WS                                  
031147     PERFORM IMS-STATUSKONTROLL                                           
031148     .                                                                    
031149     SKIP3                                                                
031150 IMS-REPL-KVAH SECTION.                                                   
031151                                                                          
031152     MOVE '  ' TO GODK-STATUSKODER                                        
031153     CALL CBLTDLI USING REPL KVAH1-PCB DLI-IO-AREA                        
031154     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031155     PERFORM IMS-STATUSKONTROLL                                           
031156     .                                                                    
031157     SKIP3                                                                
031158 IMS-DLET-KVAH SECTION.                                                   
031159                                                                          
031160     MOVE '  ' TO GODK-STATUSKODER                                        
031161     CALL CBLTDLI USING DLET KVAH1-PCB DLI-IO-AREA                        
031162     MOVE KVAH1-STATUS-CODE TO STATUS-WS                                  
031163     PERFORM IMS-STATUSKONTROLL                                           
031164     .                                                                    
031165     EJECT                                                                
031166 IMS-GU-WDP311 SECTION.                                                   
031170     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
031180          DELIMITED BY SIZE INTO SSA1                                     
031190     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
031191          DELIMITED BY SIZE INTO SSA2                                     
031192     MOVE '  GE' TO GODK-STATUSKODER                                      
031193     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
031194     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
031195     PERFORM IMS-STATUSKONTROLL                                           
031196     .                                                                    
031197     EJECT                                                                
031200 IMS-STATUSKONTROLL SECTION.                                              
031300                                                                          
031400     SET STATUS-IX TO 1                                                   
031500     SEARCH GODK-STATUS                                                   
031600       AT END                                                             
031700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031800         DELIMITED BY SIZE INTO FELTEXT                                   
031900         CALL FELLOG                                                      
032000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032100         CONTINUE                                                         
032200     END-SEARCH                                                           
032300     .                                                                    
