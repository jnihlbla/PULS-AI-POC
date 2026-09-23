001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W6032500.                                                
001600 AUTHOR.         NIHLBLAD JOHAN.                                          
001700 DATE-WRITTEN.   05/09/30.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        LÄGGA TILL FRI TEXT TILL EN SKROTORDER.                          
002200*        PGM. UPPDATERAR WDR5 HÄNDELSETYP 6321.                           
002300*                                                                         
002401*        PROGRAMMET UPPDATERAR WDR5                                       
002410*        PROGRAMMET LÄSER      WDK6                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W6T325                                              
002800*        MID:         W6I32501                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W6O32501                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500                                                                          
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W6032500'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004510 77  IX-BEEMB                    PIC S9(4)   VALUE +0  COMP SYNC.         
004511 77  INPUT-IX                    PIC S9(4)   VALUE +0  COMP SYNC.         
004512 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
004520 77  MAX-IX                      PIC S9(4)   VALUE +11 COMP SYNC.         
004600                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005101 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005102     88  INDATA-OK                           VALUE 'J'.                   
005110     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005800     88  EGEN-MID                            VALUE '6325'.                
005900     88  6323-MID                            VALUE '6323'.                
005901     88  6322-MID                            VALUE '6322'.                
005910     88  GODK-MID                            VALUE '6321' '6322'          
006000                                                   '6323' '6324'          
006100                                                   '6325' '6326'          
006200                                                   '6327' '6328'          
006210                                                   '2129' '2359'          
006300                                                   '6329'.                
006400     88  HELP-MID                            VALUE '0551'.                
006500     EJECT                                                                
006600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006700 01  GENERELLA-SUBPROGRAM.                                                
006800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
007801     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007802     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007803     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007810     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600     SKIP3                                                                
008700*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009000*                                                                         
009100 01  SPAR-AREA.                                                           
009200     03  SPAR-IDTRANS           PIC X(4)    VALUE '6325'.                 
009400     EJECT                                                                
009500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009800     SKIP3                                                                
009900*01  MID -COPY W6I32501                                                   
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010200     SKIP3                                                                
010300*01  -COPY WMSGAREA                                                       
010400     EJECT                                                                
010500     03  MOD REDEFINES MSG-AREA.                                          
010600*      05  -COPY W6O32501                                                 
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010900     SKIP3                                                                
011000*01  -COPY WMFSAREA                                                       
011100     EJECT                                                                
011101 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
011102*01  -COPY WMSGKOM                                                        
011103     EJECT                                                                
011104                                                                          
011192*    --- AREOR FÖR W006KOM SUBMODUL                                       
011193*                                                                         
011194 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
011195 01  KOM-IO-AREA.                                                         
011196   03  KOM-AREA                     PIC X(2500) VALUE SPACE.              
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700     03  W-WDGXKEY-6321.                                                  
011701         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
011702         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
011703         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
011706     03  W-DASKROT9-X.                                                    
011707         05  W-DASKROT9          PIC 9(8)   VALUE ZERO.                   
011710     03  W-KY6324-MIN-X.                                                  
011711         05  W-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.                
011712         05  W-IDDC-MIN      PIC X(2)   VALUE SPACE.                      
011713         05  FILLER          PIC X      VALUE LOW-VALUE.                  
011714     03  W-KY6324-MAX-X.                                                  
011715         05  W-IDARTNR-MAX   PIC S9(9)  VALUE ZERO COMP-3.                
011716         05  W-IDDC-MAX      PIC X(2)   VALUE SPACE.                      
011717         05  FILLER          PIC X      VALUE HIGH-VALUE.                 
011718     03  W-KY6324-KVAL-X.                                                 
011719         05  W-IDARTNR-KVAL  PIC S9(9)  VALUE ZERO COMP-3.                
011720         05  W-IDDC-KVAL     PIC X(2)   VALUE SPACE.                      
011721         05  W-KDSTASKR-KVAL PIC S9     VALUE ZERO COMP-3.                
011722     03  W-KDSTASKR-X.                                                    
011723         05  W-KDSTASKR      PIC S9     VALUE ZERO COMP-3.                
011724     03  W-IDRADNR-X.                                                     
011725         05  W-IDRADNR       PIC S9(5)  VALUE ZERO COMP-3.                
011726     03  W-KDSEGKEY-X.                                                    
011727         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011728     03  W-DASKROT9-KVAL-X.                                               
011729         05  W-DASKROT9-KVAL PIC 9(8)   VALUE ZERO.                       
011730**********************                                                    
011793     03  W-DASKROT9-MIN-X.                                                
011794         05  W-DASKROT9-MIN  PIC 9(8)   VALUE ZERO.                       
011799     03  W-IDDC-X.                                                        
011800         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
011801     03  W-KDARBTYP-X.                                                    
011802         05  W-KDARBTYP      PIC X(8)   VALUE SPACE.                      
011803     03  W-IDARTNR-X.                                                     
011804         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
011805     03  W-IDDC-6324-X.                                                   
011806         05  W-IDDC-6324      PIC X(2)   VALUE SPACE.                     
011807     03  W-IDDC-B6-X.                                                     
011808         05  W-IDDC-B6        PIC X(2)   VALUE SPACE.                     
011828     03  W-TIDATETIME-MIN-X.                                              
011829         05  W-TIDATETIME-MIN PIC 9(14) VALUE ZERO.                       
011830     03  W-TIDATETIME-MAX-X.                                              
011831         05  W-TIDATETIME-MAX PIC 9(14) VALUE 99999999999999.             
011840     SKIP2                                                                
011900*    --- STATUS-KOD FRÅN IMS                                              
012000 01  STATUS-WS                   PIC XX.                                  
012100     88  SEGMENT-FINNS                       VALUE '  '.                  
012200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012400     SKIP2                                                                
012500 01  GODK-STATUSKODER.                                                    
012600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(128).                              
012900 01  SSA2                        PIC X(128).                              
012910 01  SSA3                        PIC X(128).                              
012920 01  SSA4                        PIC X(128).                              
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
013701 01  DLI-IO-WDR501-6321.                                                  
013702*    03  -COPY WDGX6321                                                   
013703     EJECT                                                                
013708 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
013709 01  DLI-IO-WDGX6322.                                                     
013710*    03  -COPY WDGX6322                                                   
013711     EJECT                                                                
013712 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
013713 01  DLI-IO-WDGX6324.                                                     
013714*    03  -COPY WDGX6324                                                   
013715     EJECT                                                                
013716 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
013717 01  DLI-IO-WDGX6325.                                                     
013718*    03  -COPY WDGX6325                                                   
013719 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601  '.                    
013720 01  DLI-IO-WDK601.                                                       
013721*    03  -COPY WDK601                                                     
013722     EJECT                                                                
013723 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611  '.                    
013724 01  DLI-IO-WDK611.                                                       
013730*    03  -COPY WDK611                                                     
014000     EJECT                                                                
014010 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA     '.           
014020 01   DLI-IO-AREA-B601.                                                   
014030*     03  -COPY WDB601                                                    
014040     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200*01  -COPY W0009   -PRE MSG-                                              
014210     EJECT                                                                
014300*01  -COPY W0008   -PRE USEA-                                             
014400     05  FILLER                  PIC X.                                   
014501                                                                          
014502*01  -COPY W0008  -PRE 6321-                                              
014503     05  6321-KFB-IDHTYP         PIC X(4).                                
014504     05  6321-KFB-KDARBTYP       PIC X(8).                                
014505     05  6321-KFB-LOW-VALUE      PIC X(18).                               
014506     05  6321-KFB-DASKROT9-BEORD PIC 9(8).                                
014507     05  6321-KFB-IDARTNR        PIC S9(9)     COMP-3.                    
014508     05  6321-KFB-IDDC           PIC X(2).                                
014509     05  6321-KFB-KDSTASKR       PIC S9        COMP-3.                    
014510     05  6321-KFB-IDRADNR        PIC S9(5)     COMP-3.                    
014511                                                                          
014512*01  -COPY W0008  -PRE WDK6-                                              
014513     05  FILLER                  PIC X.                                   
014520                                                                          
014530*01  -COPY W0008   -PRE WDB6-                                             
014540     05  FILLER                  PIC X.                                   
014550     EJECT                                                                
014701 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 6321-PCB WDK6-PCB             
014704                           WDB6-PCB.                                      
014705 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 6321-PCB WDK6-PCB             
014720                           WDB6-PCB.                                      
014800                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-KOLLA-NYCKLAR                                            
015400       IF NYCKLAR-OK                                                      
015501         IF MFS-UPDATE                                                    
015504           PERFORM H-UPPDATERA                                            
015510         ELSE                                                             
015701           IF MFS-FIRST                                                   
015702             PERFORM C-FOERSTA-SIDA                                       
015703           ELSE                                                           
015710             PERFORM E-SAMMA-SIDA                                         
015720           END-IF                                                         
015810         END-IF                                                           
015820         IF INDATA-OK                                                     
015900           PERFORM F-LAES-VISA-INFO                                       
015910         END-IF                                                           
016000       END-IF                                                             
016300       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O32501 + 4                      
016400       PERFORM IMS-INSERT-MSG                                             
016500     END-IF                                                               
016700                                                                          
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     IF MSG-DUBBLA-TRANSKODER                                             
017500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I32501                 
017600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017800     ELSE                                                                 
017900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I32501                  
018000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018200     END-IF                                                               
018300                                                                          
018400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018700                                                                          
018800     MOVE LOW-VALUE TO MSG-AREA                                           
018900     MOVE 'W6O325N1' TO MFS-IDMOD                                         
019000     MOVE '6325' TO MOD-IDTRANS                                           
019100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019200                                                                          
019300     IF EGEN-MID OR HELP-MID                                              
019400       CONTINUE                                                           
019500     ELSE                                                                 
019600       MOVE SPACE TO MFS-KDTRTYP                                          
019700       MOVE '7' TO MFS-IDPFK                                              
019800     END-IF                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 B-KOLLA-NYCKLAR SECTION.                                                 
020400                                                                          
020500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020600     MOVE '001'             TO MSGI-KDCALL                                
020700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020900     MOVE '6325'            TO MSGI-IDTRANS                               
020901     IF EGEN-MID                                                          
020902       MOVE MID-KDARBTYP-IN   TO MSGI-KDARBTYP                            
020903       MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                            
020904       MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                             
020905     END-IF                                                               
020906     IF 6323-MID OR 6322-MID                                              
020907       MOVE MID-KDARBTYP-IN   TO MSGI-KDARBTYP                            
020908       MOVE MID-IDDC-IN       TO MSGI-IDDC-KEY                            
020909       MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                             
020910     END-IF                                                               
020911     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020920     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020930                                                                          
020940*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
020950     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
020960                                                                          
020970     MOVE JA TO NYCKLAR-SW                                                
021130*    -- KONTROLL AV KDARBTYP                                              
021140     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-IN                              
021150                                                                          
021160     IF MID-KDARBTYP-IN NOT = ALL '+'                                     
021170       MOVE '7'         TO MFS-IDPFK                                      
021180       MOVE SPACE       TO MFS-KDTRTYP                                    
021190     END-IF                                                               
021192     IF MSGI-KDARBTYP = 'ANSK' OR 'ESC' OR 'QUAL' OR 'LOC'                
021193        MOVE MSGI-KDARBTYP    TO W-6321-KDARBTYP                          
021194     ELSE                                                                 
021195        MOVE NEJ TO NYCKLAR-SW                                            
021196     END-IF                                                               
021197                                                                          
021198*    -- KONTROLL AV IDDC                                                  
021199     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
021200     IF MID-IDDC-IN NOT = ALL '+'                                         
021201        MOVE '7' TO MFS-IDPFK                                             
021202        MOVE SPACE TO MFS-KDTRTYP                                         
021204     END-IF                                                               
021206     MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                      
021207     PERFORM IMS-GU-WDB601                                                
021208     IF SEGMENT-FINNS                                                     
021209        MOVE MSGI-IDDC-KEY TO W-IDDC-6324                                 
021210                              W-IDDC-MIN                                  
021211                              W-IDDC-MAX                                  
021212     ELSE                                                                 
021213        MOVE NEJ TO NYCKLAR-SW                                            
021214     END-IF                                                               
021215     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
021216                                                                          
021217*    -- KONTROLL AV IDARTNR                                               
021218     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021219                                                                          
021220     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021221       MOVE '7'         TO MFS-IDPFK                                      
021222       MOVE SPACE       TO MFS-KDTRTYP                                    
021226     END-IF                                                               
021227     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021228     IF MSGI-IDARTNR NUMERIC                                              
021229       MOVE MSGI-IDARTNR TO W-IDARTNR-MIN W-IDARTNR-MAX                   
021230                            W-IDARTNR                                     
021231     ELSE                                                                 
021232       MOVE NEJ TO NYCKLAR-SW                                             
021233     END-IF                                                               
022000                                                                          
022101                                                                          
022222                                                                          
022223     IF NYCKLAR-OK                                                        
022224       MOVE MSGI-KDARBTYP   TO MOD-KDARBTYP-UT                            
022225       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
022226       MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                                
022227     ELSE                                                                 
022228       MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                            
022229                               MOD-IDARTNR-UT                             
022230                               MOD-IDDC-UT                                
022240     END-IF                                                               
022300                                                                          
022400     IF NYCKLAR-FEL                                                       
022500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022600       CALL WMEDKONV USING MED-WMEDAREA                                   
022700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022800       PERFORM MFS-RENSA-FAELT-IN                                         
022900       PERFORM MFS-RENSA-FAELT-UT                                         
023000     END-IF                                                               
023100     .                                                                    
023300     EJECT                                                                
023401 C-FOERSTA-SIDA SECTION.                                                  
023402                                                                          
023403     PERFORM MFS-RENSA-FAELT-IN                                           
023405     .                                                                    
023406     EJECT                                                                
023407 E-SAMMA-SIDA SECTION.                                                    
023408                                                                          
023409     IF EGEN-MID OR HELP-MID                                              
023419       IF MID-INPUT NOT = ALL '+'                                         
023420         PERFORM EA-FLYTTA-MID-INDATA-TILL-MOD                            
023421         MOVE INF-PRESS-PF11  TO MED-IDMFSFEL                             
023422         CALL WMEDKONV USING MED-WMEDAREA                                 
023423         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
023429       END-IF                                                             
023431     ELSE                                                                 
023432       PERFORM MFS-RENSA-FAELT-IN                                         
023433     END-IF                                                               
023449     .                                                                    
023450     EJECT                                                                
023460 EA-FLYTTA-MID-INDATA-TILL-MOD SECTION.                                   
023461                                                                          
023462     MOVE +1 TO IX                                                        
023463     PERFORM UNTIL IX > MAX-IX                                            
023464        IF MID-TEMEMO(IX) NOT = ALL '+'                                   
023466           MOVE MID-TEMEMO(IX)    TO MOD-TEMEMO(IX)                       
023467           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEMEMO-ATTR(IX)              
023468        ELSE                                                              
023469           MOVE MFS-RENSA-FAELT   TO MOD-TEMEMO(IX)                       
023470        END-IF                                                            
023472        ADD +1 TO IX                                                      
023474     END-PERFORM                                                          
023475     .                                                                    
023480     EJECT                                                                
023490                                                                          
023500 F-LAES-VISA-INFO SECTION.                                                
023600                                                                          
023700     PERFORM IMS-GU-WDGX6321                                              
023701     IF SEGMENT-SAKNAS                                                    
023702       MOVE ERR-INFO-MISSING    TO MED-IDMFSFEL                           
023703       CALL WMEDKONV USING MED-WMEDAREA                                   
023704       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023705       PERFORM MFS-RENSA-FAELT-UT                                         
023706     ELSE                                                                 
023710       PERFORM IMS-GNP-WDGX6324                                           
023900       IF SEGMENT-SAKNAS                                                  
024000          MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                           
024100          CALL WMEDKONV USING MED-WMEDAREA                                
024200          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
024300          PERFORM MFS-RENSA-FAELT-UT                                      
024400       ELSE                                                               
024410          MOVE 6324-IDDC         TO MOD-IDDC                              
024411                                    W-IDDC-KVAL                           
024420          MOVE 6324-KDERS-UTG    TO MOD-KDERS-UTG                         
024430          MOVE 6324-SUTPO-TOT    TO MOD-SUTPO-TOT                         
024440          MOVE 6324-KVSKROT-BEORD TO MOD-KVSKROT                          
024450          MOVE 6324-KVSKROT-ONDEM TO MOD-KVSKROT-KVAR                     
024460          MOVE 6324-KVTILLG-CDC  TO MOD-KVTILLG-CDC                       
024470          MOVE 6324-KVTILLG-SDC  TO MOD-KVTILLG-SDC                       
024480          MOVE 6324-KVAKS-CDC    TO MOD-KVAKS-CDC                         
024490          MOVE 6324-KVAKS-SDC    TO MOD-KVAKS-SDC                         
024493          MOVE +1  TO IX-BEEMB                                            
024494          PERFORM UNTIL IX-BEEMB > 20                                     
024495            MOVE 6324-BEEMBLEM(IX-BEEMB) TO MOD-BEEMBLEM(IX-BEEMB)        
024496            ADD +1 TO IX-BEEMB                                            
024497          END-PERFORM                                                     
024498          MOVE 6324-IDARTNR      TO W-IDARTNR-KVAL                        
024499          MOVE 6324-KDSTASKR     TO W-KDSTASKR-KVAL                       
024500          MOVE 6321-KFB-DASKROT9-BEORD   TO W-DASKROT9                    
024501          PERFORM IMS-GU-WDGX6324                                         
024502          PERFORM IMS-GNP-WDGX6325                                        
024530          PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-IX                     
024531            MOVE 6325-IDRADNR TO IX                                       
024540            MOVE 6325-TEMEMO    TO MOD-TEMEMO(IX)                         
024550            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEMEMO-ATTR(IX)             
024551            PERFORM IMS-GNP-WDGX6325                                      
024552            ADD +1 TO IX                                                  
024560          END-PERFORM                                                     
024561          MOVE +1 TO IX                                                   
024570          PERFORM UNTIL IX > MAX-IX                                       
024571            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEMEMO-ATTR(IX)             
024580            ADD +1 TO IX                                                  
024590          END-PERFORM                                                     
024600       END-IF                                                             
024610     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900                                                                          
025765 H-UPPDATERA SECTION.                                                     
025766                                                                          
025767     IF MID-INPUT = ALL '+'                                               
025768       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
025769       CALL WMEDKONV USING MED-WMEDAREA                                   
025770       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025771     ELSE                                                                 
025772       PERFORM IMS-GU-WDGX6321                                            
025773       PERFORM IMS-GHNP-WDGX6324                                          
025782       IF SEGMENT-FINNS                                                   
025787         MOVE +1                      TO IX                               
025789         PERFORM UNTIL IX > MAX-IX                                        
025790           IF MID-TEMEMO(IX) = ALL '+'                                    
025791             MOVE MFS-RENSA-FAELT     TO MOD-TEMEMO(IX)                   
025792                                         MID-TEMEMO(IX)                   
025793           ELSE                                                           
025794             MOVE MID-TEMEMO(IX)      TO MOD-TEMEMO(IX)                   
025795             MOVE IX TO W-IDRADNR                                         
025801             MOVE MID-TEMEMO(IX)      TO 6325-TEMEMO                      
025802             MOVE W-IDRADNR           TO 6325-IDRADNR                     
025803             MOVE 6321-KFB-KDARBTYP       TO W-6321-KDARBTYP              
025804             MOVE 6321-KFB-DASKROT9-BEORD TO W-DASKROT9                   
025805             MOVE 6321-KFB-IDARTNR        TO W-IDARTNR-KVAL               
025806             MOVE 6321-KFB-KDSTASKR       TO W-KDSTASKR-KVAL              
025807             MOVE 6321-KFB-IDDC           TO W-IDDC-KVAL                  
025809             PERFORM IMS-ISRT-WDGX6325                                    
025811             IF SEGMENT-FINNS-REDAN                                       
025812               PERFORM IMS-GHU-WDGX6325                                   
025813               MOVE MID-TEMEMO(IX)    TO 6325-TEMEMO                      
025814               PERFORM IMS-REPL-WDGX6325                                  
025815             END-IF                                                       
025816           END-IF                                                         
025817           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEMEMO-ATTR(IX)              
025818           ADD +1                     TO IX                               
025819         END-PERFORM                                                      
025820                                                                          
025822         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
025823         CALL WMEDKONV USING MED-WMEDAREA                                 
025824         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
025827       ELSE                                                               
025828         MOVE ERR-INFO-MISSING  TO MED-IDMFSFEL                           
025829         CALL WMEDKONV USING MED-WMEDAREA                                 
025830         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
025831         PERFORM MFS-RENSA-FAELT-UT                                       
025832       END-IF                                                             
025833     END-IF                                                               
025834     .                                                                    
025840     EJECT                                                                
025990 MFS-RENSA-FAELT-UT SECTION.                                              
026000                                                                          
026100*    --- ALLA UTDATA-FÄLT                                                 
026200     MOVE +1                          TO IX                               
026210     PERFORM UNTIL IX               > MAX-IX                              
026220        MOVE MFS-RENSA-FAELT          TO MOD-TEMEMO (IX)                  
026230                                                                          
026240        ADD +1                        TO IX                               
026250     END-PERFORM                                                          
026500     .                                                                    
026700     SKIP3                                                                
026800 MFS-RENSA-FAELT-IN SECTION.                                              
026900                                                                          
027000*    --- ALLA INDATA-FÄLT                                                 
027100     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-IN                              
027200                             MOD-IDDC-IN                                  
027210                             MOD-IDARTNR-IN                               
027300     .                                                                    
027400     EJECT                                                                
027500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027600                                                                          
027700*    --- ALLA UTDATA-FÄLT                                                 
027800     MOVE +1                          TO IX                               
027810     PERFORM UNTIL IX               > MAX-IX                              
027820        MOVE MFS-ROER-EJ-FAELT        TO MOD-TEMEMO (IX)                  
027830                                                                          
027840        ADD +1                        TO IX                               
027850     END-PERFORM                                                          
028200     .                                                                    
028300     SKIP3                                                                
028400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028500                                                                          
028600*    --- ALLA INDATA-FÄLT                                                 
028700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDARBTYP-IN                            
028800                               MOD-IDDC-IN                                
028810                               MOD-IDARTNR-IN                             
028900     .                                                                    
029000     EJECT                                                                
029100 MFS-FORM-ATTR SECTION.                                                   
029200                                                                          
029510     MOVE +1                          TO IX                               
029520     PERFORM UNTIL IX               > MAX-IX                              
029530        MOVE MFS-FORMATETS-ATTR       TO MOD-TEMEMO-ATTR (IX)             
029540                                                                          
029550        ADD +1                        TO IX                               
029560     END-PERFORM                                                          
029600     .                                                                    
029700     SKIP2                                                                
029800 MFS-LAES-IN-IGEN SECTION.                                                
029900                                                                          
030000*    --- ALLA INDATA-FÄLT                                                 
030010     MOVE +1                          TO IX                               
030020     PERFORM UNTIL IX               > MAX-IX                              
030030        MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-TEMEMO-ATTR (IX)             
030040                                                                          
030050        ADD +1                        TO IX                               
030060     END-PERFORM                                                          
030300     .                                                                    
030400     EJECT                                                                
030500* --- IMS SEKTIONER ---                                                   
030600     SKIP3                                                                
030700 IMS-GET-MSG SECTION.                                                     
030800                                                                          
030900     MOVE '  QC' TO GODK-STATUSKODER                                      
031000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSKONTROLL                                           
031300     .                                                                    
031400     SKIP3                                                                
031500 IMS-INSERT-MSG SECTION.                                                  
031600                                                                          
031800     MOVE 'N' TO MFS-KDHUVOMR                                             
032000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032100     MOVE SPACE TO GODK-STATUSKODER                                       
032200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032601     EJECT                                                                
032630 IMS-GU-WDGX6321 SECTION.                                                 
032640     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
032650            DELIMITED BY SIZE INTO SSA1                                   
032659     MOVE '  GE' TO GODK-STATUSKODER                                      
032660     CALL CBLTDLI USING GU 6321-PCB DLI-IO-WDGX6324 SSA1                  
032662     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032663     PERFORM IMS-STATUSKONTROLL                                           
032664     .                                                                    
032665     SKIP2                                                                
032666 IMS-GNP-WDGX6324 SECTION.                                                
032672     STRING 'WDGX6324(KY6324  >=' W-KY6324-MIN-X                          
032675                    '&KY6324  <=' W-KY6324-MAX-X ')'                      
032676          DELIMITED BY SIZE INTO SSA1                                     
032677     MOVE '  GE' TO GODK-STATUSKODER                                      
032678     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1                 
032680     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032681     PERFORM IMS-STATUSKONTROLL                                           
032682     .                                                                    
032683     SKIP2                                                                
032684 IMS-GHNP-WDGX6324 SECTION.                                               
032686     STRING 'WDGX6324(KY6324  >=' W-KY6324-MIN-X                          
032687                    '&KY6324  <=' W-KY6324-MAX-X ')'                      
032688          DELIMITED BY SIZE INTO SSA1                                     
032689     MOVE '  GE' TO GODK-STATUSKODER                                      
032690     CALL CBLTDLI USING GHNP 6321-PCB DLI-IO-WDGX6324 SSA1                
032691     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032692     PERFORM IMS-STATUSKONTROLL                                           
032693     .                                                                    
032694     SKIP2                                                                
032695 IMS-GNP-WDGX6325 SECTION.                                                
032696                                                                          
032703     MOVE 'WDGX6325 '  TO SSA1                                            
032704     MOVE '  GE' TO GODK-STATUSKODER                                      
032705     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6325 SSA1                 
032707     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032708     PERFORM IMS-STATUSKONTROLL                                           
032709     .                                                                    
032710     SKIP3                                                                
032711 IMS-GHNP-WDGX6325 SECTION.                                               
032712                                                                          
032713     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
032714          DELIMITED BY SIZE INTO SSA1                                     
032715     STRING 'WDGX6325(IDRADNR  =' W-IDRADNR-X ')'                         
032716          DELIMITED BY SIZE INTO SSA2                                     
032717     MOVE '  GE' TO GODK-STATUSKODER                                      
032718     CALL CBLTDLI USING GHNP 6321-PCB DLI-IO-WDGX6325 SSA1 SSA2           
032719     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032720     PERFORM IMS-STATUSKONTROLL                                           
032721     .                                                                    
032722     SKIP3                                                                
032723 IMS-ISRT-WDGX6325 SECTION.                                               
032724                                                                          
032725     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
032726            DELIMITED BY SIZE INTO SSA1                                   
032727     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
032728            DELIMITED BY SIZE INTO SSA2                                   
032729     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
032730          DELIMITED BY SIZE INTO SSA3                                     
032731     MOVE 'WDGX6325 '           TO SSA4                                   
032732     MOVE '  II'                TO GODK-STATUSKODER                       
032733     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6325                     
032734                                      SSA1 SSA2 SSA3 SSA4                 
032735     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
032736     PERFORM IMS-STATUSKONTROLL                                           
032737     .                                                                    
032738     EJECT                                                                
032739 IMS-REPL-WDGX6325 SECTION.                                               
032740                                                                          
032749     MOVE '  ' TO GODK-STATUSKODER                                        
032750     CALL CBLTDLI USING REPL 6321-PCB DLI-IO-WDGX6325                     
032752     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032753     PERFORM IMS-STATUSKONTROLL                                           
032754     .                                                                    
032755     SKIP3                                                                
032756 IMS-GU-WDGX6324 SECTION.                                                 
032757                                                                          
032758     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
032759            DELIMITED BY SIZE INTO SSA1                                   
032760     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
032761            DELIMITED BY SIZE INTO SSA2                                   
032762     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
032763          DELIMITED BY SIZE INTO SSA3                                     
032766     MOVE '  ' TO GODK-STATUSKODER                                        
032767     CALL CBLTDLI USING GU 6321-PCB DLI-IO-WDGX6325                       
032768                                      SSA1 SSA2 SSA3                      
032769     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032770     PERFORM IMS-STATUSKONTROLL                                           
032771     .                                                                    
032772     SKIP3                                                                
032773 IMS-GHU-WDGX6325 SECTION.                                                
032774                                                                          
032775     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
032776            DELIMITED BY SIZE INTO SSA1                                   
032777     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
032778            DELIMITED BY SIZE INTO SSA2                                   
032779     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
032780          DELIMITED BY SIZE INTO SSA3                                     
032781     STRING 'WDGX6325(IDRADNR  =' W-IDRADNR-X ')'                         
032782          DELIMITED BY SIZE INTO SSA4                                     
032783     MOVE '  ' TO GODK-STATUSKODER                                        
032784     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDGX6325                      
032785                                      SSA1 SSA2 SSA3 SSA4                 
032786     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
032787     PERFORM IMS-STATUSKONTROLL                                           
032788     .                                                                    
032789     SKIP3                                                                
032790 IMS-GET-WDK601 SECTION.                                                  
032791                                                                          
032792     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032793          DELIMITED BY SIZE INTO SSA1                                     
032794     MOVE '  GE' TO GODK-STATUSKODER                                      
032795     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
032796     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032797     PERFORM IMS-STATUSKONTROLL                                           
032798     .                                                                    
032799     EJECT                                                                
032800 IMS-GET-WDK611 SECTION.                                                  
032801                                                                          
032802     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032803          DELIMITED BY SIZE INTO SSA1                                     
032804     MOVE '  GE' TO GODK-STATUSKODER                                      
032805     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
032806     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032807     PERFORM IMS-STATUSKONTROLL                                           
032808     .                                                                    
032809     EJECT                                                                
032810 IMS-GU-WDB601    SECTION.                                                
032811     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
032812          DELIMITED BY SIZE INTO SSA1                                     
032813     MOVE '  GE' TO GODK-STATUSKODER                                      
032814     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
032815     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032816     PERFORM IMS-STATUSKONTROLL                                           
032817     .                                                                    
032820     EJECT                                                                
032840 IMS-STATUSKONTROLL SECTION.                                              
032900                                                                          
033000     SET STATUS-IX TO 1                                                   
033100     SEARCH GODK-STATUS                                                   
033200       AT END                                                             
033300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033400         DELIMITED BY SIZE INTO FELTEXT                                   
033500         CALL FELLOG                                                      
033600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033700         CONTINUE                                                         
033800     END-SEARCH                                                           
033900     .                                                                    
