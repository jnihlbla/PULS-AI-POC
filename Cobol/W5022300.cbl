001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5022300.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/09/16.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001810*    FUNKTION:                                                            
001820*        PROGRAMMET HAR TVÅ FUNKTIONER, NÄMLIGEN:                         
001830*        -SÖKA FRAM INFORMATION SOM FINNS LAGRAD I                        
001840*        EKONOMIBAS(WDGX5122) OCH PRESENTERA DENNA INFORMATION            
001850*        M.H.A INMATAD DATA FRÅN BILD 5223                                
001860*        -TA BORT INFORMATION SOM FINNS LAGRAD PÅ                         
001870*        EKONOMIBAS(WDGX5122)                                             
001880*        OM MAN ANGER "S" I KOMMANDOFÄLTET PÅ NÅGON AV                    
001890*        DETALJRADERNA OCH "ENTER" AKTIVERAS SKALL HOPP SKE TILL          
001891*        BILD 5222, OM MAN ANGER "D" I KOMMANDOFÄLTET PÅ NÅGON AV         
001892*        DETALJRADERNA OCH "F11" AKTIVERAS KOMMER DENNA POST              
001893*        ATT FÖRSVINNA FRÅN EKONOMIBAS(WDGX5122)                          
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WL5121 (WDGX)                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T223                                              
002700*        MID:         W5I22301                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W5O22301                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W5022300'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004501*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004502 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004510 77  MAX-INDX                    PIC S9(4)  VALUE +15   COMP SYNC.        
004550                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004900 77  BYT-SW                      PIC X       VALUE 'N'.                   
004901     88  BYT-BILD                            VALUE 'J'.                   
004902     88  BYT-EJ-BILD                         VALUE 'N'.                   
004903                                                                          
004904 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004905     88  ALLT-OK                             VALUE 'J'.                   
004906                                                                          
004907 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004908     88  INDATA-OK                           VALUE 'J'.                   
004910     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '5223'.                
005700     88  GODK-MID                            VALUE '5221' '5222'          
005800                                                   '5223' '5224'          
005900                                                   '5225' '5226'          
006000                                                   '5227' '5228'          
006100                                                   '5229'.                
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
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008601     EJECT                                                                
008602*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008603 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
008604*                                                                         
008605 01  SPAR-AREA.                                                           
008606     03  SPAR-IDTRANS             PIC X(4)    VALUE '5223'.               
008607     03  SPAR-BILD                PIC X(4)    VALUE '5223'.               
008611     03  SPAR-IDPRCTR-NEXT        PIC X(10).                              
008621     03  SPAR-IDANALYS            PIC X(12).                              
008622     03  SPAR-IDKONTO             PIC X(10)   VALUE SPACE.                
008623     03  SPAR-IDPRCTR             PIC X(10).                              
008624     03  SPAR-INDX                PIC S9(4) VALUE +0 COMP-3.              
008630     03  SPAR-TABELL.                                                     
008640       05  SPAR-WDGX5122 OCCURS 15.                                       
008650         07  SPAR-WDGX5122-TAB.                                           
008660           09  SPAR-IDPRCTR-TAB   PIC X(10)    VALUE SPACE.               
008700     EJECT                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W5I22301                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W5O22301                                                 
010000     EJECT                                                                
010010 01  W-PROG-TO-PROG-SW-5222.                                              
010020     03  M-SW-LL-5222            PIC S9(4)   VALUE +240 COMP SYNC.        
010030     03  M-SW-Z1-Z2-5222         PIC X(2)    VALUE LOW-VALUE.             
010040     03  M-SW-KDTRANS-5222       PIC X(8)    VALUE 'W5T222  '.            
010050     03  M-SW-IDTRANS-5222       PIC X(4)    VALUE '5223'.                
010060     03  M-SW-KDMFSTYP-5222      PIC X(1)    VALUE '2'.                   
010070                                                                          
010080*    03  MID -COPY W5I22201 -PRE 5222-                                    
010090     EJECT                                                                
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
011102     03  W-WDGXKEY-X.                                                     
011103         05  W-IDHTYP            PIC X(4)     VALUE '5121'.               
011103         05  W-IDFTG             PIC X(2)     VALUE SPACE.                
011104         05  W-WDGXKEY           PIC X(24)    VALUE LOW-VALUE.            
011105                                                                          
011190     03  W-KEY5122-X.                                                     
011191         05  W-IDKONTO           PIC S9(11)   COMP-3 VALUE ZERO.          
011192         05  W-IDPRCTR           PIC X(10)    VALUE SPACE.                
011193                                                                          
011194     03  W-KEY5122-MIN-X.                                                 
011195         05  W-IDKONTO-MIN       PIC S9(11)   COMP-3 VALUE ZERO.          
011196         05  FILLER              PIC X(10)    VALUE SPACE.                
011197                                                                          
011198     03  W-KEY5122-MAX-X.                                                 
011199         05  W-IDKONTO-MAX       PIC S9(11)   COMP-3 VALUE ZERO.          
011200         05  FILLER              PIC X(10)    VALUE SPACE.                
011210     SKIP2                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011710     88  BAS-SLUT                            VALUE 'GB'.                  
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
013100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5121'.                    
013200 01  DLI-IO-WDGX5121.                                                     
013300*    03  -COPY WDGX5121 -PRE WDGX5121-                                    
013310                                                                          
013320 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5122'.                    
013330 01  DLI-IO-WDGX5122.                                                     
013340*    03  -COPY WDGX5122 -PRE WDGX5122-                                    
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013601     EJECT                                                                
013610*01  -COPY W0009   -PRE ALT-                                              
013620     EJECT                                                                
013700*01  -COPY W0008   -PRE USEA-                                             
013800     05  FILLER                  PIC X.                                   
013901                                                                          
013902*01  -COPY W0008  -PRE 5121-                                              
013910     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014101 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB 5121-PCB.             
014102 MAIN SECTION.                                                            
014110     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB 5121-PCB.             
014200                                                                          
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600       PERFORM A-INIT                                                     
014700       PERFORM B-KOLLA-NYCKLAR                                            
014800       IF NYCKLAR-OK                                                      
014900         IF BYT-BILD                                                      
014901           PERFORM I-BYT-BILD                                             
014903         ELSE                                                             
014904           IF MFS-UPDATE                                                  
014905             PERFORM G-KOLLA-INPUT                                        
014906             IF INDATA-OK                                                 
014907               PERFORM H-UPPDATERA                                        
014908             END-IF                                                       
014910           ELSE                                                           
015001             IF MFS-FIRST                                                 
015002               PERFORM C-FOERSTA-SIDA                                     
015003             ELSE                                                         
015004               IF MFS-NEXT                                                
015005                 PERFORM D-NAESTA-SIDA                                    
015006               ELSE                                                       
015007                 PERFORM E-SAMMA-SIDA                                     
015008               END-IF                                                     
015010             END-IF                                                       
015210           END-IF                                                         
015211         END-IF                                                           
015220         IF ALLT-OK                                                       
015300           PERFORM F-LAES-VISA-INFO                                       
015400         END-IF                                                           
015410       END-IF                                                             
015600       IF BYT-EJ-BILD                                                     
015700         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22301 + 4                    
015800         PERFORM IMS-INSERT-MSG                                           
015900       END-IF                                                             
016000     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     IF MSG-DUBBLA-TRANSKODER                                             
016900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I22301                 
017000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017200     ELSE                                                                 
017300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I22301                  
017400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018100                                                                          
018200     MOVE LOW-VALUE TO MSG-AREA                                           
018300     MOVE 'W5O223N1' TO MFS-IDMOD                                         
018400     MOVE '5223' TO MOD-IDTRANS                                           
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
019810     MOVE LOW-VALUE         TO W-KEY5122-MIN-X                            
019820     MOVE HIGH-VALUE        TO W-KEY5122-MAX-X                            
019900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020000     MOVE '001'             TO MSGI-KDCALL                                
020100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020300     MOVE '5223'            TO MSGI-IDTRANS                               
020310     INSPECT MID-IDKONTO-IN REPLACING LEADING SPACE BY ZERO               
020400     IF EGEN-MID                                                          
020500       MOVE MID-IDKONTO-IN  TO MSGI-IDKONTO                               
021200     END-IF                                                               
021300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021400     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
021410                                                                          
021420     IF MSGI-IDLAND-SPR = 'GB'                                            
021430       MOVE 'GB' TO MED-IDSKYLT                                           
021440     ELSE                                                                 
021450       MOVE 'S' TO MED-IDSKYLT                                            
021460     END-IF                                                               
021500     MOVE JA TO NYCKLAR-SW                                                
021510     MOVE JA TO ALLT-SW                                                   
021700                                                                          
021710     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021720       MOVE MSGI-IDFTG      TO MOD-IDFTG-UT                               
                                     W-IDFTG                                    
021730     END-IF                                                               
021740                                                                          
021800*    -- KONTROLL AV IDKONTO                                               
021900     MOVE MFS-RENSA-FAELT TO MOD-IDKONTO-IN                               
022000                                                                          
022010     IF MSGI-IDKONTO NUMERIC AND MSGI-IDKONTO > ZERO                      
022020       MOVE MSGI-IDKONTO    TO W-IDKONTO                                  
022022                               W-IDKONTO-MIN                              
022023                               W-IDKONTO-MAX                              
022024                               SPAR-IDKONTO                               
022030     ELSE                                                                 
022040       MOVE NEJ             TO NYCKLAR-SW                                 
022050     END-IF                                                               
022060                                                                          
022095     IF GODK-MID OR NYCKLAR-OK                                            
022096       MOVE MSGI-IDKONTO    TO MOD-IDKONTO-UT                             
022097       INSPECT MOD-IDKONTO-UT REPLACING LEADING ZERO BY SPACE             
022099     ELSE                                                                 
022100       MOVE MFS-RENSA-FAELT TO MOD-IDKONTO-UT                             
022102     END-IF                                                               
022103                                                                          
022104*    -- KONTROLL AV MID-CMD                                               
022127     MOVE +1 TO INDX                                                      
022128     PERFORM UNTIL INDX > MAX-INDX                                        
022130       IF MID-CMD (INDX) NOT = ' '                                        
022131         IF MID-CMD (INDX) = 'S'                                          
022133           MOVE INDX TO SPAR-INDX                                         
022134           MOVE +15  TO INDX                                              
022135           MOVE JA   TO BYT-SW                                            
022142         END-IF                                                           
022143       END-IF                                                             
022144       ADD +1 TO INDX                                                     
022145     END-PERFORM                                                          
022146                                                                          
022147     IF NYCKLAR-FEL                                                       
022148*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
022149*---GODKÄND BILD                                                          
022150       IF GODK-MID                                                        
022151         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
022160         CALL WMEDKONV USING MED-WMEDAREA                                 
022170         MOVE MED-MFSFEL    TO MOD-TEMFSFEL                               
022180         PERFORM MFS-RENSA-FAELT-IN                                       
022190         PERFORM MFS-RENSA-FAELT-UT                                       
022191       END-IF                                                             
022192     END-IF                                                               
022200     .                                                                    
022201     EJECT                                                                
022202 C-FOERSTA-SIDA SECTION.                                                  
022203                                                                          
022204     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022205     CALL WMEDKONV USING MED-WMEDAREA                                     
022206     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022207                                                                          
022208     PERFORM MFS-RENSA-FAELT-IN                                           
022209     .                                                                    
022210     EJECT                                                                
022211 D-NAESTA-SIDA SECTION.                                                   
022212                                                                          
022213     IF SPAR-IDTRANS = '5223'                                             
022215       MOVE SPAR-IDPRCTR-NEXT  TO W-IDPRCTR                               
022219     ELSE                                                                 
022220       PERFORM MFS-RENSA-FAELT-IN                                         
022221     END-IF                                                               
022222     .                                                                    
022223     EJECT                                                                
022224 E-SAMMA-SIDA SECTION.                                                    
022225                                                                          
022226     IF SPAR-IDTRANS = '5223' OR '0551'                                   
022227       CONTINUE                                                           
022230     ELSE                                                                 
022231       PERFORM MFS-RENSA-FAELT-IN                                         
022238     END-IF                                                               
022239     .                                                                    
022240     EJECT                                                                
022500 F-LAES-VISA-INFO SECTION.                                                
022600                                                                          
022601     PERFORM IMS-GU-WDGX5121                                              
022602     IF MFS-NEXT                                                          
022603       PERFORM IMS-GHU-WDGX5122                                           
022604     ELSE                                                                 
022605       PERFORM IMS-GN-WDGX5122                                            
022606     END-IF                                                               
022800                                                                          
022900     IF SEGMENT-SAKNAS OR BAS-SLUT                                        
023000       MOVE 'ACCOUNT MISSING' TO MOD-TEMFSFEL                             
023300       PERFORM MFS-RENSA-FAELT-UT                                         
023400     ELSE                                                                 
023500       MOVE +1 TO INDX                                                    
023519       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
023520        OR BAS-SLUT                                                       
023521         IF SEGMENT-FINNS                                                 
023524           MOVE WDGX5122-5122-IDANALYS TO MOD-IDANALYS      (INDX)        
023526           MOVE WDGX5122-5122-IDPRCTR  TO MOD-IDPRCTR       (INDX)        
023527                                          SPAR-IDPRCTR-TAB  (INDX)        
023529         ELSE                                                             
023530           MOVE MFS-RENSA-FAELT TO MOD-IDANALYS (INDX)                    
023533         END-IF                                                           
023534         ADD +1 TO INDX                                                   
023535         PERFORM IMS-GN-WDGX5122                                          
023536       END-PERFORM                                                        
023537     END-IF                                                               
023538                                                                          
023539     IF SEGMENT-FINNS                                                     
023540       MOVE WDGX5122-5122-IDPRCTR  TO SPAR-IDPRCTR-NEXT                   
023541       MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                        
023542       CALL WMEDKONV USING MED-WMEDAREA                                   
023543       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
023547     END-IF                                                               
023548                                                                          
023549     MOVE '002'     TO MSGI-KDCALL                                        
023550     MOVE '5223'    TO SPAR-IDTRANS                                       
023551     MOVE '5223'    TO SPAR-BILD                                          
023552     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
023560     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023700     .                                                                    
023800     EJECT                                                                
024702 G-KOLLA-INPUT SECTION.                                                   
024703                                                                          
024704     MOVE JA  TO INDATA-SW                                                
024705     IF MID-CMD (INDX) = ALL '+'                                          
024706       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024707       CALL WMEDKONV USING MED-WMEDAREA                                   
024708       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024709       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024710       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024711       MOVE NEJ TO INDATA-SW                                              
024712     ELSE                                                                 
024713                                                                          
024714       MOVE +1 TO INDX                                                    
024715       PERFORM UNTIL INDX > MAX-INDX                                      
024716         IF MID-CMD (INDX) NOT = ' '                                      
024717           IF MID-CMD (INDX) = 'D'                                        
024718              MOVE INDX TO SPAR-INDX                                      
024720              MOVE +15 TO INDX                                            
024722           END-IF                                                         
024723         END-IF                                                           
024724         ADD +1 TO INDX                                                   
024725       END-PERFORM                                                        
024726                                                                          
024727       MOVE SPAR-INDX TO INDX                                             
024728       IF MID-CMD (INDX) = 'D'                                            
024729         MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR (INDX)                 
024730       ELSE                                                               
024731         MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR (INDX)                   
024732         MOVE NEJ TO INDATA-SW                                            
024733       END-IF                                                             
024740                                                                          
024747       IF INDATA-FEL                                                      
024748         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024749         CALL WMEDKONV USING MED-WMEDAREA                                 
024750         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024751         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024752         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024763       END-IF                                                             
024764     END-IF                                                               
024765     .                                                                    
024766     EJECT                                                                
024767 H-UPPDATERA SECTION.                                                     
024768                                                                          
024772     MOVE SPAR-INDX TO INDX                                               
024773     MOVE SPAR-IDPRCTR-TAB  (INDX) TO W-IDPRCTR                           
024775     PERFORM IMS-GU-WDGX5121                                              
024776     PERFORM IMS-GHU-WDGX5122                                             
024777                                                                          
024778     IF SEGMENT-FINNS                                                     
024779       PERFORM IMS-DLET-WDGX5122                                          
024780       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
024781       CALL WMEDKONV USING MED-WMEDAREA                                   
024782       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
024783       PERFORM MFS-FORM-ATTR                                              
024784       PERFORM MFS-RENSA-FAELT-IN                                         
024785     END-IF                                                               
024786     .                                                                    
024787     EJECT                                                                
024788 I-BYT-BILD SECTION.                                                      
024789     SKIP2                                                                
024790     MOVE NEJ TO ALLT-SW                                                  
024791     MOVE JA  TO BYT-SW                                                   
024792                                                                          
024793* ---HÄMTAR RÄTT RAD-VÄRDE TILL 5222-BILDEN                               
024795     MOVE SPAR-INDX                TO INDX                                
024796     MOVE SPAR-IDPRCTR-TAB (INDX)  TO SPAR-IDPRCTR                        
024799     MOVE '002'                    TO MSGI-KDCALL                         
024800     MOVE '5222'                   TO SPAR-IDTRANS                        
024801     MOVE SPAR-AREA                TO MSGI-SPAR-AREA                      
024802     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024803                                                                          
024804                                                                          
024805* ---SKICKAR VÄRDE TILL 5222-MID FÖR ATT SEDAN                            
024806* ---STARTA UPP 5222-BILDEN                                               
024807     MOVE LOW-VALUE          TO 5222-MID-W5I22201                         
024808     MOVE SPAR-IDKONTO       TO 5222-MID-IDKONTO-IN                       
024809     MOVE SPAR-IDPRCTR       TO 5222-MID-IDPRCTR-IN                       
024813     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22301 + 17                       
024814     PERFORM IMS-INSERT-ALT-MSG-5222                                      
024815     .                                                                    
024816     EJECT                                                                
024900 MFS-RENSA-FAELT-UT SECTION.                                              
025000                                                                          
025100*    --- ALLA UTDATA-FÄLT                                                 
025210*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025410     MOVE +1 TO INDX                                                      
025420     PERFORM UNTIL INDX > MAX-INDX                                        
025430       MOVE MFS-RENSA-FAELT TO MOD-CMD      (INDX)                        
025440                               MOD-IDANALYS (INDX)                        
025450                               MOD-IDPRCTR  (INDX)                        
025470     ADD +1 TO INDX                                                       
025480     END-PERFORM                                                          
025500     .                                                                    
025601     SKIP3                                                                
025800 MFS-RENSA-FAELT-IN SECTION.                                              
025900                                                                          
026000*    --- ALLA INDATA-FÄLT                                                 
026100     MOVE MFS-RENSA-FAELT TO MOD-IDKONTO-IN                               
026210     MOVE +1 TO INDX                                                      
026220     PERFORM UNTIL INDX > MAX-INDX                                        
026230       MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                             
026240     ADD +1 TO INDX                                                       
026250     END-PERFORM                                                          
026300     .                                                                    
026400     EJECT                                                                
026500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026600                                                                          
026700*    --- ALLA UTDATA-FÄLT                                                 
026810*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
027101     MOVE +1 TO INDX                                                      
027102     PERFORM UNTIL INDX > MAX-INDX                                        
027103       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027104       ADD +1 TO INDX                                                     
027105     END-PERFORM                                                          
027106     .                                                                    
027107     SKIP2                                                                
027108 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027109                                                                          
027110*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027111     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD      (INDX)                        
027112                               MOD-IDANALYS (INDX)                        
027113                               MOD-IDPRCTR  (INDX)                        
027200     .                                                                    
027300     SKIP3                                                                
027400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027500                                                                          
027600*    --- ALLA INDATA-FÄLT                                                 
027810     MOVE +1 TO INDX                                                      
027820     PERFORM UNTIL INDX > MAX-INDX                                        
027830       MOVE MFS-ROER-EJ-FAELT TO MOD-CMD (INDX)                           
027840       ADD +1 TO INDX                                                     
027850     END-PERFORM                                                          
027900     .                                                                    
028000     EJECT                                                                
028100 MFS-FORM-ATTR SECTION.                                                   
028200                                                                          
028300*    --- ALLA INDATA-FÄLT                                                 
028510     MOVE +1 TO INDX                                                      
028520     PERFORM UNTIL INDX > MAX-INDX                                        
028530       MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR (INDX)                     
028540       ADD +1 TO INDX                                                     
028550     END-PERFORM                                                          
028600     .                                                                    
028700     SKIP2                                                                
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
031602 IMS-INSERT-ALT-MSG-5222 SECTION.                                         
031603                                                                          
031604     MOVE SPACE TO GODK-STATUSKODER                                       
031605     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-5222               
031606     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031607     PERFORM IMS-STATUSKONTROLL                                           
031608     .                                                                    
031609     EJECT                                                                
031610 IMS-GU-WDGX5121 SECTION.                                                 
031611                                                                          
031612     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
031613          DELIMITED BY SIZE INTO SSA1                                     
031614     MOVE '  GE' TO GODK-STATUSKODER                                      
031615     CALL CBLTDLI USING GU 5121-PCB DLI-IO-WDGX5121 SSA1                  
031616     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
031617     PERFORM IMS-STATUSKONTROLL                                           
031618     .                                                                    
031619     SKIP3                                                                
031620 IMS-GHU-WDGX5122 SECTION.                                                
031621                                                                          
031622     STRING 'WDGX5122(KEY5122  =' W-KEY5122-X ')'                         
031624          DELIMITED BY SIZE INTO SSA1                                     
031625     MOVE '  GE' TO GODK-STATUSKODER                                      
031626     CALL CBLTDLI USING GHU 5121-PCB DLI-IO-WDGX5122 SSA1                 
031627     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
031628     PERFORM IMS-STATUSKONTROLL                                           
031629     .                                                                    
031630     SKIP3                                                                
031631 IMS-GN-WDGX5122 SECTION.                                                 
031632                                                                          
031633     STRING 'WDGX5122(KEY5122 >=' W-KEY5122-MIN-X                         
031634                    '&KEY5122 <=' W-KEY5122-MAX-X ')'                     
031635          DELIMITED BY SIZE INTO SSA1                                     
031636     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031637     CALL CBLTDLI USING GN 5121-PCB DLI-IO-WDGX5122 SSA1                  
031638     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
031639     PERFORM IMS-STATUSKONTROLL                                           
031640     .                                                                    
031650     SKIP3                                                                
031651 IMS-DLET-WDGX5122 SECTION.                                               
031652                                                                          
031653     MOVE '  ' TO GODK-STATUSKODER                                        
031654     CALL CBLTDLI USING DLET 5121-PCB DLI-IO-WDGX5122                     
031655     MOVE 5121-STATUS-CODE TO STATUS-WS                                   
031656     PERFORM IMS-STATUSKONTROLL                                           
031660     .                                                                    
031700     EJECT                                                                
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
