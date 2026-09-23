000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W1052200.                                                
000800 AUTHOR.         STEFAN ANDREASSON FRONTEC.                               
000900 DATE-WRITTEN.   NOV 1995.                                                
000910 DATE-COMPILED.                                                           
001000*                                                                         
001100*    FUNKTION.   TP-UPPDATERINGSPROGRAM.                                  
001200*                STARTAR SOP VIA W00606 FÖR BESTÄLLNING AV                
001210*                LÅN PÅ EN KATALOG ELLER KATALOGGRUPP.                    
001500                                                                          
001600*    INDATA.                                                              
001610*        TRANSAKTION: W1T522         ENTER                                
001620*                     W1T522U        PF11                                 
001800*        MID:         W1I52201                                            
001900                                                                          
002000*    UTDATA.                                                              
002100*        MOD:         W1O52201                                            
002300                                                                          
002400*        FELLOG                                                           
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003010*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(8)    VALUE 'W1052200'.            
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
003510                                                                          
003600 01  DAGENS-DATUM                PIC 9(6).                                
003910                                                                          
003920 01  W-FELFAELT.                                                          
003930     03  W-KDTRTYP               PIC X.                                   
003931     03  FILLER                  PIC X       VALUE SPACE.                 
003940     03  W-INDATA-OK             PIC X.                                   
003941     03  FILLER                  PIC X       VALUE SPACE.                 
003950     03  W-FLTABORT              PIC X.                                   
004000                                                                          
004110 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
004300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG '.             
004301     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004302                                                                          
004303                                                                          
004306     EJECT                                                                
004307*01  -COPY WDATAREA                                                       
004310     EJECT                                                                
004320*- - - - - - - - - - - -   - - - PARAMETRAR TILL SOP                      
004330 01  W-PROG-TO-PROG-SW.                                                   
004340*03  -COPY WMSGSOP                                                        
004400                                                                          
004500 01  FILLER                      PIC X(16)   VALUE 'KONTROLLER'.          
004600 01  KONTROLL-FALT.                                                       
004800     03  INDATA-OK               PIC X       VALUE 'J'.                   
004900     03  FRAN-KATALOG-FINNS      PIC X       VALUE 'J'.                   
005000     03  TO-KATALOG-FINNS        PIC X       VALUE 'N'.                   
005100     03  FEL-KATALOGNR           PIC X       VALUE 'N'.                   
005200                                                                          
005300 01  FILLER                      PIC X(16)   VALUE 'KONSTANTER'.          
005400 01  KONSTANTER.                                                          
005500     03  JA                      PIC X       VALUE 'J'.                   
005600     03  NEJ                     PIC X       VALUE 'N'.                   
005700     03  OK                      PIC X       VALUE 'O'.                   
005800     03  FEL                     PIC X       VALUE 'F'.                   
005900                                                                          
006000 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
006100                                                                          
006200 01  ARBETSFALT.                                                          
006300     03  A-DAGENS-DATUM          PIC 9(6).                                
006400     03  A-TID                   PIC S9(9)   COMP-3.                      
006401     03  DAGENS-AAR              PIC 9(4)    VALUE ZERO.                  
006402                                                                          
006403     03  W-DAGENS-VECKA              PIC X(6).                            
006404     03  W-INMATAD-VECKA             PIC X(4).                            
006405     03  W-INMATAD-VECKA-NUM         REDEFINES W-INMATAD-VECKA            
006406                                     PIC 9(4).                            
006407     03  W-AAR                       PIC 9(2).                            
006408     03 WS-GILTIGA-AAR.                                                   
006409       04 WS-TIAAAA              PIC 9(4)    VALUE ZERO                   
006410                                 OCCURS 4.                                
006411     03  WS-KDCATPUB-SKRIV-FOM   PIC X(6)    VALUE SPACE.                 
006412     03  WS-KDCATPUB-SKRIV-TOM   PIC X(6)    VALUE SPACE.                 
006413     03  WS-KDCATPUB-R-AVV       PIC X(3)    VALUE SPACE.                 
006414     03  WS-KDCATPUB-AAAAVV      PIC X(6)    VALUE SPACE.                 
006415                                                                          
006420*               ******     DATA SOM SKICKAS TILL SOP                      
006421 01  PARM-TESYMBV.                                                        
006430     03  FILLER                  PIC X(8)                                 
006431                                 VALUE 'KATLAES('.                        
006440     03  PARM-IDCATNR-LAES       PIC 9(5).                                
006450     03  FILLER                  PIC X       VALUE ')'.                   
006452     03  FILLER                  PIC X(8)                                 
006453                                 VALUE 'GRPLAES('.                        
006460     03  PARM-IDCATGRP-LAES      PIC 9(2).                                
006470     03  FILLER                  PIC X       VALUE ')'.                   
006480     03  FILLER                  PIC X(8)                                 
006490                                 VALUE 'PUBLAES('.                        
006491     03  PARM-KDCATPUB-LAES      PIC X(6).                                
006492     03  FILLER                  PIC X       VALUE ')'.                   
006493     03  FILLER                  PIC X(9)                                 
006494                                 VALUE 'KATSKRIV('.                       
006495     03  PARM-IDCATNR-SKRIV      PIC 9(5).                                
006497     03  FILLER                  PIC X       VALUE ')'.                   
006498     03  FILLER                  PIC X(9)                                 
006499                                 VALUE 'GRPSKRIV('.                       
006500     03  PARM-IDCATGRP-SKRIV     PIC 9(2).                                
006503     03  FILLER                  PIC X       VALUE ')'.                   
006504     03  FILLER                  PIC X(10)                                
006505                                 VALUE 'PUBSKRFOM('.                      
006506     03  PARM-KDCATPUB-SKRIV-FOM PIC X(6).                                
006509     03  FILLER                  PIC X       VALUE ')'.                   
006510     03  FILLER                  PIC X(10)                                
006511                                 VALUE 'PUBSKRTOM('.                      
006512     03  PARM-KDCATPUB-SKRIV-TOM PIC X(6).                                
006513     03  FILLER                  PIC X       VALUE ')'.                   
006514     03  FILLER                  PIC X(7)                                 
006515                                 VALUE 'IDUSER('.                         
006516     03  PARM-IDUSER             PIC X(8).                                
006517     03  FILLER                  PIC X       VALUE ')'.                   
006520                                                                          
006600     EJECT                                                                
006700 01  FILLER                      PIC X(16)   VALUE 'NYCKLAR'.             
006800 01  NYCKLAR-TILL-DLI.                                                    
008500     03  W-WDN5ASEQ-MIN-X.                                                
008600         05  W-IDCATNR-MIN       PIC 9(5)   VALUE ZERO.                   
008700         05  W-IDCATGRP-MIN      PIC 9(2)   VALUE ZERO.                   
008800         05  W-IDCATAVS-MIN      PIC 9(4)   VALUE 2.                      
008900     03  W-WDN5ASEQ-MAX-X.                                                
009000         05  W-IDCATNR-MAX       PIC 9(5)   VALUE 99999.                  
009100         05  W-IDCATGRP-MAX      PIC 9(2)   VALUE 99.                     
009200         05  W-IDCATAVS-MAX      PIC 9(4)   VALUE 9999.                   
009300     03  W-IDUSER-X.                                                      
009400         05  W-IDUSER            PIC X(8)   VALUE SPACE.                  
009500     03  W-IDCATNR-X.                                                     
009600         05  W-IDCATNR           PIC 9(5)   VALUE ZERO.                   
009700     EJECT                                                                
009800 01  FELMEDDELANDE.                                                       
009900*                                                                         
010000     03  FEL-1                   PIC X(50)                                
010100         VALUE 'EJ AUKTORISERAD ANVÄNDARE'.                               
010200                                                                          
010300     03  FEL-2                   PIC X(50)                                
010400         VALUE 'UPPLYSTA FÄLT FEL        '.                               
010500                                                                          
010600     03  FEL-3                   PIC X(50)                                
010700         VALUE 'FRÅN KATALOG SAKNAS      '.                               
010800                                                                          
010900     03  FEL-4                   PIC X(50)                                
011000         VALUE 'TILL KATALOG FINNS REDAN '.                               
011100                                                                          
011200     03  FEL-5                   PIC X(50)                                
011300         VALUE 'FRÅN KATALOG SAKNAS, TILL KATALOG FINNS'.                 
011400                                                                          
011500     03  FEL-6                   PIC X(50)                                
011600         VALUE 'LÅN FRÅN OCH TILL SAMMA KATALOG/GRUPP'.                   
011700                                                                          
011800     03  FEL-7                   PIC X(50)                                
011900         VALUE 'KATALOGNR EJ GILTIGT'.                                    
012000                                                                          
012100 01  MEDDELANDE.                                                          
012200     03  MED-1                  PIC X(25)                                 
012300         VALUE 'UPPDATERING GJORD       '.                                
012400                                                                          
012500     03  MED-2                  PIC X(25)                                 
012600         VALUE 'INGEN UPPDATERING GJORD '.                                
012700                                                                          
012800     03  MED-3                  PIC X(25)                                 
012900         VALUE 'SKA KAT/GRP LÄGGAS ÖVER?'.                                
012901                                                                          
012910     03  MED-4                  PIC X(29)                                 
012920         VALUE 'TRYCK PF11 FÖR UPPDATERING'.                              
013000                                                                          
013100     EJECT                                                                
013400*                        ****    TP-AREOR                                 
013500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013600 01  MID -COPY W1I52201                                                   
013700     EJECT                                                                
013710 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
013720     SKIP3                                                                
013800 01  -COPY WMSGAREA                                                       
013900     EJECT                                                                
014000     03  MOD REDEFINES MSG-AREA.                                          
014010       05  -COPY W1O52201                                                 
014100     EJECT                                                                
014400 01  -COPY WMFSAREA.                                                      
014500     EJECT                                                                
014600*****                                                                     
014700*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014800*****                                                                     
014900 01  IMS-WS.                                                              
015000     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
015100     SKIP3                                                                
015200*****                    **** STATUS-KOD FRÅN IMS                         
015300     03  STATUS-WS               PIC X(2).                                
015400         88  SEGMENT-FINNS                   VALUE '  '.                  
015500         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
015600     SKIP3                                                                
015700     03  GODK-STATUSKODER.                                                
015800         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
015900     SKIP3                                                                
016000 01  SSA1                        PIC X(64).                               
016100 01  SSA2                        PIC X(64).                               
016200     EJECT                                                                
016300*                            IMS FUNKTIONSKODER                           
016400 01  -COPY W0003                                                          
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'IO-AREA'.             
016610 01  DLI-IO-AREA.                                                         
016620     03  IO-AREA                 PIC X(500)  VALUE SPACE.                 
016630     SKIP3                                                                
016640*    03  WLKATH01  -COPY WDN501  -RED IO-AREA -PRE WLKATH-.               
016650     EJECT                                                                
016660     03  IO-AREA-2               PIC X(500)  VALUE SPACE.                 
016670     SKIP3                                                                
016680*    03  WLKATM01  -COPY WDN101  -RED IO-AREA-2 -PRE WLKATM-.             
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300     SKIP2                                                                
018400 01  -COPY W0009     -PRE MSG-                                            
018500     EJECT                                                                
018600 01  -COPY W0009     -PRE ALT-                                            
019300     EJECT                                                                
019400 01  -COPY W0008     -PRE WLKATH-                                         
019500         05  FILLER              PIC X.                                   
019600     EJECT                                                                
019700 01  -COPY W0008     -PRE WLKATM-                                         
019800         05  FILLER              PIC X.                                   
019900     EJECT                                                                
020000 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB                                
020100                                      WLKATH-PCB WLKATM-PCB.              
020110 MAIN SECTION.                                                            
020200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
020300                                      WLKATH-PCB WLKATM-PCB.              
020400                                                                          
020500     PERFORM IMS-GET-MSG                                                  
020600     IF SEGMENT-FINNS                                                     
020700       PERFORM A-INIT-SPARA-INPUT                                         
020800       IF MFS-IDTRANS = '1522'                                            
020900         IF  MFS-UPDATE                                                   
021100           PERFORM C-KOLLA-INDATA                                         
021200                                                                          
021210           IF  INDATA-OK = JA                                             
021220           AND MID-FLTABORT NOT = NEJ                                     
021400             PERFORM D-UPPD-PARAMETER                                     
021500             PERFORM E-STARTA-JOB                                         
021621             MOVE MED-1 TO MOD-TEMFSINF                                   
021630             PERFORM F-RENSA-BILD                                         
021700           ELSE                                                           
021701             MOVE MED-2 TO MOD-TEMFSINF                                   
021715             IF MID-FLTABORT = JA                                         
021716             OR MID-FLTABORT = NEJ                                        
021717               PERFORM F-RENSA-BILD                                       
021718             ELSE                                                         
021720               PERFORM G-VISA-BILD-IGEN                                   
021780             END-IF                                                       
021900           END-IF                                                         
023200         ELSE                                                             
023201           IF MID-FLTABORT NOT = ALL '+'                                  
023204             MOVE MED-3 TO MOD-MESSAGE-RAD10                              
023205             MOVE MFS-OEPPNA-ALFA-FAELT TO                                
023206                   MOD-FLTABORT-ATTR                                      
023207           END-IF                                                         
023208           MOVE MED-4 TO MOD-TEMFSINF                                     
023210           PERFORM G-VISA-BILD-IGEN                                       
023240         END-IF                                                           
023300       ELSE                                                               
023400         PERFORM F-RENSA-BILD                                             
023700       END-IF                                                             
023710       MOVE LENGTH OF MOD      TO MSG-KVLL                                
023720       ADD +4                  TO MSG-KVLL                                
023730       PERFORM IMS-INSERT-MSG                                             
023800     END-IF                                                               
023900     MOVE ZERO TO RETURN-CODE                                             
024000     GOBACK                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 A-INIT-SPARA-INPUT SECTION.                                              
024400                                                                          
024500     IF MSG-DUBBLA-TRANSKODER                                             
024600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I52201                 
024700       MOVE MSG-IDTRANS-2   TO MFS-IDTRANS                                
024800       MOVE MSG-KDMFSFOR-2  TO MFS-KDMFSFOR                               
024900       MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                
025000     ELSE                                                                 
025100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I52201                  
025200       MOVE MSG-IDTRANS-1   TO MFS-IDTRANS                                
025300       MOVE MSG-KDMFSFOR-1  TO MFS-KDMFSFOR                               
025400       MOVE ' '             TO MFS-KDTRTYP                                
025500     END-IF                                                               
025600     MOVE LOW-VALUE TO MOD-W1O52201                                       
025700     MOVE 'W1O52201' TO MFS-IDMOD                                         
025800     MOVE '1522' TO MOD-IDTRANS                                           
025900                                                                          
026200     MOVE MFS-RENSA-FAELT  TO MOD-MESSAGE-RAD1                            
026300                              MOD-MESSAGE-RAD10                           
026400                              MOD-TEMFSINF                                
026500                              MOD-FLTABORT                                
026600     MOVE MFS-STAENG-FAELT TO MOD-FLTABORT-ATTR                           
026620                                                                          
026700                                                                          
026800     ACCEPT A-DAGENS-DATUM FROM DATE                                      
026900     ACCEPT A-TID FROM TIME                                               
026910                                                                          
026920     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
026930     MOVE A-DAGENS-DATUM                                                  
026940                   TO DAT-I-TIDATUM                                       
026950                                                                          
026960     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
026970                     DAT-O-TIDATUM DAT-KDSVAR                             
026980                                                                          
026990     IF DAT-KDSVAR-OK                                                     
026991****             HÄMTA SEKELSIFFROR                                       
026992                                                                          
026993       MOVE DAT-TISEKEL    TO DAGENS-AAR(1:2)                             
026995                              W-DAGENS-VECKA (1:2)                        
026996       MOVE DAT-TIAA-VECKA                                                
026997                           TO W-DAGENS-VECKA (3:2)                        
026998       MOVE DAT-TIVV       TO W-DAGENS-VECKA (5:2)                        
026999                                                                          
027000     ELSE                                                                 
027001         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
027002         DELIMITED BY SIZE INTO FELTEXT                                   
027003         CALL FELLOG                                                      
027004     END-IF                                                               
027005                                                                          
027006     MOVE DAGENS-DATUM(1:2)  TO DAGENS-AAR(3:2)                           
027007                                                                          
027008     COMPUTE WS-TIAAAA(1) = DAGENS-AAR - 1                                
027009     COMPUTE WS-TIAAAA(2) = DAGENS-AAR                                    
027010     COMPUTE WS-TIAAAA(3) = DAGENS-AAR + 1                                
027011     COMPUTE WS-TIAAAA(4) = DAGENS-AAR + 2                                
027020     .                                                                    
027100                                                                          
027200     EJECT                                                                
027300                                                                          
028800 C-KOLLA-INDATA SECTION.                                                  
028900                                                                          
029000     MOVE JA  TO INDATA-OK                                                
029100     MOVE JA  TO FRAN-KATALOG-FINNS                                       
029200     MOVE NEJ TO TO-KATALOG-FINNS                                         
029300                                                                          
029400     IF MID-IDCATNR-LAES = ALL '+'                                        
029500       MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-LAES-ATTR                    
029600       MOVE NEJ TO INDATA-OK                                              
029700     END-IF                                                               
029800     IF MID-IDCATNR-SKRIV = ALL '+'                                       
029900       MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-SKRIV-ATTR                   
030000       MOVE NEJ TO INDATA-OK                                              
030100     END-IF                                                               
030110                                                                          
030218                                                                          
030219     IF MID-KDCATPUB-R-LAES = ALL '+'                                     
030220     OR MID-KDCATPUB-R-LAES = SPACE                                       
030221       MOVE MFS-ALFA-FAELT-RAETT                                          
030222                      TO MOD-KDCATPUB-R-LAES-ATTR                         
030223     ELSE                                                                 
030229                                                                          
030230*******   KONTROLLERA MOT DATUMRUTIN                                      
030231                                                                          
030232        MOVE MID-KDCATPUB-R-LAES                                          
030233                                TO WS-KDCATPUB-R-AVV                      
030234        PERFORM S50-Y2K-KDCATPUB-R                                        
030278        IF WS-KDCATPUB-AAAAVV < W-DAGENS-VECKA                            
030279          MOVE MFS-ALFA-FAELT-FEL                                         
030280                                TO MOD-KDCATPUB-R-LAES-ATTR               
030281          MOVE NEJ TO INDATA-OK                                           
030282        ELSE                                                              
030290          MOVE WS-KDCATPUB-AAAAVV (3:4)                                   
030291                                TO DAT-I-TIDATUM                          
030292          MOVE 'AAVV  '         TO DAT-KDDATFORM                          
030293                                                                          
030294          CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                 
030295                            DAT-O-TIDATUM DAT-KDSVAR                      
030296                                                                          
030297          IF DAT-KDSVAR-OK                                                
030298            MOVE MFS-ALFA-FAELT-RAETT                                     
030299                               TO MOD-KDCATPUB-R-LAES-ATTR                
030300          ELSE                                                            
030301            MOVE MFS-ALFA-FAELT-FEL                                       
030302                               TO MOD-KDCATPUB-R-LAES-ATTR                
030303            MOVE NEJ TO INDATA-OK                                         
030305          END-IF                                                          
030306        END-IF                                                            
030312                                                                          
030318     END-IF                                                               
030319                                                                          
030320     IF MID-KDCATPUB-R-SKRIV-FOM = ALL '+'                                
030321     OR MID-KDCATPUB-R-SKRIV-FOM = SPACE                                  
030322        MOVE MFS-ALFA-FAELT-RAETT                                         
030323                      TO MOD-KDCATPUB-R-SKRIV-FOM-ATTR                    
030324     ELSE                                                                 
030328                                                                          
030329*******   KONTROLLERA MOT DATUMRUTIN                                      
030330                                                                          
030331        MOVE MID-KDCATPUB-R-SKRIV-FOM                                     
030340                                TO WS-KDCATPUB-R-AVV                      
030350        PERFORM S50-Y2K-KDCATPUB-R                                        
030360        IF WS-KDCATPUB-AAAAVV < W-DAGENS-VECKA                            
030376          MOVE MFS-ALFA-FAELT-FEL                                         
030377                                TO MOD-KDCATPUB-R-SKRIV-FOM-ATTR          
030378          MOVE NEJ TO INDATA-OK                                           
030379        ELSE                                                              
030380          MOVE WS-KDCATPUB-AAAAVV (3:4)                                   
030387                                TO DAT-I-TIDATUM                          
030388          MOVE 'AAVV  '         TO DAT-KDDATFORM                          
030389                                                                          
030390          CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                 
030391                                DAT-O-TIDATUM DAT-KDSVAR                  
030392                                                                          
030393          IF DAT-KDSVAR-OK                                                
030394            MOVE MFS-ALFA-FAELT-RAETT                                     
030395                                TO MOD-KDCATPUB-R-SKRIV-FOM-ATTR          
030396          ELSE                                                            
030397            MOVE MFS-ALFA-FAELT-FEL                                       
030398                                TO MOD-KDCATPUB-R-SKRIV-FOM-ATTR          
030399            MOVE NEJ TO INDATA-OK                                         
030400          END-IF                                                          
030401        END-IF                                                            
030411                                                                          
030417     END-IF                                                               
030418                                                                          
030419     IF MID-KDCATPUB-R-SKRIV-TOM = ALL '+'                                
030420     OR MID-KDCATPUB-R-SKRIV-TOM = SPACE                                  
030421       MOVE MFS-ALFA-FAELT-RAETT                                          
030422                      TO MOD-KDCATPUB-R-SKRIV-TOM-ATTR                    
030423     ELSE                                                                 
030424                                                                          
030425*******   KONTROLLERA MOT DATUMRUTIN                                      
030426                                                                          
030427        MOVE MID-KDCATPUB-R-SKRIV-TOM                                     
030428                                TO WS-KDCATPUB-R-AVV                      
030429        PERFORM S50-Y2K-KDCATPUB-R                                        
030430        IF WS-KDCATPUB-AAAAVV < W-DAGENS-VECKA                            
030463          MOVE MFS-ALFA-FAELT-FEL                                         
030464                                TO MOD-KDCATPUB-R-SKRIV-TOM-ATTR          
030465          MOVE NEJ TO INDATA-OK                                           
030466        ELSE                                                              
030472          MOVE WS-KDCATPUB-AAAAVV (3:4)                                   
030473                                    TO DAT-I-TIDATUM                      
030474          MOVE 'AAVV  '       TO DAT-KDDATFORM                            
030475                                                                          
030476          CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                 
030477                                DAT-O-TIDATUM DAT-KDSVAR                  
030478                                                                          
030479          IF DAT-KDSVAR-OK                                                
030480            MOVE MFS-ALFA-FAELT-RAETT                                     
030481                               TO MOD-KDCATPUB-R-SKRIV-TOM-ATTR           
030482          ELSE                                                            
030483            MOVE MFS-ALFA-FAELT-FEL                                       
030484                               TO MOD-KDCATPUB-R-SKRIV-TOM-ATTR           
030485            MOVE NEJ TO INDATA-OK                                         
030486          END-IF                                                          
030487        END-IF                                                            
030497                                                                          
030498     END-IF                                                               
030499                                                                          
030500     IF MID-FLTABORT NOT = ALL '+'                                        
030501       IF MID-FLTABORT = JA OR NEJ                                        
030502         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTABORT-ATTR                   
030503       ELSE                                                               
030504         MOVE MED-3             TO MOD-MESSAGE-RAD10                      
030505         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTABORT-ATTR                     
030506         MOVE NEJ TO INDATA-OK                                            
030507       END-IF                                                             
030510     END-IF                                                               
030600                                                                          
031100     IF INDATA-OK = JA                                                    
031200       MOVE MID-IDCATNR-SKRIV TO W-IDCATNR                                
031300       PERFORM IMS-GET-WLKATM01                                           
031400       IF SEGMENT-SAKNAS                                                  
031500         MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-SKRIV-ATTR                 
031600         MOVE NEJ               TO INDATA-OK                              
031700         MOVE JA                TO FEL-KATALOGNR                          
032000       END-IF                                                             
032100     END-IF                                                               
032200     INSPECT MID-IDCATGRP-LAES REPLACING ALL '+' BY ZERO                  
032300     INSPECT MID-IDCATGRP-SKRIV REPLACING ALL '+' BY ZERO                 
032310     INSPECT MID-KDCATPUB-R-LAES REPLACING ALL '+' BY SPACE               
032320     INSPECT MID-KDCATPUB-R-SKRIV-FOM                                     
032330                             REPLACING ALL '+' BY SPACE                   
032340     INSPECT MID-KDCATPUB-R-SKRIV-TOM                                     
032350                             REPLACING ALL '+' BY SPACE                   
032400                                                                          
032500     IF INDATA-OK = JA                                                    
032600       IF MID-IDCATNR-LAES = MID-IDCATNR-SKRIV                            
032700       AND MID-IDCATGRP-LAES = MID-IDCATGRP-SKRIV                         
032800         MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-LAES-ATTR                  
032900         MOVE NEJ TO INDATA-OK                                            
033000         MOVE FEL-6 TO MOD-MESSAGE-RAD1                                   
033100       ELSE                                                               
033200         MOVE MID-IDCATNR-LAES   TO W-IDCATNR-MIN                         
033300                                    W-IDCATNR-MAX                         
033400         IF MID-IDCATGRP-LAES NOT = ZERO                                  
033500           MOVE MID-IDCATGRP-LAES TO W-IDCATGRP-MIN                       
033600                                      W-IDCATGRP-MAX                      
033700         END-IF                                                           
033800         PERFORM IMS-GET-WLKATH01                                         
033900         IF SEGMENT-FINNS                                                 
034000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATNR-LAES-ATTR              
034100                                     MOD-IDCATGRP-LAES-ATTR               
034200         ELSE                                                             
034300           MOVE NEJ TO FRAN-KATALOG-FINNS                                 
034400           MOVE NEJ TO INDATA-OK                                          
034500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATNR-LAES-ATTR                
034600                                   MOD-IDCATGRP-LAES-ATTR                 
034700         END-IF                                                           
034800         MOVE MID-IDCATNR-SKRIV  TO W-IDCATNR-MIN                         
034900                                    W-IDCATNR-MAX                         
035000         IF MID-IDCATGRP-SKRIV NOT = ZERO                                 
035100           MOVE MID-IDCATGRP-SKRIV TO W-IDCATGRP-MIN                      
035200                                    W-IDCATGRP-MAX                        
035300         END-IF                                                           
035600         PERFORM IMS-GET-WLKATH01                                         
035700         IF SEGMENT-FINNS                                                 
035800           MOVE JA TO TO-KATALOG-FINNS                                    
035810           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDCATNR-SKRIV-ATTR             
035820                                      MOD-IDCATGRP-SKRIV-ATTR             
035900         END-IF                                                           
035901                                                                          
035902         IF FRAN-KATALOG-FINNS = NEJ                                      
035903           IF TO-KATALOG-FINNS = JA                                       
035904             IF MID-FLTABORT = ALL '+'                                    
035905               MOVE NEJ TO INDATA-OK                                      
035906               MOVE FEL-5 TO MOD-MESSAGE-RAD1                             
035910             ELSE                                                         
035911               MOVE FEL-3 TO MOD-MESSAGE-RAD1                             
035912               MOVE NEJ TO INDATA-OK                                      
035913             END-IF                                                       
035914           ELSE                                                           
035915             MOVE FEL-3 TO MOD-MESSAGE-RAD1                               
035916             MOVE NEJ TO INDATA-OK                                        
035917           END-IF                                                         
035918         ELSE                                                             
035919           IF TO-KATALOG-FINNS = JA                                       
035920             IF MID-FLTABORT = ALL '+'                                    
035921               MOVE NEJ TO INDATA-OK                                      
035922               MOVE FEL-4 TO MOD-MESSAGE-RAD1                             
035923               MOVE MED-3 TO MOD-MESSAGE-RAD10                            
035924               MOVE MFS-OEPPNA-ALFA-FAELT TO                              
035925                     MOD-FLTABORT-ATTR                                    
035926             END-IF                                                       
035927           END-IF                                                         
035928         END-IF                                                           
035929                                                                          
035930         MOVE MID-KDCATPUB-R-SKRIV-FOM                                    
035931                             TO WS-KDCATPUB-R-AVV                         
035932         PERFORM S50-Y2K-KDCATPUB-R                                       
035933         MOVE WS-KDCATPUB-AAAAVV                                          
035934                             TO WS-KDCATPUB-SKRIV-FOM                     
035935                                                                          
035936         MOVE MID-KDCATPUB-R-SKRIV-TOM                                    
035937                             TO WS-KDCATPUB-R-AVV                         
035938         PERFORM S50-Y2K-KDCATPUB-R                                       
035939         MOVE WS-KDCATPUB-AAAAVV                                          
035940                             TO WS-KDCATPUB-SKRIV-TOM                     
035941                                                                          
035942         IF (MID-KDCATPUB-R-SKRIV-FOM = SPACE                             
035943         AND MID-KDCATPUB-R-SKRIV-TOM NOT = SPACE)                        
035950         OR (MID-KDCATPUB-R-SKRIV-FOM NOT = SPACE                         
035960         AND MID-KDCATPUB-R-SKRIV-TOM NOT = SPACE                         
035970         AND WS-KDCATPUB-SKRIV-FOM >                                      
035980                          WS-KDCATPUB-SKRIV-TOM)                          
035990              MOVE MFS-ALFA-FAELT-FEL                                     
036000                                 TO MOD-KDCATPUB-R-SKRIV-FOM-ATTR         
036010                                    MOD-KDCATPUB-R-SKRIV-TOM-ATTR         
036100              MOVE NEJ TO INDATA-OK                                       
036200         END-IF                                                           
038700       END-IF                                                             
038800                                                                          
038820     ELSE                                                                 
038900       IF FEL-KATALOGNR = NEJ                                             
039000         MOVE FEL-2 TO MOD-MESSAGE-RAD1                                   
039100       ELSE                                                               
039200         MOVE FEL-7 TO MOD-MESSAGE-RAD1                                   
039300       END-IF                                                             
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700                                                                          
039900 D-UPPD-PARAMETER SECTION.                                                
040000                                                                          
041100     MOVE MID-IDCATNR-LAES     TO PARM-IDCATNR-LAES                       
041200     MOVE MID-IDCATNR-SKRIV    TO PARM-IDCATNR-SKRIV                      
041300     MOVE MID-IDCATGRP-LAES    TO PARM-IDCATGRP-LAES                      
041400     MOVE MID-IDCATGRP-SKRIV   TO PARM-IDCATGRP-SKRIV                     
041411     MOVE MID-KDCATPUB-R-LAES  TO WS-KDCATPUB-R-AVV                       
041412     PERFORM S50-Y2K-KDCATPUB-R                                           
041413     MOVE WS-KDCATPUB-AAAAVV   TO PARM-KDCATPUB-LAES                      
041415     MOVE MID-KDCATPUB-R-SKRIV-FOM                                        
041418                               TO WS-KDCATPUB-R-AVV                       
041419     PERFORM S50-Y2K-KDCATPUB-R                                           
041422     MOVE WS-KDCATPUB-AAAAVV   TO PARM-KDCATPUB-SKRIV-FOM                 
041423     MOVE MID-KDCATPUB-R-SKRIV-TOM                                        
041425                               TO WS-KDCATPUB-R-AVV                       
041426     PERFORM S50-Y2K-KDCATPUB-R                                           
041428     MOVE WS-KDCATPUB-AAAAVV   TO PARM-KDCATPUB-SKRIV-TOM                 
041430     MOVE MSG-SIGNON-USERID    TO PARM-IDUSER                             
041500                                                                          
044800     .                                                                    
044900     EJECT                                                                
045000                                                                          
045100 E-STARTA-JOB SECTION.                                                    
045200                                                                          
046240     MOVE '1522'           TO MSGSOP-IDTRANS                              
046250     MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                             
046260     MOVE 'W154B7    '     TO MSGSOP-IDPROCESS                            
046270     MOVE 'O'              TO MSGSOP-KDSOPFUNK                            
046280     MOVE PARM-TESYMBV     TO MSGSOP-TESYMBV                              
046290     PERFORM IMS-INSERT-ALT-MSG                                           
046291                                                                          
046292     MOVE '1522'           TO MSGSOP-IDTRANS                              
046293     MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                             
046294     MOVE 'W154B7BLK '     TO MSGSOP-IDPROCESS                            
046295     MOVE 'O'              TO MSGSOP-KDSOPFUNK                            
046296     MOVE SPACE            TO MSGSOP-TESYMBV                              
046297     PERFORM IMS-INSERT-ALT-MSG                                           
046300     .                                                                    
046400                                                                          
046500     EJECT                                                                
046600  F-RENSA-BILD SECTION.                                                   
046700                                                                          
046800     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-LAES                             
046900                             MOD-IDCATNR-SKRIV                            
047000                             MOD-IDCATGRP-LAES                            
047100                             MOD-IDCATGRP-SKRIV                           
047110                             MOD-KDCATPUB-R-LAES                          
047111                             MOD-KDCATPUB-R-SKRIV-FOM                     
047112                             MOD-KDCATPUB-R-SKRIV-TOM                     
047200                             MOD-FLTABORT                                 
047300     .                                                                    
047400                                                                          
047500     EJECT                                                                
047600  G-VISA-BILD-IGEN SECTION.                                               
047700                                                                          
047800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDCATNR-LAES                           
047900                               MOD-IDCATNR-SKRIV                          
048000                               MOD-IDCATGRP-LAES                          
048100                               MOD-IDCATGRP-SKRIV                         
048101                               MOD-KDCATPUB-R-LAES                        
048102                               MOD-KDCATPUB-R-SKRIV-FOM                   
048103                               MOD-KDCATPUB-R-SKRIV-TOM                   
048200                               MOD-FLTABORT                               
048300     .                                                                    
048400     EJECT                                                                
048410*                                                                         
048420* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
048430* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
048440*                                                                         
048450*    -COPY W150Y2K1                                                       
048500     EJECT                                                                
048650* IMS SEKTIONER                                                           
048700     SKIP3                                                                
048800 IMS-GET-MSG SECTION.                                                     
048900                                                                          
049000     MOVE '  QC' TO GODK-STATUSKODER                                      
049100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
049200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     .                                                                    
049500     SKIP3                                                                
049600 IMS-INSERT-MSG SECTION.                                                  
049700                                                                          
049800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
049900     MOVE SPACE TO GODK-STATUSKODER                                       
050000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
050100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400     SKIP3                                                                
050500 IMS-INSERT-ALT-MSG SECTION.                                              
050600                                                                          
050800     MOVE SPACE TO GODK-STATUSKODER                                       
050900     CALL CBLTDLI USING PURG ALT-PCB W-PROG-TO-PROG-SW                    
051000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     .                                                                    
051300     EJECT                                                                
059200 IMS-GET-WLKATH01 SECTION.                                                
059300                                                                          
059400     STRING 'WLKATH01(WDN5ASEQ>=' W-WDN5ASEQ-MIN-X                        
059500                    '&WDN5ASEQ=<' W-WDN5ASEQ-MAX-X ')'                    
059600             DELIMITED BY SIZE INTO SSA1                                  
059700     MOVE '  GE' TO GODK-STATUSKODER                                      
059800     CALL CBLTDLI USING GU WLKATH-PCB IO-AREA SSA1                        
059900     MOVE WLKATH-STATUS-CODE TO STATUS-WS                                 
060000     PERFORM IMS-STATUSKONTROLL                                           
060100     .                                                                    
060200     EJECT                                                                
060300 IMS-GET-WLKATM01 SECTION.                                                
060400                                                                          
060500     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
060600             DELIMITED BY SIZE INTO SSA1                                  
060700     MOVE '  GE' TO GODK-STATUSKODER                                      
060800     CALL CBLTDLI USING GU WLKATM-PCB IO-AREA-2 SSA1                      
060900     MOVE WLKATM-STATUS-CODE TO STATUS-WS                                 
061000     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061200     EJECT                                                                
061300 IMS-STATUSKONTROLL SECTION.                                              
061400     SET STATUS-IX TO 1                                                   
061500     SEARCH GODK-STATUS                                                   
061510       AT END                                                             
061520         CALL FELLOG                                                      
061600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
061610       CONTINUE                                                           
061700     END-SEARCH                                                           
061800     .                                                                    
