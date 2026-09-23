001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021300.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/06/30.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002010*        PROGRAMMET HAR TVÅ FUNKTIONER. DESSA TVÅ ÄR:                     
002020*        1. HANTERA INMATADE NYCKLAR FRÅN BILD 5213 OCH HÄMTA             
002030*        INFORMATION FRÅN STYRDB OCH SEDAN PRESENTERA DETTA               
002040*        PÅ BILD 5213                                                     
002050*        2. OM MAN ANGER "S" PÅ NÅGON AV DETALJRADERNA PÅ BILD            
002060*        5213 OCH SEDAN TRYCKER "ENTER" SKALL HOPP SKE TILL BILD          
002070*        5221                                                             
002100*                                                                         
002210*        PROGRAMMET LÄSER      WDH5                                       
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W5T213                                              
002600*        MID:         W5I21301                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W5O21301                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W5021300'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +15   COMP SYNC.        
004500*    --- GENERELLA ARBETSFÄLT                                             
004510 01  W-AREA-INDEX.                                                        
004520     03  W-INDX                  PIC S9(4)  VALUE +0   COMP SYNC.         
004521                                                                          
004523 01  WS-IDFTG                    PIC 9(2)    VALUE ZERO.                  
004524 01  WS-KDEKHHT                  PIC X(3)    VALUE SPACE.                 
004525 01  WS-KDEKSHT                  PIC X(3)    VALUE SPACE.                 
004526 01  WS-BEEKSHT                  PIC X(25)   VALUE SPACE.                 
004527 01  WS-KDEKNIVA                 PIC X(5)    VALUE SPACE.                 
004528 01  WS-IDSYSMOT                 PIC X(6)    VALUE SPACE.                 
004529 01  WS-IDPTYP                   PIC X(3)    VALUE SPACE.                 
004530                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004900                                                                          
004901 77  BYT-SW                      PIC X       VALUE 'N'.                   
004902     88  BYT-BILD                            VALUE 'J'.                   
004903     88  BYT-EJ-BILD                         VALUE 'N'.                   
004904                                                                          
004910 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004920     88  ALLT-OK                             VALUE 'J'.                   
004930                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '5213'.                
005510     88  HOPP-MID                            VALUE '5221'.                
005600     88  GODK-MID                            VALUE '5213' '5214'          
005800                                                   '5215' '5216'          
005900                                                   '5217' '5218'          
006000                                                   '5219'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007820     03  KDEKHHT-MISSING         PIC X(3)    VALUE '269'.                 
007830     03  KDEKSHT-MISSING         PIC X(3)    VALUE '270'.                 
007840     03  KDEKNIVA-MISSING        PIC X(3)    VALUE '271'.                 
007850     03  SYSTNAME-MISSING        PIC X(3)    VALUE '272'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
008503     SKIP3                                                                
008504*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008505*                                                                         
008506 01  SPAR-AREA.                                                           
008507     03  SPAR-IDTRANS             PIC X(4)    VALUE '5213'.               
008508     03  SPAR-IDFTG               PIC 9(2)    VALUE ZERO.                 
008509     03  SPAR-KDEKHHT             PIC X(3)    VALUE SPACE.                
008510     03  SPAR-KDEKSHT             PIC X(3)    VALUE SPACE.                
008511     03  SPAR-KDEKNIVA            PIC X(5)    VALUE SPACE.                
008512     03  SPAR-BILD                PIC X(4).                               
008513     03  SPAR-IDSYSMOT            PIC X(6)    VALUE SPACE.                
008514     03  SPAR-IDPTYP              PIC X(3).                               
008515     03  SPAR-IDFTG-ENTER         PIC 9(2).                               
008515     03  SPAR-IDFTG-NEXT          PIC 9(2)    VALUE ZERO.                 
008517     03  SPAR-KDEKHHT-ENTER       PIC X(3).                               
008518     03  SPAR-KDEKHHT-NEXT        PIC X(3)    VALUE SPACE.                
008519     03  SPAR-KDEKSHT-ENTER       PIC X(3).                               
008520     03  SPAR-KDEKSHT-NEXT        PIC X(3)    VALUE SPACE.                
008521     03  SPAR-KDEKNIVA-ENTER      PIC X(5).                               
008522     03  SPAR-KDEKNIVA-NEXT       PIC X(5)    VALUE SPACE.                
008523     03  SPAR-IDSYSMOT-ENTER      PIC X(6).                               
008524     03  SPAR-IDSYSMOT-NEXT       PIC X(6)    VALUE SPACE.                
008525     03  SPAR-IDPTYP-ENTER        PIC X(3).                               
008530     03  SPAR-IDPTYP-NEXT         PIC X(3)    VALUE SPACE.                
008572     03  SPAR-VILKET-SEGMENT      PIC X(8)    VALUE SPACE.                
008573     03  SPAR-BEEKSHT             PIC X(25)   VALUE SPACE.                
008580     03  SPAR-TABELL.                                                     
008590       05  SPAR-WDH5   OCCURS 15.                                         
008591         07  SPAR-WDH5-TAB.                                               
008592           09  SPAR-IDFTG-TAB     PIC 9(2)    VALUE ZERO.                 
008593           09  SPAR-KDEKHHT-TAB   PIC X(3)    VALUE SPACE.                
008594           09  SPAR-KDEKSHT-TAB   PIC X(3)    VALUE SPACE.                
008595           09  SPAR-KDEKNIVA-TAB  PIC X(5)    VALUE SPACE.                
008596           09  SPAR-IDSYSMOT-TAB  PIC X(6)    VALUE SPACE.                
008597           09  SPAR-IDPTYP-TAB    PIC X(3)    VALUE SPACE.                
008598           09  SPAR-IDSEKVNR-TAB  PIC 9(3)    VALUE ZERO.                 
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W5I21301                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O21301                                                 
009900     EJECT                                                                
009910 01  W-PROG-TO-PROG-SW-5221.                                              
009920     03  M-SW-LL-5221            PIC S9(4)   VALUE +240 COMP SYNC.        
009930     03  M-SW-Z1-Z2-5221         PIC X(2)    VALUE LOW-VALUE.             
009940     03  M-SW-KDTRANS-5221       PIC X(8)    VALUE 'W5T221  '.            
009950     03  M-SW-IDTRANS-5221       PIC X(4)    VALUE '5213'.                
009960     03  M-SW-KDMFSTYP-5221      PIC X(1)    VALUE '2'.                   
009970                                                                          
009980*    03  MID -COPY W5I22101 -PRE 5221-                                    
009990     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011010                                                                          
011021     03  W-WDH501KY-X.                                                    
011022         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011023         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
011024                                                                          
011030     03  W-KDEKSHT-X.                                                     
011031         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
011032                                                                          
011039     03  W-KDEKNIVA-X.                                                    
011040         05  W-KDEKNIVA          PIC X(5)    VALUE SPACE.                 
011047                                                                          
011048     03  W-WDH531KY-X.                                                    
011049         05  W-IDSYSMOT-X        PIC X(6)    VALUE SPACE.                 
011050         05  W-IDPTYP-X          PIC X(3)    VALUE SPACE.                 
011060         05  W-IDSEKVNR-X        PIC S9(3)   COMP-3 VALUE +1.             
011100                                                                          
011196     03  W-WDH5ASEQ-X.                                                    
011197         05  W-IDSYSMOT          PIC X(6)    VALUE SPACE.                 
011198         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
011199         05  W-IDSEKVNR          PIC S9(3)   COMP-3 VALUE +1.             
011200                                                                          
011205     SKIP2                                                                
011210*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011610     88  BAS-SLUT                            VALUE 'GB'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012210 01  SSA3                        PIC X(64).                               
012220 01  SSA4                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
012802 01  DLI-IO-WDH531.                                                       
012803*    03  -COPY WDH531 -PRE WDH5-                                          
012804     EJECT                                                                
012810 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
012820     SKIP3                                                                
012830 01  DLI-IO-AREA.                                                         
012840   03  IO-AREA               PIC X(350).                                  
012850     SKIP3                                                                
012860 03  WDH501 -COPY WDH501                     -RED IO-AREA                 
012870     EJECT                                                                
012880 03  WDH511 -COPY WDH511                     -RED IO-AREA                 
012890     EJECT                                                                
012891 03  WDH521 -COPY WDH521                     -RED IO-AREA                 
012892     EJECT                                                                
012893 03  WDH531 -COPY WDH531                     -RED IO-AREA                 
012894     EJECT                                                                
012900                                                                          
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013501     EJECT                                                                
013510*01  -COPY W0009   -PRE ALT-                                              
013520     EJECT                                                                
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE WDH5-                                              
013810     05  FILLER                  PIC X.                                   
013900                                                                          
014000*01  -COPY W0008  -PRE WDH51-                                             
014001     05  FILLER                  PIC X.                                   
014002                                                                          
014003     EJECT                                                                
014004 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
014005                           WDH5-PCB WDH51-PCB.                            
014006 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
014020                           WDH5-PCB WDH51-PCB.                            
014200                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014800          IF BYT-BILD                                                     
014900             PERFORM H-BYT-BILD                                           
014901          ELSE                                                            
014902           IF MFS-FIRST                                                   
014903             PERFORM C-FOERSTA-SIDA                                       
014904           ELSE                                                           
014905             IF MFS-NEXT                                                  
014906               PERFORM D-NAESTA-SIDA                                      
014907             ELSE                                                         
014908               PERFORM E-SAMMA-SIDA                                       
014909             END-IF                                                       
014910           END-IF                                                         
014920          END-IF                                                          
014930          IF ALLT-OK                                                      
015200            PERFORM F-LAES-VISA-INFO                                      
015210          END-IF                                                          
015300       END-IF                                                             
015310       IF BYT-EJ-BILD                                                     
015600         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21301 + 4                    
015700         PERFORM IMS-INSERT-MSG                                           
015710       END-IF                                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21301                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21301                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W5O213N1' TO MFS-IDMOD                                         
018300     MOVE '5213' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'             TO MSGI-WMSGINIT                            
019900     MOVE '001'               TO MSGI-KDCALL                              
020000     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
020100     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
020200     MOVE '5213'              TO MSGI-IDTRANS                             
020300     IF EGEN-MID OR HOPP-MID OR GODK-MID                                  
020400       MOVE MID-KDEKHHT-IN    TO MSGI-KDEKHHT                             
020401       MOVE MID-KDEKSHT-IN    TO MSGI-KDEKSHT                             
020402       MOVE MID-KDEKNIVA-IN   TO MSGI-KDEKNIVA                            
020403       MOVE MID-IDSYSMOT-IN   TO MSGI-IDSYSMOT                            
020404       MOVE MID-IDPTYP-IN     TO MSGI-IDPTYP                              
020405     ELSE                                                                 
020407       MOVE SPACE             TO MSGI-KDEKHHT                             
020410       MOVE SPACE             TO MSGI-KDEKSHT                             
020411       MOVE SPACE             TO MSGI-KDEKNIVA                            
020412       MOVE SPACE             TO MSGI-IDSYSMOT                            
020413       MOVE SPACE             TO MSGI-IDPTYP                              
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020601     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
020602                                                                          
020603     IF MSGI-IDLAND-SPR = 'GB'                                            
020604       MOVE 'GB' TO MED-IDSKYLT                                           
020605     ELSE                                                                 
020606       MOVE 'S' TO MED-IDSKYLT                                            
020607     END-IF                                                               
020608                                                                          
020712     MOVE JA    TO ALLT-SW                                                
020800     MOVE JA    TO NYCKLAR-SW                                             
020810     MOVE NEJ   TO BYT-SW                                                 
020820     MOVE SPACE TO MED-IDMFSFEL                                           
020830     MOVE SPACE TO MED-IDMFSINF                                           
020900                                                                          
021001                                                                          
021002*    -- KONTROLL AV KDEKHHT                                               
021003     MOVE MFS-RENSA-FAELT   TO MOD-KDEKHHT-IN                             
021004                                                                          
021009     IF MSGI-KDEKHHT NUMERIC AND MSGI-KDEKHHT > ZERO                      
021011       MOVE MSGI-KDEKHHT    TO W-KDEKHHT                                  
021017     END-IF                                                               
021018                                                                          
021019     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021020       MOVE MSGI-IDFTG      TO W-IDFTG                                    
021021     END-IF                                                               
021022                                                                          
021023     IF NOT EGEN-MID AND MSGI-KDEKHHT = SPACE                             
021024       MOVE NEJ             TO NYCKLAR-SW                                 
021025     END-IF                                                               
021028                                                                          
021029*    -- KONTROLL AV KDEKSHT                                               
021030     MOVE MFS-RENSA-FAELT   TO MOD-KDEKSHT-IN                             
021031                                                                          
021037     IF MID-KDEKSHT-IN NOT = ALL '+'                                      
021038       MOVE MID-KDEKSHT-IN  TO MSGI-KDEKSHT                               
021039     END-IF                                                               
021040                                                                          
021041     IF MSGI-KDEKSHT NOT = SPACE                                          
021042       MOVE MSGI-KDEKSHT    TO W-KDEKSHT                                  
021044     END-IF                                                               
021045*    -- KONTROLL AV KDEKNIVA                                              
021046     MOVE MFS-RENSA-FAELT   TO MOD-KDEKNIVA-IN                            
021047                                                                          
021057     IF MID-KDEKNIVA-IN NOT = ALL '+'                                     
021058       MOVE MID-KDEKNIVA-IN TO MSGI-KDEKNIVA                              
021059     END-IF                                                               
021060                                                                          
021061     IF MSGI-KDEKNIVA NOT = SPACE                                         
021062       MOVE MSGI-KDEKNIVA   TO W-KDEKNIVA                                 
021063                               SPAR-KDEKNIVA                              
021064     END-IF                                                               
021066                                                                          
021067*    -- KONTROLL AV IDSYSMOT                                              
021068     MOVE MFS-RENSA-FAELT   TO MOD-IDSYSMOT-IN                            
021069                                                                          
021070     IF MID-IDSYSMOT-IN NOT = ALL '+'                                     
021071       MOVE MID-IDSYSMOT-IN TO MSGI-IDSYSMOT                              
021078     END-IF                                                               
021079                                                                          
021080     IF MSGI-IDSYSMOT NOT = SPACE                                         
021081       MOVE MSGI-IDSYSMOT   TO W-IDSYSMOT                                 
021082                               W-IDSYSMOT-X                               
021083                               SPAR-IDSYSMOT                              
021090     END-IF                                                               
021101                                                                          
021102*    -- KONTROLL AV IDPTYP                                                
021103     MOVE MFS-RENSA-FAELT   TO MOD-IDPTYP-IN                              
021104                                                                          
021110     IF MID-IDPTYP-IN NOT = ALL '+'                                       
021111       MOVE MID-IDPTYP-IN   TO MSGI-IDPTYP                                
021112     END-IF                                                               
021113                                                                          
021114     IF MSGI-IDPTYP NOT = SPACE                                           
021115       MOVE MSGI-IDPTYP     TO W-IDPTYP                                   
021116                               W-IDPTYP-X                                 
021117                               SPAR-IDPTYP                                
021119     END-IF                                                               
021121                                                                          
021122     IF GODK-MID OR NYCKLAR-OK                                            
021123       MOVE MSGI-IDFTG      TO MOD-IDFTG-UT                               
021124       MOVE MSGI-KDEKHHT    TO MOD-KDEKHHT-UT                             
021125       MOVE MSGI-KDEKSHT    TO MOD-KDEKSHT-UT                             
021126       MOVE MSGI-KDEKNIVA   TO MOD-KDEKNIVA-UT                            
021127       MOVE MSGI-IDSYSMOT   TO MOD-IDSYSMOT-UT                            
021128       MOVE MSGI-IDPTYP     TO MOD-IDPTYP-UT                              
021129     ELSE                                                                 
021130       MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-UT                             
021131                               MOD-IDFTG-UT                               
021132                               MOD-KDEKSHT-UT                             
021133                               MOD-KDEKNIVA-UT                            
021134                               MOD-IDSYSMOT-UT                            
021135                               MOD-IDPTYP-UT                              
021140     END-IF                                                               
021200                                                                          
021201*    -- KONTROLL AV MID-CMD                                               
021202     MOVE +1 TO INDX                                                      
021203     PERFORM UNTIL INDX > MAX-INDX                                        
021204       IF MID-CMD (INDX) NOT = ' '                                        
021205         IF MID-CMD (INDX) = 'S'                                          
021206           MOVE INDX TO W-INDX                                            
021207           MOVE +15 TO INDX                                               
021208           MOVE JA TO BYT-SW                                              
021217         END-IF                                                           
021218       END-IF                                                             
021219       ADD +1 TO INDX                                                     
021220     END-PERFORM                                                          
021230                                                                          
021300     IF NYCKLAR-FEL                                                       
021310*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021320*---GODKÄND BILD                                                          
021330       IF GODK-MID                                                        
021400         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021500         CALL WMEDKONV USING MED-WMEDAREA                                 
021600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021700         PERFORM MFS-RENSA-FAELT-IN                                       
021800         PERFORM MFS-RENSA-FAELT-UT                                       
021810       END-IF                                                             
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
022107                                                                          
022108     PERFORM MFS-RENSA-FAELT-IN                                           
022109     .                                                                    
022110     EJECT                                                                
022111 D-NAESTA-SIDA SECTION.                                                   
022112                                                                          
022113     IF SPAR-IDTRANS = '5213'                                             
022114       MOVE SPAR-IDFTG-NEXT    TO W-IDFTG                                 
022115                                  SPAR-IDFTG-TAB (1)                      
022116       MOVE SPAR-KDEKHHT-NEXT  TO W-KDEKHHT                               
022117                                  SPAR-KDEKHHT-TAB (1)                    
022118       MOVE SPAR-KDEKSHT-NEXT  TO W-KDEKSHT                               
022119                                  WS-KDEKSHT                              
022120                                  SPAR-KDEKSHT-TAB (1)                    
022121       MOVE SPAR-KDEKNIVA-NEXT TO W-KDEKNIVA                              
022122                                  WS-KDEKNIVA                             
022123                                  SPAR-KDEKNIVA-TAB (1)                   
022124       MOVE SPAR-IDSYSMOT-NEXT TO W-IDSYSMOT                              
022125                                  W-IDSYSMOT-X                            
022126                                  SPAR-IDSYSMOT-TAB (1)                   
022127       MOVE SPAR-IDPTYP-NEXT   TO W-IDPTYP                                
022128                                  SPAR-IDPTYP-TAB (1)                     
022129       MOVE SPAR-BEEKSHT       TO WS-BEEKSHT                              
022130     ELSE                                                                 
022131       PERFORM MFS-RENSA-FAELT-IN                                         
022132     END-IF                                                               
022133     .                                                                    
022134     EJECT                                                                
022135 E-SAMMA-SIDA SECTION.                                                    
022136                                                                          
022137     IF SPAR-IDTRANS = '5213' OR '0551'                                   
022141       CONTINUE                                                           
022142     ELSE                                                                 
022143       PERFORM MFS-RENSA-FAELT-IN                                         
022144     END-IF                                                               
022145     .                                                                    
022146     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022510     IF (MSGI-IDFTG = SPACE OR MSGI-KDEKHHT = SPACE)                      
022511      AND MSGI-IDSYSMOT NOT = SPACE                                       
022520      AND MSGI-IDPTYP NOT = SPACE                                         
022530       PERFORM FA-LAES-GU-HHT-ASEQ                                        
022540     ELSE                                                                 
022550       PERFORM FB-LAES-GU-HHT                                             
022560     END-IF                                                               
022763                                                                          
022764     IF SEGMENT-SAKNAS                                                    
022768       CALL WMEDKONV USING MED-WMEDAREA                                   
022769       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022776     END-IF                                                               
022777                                                                          
022778     MOVE '002'     TO MSGI-KDCALL                                        
022779     MOVE '5213'    TO SPAR-IDTRANS                                       
022780     MOVE '5213'    TO SPAR-BILD                                          
022781     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
022782     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022783     .                                                                    
022784     EJECT                                                                
022785 FA-LAES-GU-HHT-ASEQ SECTION.                                             
022786                                                                          
022787     IF MFS-NEXT                                                          
022788       PERFORM IMS-GU-POST-ASEQ                                           
022789     ELSE                                                                 
022790       PERFORM IMS-GU-HHT-ASEQ                                            
022791     END-IF                                                               
022792                                                                          
022793     IF SEGMENT-SAKNAS                                                    
022794       MOVE SYSTNAME-MISSING TO MED-IDMFSFEL                              
022795     ELSE                                                                 
022797       MOVE +1 TO INDX                                                    
022798       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
022799         IF SEGMENT-FINNS                                                 
022811           IF MFS-NEXT  AND                                               
022812              INDX = +1                                                   
022814             MOVE SPAR-IDSYSMOT-NEXT TO MOD-IDSYSMOT      (INDX)          
022815                                        SPAR-IDSYSMOT-TAB (INDX)          
022817                                        WS-IDSYSMOT                       
022818             MOVE SPAR-IDPTYP-NEXT   TO MOD-IDPTYP        (INDX)          
022819                                        SPAR-IDPTYP-TAB   (INDX)          
022821                                        WS-IDPTYP                         
022822           ELSE                                                           
022823             MOVE WDH5-SYST-IDSYSMOT TO MOD-IDSYSMOT      (INDX)          
022824                                        SPAR-IDSYSMOT-TAB (INDX)          
022825                                        SPAR-IDSYSMOT-NEXT                
022826                                        WS-IDSYSMOT                       
022827             MOVE WDH5-SYST-IDPTYP   TO MOD-IDPTYP        (INDX)          
022828                                        SPAR-IDPTYP-TAB   (INDX)          
022829                                        SPAR-IDPTYP-NEXT                  
022830                                        WS-IDPTYP                         
022831           END-IF                                                         
022832           PERFORM FAA-LAES-VISA-POSTER                                   
022833         ELSE                                                             
022834           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
022835         END-IF                                                           
022836         PERFORM IMS-GU-HHT-ASEQ                                          
022837       END-PERFORM                                                        
022838       IF SEGMENT-FINNS                                                   
022839         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
022840         CALL WMEDKONV USING MED-WMEDAREA                                 
022841         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
022842       END-IF                                                             
022843     END-IF                                                               
022844     .                                                                    
022845     EJECT                                                                
022848 FAA-LAES-VISA-POSTER SECTION.                                            
022849                                                                          
022853     PERFORM IMS-GNP-WDH5                                                 
022855                                                                          
022856     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                      
022857       EVALUATE WDH5-SEG-NAME-FB                                          
022858         WHEN 'WDH501'                                                    
022859             MOVE HHT-IDFTG     TO WS-IDFTG                               
022860                                   SPAR-IDFTG-NEXT                        
022862             MOVE HHT-KDEKHHT   TO MOD-KDEKHHT       (INDX)               
022863                                   WS-KDEKHHT                             
022864                                   SPAR-KDEKHHT-NEXT                      
022865             MOVE HHT-BEEKHHT   TO MOD-BEEKHHT       (INDX)               
022866             MOVE WS-IDFTG      TO SPAR-IDFTG-TAB    (INDX)               
022867             MOVE WS-KDEKHHT    TO SPAR-KDEKHHT-TAB  (INDX)               
022868             MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB  (INDX)               
022869             MOVE WS-KDEKNIVA   TO SPAR-KDEKNIVA-TAB (INDX)               
022877             MOVE WS-IDSYSMOT   TO SPAR-IDSYSMOT-TAB (INDX)               
022878             MOVE WS-IDPTYP     TO SPAR-IDPTYP-TAB   (INDX)               
022880             ADD +1 TO INDX                                               
022881         WHEN 'WDH511'                                                    
022882             MOVE SHT-KDEKSHT   TO MOD-KDEKSHT       (INDX)               
022883                                   WS-KDEKSHT                             
022884                                   SPAR-KDEKSHT-NEXT                      
022885             MOVE SHT-BEEKSHT   TO MOD-BEEKSHT       (INDX)               
022886                                   SPAR-BEEKSHT                           
022887             MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB  (INDX)               
022888*            MOVE WS-KDEKNIVA   TO SPAR-KDEKNIVA-TAB (INDX)               
022889*            MOVE WS-IDSYSMOT   TO SPAR-IDSYSMOT-TAB (INDX)               
022890*            MOVE WS-IDPTYP     TO SPAR-IDPTYP-TAB   (INDX)               
022891         WHEN 'WDH521'                                                    
022892           MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA        (INDX)               
022893                                 WS-KDEKNIVA                              
022894                                 SPAR-KDEKNIVA-NEXT                       
022895                                 SPAR-KDEKNIVA                            
022896*          MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB    (INDX)               
022897           MOVE WS-KDEKNIVA   TO SPAR-KDEKNIVA-TAB   (INDX)               
022898*          MOVE WS-IDSYSMOT   TO SPAR-IDSYSMOT-TAB   (INDX)               
022899*          MOVE WS-IDPTYP     TO SPAR-IDPTYP-TAB     (INDX)               
022900       END-EVALUATE                                                       
022901         PERFORM IMS-GNP-WDH5                                             
022910     END-PERFORM                                                          
022915     .                                                                    
022916     EJECT                                                                
022917 FB-LAES-GU-HHT SECTION.                                                  
022918                                                                          
022919     PERFORM IMS-GU-HHT                                                   
022920     IF SEGMENT-SAKNAS                                                    
022921       MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                               
022922     ELSE                                                                 
022923       MOVE +1 TO INDX                                                    
022932       MOVE HHT-IDFTG       TO SPAR-IDFTG-NEXT                            
022933                               SPAR-IDFTG-TAB (INDX)                      
022935       MOVE HHT-KDEKHHT     TO MOD-KDEKHHT (INDX)                         
022936                               SPAR-KDEKHHT-NEXT                          
022937                               SPAR-KDEKHHT-TAB (INDX)                    
022938       MOVE HHT-BEEKHHT     TO MOD-BEEKHHT (INDX)                         
022941       IF MSGI-KDEKHHT NOT = SPACE                                        
022942        AND MSGI-IDFTG NOT = SPACE                                        
022943        AND MSGI-KDEKSHT   = SPACE                                        
022944         PERFORM FBA-LAES-VISA-SHT                                        
022945       ELSE                                                               
022946         IF MSGI-KDEKHHT   NOT = SPACE                                    
022947          AND MSGI-IDFTG   NOT = SPACE                                    
022948          AND MSGI-KDEKSHT NOT = SPACE                                    
022949          AND MSGI-KDEKNIVA    = SPACE                                    
022950           PERFORM FBB-LAES-VISA-NIVA                                     
022951         ELSE                                                             
022952           IF MSGI-KDEKHHT    NOT = SPACE                                 
022953            AND MSGI-IDFTG    NOT = SPACE                                 
022954            AND MSGI-KDEKSHT  NOT = SPACE                                 
022955            AND MSGI-KDEKNIVA NOT = SPACE                                 
022956            AND MSGI-IDSYSMOT     = SPACE                                 
022957             PERFORM FBC-LAES-VISA-SYST                                   
022958           ELSE                                                           
022959             IF MSGI-KDEKHHT    NOT = SPACE                               
022960              AND MSGI-IDFTG    NOT = SPACE                               
022961              AND MSGI-KDEKSHT  NOT = SPACE                               
022962              AND MSGI-KDEKNIVA NOT = SPACE                               
022963              AND MSGI-IDSYSMOT NOT = SPACE                               
022964              AND MSGI-IDPTYP   NOT = SPACE                               
022965                PERFORM FBD-LAES-VISA-UNIK-POST                           
022966             END-IF                                                       
022967           END-IF                                                         
022968         END-IF                                                           
022969       END-IF                                                             
022970     END-IF                                                               
022971     .                                                                    
022972     EJECT                                                                
022973 FBA-LAES-VISA-SHT SECTION.                                               
022974                                                                          
022975     IF MFS-NEXT                                                          
022976       PERFORM FBAA-KOLLA-VILKET-SEGMENT                                  
022977     ELSE                                                                 
022978       PERFORM IMS-GNP-WDH51                                              
022979     END-IF                                                               
022980                                                                          
022981     PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                      
022983       EVALUATE WDH51-SEG-NAME-FB                                         
022984         WHEN 'WDH511'                                                    
022985             MOVE SHT-KDEKSHT   TO MOD-KDEKSHT (INDX)                     
022986                                   WS-KDEKSHT                             
022987                                   SPAR-KDEKSHT-NEXT                      
022988             MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB (INDX)                
022989             MOVE SHT-BEEKSHT   TO MOD-BEEKSHT (INDX)                     
022990                                   SPAR-BEEKSHT                           
022994         WHEN 'WDH521'                                                    
022995           MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA (INDX)                      
022996                                 WS-KDEKNIVA                              
022997                                 SPAR-KDEKNIVA-NEXT                       
022998                                 SPAR-KDEKNIVA                            
022999*          MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB  (INDX)                 
023000           MOVE WS-KDEKNIVA   TO SPAR-KDEKNIVA-TAB (INDX)                 
023004         WHEN 'WDH531'                                                    
023005           IF MFS-NEXT  AND                                               
023006              INDX = +1                                                   
023007             MOVE SPAR-IDSYSMOT-NEXT TO MOD-IDSYSMOT      (INDX)          
023008                                        SPAR-IDSYSMOT-TAB (INDX)          
023010             MOVE SPAR-IDPTYP-NEXT   TO MOD-IDPTYP        (INDX)          
023011                                        SPAR-IDPTYP-TAB   (INDX)          
023013           ELSE                                                           
023014             MOVE SYST-IDSYSMOT TO MOD-IDSYSMOT      (INDX)               
023015                                   SPAR-IDSYSMOT-TAB (INDX)               
023016                                   SPAR-IDSYSMOT-NEXT                     
023017             MOVE SYST-IDPTYP   TO MOD-IDPTYP        (INDX)               
023018                                   SPAR-IDPTYP-TAB   (INDX)               
023019                                   SPAR-IDPTYP-NEXT                       
023020           END-IF                                                         
023021           MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB  (INDX)                 
023022           MOVE WS-KDEKNIVA   TO SPAR-KDEKNIVA-TAB (INDX)                 
023023           ADD +1 TO INDX                                                 
023024       END-EVALUATE                                                       
023025       PERFORM IMS-GNP-WDH51                                              
023026     END-PERFORM                                                          
023027                                                                          
023028     IF SEGMENT-FINNS                                                     
023029       EVALUATE WDH51-SEG-NAME-FB                                         
023030         WHEN 'WDH511'                                                    
023031           MOVE 'WDH511'    TO SPAR-VILKET-SEGMENT                        
023032         WHEN 'WDH521'                                                    
023033           MOVE 'WDH521'    TO SPAR-VILKET-SEGMENT                        
023034         WHEN 'WDH531'                                                    
023035           MOVE 'WDH531'    TO SPAR-VILKET-SEGMENT                        
023036       END-EVALUATE                                                       
023037       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
023038       CALL WMEDKONV USING MED-WMEDAREA                                   
023039       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
023040     END-IF                                                               
023041     .                                                                    
023042     EJECT                                                                
023043 FBAA-KOLLA-VILKET-SEGMENT SECTION.                                       
023044                                                                          
023045     MOVE +1 TO INDX                                                      
023051     EVALUATE SPAR-VILKET-SEGMENT                                         
023052       WHEN 'WDH511'                                                      
023053         PERFORM IMS-GNP-SHT                                              
023054         MOVE SHT-KDEKSHT   TO MOD-KDEKSHT  (INDX)                        
023055         MOVE SHT-BEEKSHT   TO MOD-BEEKSHT  (INDX)                        
023056       WHEN 'WDH521'                                                      
023057         PERFORM IMS-GNP-NIVA                                             
023058         MOVE WS-KDEKSHT    TO MOD-KDEKSHT  (INDX)                        
023059         MOVE WS-BEEKSHT    TO MOD-BEEKSHT  (INDX)                        
023060         MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA (INDX)                        
023061       WHEN 'WDH531'                                                      
023062         PERFORM IMS-GNP-SYST                                             
023063         MOVE WS-KDEKSHT    TO MOD-KDEKSHT  (INDX)                        
023064         MOVE WS-BEEKSHT    TO MOD-BEEKSHT  (INDX)                        
023065         MOVE WS-KDEKNIVA   TO MOD-KDEKNIVA (INDX)                        
023066         IF MFS-NEXT  AND                                                 
023067            INDX = +1                                                     
023068           MOVE SPAR-IDSYSMOT-NEXT TO MOD-IDSYSMOT (INDX)                 
023069           MOVE SPAR-IDPTYP-NEXT   TO MOD-IDPTYP   (INDX)                 
023070         ELSE                                                             
023071           MOVE SYST-IDSYSMOT      TO MOD-IDSYSMOT (INDX)                 
023072           MOVE SYST-IDPTYP        TO MOD-IDPTYP   (INDX)                 
023073         END-IF                                                           
023074     END-EVALUATE                                                         
023075     ADD +1 TO INDX                                                       
023076     PERFORM IMS-GNP-WDH51                                                
023077     .                                                                    
023078     EJECT                                                                
023079 FBB-LAES-VISA-NIVA SECTION.                                              
023080                                                                          
023081     PERFORM IMS-GU-SHT                                                   
023082     IF SEGMENT-SAKNAS                                                    
023083       MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                               
023084     ELSE                                                                 
023085       MOVE SHT-KDEKSHT     TO MOD-KDEKSHT (INDX)                         
023086                               SPAR-KDEKSHT-NEXT                          
023087                               WS-KDEKSHT                                 
023088       MOVE SHT-BEEKSHT     TO MOD-BEEKSHT (INDX)                         
023089                                                                          
023090       IF MFS-NEXT                                                        
023091         PERFORM FBBA-KOLLA-VILKET-SEGMENT                                
023092       ELSE                                                               
023093         PERFORM IMS-GNP-WDH51                                            
023094       END-IF                                                             
023095                                                                          
023096       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
023097         EVALUATE WDH51-SEG-NAME-FB                                       
023098           WHEN 'WDH521'                                                  
023099             MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA      (INDX)               
023100                                   WS-KDEKNIVA                            
023101                                   SPAR-KDEKNIVA-NEXT                     
023102                                   SPAR-KDEKNIVA                          
023103*            MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB (INDX)                
023104             MOVE WS-KDEKNIVA   TO SPAR-KDEKNIVA-TAB (INDX)               
023105           WHEN 'WDH531'                                                  
023106             IF MFS-NEXT  AND                                             
023107                INDX = +1                                                 
023108               MOVE SPAR-IDSYSMOT-NEXT TO MOD-IDSYSMOT      (INDX)        
023109                                          SPAR-IDSYSMOT-TAB (INDX)        
023111               MOVE SPAR-IDPTYP-NEXT   TO MOD-IDPTYP        (INDX)        
023112                                          SPAR-IDPTYP-TAB   (INDX)        
023114             ELSE                                                         
023115               MOVE SYST-IDSYSMOT TO MOD-IDSYSMOT      (INDX)             
023116                                     SPAR-IDSYSMOT-TAB (INDX)             
023117                                     SPAR-IDSYSMOT-NEXT                   
023118               MOVE SYST-IDPTYP   TO MOD-IDPTYP        (INDX)             
023119                                     SPAR-IDPTYP-TAB   (INDX)             
023120                                     SPAR-IDPTYP-NEXT                     
023121             END-IF                                                       
023122             MOVE WS-KDEKSHT    TO SPAR-KDEKSHT-TAB (INDX)                
023123             MOVE WS-KDEKNIVA   TO SPAR-KDEKNIVA-TAB (INDX)               
023124           ADD +1 TO INDX                                                 
023125         END-EVALUATE                                                     
023126         PERFORM IMS-GNP-WDH51                                            
023127       END-PERFORM                                                        
023128                                                                          
023129       IF SEGMENT-FINNS                                                   
023130       EVALUATE WDH51-SEG-NAME-FB                                         
023131         WHEN 'WDH521'                                                    
023132           MOVE 'WDH521'    TO SPAR-VILKET-SEGMENT                        
023133         WHEN 'WDH531'                                                    
023134           MOVE 'WDH531'    TO SPAR-VILKET-SEGMENT                        
023135       END-EVALUATE                                                       
023136       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
023137       CALL WMEDKONV USING MED-WMEDAREA                                   
023138       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
023139       END-IF                                                             
023140     END-IF                                                               
023141     .                                                                    
023142     EJECT                                                                
023143 FBBA-KOLLA-VILKET-SEGMENT SECTION.                                       
023144                                                                          
023145     EVALUATE SPAR-VILKET-SEGMENT                                         
023146       WHEN 'WDH521'                                                      
023147         PERFORM IMS-GNP-NIVA                                             
023148         MOVE WS-KDEKSHT    TO MOD-KDEKSHT  (INDX)                        
023149         MOVE WS-BEEKSHT    TO MOD-BEEKSHT  (INDX)                        
023150         MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA (INDX)                        
023151       WHEN 'WDH531'                                                      
023152         PERFORM IMS-GNP-SYST                                             
023153         MOVE WS-KDEKSHT    TO MOD-KDEKSHT  (INDX)                        
023154         MOVE WS-BEEKSHT    TO MOD-BEEKSHT  (INDX)                        
023155         MOVE WS-KDEKNIVA   TO MOD-KDEKNIVA (INDX)                        
023156         IF MFS-NEXT  AND                                                 
023157            INDX = +1                                                     
023158           MOVE SPAR-IDSYSMOT-NEXT TO MOD-IDSYSMOT (INDX)                 
023159           MOVE SPAR-IDPTYP-NEXT   TO MOD-IDPTYP   (INDX)                 
023160         ELSE                                                             
023161           MOVE SYST-IDSYSMOT      TO MOD-IDSYSMOT (INDX)                 
023162           MOVE SYST-IDPTYP        TO MOD-IDPTYP   (INDX)                 
023163         END-IF                                                           
023164     END-EVALUATE                                                         
023165     ADD +1 TO INDX                                                       
023166     PERFORM IMS-GNP-WDH51                                                
023167     .                                                                    
023168     EJECT                                                                
023170 FBC-LAES-VISA-SYST SECTION.                                              
023200                                                                          
023300     PERFORM IMS-GU-SHT                                                   
023400     IF SEGMENT-SAKNAS                                                    
023500       MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                               
023600     ELSE                                                                 
023700       MOVE SHT-KDEKSHT     TO MOD-KDEKSHT (INDX)                         
023800                               SPAR-KDEKSHT-NEXT                          
023900       MOVE SHT-BEEKSHT     TO MOD-BEEKSHT (INDX)                         
024000       PERFORM IMS-GU-NIVA                                                
024010       IF SEGMENT-SAKNAS                                                  
024020         MOVE KDEKNIVA-MISSING  TO MED-IDMFSFEL                           
024030       ELSE                                                               
024040         MOVE NIVA-KDEKNIVA     TO MOD-KDEKNIVA (INDX)                    
024050                                   SPAR-KDEKNIVA-NEXT                     
024070                                                                          
024071         IF MFS-NEXT                                                      
024072           PERFORM FBBC-KOLLA-VILKET-SEGMENT                              
024073         ELSE                                                             
024074           PERFORM IMS-GNP-WDH51                                          
024075         END-IF                                                           
024100         PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                  
024300           EVALUATE WDH51-SEG-NAME-FB                                     
024710             WHEN 'WDH531'                                                
024711               IF MFS-NEXT  AND                                           
024712                  INDX = +1                                               
024720                 MOVE SPAR-IDSYSMOT-NEXT TO                               
024721                                         MOD-IDSYSMOT      (INDX)         
024723                                         SPAR-IDSYSMOT-TAB (INDX)         
024724                 MOVE SPAR-IDPTYP-NEXT   TO                               
024725                                         MOD-IDPTYP      (INDX)           
024727                                         SPAR-IDPTYP-TAB (INDX)           
024728               ELSE                                                       
024729                 MOVE SYST-IDSYSMOT TO MOD-IDSYSMOT      (INDX)           
024730                                       SPAR-IDSYSMOT-NEXT                 
024731                                       SPAR-IDSYSMOT-TAB (INDX)           
024732                 MOVE SYST-IDPTYP   TO MOD-IDPTYP        (INDX)           
024733                                       SPAR-IDPTYP-NEXT                   
024734                                       SPAR-IDPTYP-TAB   (INDX)           
024735               END-IF                                                     
024736               ADD +1 TO INDX                                             
024737           END-EVALUATE                                                   
024738           PERFORM IMS-GNP-WDH51                                          
024739         END-PERFORM                                                      
024740         IF SEGMENT-FINNS                                                 
024741           MOVE SYST-IDSYSMOT        TO SPAR-IDSYSMOT-NEXT                
024742           MOVE SYST-IDPTYP          TO SPAR-IDPTYP-NEXT                  
024743           MOVE 'WDH531'             TO SPAR-VILKET-SEGMENT               
024744           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
024745           CALL WMEDKONV USING MED-WMEDAREA                               
024746           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
024747         END-IF                                                           
024748       END-IF                                                             
024749     END-IF                                                               
024750     .                                                                    
024751     EJECT                                                                
024752 FBBC-KOLLA-VILKET-SEGMENT SECTION.                                       
024753                                                                          
024754     EVALUATE SPAR-VILKET-SEGMENT                                         
024755       WHEN 'WDH531'                                                      
024756         PERFORM IMS-GNP-SYST                                             
024757         MOVE WS-KDEKSHT    TO MOD-KDEKSHT  (INDX)                        
024758         MOVE WS-BEEKSHT    TO MOD-BEEKSHT  (INDX)                        
024759         MOVE WS-KDEKNIVA   TO MOD-KDEKNIVA (INDX)                        
024760         IF MFS-NEXT  AND                                                 
024761           INDX = +1                                                      
024762           MOVE SPAR-IDSYSMOT-NEXT TO MOD-IDSYSMOT (INDX)                 
024763           MOVE SPAR-IDPTYP-NEXT   TO MOD-IDPTYP   (INDX)                 
024764         ELSE                                                             
024765           MOVE SYST-IDSYSMOT      TO MOD-IDSYSMOT (INDX)                 
024766           MOVE SYST-IDPTYP        TO MOD-IDPTYP   (INDX)                 
024767         END-IF                                                           
024768     END-EVALUATE                                                         
024769     ADD +1 TO INDX                                                       
024770     PERFORM IMS-GNP-WDH51                                                
024771     .                                                                    
024772     EJECT                                                                
024773 FBD-LAES-VISA-UNIK-POST SECTION.                                         
024774                                                                          
024775     PERFORM IMS-GU-SHT                                                   
024776                                                                          
024777     IF SEGMENT-SAKNAS                                                    
024778       MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                               
024779     ELSE                                                                 
024780       MOVE SHT-KDEKSHT     TO MOD-KDEKSHT (INDX)                         
024781                               SPAR-KDEKSHT-NEXT                          
024782       MOVE SHT-BEEKSHT     TO MOD-BEEKSHT (INDX)                         
024783                                                                          
024784       PERFORM IMS-GU-NIVA                                                
024785                                                                          
024786       IF SEGMENT-SAKNAS                                                  
024787         MOVE KDEKNIVA-MISSING  TO MED-IDMFSFEL                           
024788       ELSE                                                               
024789         MOVE NIVA-KDEKNIVA     TO MOD-KDEKNIVA (INDX)                    
024790                                   SPAR-KDEKNIVA-NEXT                     
024791         PERFORM IMS-GU-SYST                                              
024792                                                                          
024793         IF SEGMENT-SAKNAS                                                
024794           MOVE SYSTNAME-MISSING  TO MED-IDMFSFEL                         
024795         ELSE                                                             
024796           IF MFS-NEXT  AND                                               
024797              INDX = +1                                                   
024798             MOVE SPAR-IDSYSMOT-NEXT TO MOD-IDSYSMOT (INDX)               
024800             MOVE SPAR-IDPTYP-NEXT   TO MOD-IDPTYP   (INDX)               
024802           ELSE                                                           
024803             MOVE SYST-IDSYSMOT      TO MOD-IDSYSMOT (INDX)               
024804                                        SPAR-IDSYSMOT-NEXT                
024805             MOVE SYST-IDPTYP        TO MOD-IDPTYP   (INDX)               
024806                                        SPAR-IDPTYP-NEXT                  
024807           END-IF                                                         
024808         END-IF                                                           
024809       END-IF                                                             
024810     END-IF                                                               
024811     .                                                                    
024812     EJECT                                                                
024813 H-BYT-BILD SECTION.                                                      
024814     SKIP2                                                                
024815     MOVE NEJ TO ALLT-SW                                                  
024816     MOVE JA  TO BYT-SW                                                   
024817                                                                          
024818* ---HÄMTAR RÄTT RAD-VÄRDE TILL 5221-BILDEN                               
024819     MOVE W-INDX                   TO INDX                                
024820     MOVE MSGI-KDEKHHT             TO SPAR-KDEKHHT                        
024821     MOVE MSGI-IDFTG               TO SPAR-IDFTG                          
024822     MOVE SPAR-KDEKSHT-TAB  (INDX) TO SPAR-KDEKSHT                        
024823     MOVE SPAR-KDEKNIVA-TAB (INDX) TO SPAR-KDEKNIVA                       
024824     MOVE SPAR-IDSYSMOT-TAB (INDX) TO SPAR-IDSYSMOT                       
024825     MOVE SPAR-IDPTYP-TAB   (INDX) TO SPAR-IDPTYP                         
024826                                                                          
024827     MOVE '002'                    TO MSGI-KDCALL                         
024828     MOVE '5221'                   TO SPAR-IDTRANS                        
024829     MOVE SPAR-AREA                TO MSGI-SPAR-AREA                      
024830     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024831                                                                          
024832                                                                          
024833* ---SKICKAR VÄRDE TILL 5221-MID FÖR ATT SEDAN                            
024834* ---STARTA UPP 5221-BILDEN                                               
024835     MOVE LOW-VALUE          TO 5221-MID-W5I22101                         
024836     MOVE SPAR-KDEKHHT       TO 5221-MID-KDEKHHT-IN                       
024837     MOVE SPAR-KDEKSHT       TO 5221-MID-KDEKSHT-IN                       
024838     MOVE SPAR-KDEKNIVA      TO 5221-MID-KDEKNIVA-IN                      
024839     MOVE SPAR-IDSYSMOT      TO 5221-MID-IDSYSMOT-IN                      
024840     MOVE SPAR-IDPTYP        TO 5221-MID-IDPTYP-IN                        
024841     MOVE SPAR-AREA          TO MSGI-SPAR-AREA                            
024842     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024843     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21301 + 17                       
024844     PERFORM IMS-INSERT-ALT-MSG-5221                                      
024845     .                                                                    
024846     EJECT                                                                
024850 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025200*    MOVE MFS-RENSA-FAELT   TO MOD-KDEKHHT-UT                             
025310*                              MOD-KDEKSHT-UT                             
025310*                              MOD-KDEKNIVA-UT                            
025310*                              MOD-IDSYSMOT-UT                            
025310*                              MOD-IDPTYP-UT                              
025310*                              MOD-IDFTG-UT                               
025340     MOVE +1 TO INDX                                                      
025350     PERFORM UNTIL INDX > MAX-INDX                                        
025360       MOVE MFS-RENSA-FAELT TO MOD-CMD      (INDX)                        
025391                               MOD-KDEKHHT  (INDX)                        
025301                               MOD-BEEKHHT  (INDX)                        
025393                               MOD-KDEKSHT  (INDX)                        
025320                               MOD-BEEKSHT  (INDX)                        
025394                               MOD-KDEKNIVA (INDX)                        
025395                               MOD-IDSYSMOT (INDX)                        
025396                               MOD-IDPTYP   (INDX)                        
025397     ADD +1 TO INDX                                                       
025398     END-PERFORM                                                          
025400     .                                                                    
025501     SKIP3                                                                
025502 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025503                                                                          
025504*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025516     MOVE +1 TO INDX                                                      
025517     PERFORM UNTIL INDX > MAX-INDX                                        
025519       MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT  (INDX)                        
025521                               MOD-BEEKHHT  (INDX)                        
025522                               MOD-KDEKSHT  (INDX)                        
025523                               MOD-BEEKSHT  (INDX)                        
025524                               MOD-KDEKNIVA (INDX)                        
025525                               MOD-IDSYSMOT (INDX)                        
025526                               MOD-IDPTYP   (INDX)                        
025527     ADD +1 TO INDX                                                       
025528     END-PERFORM                                                          
025530     .                                                                    
025600     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026110     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
026121                             MOD-KDEKSHT-IN                               
026130                             MOD-KDEKNIVA-IN                              
026140                             MOD-IDSYSMOT-IN                              
026150                             MOD-IDPTYP-IN                                
026160     MOVE +1 TO INDX                                                      
026170     PERFORM UNTIL INDX > MAX-INDX                                        
026180       MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                             
026197     ADD +1 TO INDX                                                       
026198     END-PERFORM                                                          
026200     .                                                                    
026300     EJECT                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-INSERT-ALT-MSG-5221 SECTION.                                         
031503                                                                          
031504     MOVE SPACE TO GODK-STATUSKODER                                       
031505     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-5221               
031506     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031507     PERFORM IMS-STATUSKONTROLL                                           
031508     .                                                                    
031509     EJECT                                                                
031510 IMS-GU-HHT SECTION.                                                      
031520                                                                          
031530     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031540          DELIMITED BY SIZE INTO SSA1                                     
031541     MOVE '  GE' TO GODK-STATUSKODER                                      
031542     CALL CBLTDLI USING GU WDH51-PCB DLI-IO-AREA SSA1                     
031543     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031544     PERFORM IMS-STATUSKONTROLL                                           
031545     .                                                                    
031546     EJECT                                                                
031547 IMS-GU-SHT SECTION.                                                      
031548                                                                          
031549     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031550          DELIMITED BY SIZE INTO SSA1                                     
031551     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031552          DELIMITED BY SIZE INTO SSA2                                     
031560     MOVE '  GE' TO GODK-STATUSKODER                                      
031570     CALL CBLTDLI USING GU WDH51-PCB DLI-IO-AREA SSA1 SSA2                
031580     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031581     PERFORM IMS-STATUSKONTROLL                                           
031582     .                                                                    
031583     EJECT                                                                
031657 IMS-GU-NIVA SECTION.                                                     
031658                                                                          
031659     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031660          DELIMITED BY SIZE INTO SSA1                                     
031661     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031662          DELIMITED BY SIZE INTO SSA2                                     
031663     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031664          DELIMITED BY SIZE INTO SSA3                                     
031670     MOVE '  GE' TO GODK-STATUSKODER                                      
031700     CALL CBLTDLI USING GU WDH51-PCB DLI-IO-AREA SSA1                     
031710                                                 SSA2                     
031711                                                 SSA3                     
031713     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031714     PERFORM IMS-STATUSKONTROLL                                           
031715     .                                                                    
031716     EJECT                                                                
031725 IMS-GU-SYST SECTION.                                                     
031726                                                                          
031727     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031728          DELIMITED BY SIZE INTO SSA1                                     
031730     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031740          DELIMITED BY SIZE INTO SSA2                                     
031741     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031742          DELIMITED BY SIZE INTO SSA3                                     
031743     STRING 'WDH531  (WDH531KY =' W-WDH531KY-X ')'                        
031744          DELIMITED BY SIZE INTO SSA4                                     
031745     MOVE '  GE' TO GODK-STATUSKODER                                      
031746     CALL CBLTDLI USING GU WDH51-PCB DLI-IO-AREA SSA1                     
031747                                                 SSA2                     
031748                                                 SSA3                     
031749                                                 SSA4                     
031750     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031751     PERFORM IMS-STATUSKONTROLL                                           
031752     .                                                                    
031753     EJECT                                                                
031754 IMS-GNP-SHT SECTION.                                                     
031755                                                                          
031756     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031757          DELIMITED BY SIZE INTO SSA1                                     
031758     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031759          DELIMITED BY SIZE INTO SSA2                                     
031760     MOVE '  GE' TO GODK-STATUSKODER                                      
031761     CALL CBLTDLI USING GNP WDH51-PCB DLI-IO-AREA SSA1 SSA2               
031762     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031763     PERFORM IMS-STATUSKONTROLL                                           
031764     .                                                                    
031765     EJECT                                                                
031766 IMS-GNP-NIVA SECTION.                                                    
031767                                                                          
031768     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031769          DELIMITED BY SIZE INTO SSA1                                     
031770     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031771          DELIMITED BY SIZE INTO SSA2                                     
031772     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031773          DELIMITED BY SIZE INTO SSA3                                     
031774     MOVE '  GE' TO GODK-STATUSKODER                                      
031775     CALL CBLTDLI USING GNP WDH51-PCB DLI-IO-AREA SSA1                    
031776                                                  SSA2                    
031777                                                  SSA3                    
031778     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031779     PERFORM IMS-STATUSKONTROLL                                           
031780     .                                                                    
031781     EJECT                                                                
031782 IMS-GNP-SYST SECTION.                                                    
031783                                                                          
031784     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031785          DELIMITED BY SIZE INTO SSA1                                     
031786     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031787          DELIMITED BY SIZE INTO SSA2                                     
031788     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031789          DELIMITED BY SIZE INTO SSA3                                     
031790     STRING 'WDH531  (WDH531KY =' W-WDH531KY-X ')'                        
031791          DELIMITED BY SIZE INTO SSA4                                     
031792     MOVE '  GE' TO GODK-STATUSKODER                                      
031793     CALL CBLTDLI USING GNP WDH51-PCB DLI-IO-AREA SSA1                    
031794                                                  SSA2                    
031795                                                  SSA3                    
031796                                                  SSA4                    
031797     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031798     PERFORM IMS-STATUSKONTROLL                                           
031799     .                                                                    
031800     EJECT                                                                
031801 IMS-GNP-WDH5 SECTION.                                                    
031802                                                                          
031803     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-AREA                          
031804     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031805     MOVE '  GEGAGK' TO GODK-STATUSKODER                                  
031806     PERFORM IMS-STATUSKONTROLL                                           
031807     .                                                                    
031808     EJECT                                                                
031809 IMS-GNP-WDH51 SECTION.                                                   
031810                                                                          
031811     CALL CBLTDLI USING GNP WDH51-PCB DLI-IO-AREA                         
031812     MOVE WDH51-STATUS-CODE TO STATUS-WS                                  
031813     MOVE '  GEGAGK' TO GODK-STATUSKODER                                  
031814     PERFORM IMS-STATUSKONTROLL                                           
031815     .                                                                    
031816     EJECT                                                                
031817 IMS-GU-HHT-ASEQ SECTION.                                                 
031818                                                                          
031819     STRING 'WDH531  (WDH5ASEQ =' W-WDH5ASEQ-X ')'                        
031821          DELIMITED BY SIZE INTO SSA1                                     
031822     MOVE '  GE' TO GODK-STATUSKODER                                      
031823     CALL CBLTDLI USING GN WDH5-PCB DLI-IO-WDH531 SSA1                    
031824     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031825     PERFORM IMS-STATUSKONTROLL                                           
031826     .                                                                    
031827     EJECT                                                                
031828 IMS-GU-POST-ASEQ SECTION.                                                
031829                                                                          
031830     STRING 'WDH531  (WDH5ASEQ =' W-WDH5ASEQ-X ')'                        
031832          DELIMITED BY SIZE INTO SSA1                                     
031833     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031834          DELIMITED BY SIZE INTO SSA2                                     
031835     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031836          DELIMITED BY SIZE INTO SSA3                                     
031837     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031838          DELIMITED BY SIZE INTO SSA4                                     
031839     MOVE '  GE' TO GODK-STATUSKODER                                      
031840     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH531 SSA1                    
031841                                                    SSA2                  
031842                                                    SSA3                  
031843                                                    SSA4                  
031844     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031845     PERFORM IMS-STATUSKONTROLL                                           
031846     .                                                                    
031847     EJECT                                                                
031848 IMS-STATUSKONTROLL SECTION.                                              
031850                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
