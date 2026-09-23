001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3012400.                                                
001500 AUTHOR.         BO HAMMARIN.                                             
001600 DATE-WRITTEN.   DECEMBER 1999.                                           
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMETS HUVUDFUNKTIONER ÄR FÖLJANDE:                         
002100*        -FRÅGA PÅ ARTIKLAR, MATA IN ARTIKELNR (1-8 SIFFROR) OCH          
002200*         FÅ INFORMATION OM VILKA ARTIKLAR SOM HAR UTÖKAD LEDTID          
002300*                                                                         
002400*        -LÄGGA UPP NYA ARTIKLAR      (N)                                 
002500*        -ÄNDRA BEFINTLIGA ARTIKLAR   (C)                                 
002600*        -TA BORT BEFINTLIGA ARTIKLAR (D)                                 
002700*                                                                         
002810*        PROGRAMMET LÄSER            WDD3                                 
002820*        PROGRAMMET LÄSER/UPPDATERAR WDK6                                 
002900*                                                                         
002910*                                                                         
003000*    INDATA.                                                              
003100*        TRANSAKTION: W3T124                                              
003110*                     W3T124U                                             
003200*        MID:         W3I12401                                            
003300*                                                                         
003400*    UTDATA.                                                              
003500*        MOD:         W3O12401                                            
003600     EJECT                                                                
003700                                                                          
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004110                                                                          
004111*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'W3012400'.            
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
004910*    --- GENERELLA ARBETSFÄLT                                             
004920 01  DAGENS-DATUM                PIC 9(8).                                
004930 01  DAGENS-TID                  PIC 9(8).                                
004940 01  WS-ART-ALFA                 PIC X(8).                                
004941 01  WS-TIVV-ALFA                PIC X(2).                                
004950 01  WS-TIVV-NUM                 PIC 9(2).                                
004960 01  WS-IDARTNR-8                PIC X(8).                                
005009                                                                          
005014*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005015 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005020 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
005300                                                                          
005400 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005401     88  ALLT-OK                             VALUE 'J'.                   
005402                                                                          
005403 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005404     88  INDATA-OK                           VALUE 'J'.                   
005410     88  INDATA-FEL                          VALUE 'N'.                   
005500                                                                          
005510 77  TEXT-SW                     PIC X       VALUE 'N'.                   
005520     88  TEXT-KLAR                           VALUE 'J'.                   
005540                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '3124'.                
006200     88  GODK-MID                            VALUE '3124'.                
006700     88  HELP-MID                            VALUE '0551'.                
006800     EJECT                                                                
006810                                                                          
006900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007800*01 -COPY WMEDAREA                                                        
007801     EJECT                                                                
007900                                                                          
008000 01  MESSAGE-CODES.                                                       
008101     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008103     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008104     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
008105     03  ERR-ITEM-MISSING        PIC X(3)    VALUE '029'.                 
008106     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008107     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008201     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008202     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008210     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008500     EJECT                                                                
008510                                                                          
008600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008900                                                                          
009000*01 -COPY WMSGINIT                                                        
009101     EJECT                                                                
009102                                                                          
009103 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
009104                                                                          
009105*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009106*                                                                         
009107 01  SPAR-AREA.                                                           
009108     03  SPAR-IDTRANS            PIC X(4)    VALUE '3124'.                
009109     03  SPAR-IDARTNR-ENTER      PIC 9(8)    VALUE ZERO.                  
009110     03  SPAR-IDARTNR-NEXT       PIC 9(8)    VALUE ZERO.                  
009200     EJECT                                                                
009210                                                                          
009300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009600                                                                          
009700*01  MID -COPY W3I12401                                                   
009800     EJECT                                                                
009810                                                                          
009900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010000                                                                          
010100*01  -COPY WMSGAREA                                                       
010200     EJECT                                                                
010210                                                                          
010300     03  MOD REDEFINES MSG-AREA.                                          
010400*      05  -COPY W3O12401                                                 
010500     EJECT                                                                
010510                                                                          
010600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010700                                                                          
010800*01  -COPY WMFSAREA                                                       
010900     EJECT                                                                
010910                                                                          
011000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200     EJECT                                                                
011210                                                                          
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400                                                                          
011500 01  NYCKLAR-TILL-DLI.                                                    
011601*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011602                                                                          
011603     03  W-IDARTNR-X.                                                     
011604         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
011607     03  W-KDSEGKEY-X.                                                    
011608         05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                       
011609     03  W-IDARTNR-WDD3-X.                                                
011610         05  W-IDARTNR-WDD3  PIC S9(9)   VALUE +0  COMP-3.                
011611     03  W-IDSKYLT-X.                                                     
011620         05  W-IDSKYLT       PIC X(3)    VALUE SPACE.                     
011700                                                                          
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012210     88  BAS-SLUT                            VALUE 'GB'.                  
012300                                                                          
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600                                                                          
012700 01  SSA1                        PIC X(128).                              
012800 01  SSA2                        PIC X(128).                              
012810 01  SSA3                        PIC X(64).                               
012900     EJECT                                                                
012910                                                                          
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013300     EJECT                                                                
013310                                                                          
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601  '.                    
013602 01  DLI-IO-WDK601.                                                       
013603*    03  -COPY WDK601                                                     
013604     EJECT                                                                
013605                                                                          
013606 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611  '.                    
013607 01  DLI-IO-WDK611.                                                       
013610*    03  -COPY WDK611                                                     
013900     EJECT                                                                
013901                                                                          
013910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK628  '.                    
013920 01  DLI-IO-WDK628.                                                       
013930*    03  -COPY WDK628                                                     
013940     EJECT                                                                
013941                                                                          
013942 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311  '.                    
013943 01  DLI-IO-WDD311.                                                       
013944*    03  -COPY WDD311                                                     
013945     EJECT                                                                
013993                                                                          
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009  -PRE MSG-                                               
014200*01  -COPY W0008  -PRE USEA-                                              
014300     05  FILLER                  PIC X.                                   
014401                                                                          
014402*01  -COPY W0008  -PRE WDK6D-                                             
014410     05  FILLER                  PIC X.                                   
014411*01  -COPY W0008  -PRE WDK6-                                              
014412     05  FILLER                  PIC X.                                   
014413*01  -COPY W0008  -PRE WDD3B-                                             
014414     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014600                                                                          
014601 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
014602                           WDK6D-PCB WDK6-PCB WDD3B-PCB.                  
014603                                                                          
014604 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
014611                           WDK6D-PCB WDK6-PCB WDD3B-PCB.                  
014620                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FINNS                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-KOLLA-NYCKLAR                                            
015300       IF NYCKLAR-OK                                                      
015401         IF MFS-UPDATE                                                    
015402           PERFORM G-KOLLA-INPUT                                          
015403           IF INDATA-OK                                                   
015404             PERFORM H-UPPDATERA                                          
015405           END-IF                                                         
015410         ELSE                                                             
015501           IF MFS-FIRST                                                   
015502             PERFORM C-FOERSTA-SIDA                                       
015503           ELSE                                                           
015504             IF MFS-NEXT                                                  
015505               PERFORM D-NAESTA-SIDA                                      
015506             ELSE                                                         
015507               PERFORM E-SAMMA-SIDA                                       
015508             END-IF                                                       
015510           END-IF                                                         
015710         END-IF                                                           
015720         IF ALLT-OK                                                       
015721           PERFORM F-LAES-VISA                                            
015730         END-IF                                                           
015900       END-IF                                                             
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O12401 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017010                                                                          
017100 A-INIT SECTION.                                                          
017210     MOVE FUNCTION CURRENT-DATE(1:8)      TO   DAGENS-DATUM               
017212     ACCEPT DAGENS-TID                    FROM TIME                       
017220                                                                          
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I12401                 
017500       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017600       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I12401                 
017900       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
018000       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
018400     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
018500     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
018600                                                                          
018700     MOVE LOW-VALUE                       TO MSG-AREA                     
018800     MOVE 'W3O124N1'                      TO MFS-IDMOD                    
018900     MOVE '3124'                          TO MOD-IDTRANS                  
019000     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
019001                                             MOD-TEMFSINF                 
019002                                                                          
019200     IF EGEN-MID OR HELP-MID                                              
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE                         TO MFS-KDTRTYP                  
019600       MOVE '7'                           TO MFS-IDPFK                    
019700     END-IF                                                               
019710                                                                          
019800     MOVE 'GB'                            TO MED-IDSKYLT                  
019900                                             W-IDSKYLT                    
020000     .                                                                    
020100     EJECT                                                                
020110                                                                          
020200 B-KOLLA-NYCKLAR SECTION.                                                 
020400     MOVE ALL '+'             TO MSGI-WMSGINIT                            
020500     MOVE '001'               TO MSGI-KDCALL                              
020600     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
020700     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
020800     MOVE '3124'              TO MSGI-IDTRANS                             
020810                                                                          
020900     IF EGEN-MID                                                          
020910       IF MID-IDARTNR-IN NOT = ALL '+'                                    
021000         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
022300       END-IF                                                             
022301     END-IF                                                               
022310                                                                          
022400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022500     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
022501                                                                          
022520     MOVE JA                  TO ALLT-SW                                  
022530                                 NYCKLAR-SW                               
022570                                                                          
022580*    -- KONTROLL AV IDARTNR                                               
022590     MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-IN                           
022591                                                                          
022593     IF MSGI-IDARTNR NOT = ALL '+'                                        
022594       INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO               
022595       MOVE MSGI-IDARTNR(1:8) TO WS-IDARTNR-8                             
022597       IF WS-IDARTNR-8 NUMERIC                                            
022598         MOVE WS-IDARTNR-8    TO W-IDARTNR                                
022599         COMPUTE W-IDARTNR = W-IDARTNR / 10                               
022600         END-COMPUTE                                                      
022601       ELSE                                                               
022610         MOVE NEJ             TO NYCKLAR-SW                               
022620                                 ALLT-SW                                  
022621       END-IF                                                             
022630     ELSE                                                                 
022640       MOVE NEJ               TO NYCKLAR-SW                               
022650                                 ALLT-SW                                  
022700     END-IF                                                               
022724                                                                          
022725     IF GODK-MID OR NYCKLAR-OK                                            
022726       MOVE MSGI-IDARTNR(1:8) TO MOD-IDARTNR-UT                           
022727       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
022731     ELSE                                                                 
022732       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                           
022737     END-IF                                                               
022738     IF MID-IDARTNR-IN NOT = ALL '+'                                      
022739       MOVE SPACE             TO MFS-KDTRTYP                              
022740       MOVE '7'               TO MFS-IDPFK                                
022741     END-IF                                                               
022742                                                                          
022743     IF NYCKLAR-FEL                                                       
022745       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
022746       CALL WMEDKONV USING MED-WMEDAREA                                   
022747       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
022748       PERFORM MFS-RENSA-FAELT-IN                                         
022749       PERFORM MFS-RENSA-FAELT-UT                                         
022750     END-IF                                                               
022751     .                                                                    
022752     EJECT                                                                
022753                                                                          
022754 C-FOERSTA-SIDA SECTION.                                                  
022755     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
022756     CALL WMEDKONV USING MED-WMEDAREA                                     
022757     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
022758                                                                          
022759     PERFORM MFS-RENSA-FAELT-IN                                           
022760     .                                                                    
022761     EJECT                                                                
022762                                                                          
022763 D-NAESTA-SIDA SECTION.                                                   
022765     IF SPAR-IDTRANS = '3124'                                             
022766       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNR                                
022768     ELSE                                                                 
022769       PERFORM MFS-RENSA-FAELT-IN                                         
022770     END-IF                                                               
022771     .                                                                    
022772     EJECT                                                                
022773                                                                          
022774 E-SAMMA-SIDA SECTION.                                                    
022789     IF SPAR-IDTRANS = '3124' OR                                          
022790                       '0551'                                             
022791       MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR                               
022799       MOVE INF-PRESS-PF11     TO MED-IDMFSFEL                            
022800       CALL WMEDKONV USING MED-WMEDAREA                                   
022801       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
022802       PERFORM MFS-ROER-EJ-FAELT-IN                                       
022803       PERFORM MFS-LAES-IN-IGEN                                           
022805     ELSE                                                                 
022806       PERFORM MFS-RENSA-FAELT-IN                                         
022807     END-IF                                                               
022810     .                                                                    
022900     EJECT                                                                
023000                                                                          
024310 F-LAES-VISA SECTION.                                                     
024311     IF MFS-NEXT                                                          
024315       PERFORM IMS-GU-WDK628-DSEQ                                         
024318     ELSE                                                                 
024321       IF MFS-UPDATE                                                      
024322         IF W-IDARTNR > SPAR-IDARTNR-ENTER                                
024323           MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR                           
024324         END-IF                                                           
024325       END-IF                                                             
024326       PERFORM IMS-GN-WDK628-DSEQ                                         
024327       IF SEGMENT-SAKNAS OR                                               
024328          BAS-SLUT                                                        
024329         MOVE ERR-ITEM-MISSING   TO MED-IDMFSFEL                          
024330         CALL WMEDKONV USING MED-WMEDAREA                                 
024331         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
024340       END-IF                                                             
024350     END-IF                                                               
024400     MOVE BYT-IDARTNR            TO SPAR-IDARTNR-ENTER                    
024500                                                                          
024510     MOVE +1 TO INDX                                                      
024520     PERFORM UNTIL INDX > MAX-INDX OR                                     
024521                   SEGMENT-SAKNAS  OR                                     
024522                   BAS-SLUT                                               
024530       IF SEGMENT-FINNS                                                   
024540         MOVE BYT-IDARTNR        TO MOD-IDARTNR (INDX)                    
024541                                    W-IDARTNR-WDD3                        
024550         PERFORM IMS-GU-WDD311-BSEQ                                       
024551         IF SEGMENT-FINNS                                                 
024552           MOVE TEXT-BEART       TO MOD-BEART (INDX)                      
024553         ELSE                                                             
024554           MOVE 'UNKNOWN'        TO MOD-BEART (INDX)                      
024555         END-IF                                                           
024556         MOVE BYT-KVVECKOR       TO MOD-KVVECKOR (INDX)                   
024557         MOVE BYT-IDUSER         TO MOD-IDUSER (INDX)                     
024558         MOVE BYT-DAREGDAT       TO MOD-DAREGDAT (INDX)                   
024560       ELSE                                                               
024570         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
024580       END-IF                                                             
024590       ADD +1 TO INDX                                                     
024591       PERFORM IMS-GN-WDK628-DSEQ                                         
024595     END-PERFORM                                                          
024596                                                                          
024597     IF SEGMENT-FINNS                                                     
024598       MOVE BYT-IDARTNR            TO SPAR-IDARTNR-NEXT                   
024599       IF NOT MFS-UPDATE                                                  
024600         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
024601         CALL WMEDKONV USING MED-WMEDAREA                                 
024602         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
024603       END-IF                                                             
024604     END-IF                                                               
024605                                                                          
024606     MOVE '002'          TO MSGI-KDCALL                                   
024607     MOVE '3124'         TO SPAR-IDTRANS                                  
024608     MOVE SPAR-AREA      TO MSGI-SPAR-AREA                                
024609     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024610     .                                                                    
024700     EJECT                                                                
024800                                                                          
025202 G-KOLLA-INPUT SECTION.                                                   
025204     MOVE JA                        TO INDATA-SW                          
025205                                       ALLT-SW                            
025206     MOVE NEJ                       TO TEXT-SW                            
025207                                                                          
025208     IF MID-CMD          = ALL '+' AND                                    
025209        MID-IDARTNR-UPD  = ALL '+' AND                                    
025210        MID-KVVECKOR-UPD = ALL '+'                                        
025211       MOVE ERR-PF11-AND-NO-DATA    TO MED-IDMFSFEL                       
025212       CALL WMEDKONV USING MED-WMEDAREA                                   
025213       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
025214       PERFORM MFS-ROER-EJ-FAELT-IN                                       
025215       PERFORM MFS-ROER-EJ-FAELT-UT                                       
025216       MOVE NEJ                     TO INDATA-SW                          
025217                                       ALLT-SW                            
025218     ELSE                                                                 
025219                                                                          
025220*   KONTROLLERA KOMMANDO                                                  
025224       IF MID-CMD = 'N' OR                                                
025225          MID-CMD = 'C' OR                                                
025226          MID-CMD = 'D'                                                   
025227         MOVE MID-CMD               TO MOD-CMD                            
025228         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-CMD-ATTR                       
025229       ELSE                                                               
025230         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
025231         MOVE MFS-ALFA-FAELT-FEL    TO MOD-CMD-ATTR                       
025232         MOVE NEJ                   TO INDATA-SW                          
025233                                       ALLT-SW                            
025234       END-IF                                                             
025235                                                                          
025236*   KONTROLLERA ARTIKEL                                                   
025264         MOVE MID-IDARTNR-UPD       TO W-IDARTNR                          
025265         PERFORM IMS-GU-WDK611                                            
025266         IF SEGMENT-SAKNAS                                                
025267           MOVE ERR-PART-MISSING    TO MED-IDMFSFEL                       
025268           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-UPD-ATTR               
025269           MOVE NEJ                 TO INDATA-SW                          
025270                                       ALLT-SW                            
025271         ELSE                                                             
025272           MOVE MID-IDARTNR-UPD     TO MOD-IDARTNR-UPD                    
025273           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-UPD-ATTR               
025274         END-IF                                                           
025283                                                                          
025284*   KONTROLLERA ANTAL VECKOR                                              
025285       IF MID-CMD = 'C' OR 'N'                                            
025286         MOVE MID-KVVECKOR-UPD         TO WS-TIVV-ALFA                    
025287         INSPECT WS-TIVV-ALFA REPLACING ALL ',' BY '0'                    
025288         INSPECT WS-TIVV-ALFA REPLACING ALL '.' BY '0'                    
025289         INSPECT WS-TIVV-ALFA REPLACING ALL '-' BY '0'                    
025290         INSPECT WS-TIVV-ALFA REPLACING ALL '+' BY '0'                    
025291         INSPECT WS-TIVV-ALFA REPLACING ALL '!' BY '0'                    
025292         INSPECT WS-TIVV-ALFA REPLACING ALL '"' BY '0'                    
025293         INSPECT WS-TIVV-ALFA REPLACING ALL ' ' BY '0'                    
025294         INSPECT WS-TIVV-ALFA REPLACING ALL '%' BY '0'                    
025295         INSPECT WS-TIVV-ALFA REPLACING ALL '&' BY '0'                    
025296         INSPECT WS-TIVV-ALFA REPLACING ALL '/' BY '0'                    
025297         INSPECT WS-TIVV-ALFA REPLACING ALL '(' BY '0'                    
025298         INSPECT WS-TIVV-ALFA REPLACING ALL ')' BY '0'                    
025299         INSPECT WS-TIVV-ALFA REPLACING ALL '?' BY '0'                    
025300         INSPECT WS-TIVV-ALFA REPLACING ALL ':' BY '0'                    
025301         INSPECT WS-TIVV-ALFA REPLACING ALL '_' BY '0'                    
025302         INSPECT WS-TIVV-ALFA REPLACING ALL '*' BY '0'                    
025303         IF WS-TIVV-ALFA NUMERIC                                          
025304           MOVE WS-TIVV-ALFA           TO WS-TIVV-NUM                     
025305           IF WS-TIVV-NUM < 01 OR > 52                                    
025306             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
025307             MOVE MFS-NUM-FAELT-FEL    TO MOD-KVVECKOR-UPD-ATTR           
025308             MOVE NEJ                  TO INDATA-SW                       
025309                                          ALLT-SW                         
025310           ELSE                                                           
025311             MOVE MID-KVVECKOR-UPD     TO MOD-KVVECKOR-UPD-ATTR           
025312             MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVVECKOR-UPD-ATTR           
025313           END-IF                                                         
025314         ELSE                                                             
025315           MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                    
025316           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVVECKOR-UPD-ATTR           
025317           MOVE NEJ                    TO INDATA-SW                       
025318                                          ALLT-SW                         
025319         END-IF                                                           
025320       END-IF                                                             
025321                                                                          
025322       IF ALLT-OK                                                         
025323         IF MID-CMD = 'N'                                                 
025324*---KONTROLLERA SÅ ATT INTE SEGMENT REDAN FINNS PÅ BAS                    
025325           MOVE MID-IDARTNR-UPD TO W-IDARTNR                              
025326           PERFORM IMS-GHU-WDK628                                         
025327           IF SEGMENT-FINNS                                               
025328             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-UPD-ATTR            
025329             MOVE NEJ                  TO INDATA-SW                       
025330             MOVE JA                   TO TEXT-SW                         
025331             MOVE '999 ITEM ALREADY EXISTS'                               
025332                                       TO MOD-TEMFSFEL                    
025333           ELSE                                                           
025334             MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-UPD-ATTR            
025335           END-IF                                                         
025336         END-IF                                                           
025337                                                                          
025338         IF MID-CMD = 'C'                                                 
025339*---KONTROLLERA SÅ SEGMENT FINNS PÅ BAS                                   
025340           MOVE MID-IDARTNR-UPD        TO W-IDARTNR                       
025341           PERFORM IMS-GHU-WDK628                                         
025342           IF SEGMENT-SAKNAS                                              
025343             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-UPD-ATTR            
025344             MOVE NEJ                  TO INDATA-SW                       
025345             MOVE JA                   TO TEXT-SW                         
025346             MOVE '999 ITEM MISSING'   TO MOD-TEMFSFEL                    
025347           ELSE                                                           
025348             MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-UPD-ATTR            
025349           END-IF                                                         
025350         END-IF                                                           
025351                                                                          
025352         IF MID-CMD = 'D'                                                 
025353*---KONTROLLERA SÅ SEGMENT FINNS PÅ BAS                                   
025354           MOVE MID-IDARTNR-UPD        TO W-IDARTNR                       
025355           PERFORM IMS-GHU-WDK628                                         
025356           IF SEGMENT-SAKNAS                                              
025357             MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-UPD-ATTR            
025358             MOVE NEJ                  TO INDATA-SW                       
025359             MOVE JA                   TO TEXT-SW                         
025360             MOVE '999 ITEM MISSING'   TO MOD-TEMFSFEL                    
025366           END-IF                                                         
025367         END-IF                                                           
025368       END-IF                                                             
025369                                                                          
025370       IF INDATA-FEL                                                      
025371         IF NOT TEXT-KLAR                                                 
025372           CALL WMEDKONV USING MED-WMEDAREA                               
025373           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
025374         END-IF                                                           
025375         PERFORM MFS-ROER-EJ-FAELT-UT                                     
025376         PERFORM MFS-ROER-EJ-FAELT-IN                                     
025378         MOVE NEJ                      TO ALLT-SW                         
025379       END-IF                                                             
025380     END-IF                                                               
025381     .                                                                    
025382     EJECT                                                                
025383                                                                          
025384 H-UPPDATERA SECTION.                                                     
025385     MOVE MID-IDARTNR-UPD         TO W-IDARTNR                            
025386     PERFORM IMS-GHU-WDK628                                               
025387                                                                          
025388     IF SEGMENT-FINNS                                                     
025389       IF MID-CMD = 'D'                                                   
025390         PERFORM IMS-DLET-WDK628                                          
025391       ELSE                                                               
025392         IF MID-CMD = 'C'                                                 
025393           MOVE MID-KVVECKOR-UPD  TO BYT-KVVECKOR                         
025394           MOVE MSGI-IDUSER       TO BYT-IDUSER                           
025395           MOVE DAGENS-DATUM      TO BYT-DAREGDAT                         
025396           MOVE DAGENS-TID        TO BYT-TIKLOCK                          
025397           PERFORM IMS-REPL-WDK628                                        
025398         END-IF                                                           
025399       END-IF                                                             
025400     ELSE                                                                 
025401       IF MID-CMD = 'N'                                                   
025402         MOVE MID-IDARTNR-UPD     TO BYT-IDARTNR                          
025403         MOVE MID-KVVECKOR-UPD    TO BYT-KVVECKOR                         
025404         MOVE MSGI-IDUSER         TO BYT-IDUSER                           
025405         MOVE DAGENS-DATUM        TO BYT-DAREGDAT                         
025406         MOVE DAGENS-TID          TO BYT-TIKLOCK                          
025410         MOVE ZERO TO                BYT-RELARM-FAC                       
025411                                     BYT-RELARM-PER                       
025412                                     BYT-RERETUR                          
025413                                     BYT-REREUSE                          
025414         MOVE 'N'                 TO BYT-FLLARM-ACT                       
025415         PERFORM IMS-ISRT-WDK628                                          
025416       END-IF                                                             
025417     END-IF                                                               
025418                                                                          
025419     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
025420     CALL WMEDKONV USING MED-WMEDAREA                                     
025421     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
025422     PERFORM MFS-FORM-ATTR                                                
025423     PERFORM MFS-RENSA-FAELT-IN                                           
025424     .                                                                    
025425     EJECT                                                                
025426                                                                          
025430 MFS-RENSA-FAELT-UT SECTION.                                              
025600*    --- ALLA UTDATA-FÄLT                                                 
025710*    --- INKL. BLÄDDRINGSRADER                                            
025800     MOVE MFS-RENSA-FAELT TO MOD-CMD                                      
025900                             MOD-IDARTNR-UPD                              
025910                             MOD-KVVECKOR-UPD                             
025920                                                                          
025931     PERFORM UNTIL INDX > MAX-INDX                                        
025940       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
025950       ADD +1 TO INDX                                                     
025960     END-PERFORM                                                          
026000     .                                                                    
026101                                                                          
026102 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
026104*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
026105     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                           
026106                             MOD-KVVECKOR (INDX)                          
026107                             MOD-IDUSER (INDX)                            
026108                             MOD-DAREGDAT (INDX)                          
026110     .                                                                    
026200                                                                          
026300 MFS-RENSA-FAELT-IN SECTION.                                              
026500*    --- ALLA INDATA-FÄLT                                                 
026600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
026700                             MOD-CMD                                      
026710                             MOD-IDARTNR-UPD                              
026720                             MOD-KVVECKOR-UPD                             
026800     .                                                                    
026900                                                                          
027000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027200*    --- ALLA UTDATA-FÄLT                                                 
027310*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
027400     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD                                    
027500                               MOD-IDARTNR-UPD                            
027600                               MOD-KVVECKOR-UPD                           
027601                                                                          
027602     MOVE +1 TO INDX                                                      
027603     PERFORM UNTIL INDX > MAX-INDX                                        
027604       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027605       ADD +1 TO INDX                                                     
027606     END-PERFORM                                                          
027608     .                                                                    
027609                                                                          
027610 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027612*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027613     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR (INDX)                         
027614                               MOD-KVVECKOR (INDX)                        
027615                               MOD-IDUSER (INDX)                          
027616                               MOD-DAREGDAT (INDX)                        
027620     .                                                                    
027800                                                                          
027900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028100*    --- ALLA INDATA-FÄLT                                                 
028200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
028300                               MOD-CMD                                    
028310                               MOD-IDARTNR-UPD                            
028320                               MOD-KVVECKOR-UPD                           
028400     .                                                                    
028500                                                                          
028600 MFS-FORM-ATTR SECTION.                                                   
028800*    --- ALLA INDATA-FÄLT                                                 
028900     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR                              
029000                                MOD-IDARTNR-UPD-ATTR                      
029010                                MOD-KVVECKOR-UPD-ATTR                     
029100     .                                                                    
029200                                                                          
029300 MFS-LAES-IN-IGEN SECTION.                                                
029500*    --- ALLA INDATA-FÄLT                                                 
029600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR                           
029700                                   MOD-IDARTNR-UPD-ATTR                   
029710                                   MOD-KVVECKOR-UPD-ATTR                  
029800     .                                                                    
029900     EJECT                                                                
029910                                                                          
030000* --- IMS SEKTIONER ---                                                   
030100                                                                          
030200 IMS-GET-MSG SECTION.                                                     
030400     MOVE '  QC'          TO GODK-STATUSKODER                             
030500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030700     PERFORM IMS-STATUSKONTROLL                                           
030800     .                                                                    
030900                                                                          
031000 IMS-INSERT-MSG SECTION.                                                  
031100     IF MSGI-IDLAND-SPR = 'GB'                                            
031200       MOVE 'N'           TO MFS-KDHUVOMR                                 
031300     END-IF                                                               
031500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031600     MOVE SPACE           TO GODK-STATUSKODER                             
031700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031900     PERFORM IMS-STATUSKONTROLL                                           
032000     .                                                                    
032101     EJECT                                                                
032102                                                                          
032174 IMS-GU-WDK628-DSEQ SECTION.                                              
032175     STRING 'WDK601  (WDK6DSEQ =' W-IDARTNR-X ')'                         
032176     DELIMITED BY SIZE INTO SSA1                                          
032177     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032178     DELIMITED BY SIZE INTO SSA2                                          
032181     MOVE 'WDK628  '        TO SSA3                                       
032182     MOVE '  GE'            TO GODK-STATUSKODER                           
032183     CALL CBLTDLI USING GU WDK6D-PCB DLI-IO-WDK628 SSA1 SSA2 SSA3         
032184     MOVE WDK6D-STATUS-CODE TO STATUS-WS                                  
032185     PERFORM IMS-STATUSKONTROLL                                           
032190     .                                                                    
032191                                                                          
032192 IMS-GN-WDK628-DSEQ SECTION.                                              
032193     STRING 'WDK601  (WDK6DSEQ>=' W-IDARTNR-X ')'                         
032194     DELIMITED BY SIZE INTO SSA1                                          
032195     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032196     DELIMITED BY SIZE INTO SSA2                                          
032197     MOVE 'WDK628  '        TO SSA3                                       
032198     MOVE '  GEGB'          TO GODK-STATUSKODER                           
032199     CALL CBLTDLI USING GN WDK6D-PCB DLI-IO-WDK628 SSA1 SSA2 SSA3         
032200     MOVE WDK6D-STATUS-CODE TO STATUS-WS                                  
032201     PERFORM IMS-STATUSKONTROLL                                           
032202     .                                                                    
032203     EJECT                                                                
032204                                                                          
032205 IMS-GU-WDK611 SECTION.                                                   
032206     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032207     DELIMITED BY SIZE INTO SSA1                                          
032208     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032209     DELIMITED BY SIZE INTO SSA2                                          
032210     MOVE '  GE'           TO GODK-STATUSKODER                            
032211     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
032212     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032213     PERFORM IMS-STATUSKONTROLL                                           
032214     .                                                                    
032215                                                                          
032259 IMS-GHU-WDK628 SECTION.                                                  
032260     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032261     DELIMITED BY SIZE INTO SSA1                                          
032262     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032263     DELIMITED BY SIZE INTO SSA2                                          
032264     MOVE   'WDK628  '     TO SSA3                                        
032265     MOVE '  GE'           TO GODK-STATUSKODER                            
032266     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK628 SSA1 SSA2 SSA3         
032267     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032268     PERFORM IMS-STATUSKONTROLL                                           
032269     .                                                                    
032270                                                                          
032271 IMS-ISRT-WDK628 SECTION.                                                 
032272     MOVE 'WDK628   '      TO SSA1                                        
032273     MOVE '  '             TO GODK-STATUSKODER                            
032274     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK628 SSA1                  
032275     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032276     PERFORM IMS-STATUSKONTROLL                                           
032277     .                                                                    
032278                                                                          
032279 IMS-REPL-WDK628 SECTION.                                                 
032280     MOVE '  '             TO GODK-STATUSKODER                            
032281     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK628                       
032282     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032283     PERFORM IMS-STATUSKONTROLL                                           
032284     .                                                                    
032285                                                                          
032286 IMS-DLET-WDK628 SECTION.                                                 
032287     MOVE '  '             TO GODK-STATUSKODER                            
032288     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK628                       
032289     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032290     PERFORM IMS-STATUSKONTROLL                                           
032291     .                                                                    
032292     EJECT                                                                
032293                                                                          
032294 IMS-GU-WDD311-BSEQ SECTION.                                              
032295     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
032296     DELIMITED BY SIZE INTO SSA1                                          
032297     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
032298     DELIMITED BY SIZE INTO SSA2                                          
032299     MOVE '  GE'            TO GODK-STATUSKODER                           
032300     CALL CBLTDLI USING GU WDD3B-PCB DLI-IO-WDD311 SSA1 SSA2              
032301     MOVE WDD3B-STATUS-CODE TO STATUS-WS                                  
032302     PERFORM IMS-STATUSKONTROLL                                           
032303     .                                                                    
032304     EJECT                                                                
032305                                                                          
032310 IMS-STATUSKONTROLL SECTION.                                              
032500     SET STATUS-IX TO 1                                                   
032600     SEARCH GODK-STATUS                                                   
032700       AT END                                                             
032800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032900         DELIMITED BY SIZE INTO FELTEXT                                   
033000         CALL FELLOG                                                      
033100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033200         CONTINUE                                                         
033300     END-SEARCH                                                           
033400     .                                                                    
