001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W0081600.                                                
001500 AUTHOR.         SUSANNE OLSSON.                                          
001600 DATE-WRITTEN.   98/02/06.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET UPPDATERAR KULLAGERUPPGIFTER FÖR USA ENLIGT           
002100*        SPECIFIKATION PÅ BILD. (KULLAGERREG. WDGX4747)                   
002200*                                                                         
002210*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002220*        PROGRAMMET UPPDATERAR WLXXDT (WDGX)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W0T816                                              
002600*        MID:         W0I81601                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W0O81601                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W0081600'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  WS-DIKULLAG-INNER           PIC S9(4)V9(1) VALUE ZERO.               
004410 77  WS-DIKULLAG-YTTER           PIC S9(4)V9(1) VALUE ZERO.               
004420 77  WS-VKARTNTO                 PIC S9(4)V9(3) VALUE ZERO.               
004430                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004802     88  INDATA-OK                           VALUE 'J'.                   
004810     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
004901 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
004902     88  KDCMD-FINNS                         VALUE 'J'.                   
004903     88  KDCMD-SAKNAS                        VALUE 'N'.                   
004904                                                                          
004910 77  IFYLLT-SW                   PIC X       VALUE 'J'.                   
004920     88  IFYLLT-OK                           VALUE 'J'.                   
004930     88  IFYLLT-FEL                          VALUE 'N'.                   
004940                                                                          
004950 77  INSERT-SW                   PIC X       VALUE 'J'.                   
004960     88  INSERT-OK                           VALUE 'J'.                   
004970     88  INSERT-FEL                          VALUE 'N'.                   
004980                                                                          
004990 77  VISA-INFO-SW                PIC X       VALUE 'J'.                   
004991     88  VISA-INFO-OK                        VALUE 'J'.                   
004992     88  VISA-INFO-FEL                       VALUE 'N'.                   
004993                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '0816'.                
005600     88  GODK-MID                            VALUE '0811' '0812'          
005700                                                   '0813' '0814'          
005800                                                   '0815' '0816'          
005900                                                   '0817' '0818'          
006000                                                   '0819'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006910     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007502     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007503     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ERR-ADD-AND-CANCEL      PIC X(3)    VALUE '780'.                 
007810     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007820     03  ERR-INVALID-COMBINATION PIC X(3)    VALUE '238'.                 
007830     03  ERR-PART-MISS-ON-FILE   PIC X(3)    VALUE '769'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008601*    --- PARAMETRAR TILL SUBPROGRAM WDECAREA                              
008602                                                                          
008603*01 -COPY WDECAREA                                                        
008610     EJECT                                                                
008620*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
008630 01  FILLER                      PIC X(16)  VALUE 'STARTARTU'.            
008640*    -COPY W400ARTU                                                       
008650     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W0I81601                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W0O81601                                                 
009900     EJECT                                                                
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
011003     03  W-WDGX01KEY-X.                                                   
011004         05 W-4747-IDHTYP             PIC X(4)    VALUE '4747'.           
011005         05 W-4747-IDARTNR            PIC S9(9)   COMP-3.                 
011006         05 W-4747-LOWVALUE           PIC X(21)   VALUE LOW-VALUE.        
011007                                                                          
011011     03  W-KDSEGKEY-X.                                                    
011020         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
011021                                                                          
011030     03  W-IDARTNR-X.                                                     
011040         05  W-IDARTNR           PIC S9(9)      COMP-3.                   
011050                                                                          
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXDT01'.                    
013002 01  DLI-IO-WLXXDT01.                                                     
013003*    03  -COPY WDGR4747 -PRE XXDT-                                        
013005     EJECT                                                                
013006 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXDT11'.                    
013007 01  DLI-IO-WLXXDT11.                                                     
013010*    03  -COPY WDGX4747 -PRE XXDT-                                        
013310     EJECT                                                                
013320 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
013330 01  DLI-IO-WLARTC01.                                                     
013340*    03  -COPY WDK601   -PRE ARTC-                                        
013350     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE XXDT-                                              
013810     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014001*01  -COPY W0008  -PRE ARTC-                                              
014002     05  FILLER                  PIC X.                                   
014003     EJECT                                                                
014004 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB XXDT-PCB ARTC-PCB.            
014005 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXDT-PCB ARTC-PCB.            
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014801         IF MFS-UPDATE                                                    
014802           PERFORM G-KOLLA-INPUT                                          
014803           IF INDATA-OK                                                   
014804             PERFORM H-UPPDATERA                                          
014805           END-IF                                                         
014810         ELSE                                                             
015001           IF MFS-FIRST                                                   
015002             PERFORM C-FOERSTA-SIDA                                       
015003           ELSE                                                           
015004             PERFORM E-SAMMA-SIDA                                         
015010           END-IF                                                         
015110         END-IF                                                           
015200         PERFORM F-LAES-VISA-INFO                                         
015400       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O81601 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I81601                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I81601                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W0O816N1' TO MFS-IDMOD                                         
018300     MOVE '0816' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018510     PERFORM MFS-RENSA-FAELT-IN                                           
018511     MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                                 
018520                                                                          
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
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '0816'            TO MSGI-IDTRANS                               
020300     IF GODK-MID                                                          
020410         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700                                                                          
020710     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
020720     MOVE '2'               TO MFS-KDMFSFOR                               
020730                                                                          
020800     MOVE JA TO NYCKLAR-SW                                                
020900                                                                          
021001                                                                          
021002*    -- KONTROLL AV IDARTNR                                               
021003     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021004                                                                          
021005     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021006       MOVE '7'         TO MFS-IDPFK                                      
021007       MOVE SPACE       TO MFS-KDTRTYP                                    
021008     END-IF                                                               
021009     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021010     IF MSGI-IDARTNR NUMERIC                                              
021011       MOVE MSGI-IDARTNR TO W-4747-IDARTNR                                
021012       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
021013     ELSE                                                                 
021014       MOVE NEJ TO NYCKLAR-SW                                             
021020     END-IF                                                               
021101                                                                          
021102     IF GODK-MID OR NYCKLAR-OK                                            
021103       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
021104       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021105     ELSE                                                                 
021106       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
021110     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700       PERFORM MFS-RENSA-FAELT-IN                                         
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021810       MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                               
021900     END-IF                                                               
022000     .                                                                    
022200     EJECT                                                                
022301 C-FOERSTA-SIDA SECTION.                                                  
022302                                                                          
022303     PERFORM MFS-RENSA-FAELT-IN                                           
022304     MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                                 
022305     .                                                                    
022306     EJECT                                                                
022307 E-SAMMA-SIDA SECTION.                                                    
022308                                                                          
022309     IF EGEN-MID OR HELP-MID                                              
022310       IF MID-INPUT = ALL '+' AND                                         
022311         MID-FLANNULL = '+'                                               
022312         PERFORM MFS-RENSA-FAELT-IN                                       
022313         MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                             
022321       ELSE                                                               
022322         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022323         CALL WMEDKONV USING MED-WMEDAREA                                 
022324         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
022325         PERFORM EA-MID-INDATA-TILL-MOD                                   
022326       END-IF                                                             
022328     ELSE                                                                 
022329       PERFORM MFS-RENSA-FAELT-IN                                         
022330       MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                               
022331     END-IF                                                               
022332     .                                                                    
022333     EJECT                                                                
022334 EA-MID-INDATA-TILL-MOD SECTION.                                          
022335                                                                          
022336* * * * * FÖR VARJE MID-FÄLT                                              
022337* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
022338* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
022339* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
022340                                                                          
022341     IF MID-INPUT = ALL '+'                                               
022342       PERFORM MFS-RENSA-FAELT-IN                                         
022343     ELSE                                                                 
022347       IF MID-KDCMD-BEARTKUL = '+'                                        
022348         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-BEARTKUL                       
022349       ELSE                                                               
022350         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-BEARTKUL-ATTR            
022351         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-BEARTKUL                     
022352       END-IF                                                             
022353                                                                          
022354       IF MID-BEARTKUL-1 = ALL '+'                                        
022355         MOVE MFS-RENSA-FAELT TO MOD-BEARTKUL-1-IN                        
022356       ELSE                                                               
022357         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEARTKUL-1-IN-ATTR             
022358         MOVE MFS-ROER-EJ-FAELT TO MOD-BEARTKUL-1-IN                      
022359       END-IF                                                             
022360                                                                          
022361       IF MID-BEARTKUL-2 = ALL '+'                                        
022362         MOVE MFS-RENSA-FAELT TO MOD-BEARTKUL-2-IN                        
022363       ELSE                                                               
022364         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEARTKUL-2-IN-ATTR             
022365         MOVE MFS-ROER-EJ-FAELT TO MOD-BEARTKUL-2-IN                      
022366       END-IF                                                             
022367                                                                          
022368       IF MID-KDCMD-BELEVKUL = '+'                                        
022369         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-BELEVKUL                       
022370       ELSE                                                               
022371         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-BELEVKUL-ATTR            
022372         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-BELEVKUL                     
022373       END-IF                                                             
022374                                                                          
022375       IF MID-BELEVKUL-1 = ALL '+'                                        
022376         MOVE MFS-RENSA-FAELT TO MOD-BELEVKUL-1-IN                        
022377       ELSE                                                               
022378         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEVKUL-1-IN-ATTR             
022379         MOVE MFS-ROER-EJ-FAELT TO MOD-BELEVKUL-1-IN                      
022380       END-IF                                                             
022381                                                                          
022382       IF MID-BELEVKUL-2 = ALL '+'                                        
022383         MOVE MFS-RENSA-FAELT TO MOD-BELEVKUL-2-IN                        
022384       ELSE                                                               
022385         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEVKUL-2-IN-ATTR             
022386         MOVE MFS-ROER-EJ-FAELT TO MOD-BELEVKUL-2-IN                      
022387       END-IF                                                             
022388                                                                          
022389       IF MID-KDCMD-DIKULLAG-INNER = '+'                                  
022390         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-DIKULLAG-INNER                 
022391       ELSE                                                               
022392         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
022393                            MOD-KDCMD-DIKULLAG-INNER-ATTR                 
022394         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-DIKULLAG-INNER               
022395       END-IF                                                             
022396                                                                          
022397       IF MID-DIKULLAG-INNER = ALL '+'                                    
022398         MOVE MFS-RENSA-FAELT TO MOD-DIKULLAG-INNER-IN                    
022399       ELSE                                                               
022400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DIKULLAG-INNER-IN-ATTR         
022401         MOVE MFS-ROER-EJ-FAELT TO MOD-DIKULLAG-INNER-IN                  
022402       END-IF                                                             
022403                                                                          
022404       IF MID-KDCMD-DIKULLAG-YTTER = '+'                                  
022405         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-DIKULLAG-YTTER                 
022406       ELSE                                                               
022407         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
022408                            MOD-KDCMD-DIKULLAG-YTTER-ATTR                 
022409         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-DIKULLAG-YTTER               
022410       END-IF                                                             
022411                                                                          
022412       IF MID-DIKULLAG-YTTER = ALL '+'                                    
022413         MOVE MFS-RENSA-FAELT TO MOD-DIKULLAG-YTTER-IN                    
022414       ELSE                                                               
022415         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DIKULLAG-YTTER-IN-ATTR         
022416         MOVE MFS-ROER-EJ-FAELT TO MOD-DIKULLAG-YTTER-IN                  
022417       END-IF                                                             
022418                                                                          
022419       IF MID-KDCMD-IDLEVKUL = '+'                                        
022420         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IDLEVKUL                       
022421       ELSE                                                               
022422         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IDLEVKUL-ATTR            
022423         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IDLEVKUL                     
022424       END-IF                                                             
022425                                                                          
022426       IF MID-IDLEVKUL-1 = ALL '+'                                        
022427         MOVE MFS-RENSA-FAELT TO MOD-IDLEVKUL-1-IN                        
022428       ELSE                                                               
022429         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVKUL-1-IN-ATTR             
022430         MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVKUL-1-IN                      
022431       END-IF                                                             
022432                                                                          
022433       IF MID-IDLEVKUL-2 = ALL '+'                                        
022434         MOVE MFS-RENSA-FAELT TO MOD-IDLEVKUL-2-IN                        
022435       ELSE                                                               
022436         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVKUL-2-IN-ATTR             
022437         MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVKUL-2-IN                      
022438       END-IF                                                             
022439                                                                          
022440       IF MID-KDCMD-IDSTAKUL = '+'                                        
022441         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-IDSTAKUL                       
022442       ELSE                                                               
022443         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IDSTAKUL-ATTR            
022444         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IDSTAKUL                     
022445       END-IF                                                             
022446                                                                          
022447       IF MID-IDSTAKUL = ALL '+'                                          
022448         MOVE MFS-RENSA-FAELT TO MOD-IDSTAKUL-IN                          
022449       ELSE                                                               
022450         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDSTAKUL-IN-ATTR               
022451         MOVE MFS-ROER-EJ-FAELT TO MOD-IDSTAKUL-IN                        
022452       END-IF                                                             
022453                                                                          
022454       IF MID-KDCMD-VKARTNTO = '+'                                        
022455         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-VKARTNTO                       
022456       ELSE                                                               
022457         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-VKARTNTO-ATTR            
022458         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-VKARTNTO                     
022459       END-IF                                                             
022460                                                                          
022461       IF MID-VKARTNTO = ALL '+'                                          
022462         MOVE MFS-RENSA-FAELT TO MOD-VKARTNTO-IN                          
022463       ELSE                                                               
022464         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKARTNTO-IN-ATTR               
022465         MOVE MFS-ROER-EJ-FAELT TO MOD-VKARTNTO-IN                        
022466       END-IF                                                             
022467                                                                          
022468       IF MID-KDCMD-KDARTURS = '+'                                        
022469         MOVE MFS-RENSA-FAELT TO MOD-KDCMD-KDARTURS                       
022470       ELSE                                                               
022471         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-KDARTURS-ATTR            
022472         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-KDARTURS                     
022473       END-IF                                                             
022474                                                                          
022475       IF MID-KDARTURS = ALL '+'                                          
022476         MOVE MFS-RENSA-FAELT TO MOD-KDARTURS-IN                          
022477       ELSE                                                               
022478         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDARTURS-IN-ATTR               
022479         MOVE MFS-ROER-EJ-FAELT TO MOD-KDARTURS-IN                        
022480       END-IF                                                             
022481     END-IF                                                               
022482                                                                          
022483     IF MID-FLANNULL = '+'                                                
022484       MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                               
022485     ELSE                                                                 
022486       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLANNULL-ATTR                    
022487       MOVE MFS-ROER-EJ-FAELT TO MOD-FLANNULL                             
022488     END-IF                                                               
022489                                                                          
022496     .                                                                    
022497     EJECT                                                                
022515 F-LAES-VISA-INFO SECTION.                                                
022520                                                                          
022521     MOVE JA TO VISA-INFO-SW                                              
022530     IF MFS-FIRST                                                         
022550       PERFORM IMS-GET-WLARTC01                                           
022560       IF SEGMENT-SAKNAS                                                  
022590         MOVE ERR-PART-MISS-ON-FILE TO MED-IDMFSFEL                       
022591         CALL WMEDKONV USING MED-WMEDAREA                                 
022592         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022593         PERFORM MFS-RENSA-FAELT-IN                                       
022594         PERFORM MFS-RENSA-FAELT-UT                                       
022595         MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                             
022596         PERFORM MFS-STAENG-FAELT-IN                                      
022597         MOVE NEJ TO VISA-INFO-SW                                         
022598       END-IF                                                             
022599     END-IF                                                               
022600                                                                          
022601     IF VISA-INFO-OK                                                      
022602                                                                          
022610       PERFORM IMS-GET-XXDT-KUL01                                         
022700                                                                          
022800       IF SEGMENT-SAKNAS                                                  
022810         IF MED-IDMFSFEL = '238' OR '011'                                 
022820           CONTINUE                                                       
022830         ELSE                                                             
022900           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
023000           CALL WMEDKONV USING MED-WMEDAREA                               
023100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
023200           PERFORM MFS-RENSA-FAELT-UT                                     
023210           PERFORM MFS-STAENG-KDCMD                                       
023220         END-IF                                                           
023300       ELSE                                                               
023310         PERFORM IMS-GET-XXDT-KUL11                                       
023320                                                                          
023330         IF SEGMENT-FINNS                                                 
023400           MOVE XXDT-KUL-BEARTKUL-1     TO MOD-BEARTKUL-1                 
023410           MOVE XXDT-KUL-BEARTKUL-2     TO MOD-BEARTKUL-2                 
023420           MOVE XXDT-KUL-BELEVKUL-1     TO MOD-BELEVKUL-1                 
023430           MOVE XXDT-KUL-BELEVKUL-2     TO MOD-BELEVKUL-2                 
023440           MOVE XXDT-KUL-DIKULLAG-INNER TO MOD-DIKULLAG-INNER             
023441           MOVE XXDT-KUL-DIKULLAG-YTTER TO MOD-DIKULLAG-YTTER             
023450           MOVE XXDT-KUL-IDLEVKUL-1     TO MOD-IDLEVKUL-1                 
023460           MOVE XXDT-KUL-IDLEVKUL-2     TO MOD-IDLEVKUL-2                 
023470           MOVE XXDT-KUL-IDSTAKUL       TO MOD-IDSTAKUL                   
023480           MOVE XXDT-KUL-VKARTNTO       TO MOD-VKARTNTO                   
023490           MOVE XXDT-KUL-KDARTURS       TO MOD-KDARTURS                   
023500         END-IF                                                           
023510       END-IF                                                             
023520     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
024608 G-KOLLA-INPUT SECTION.                                                   
024609                                                                          
024610     IF MID-INPUT = ALL '+' AND                                           
024611       MID-FLANNULL = '+'                                                 
024612       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024613       CALL WMEDKONV USING MED-WMEDAREA                                   
024614       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024615       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024616       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024617       MOVE MFS-ROER-EJ-FAELT TO MOD-FLANNULL                             
024618       MOVE NEJ TO INDATA-SW                                              
024619     ELSE                                                                 
024620       MOVE JA TO INDATA-SW                                               
024621       PERFORM GA-KOLLA-INPUT                                             
024622     END-IF                                                               
024623     .                                                                    
024624     EJECT                                                                
024625 GA-KOLLA-INPUT SECTION.                                                  
024626                                                                          
024627     IF MID-INPUT NOT = ALL '+' AND                                       
024628       MID-FLANNULL NOT = '+'                                             
024629       MOVE ERR-ADD-AND-CANCEL TO MED-IDMFSFEL                            
024630       CALL WMEDKONV USING MED-WMEDAREA                                   
024631       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024632       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024633       MOVE MFS-ROER-EJ-FAELT TO MOD-FLANNULL                             
024634       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR                       
024635       PERFORM MFS-LAES-IN-IGEN                                           
024636       MOVE NEJ TO INDATA-SW                                              
024644     ELSE                                                                 
024645                                                                          
024646******* FÖR INMATN.FÄLT : MID-FLANNULL ************************           
024647                                                                          
024648       IF MID-FLANNULL NOT = ALL '+'                                      
024649         IF MID-FLANNULL = 'Y'                                            
024650           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLANNULL-ATTR                 
024651         ELSE                                                             
024652           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLANNULL-ATTR                   
024653           MOVE NEJ TO INDATA-SW                                          
024654         END-IF                                                           
024655       ELSE                                                               
024656         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLANNULL-ATTR                   
024657       END-IF                                                             
024658                                                                          
024659******* FÖR INMATN.FÄLT : MID-INPUT ***************************           
024660                                                                          
024661       IF MID-KDCMD-BEARTKUL NOT = ALL '+'                                
024662         IF MID-KDCMD-BEARTKUL = 'R'                                      
024663           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-BEARTKUL-ATTR           
024664         ELSE                                                             
024665           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-BEARTKUL-ATTR             
024666           MOVE NEJ TO INDATA-SW                                          
024667         END-IF                                                           
024668         MOVE JA TO KDCMD-SW                                              
024669       END-IF                                                             
024670                                                                          
024671       IF MID-BEARTKUL NOT = ALL '+'                                      
024672         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEARTKUL-1-IN-ATTR              
024673         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEARTKUL-2-IN-ATTR              
024674       ELSE                                                               
024675         MOVE NEJ TO INSERT-SW                                            
024676       END-IF                                                             
024677                                                                          
024678       IF MID-KDCMD-BEARTKUL = 'R' AND                                    
024679         MID-BEARTKUL = ALL '+'                                           
024681         MOVE NEJ TO IFYLLT-SW                                            
024682       ELSE                                                               
024683         IF MID-KDCMD-BEARTKUL = '+' AND                                  
024684           MID-BEARTKUL NOT = ALL '+'                                     
024687           MOVE NEJ TO IFYLLT-SW                                          
024688         END-IF                                                           
024689       END-IF                                                             
024690                                                                          
024691       IF MID-KDCMD-BELEVKUL NOT = ALL '+'                                
024692         IF MID-KDCMD-BELEVKUL = 'R'                                      
024693           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-BELEVKUL-ATTR           
024694         ELSE                                                             
024695           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-BELEVKUL-ATTR             
024696           MOVE NEJ TO INDATA-SW                                          
024697         END-IF                                                           
024698         MOVE JA TO KDCMD-SW                                              
024699       END-IF                                                             
024700                                                                          
024701       IF MID-BELEVKUL NOT = ALL '+'                                      
024702         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEVKUL-1-IN-ATTR              
024703         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEVKUL-2-IN-ATTR              
024704       ELSE                                                               
024705         MOVE NEJ TO INSERT-SW                                            
024706       END-IF                                                             
024707                                                                          
024708       IF MID-KDCMD-BELEVKUL = 'R' AND                                    
024709         MID-BELEVKUL = ALL '+'                                           
024710         MOVE NEJ TO IFYLLT-SW                                            
024711       ELSE                                                               
024712         IF MID-KDCMD-BELEVKUL = '+' AND                                  
024713           MID-BELEVKUL NOT = ALL '+'                                     
024714           MOVE NEJ TO IFYLLT-SW                                          
024715         END-IF                                                           
024716       END-IF                                                             
024717                                                                          
024718       IF MID-KDCMD-DIKULLAG-INNER NOT = ALL '+'                          
024719         IF MID-KDCMD-DIKULLAG-INNER = 'R'                                
024720           MOVE MFS-ALFA-FAELT-RAETT TO                                   
024721                             MOD-KDCMD-DIKULLAG-INNER-ATTR                
024722         ELSE                                                             
024723           MOVE MFS-ALFA-FAELT-FEL TO                                     
024724                             MOD-KDCMD-DIKULLAG-INNER-ATTR                
024725           MOVE NEJ TO INDATA-SW                                          
024726         END-IF                                                           
024727         MOVE JA TO KDCMD-SW                                              
024728       END-IF                                                             
024729                                                                          
024730       IF MID-DIKULLAG-INNER NOT = ALL '+'                                
024731         MOVE MID-DIKULLAG-INNER TO DEC-IDFRIDATA                         
024732         MOVE +4                 TO DEC-KVHELTAL                          
024733         MOVE +1                 TO DEC-KVDECIMAL                         
024734         CALL WDECEDIT USING DEC-WDECAREA                                 
024735         IF DEC-KDSVAR-FEL                                                
024736           MOVE MFS-NUM-FAELT-FEL TO MOD-DIKULLAG-INNER-IN-ATTR           
024737           MOVE NEJ TO INDATA-SW                                          
024738         ELSE                                                             
024739           MOVE DEC-IDEDITDATA TO WS-DIKULLAG-INNER                       
024740           MOVE MFS-NUM-FAELT-RAETT TO MOD-DIKULLAG-INNER-IN-ATTR         
024741         END-IF                                                           
024742       ELSE                                                               
024743         MOVE NEJ TO INSERT-SW                                            
024744       END-IF                                                             
024745                                                                          
024746       IF MID-KDCMD-DIKULLAG-INNER = 'R' AND                              
024747         MID-DIKULLAG-INNER = ALL '+'                                     
024748         MOVE NEJ TO IFYLLT-SW                                            
024749       ELSE                                                               
024750         IF MID-KDCMD-DIKULLAG-INNER = '+' AND                            
024751           MID-DIKULLAG-INNER NOT = ALL '+'                               
024752           MOVE NEJ TO IFYLLT-SW                                          
024753         END-IF                                                           
024754       END-IF                                                             
024755                                                                          
024756       IF MID-KDCMD-DIKULLAG-YTTER NOT = ALL '+'                          
024757         IF MID-KDCMD-DIKULLAG-YTTER = 'R'                                
024758           MOVE MFS-ALFA-FAELT-RAETT TO                                   
024759                             MOD-KDCMD-DIKULLAG-YTTER-ATTR                
024760         ELSE                                                             
024761           MOVE MFS-ALFA-FAELT-FEL TO                                     
024762                             MOD-KDCMD-DIKULLAG-YTTER-ATTR                
024763           MOVE NEJ TO INDATA-SW                                          
024764         END-IF                                                           
024765         MOVE JA TO KDCMD-SW                                              
024766       END-IF                                                             
024767                                                                          
024768       IF MID-DIKULLAG-YTTER NOT = ALL '+'                                
024769         MOVE MID-DIKULLAG-YTTER TO DEC-IDFRIDATA                         
024770         MOVE +4                 TO DEC-KVHELTAL                          
024771         MOVE +1                 TO DEC-KVDECIMAL                         
024772         CALL WDECEDIT USING DEC-WDECAREA                                 
024773         IF DEC-KDSVAR-FEL                                                
024774           MOVE MFS-NUM-FAELT-FEL TO MOD-DIKULLAG-YTTER-IN-ATTR           
024775           MOVE NEJ TO INDATA-SW                                          
024776         ELSE                                                             
024777           MOVE DEC-IDEDITDATA TO WS-DIKULLAG-YTTER                       
024778           MOVE MFS-NUM-FAELT-RAETT TO MOD-DIKULLAG-YTTER-IN-ATTR         
024779         END-IF                                                           
024780       ELSE                                                               
024781         MOVE NEJ TO INSERT-SW                                            
024782       END-IF                                                             
024783                                                                          
024784       IF MID-KDCMD-DIKULLAG-YTTER = 'R' AND                              
024785         MID-DIKULLAG-YTTER = ALL '+'                                     
024786         MOVE NEJ TO IFYLLT-SW                                            
024787       ELSE                                                               
024788         IF MID-KDCMD-DIKULLAG-YTTER = '+' AND                            
024789           MID-DIKULLAG-YTTER NOT = ALL '+'                               
024790           MOVE NEJ TO IFYLLT-SW                                          
024791         END-IF                                                           
024792       END-IF                                                             
024793                                                                          
024794       IF MID-KDCMD-IDLEVKUL NOT = ALL '+'                                
024795         IF MID-KDCMD-IDLEVKUL = 'R'                                      
024796           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IDLEVKUL-ATTR           
024797         ELSE                                                             
024798           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IDLEVKUL-ATTR             
024799           MOVE NEJ TO INDATA-SW                                          
024800         END-IF                                                           
024801         MOVE JA TO KDCMD-SW                                              
024802       END-IF                                                             
024803                                                                          
024804       IF MID-IDLEVKUL NOT = ALL '+'                                      
024805         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKUL-1-IN-ATTR              
024806         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKUL-2-IN-ATTR              
024807       ELSE                                                               
024808         MOVE NEJ TO INSERT-SW                                            
024809       END-IF                                                             
024810                                                                          
024811       IF MID-KDCMD-IDLEVKUL = 'R' AND                                    
024812         MID-IDLEVKUL = ALL '+'                                           
024813         MOVE NEJ TO IFYLLT-SW                                            
024814       ELSE                                                               
024815         IF MID-KDCMD-IDLEVKUL = '+' AND                                  
024816           MID-IDLEVKUL NOT = ALL '+'                                     
024817           MOVE NEJ TO IFYLLT-SW                                          
024818         END-IF                                                           
024819       END-IF                                                             
024820                                                                          
024821       IF MID-KDCMD-IDSTAKUL NOT = ALL '+'                                
024822         IF MID-KDCMD-IDSTAKUL = 'R'                                      
024823           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IDSTAKUL-ATTR           
024824         ELSE                                                             
024825           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IDSTAKUL-ATTR             
024826           MOVE NEJ TO INDATA-SW                                          
024827         END-IF                                                           
024828         MOVE JA TO KDCMD-SW                                              
024829       END-IF                                                             
024830                                                                          
024831       IF MID-IDSTAKUL NOT = ALL '+'                                      
024832         IF MID-IDSTAKUL NOT NUMERIC                                      
024833           MOVE MFS-NUM-FAELT-FEL TO MOD-IDSTAKUL-IN-ATTR                 
024834           MOVE NEJ TO INDATA-SW                                          
024835         ELSE                                                             
024836           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDSTAKUL-IN-ATTR               
024837         END-IF                                                           
024838       ELSE                                                               
024839         MOVE NEJ TO INSERT-SW                                            
024840       END-IF                                                             
024841                                                                          
024842       IF MID-KDCMD-IDSTAKUL = 'R' AND                                    
024843         MID-IDSTAKUL = ALL '+'                                           
024844         MOVE NEJ TO IFYLLT-SW                                            
024845       ELSE                                                               
024846         IF MID-KDCMD-IDSTAKUL = '+' AND                                  
024847           MID-IDSTAKUL NOT = ALL '+'                                     
024848           MOVE NEJ TO IFYLLT-SW                                          
024849         END-IF                                                           
024850       END-IF                                                             
024851                                                                          
024852       IF MID-KDCMD-VKARTNTO NOT = ALL '+'                                
024853         IF MID-KDCMD-VKARTNTO = 'R'                                      
024854           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-VKARTNTO-ATTR           
024855         ELSE                                                             
024856           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-VKARTNTO-ATTR             
024857           MOVE NEJ TO INDATA-SW                                          
024858         END-IF                                                           
024859         MOVE JA TO KDCMD-SW                                              
024860       END-IF                                                             
024861                                                                          
024862       IF MID-VKARTNTO NOT = ALL '+'                                      
024863         MOVE MID-VKARTNTO       TO DEC-IDFRIDATA                         
024864         MOVE +4                 TO DEC-KVHELTAL                          
024865         MOVE +3                 TO DEC-KVDECIMAL                         
024866         CALL WDECEDIT USING DEC-WDECAREA                                 
024867         IF DEC-KDSVAR-FEL                                                
024868           MOVE MFS-NUM-FAELT-FEL TO MOD-VKARTNTO-IN-ATTR                 
024869           MOVE NEJ TO INDATA-SW                                          
024870         ELSE                                                             
024871           MOVE DEC-IDEDITDATA TO WS-VKARTNTO                             
024872           MOVE MFS-NUM-FAELT-RAETT TO MOD-VKARTNTO-IN-ATTR               
024873         END-IF                                                           
024874       ELSE                                                               
024875         MOVE NEJ TO INSERT-SW                                            
024876       END-IF                                                             
024877                                                                          
024878       IF MID-KDCMD-VKARTNTO = 'R' AND                                    
024879         MID-VKARTNTO = ALL '+'                                           
024880         MOVE NEJ TO IFYLLT-SW                                            
024881       ELSE                                                               
024882         IF MID-KDCMD-VKARTNTO = '+' AND                                  
024883           MID-VKARTNTO NOT = ALL '+'                                     
024884           MOVE NEJ TO IFYLLT-SW                                          
024885         END-IF                                                           
024886       END-IF                                                             
024887                                                                          
024888       IF MID-KDCMD-KDARTURS NOT = ALL '+'                                
024889         IF MID-KDCMD-KDARTURS = 'R'                                      
024890           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-KDARTURS-ATTR           
024891         ELSE                                                             
024892           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-KDARTURS-ATTR             
024893           MOVE NEJ TO INDATA-SW                                          
024894         END-IF                                                           
024895         MOVE JA TO KDCMD-SW                                              
024896       END-IF                                                             
024897                                                                          
024898       IF MID-KDARTURS NOT = ALL '+'                                      
024899         MOVE MID-KDARTURS TO ARTU-KDARTURS                               
024900         MOVE SPACE        TO ARTU-IDDC                                   
024901         MOVE ZERO         TO ARTU-IDDISTR                                
024902         CALL W400ARTU USING ARTU-W400ARTU                                
024903         IF ARTU-KDARTURS = SPACE OR                                      
024904            ARTU-KDARTURS-NUM = ZERO                                      
024905           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDARTURS-IN-ATTR                
024906           MOVE NEJ TO INDATA-SW                                          
024907         ELSE                                                             
024908           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDARTURS-IN-ATTR              
024909         END-IF                                                           
024910       ELSE                                                               
024911         MOVE NEJ TO INSERT-SW                                            
024912       END-IF                                                             
024913                                                                          
024914       IF MID-KDCMD-KDARTURS = 'R' AND                                    
024915         MID-KDARTURS = ALL '+'                                           
024916         MOVE NEJ TO IFYLLT-SW                                            
024917       ELSE                                                               
024918         IF MID-KDCMD-KDARTURS = '+' AND                                  
024919           MID-KDARTURS NOT = ALL '+'                                     
024920           MOVE NEJ TO IFYLLT-SW                                          
024921         END-IF                                                           
024922       END-IF                                                             
024923                                                                          
024924       IF INDATA-FEL                                                      
024925         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024926         CALL WMEDKONV USING MED-WMEDAREA                                 
024927         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024928         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024929         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024930         MOVE MFS-ROER-EJ-FAELT TO MOD-FLANNULL                           
024931       END-IF                                                             
024932     END-IF                                                               
024933     .                                                                    
024934     EJECT                                                                
024935 H-UPPDATERA SECTION.                                                     
024936                                                                          
024937     PERFORM IMS-GET-XXDT-KUL01                                           
024938                                                                          
024939     IF MID-FLANNULL = 'Y'                                                
024940       IF SEGMENT-FINNS                                                   
024941         PERFORM IMS-DLET-XXDT-KUL01                                      
024942         PERFORM S01-INFO-UPDATE                                          
024943       END-IF                                                             
024944     ELSE                                                                 
024945       IF SEGMENT-FINNS                                                   
024946         IF MID-INPUT NOT = ALL '+'                                       
024947           IF KDCMD-FINNS AND IFYLLT-OK                                   
024948             PERFORM HB-REPLACE-BARN                                      
024949             PERFORM S01-INFO-UPDATE                                      
024950           ELSE                                                           
024951             MOVE ERR-INVALID-COMBINATION TO MED-IDMFSFEL                 
024952             CALL WMEDKONV USING MED-WMEDAREA                             
024953             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
024954             PERFORM MFS-ROER-EJ-FAELT-IN                                 
024955             PERFORM MFS-LAES-IN-IGEN                                     
024956           END-IF                                                         
024957         END-IF                                                           
024958       END-IF                                                             
024959                                                                          
024960*************************************************************             
024961* IFALL ARTIKELN FINNS PÅ ART.REG. WDK6 OCH SAKNAS PÅ KUL-  *             
024962* LAGERBASEN WLXXDT-4747, SÅ ÄR DET OK MED INSERT.          *             
024963*************************************************************             
024964                                                                          
024965       IF SEGMENT-SAKNAS                                                  
024966         IF MID-INPUT NOT = ALL '+'                                       
024967           IF INSERT-OK AND KDCMD-SAKNAS                                  
024970             PERFORM HA-INSERT-ROT-BARN                                   
024971             PERFORM S01-INFO-UPDATE                                      
024980           ELSE                                                           
024981             MOVE ERR-INVALID-COMBINATION TO MED-IDMFSFEL                 
024982             CALL WMEDKONV USING MED-WMEDAREA                             
024983             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
024984             PERFORM MFS-ROER-EJ-FAELT-IN                                 
024985             PERFORM MFS-LAES-IN-IGEN                                     
024986           END-IF                                                         
024987         END-IF                                                           
024988       END-IF                                                             
024989     END-IF                                                               
024990     .                                                                    
024991     EJECT                                                                
024992 HA-INSERT-ROT-BARN SECTION.                                              
024993                                                                          
024994     MOVE '4747'             TO XXDT-4747-IDHTYP                          
024995     MOVE MSGI-IDARTNR       TO XXDT-4747-IDARTNR                         
024996     MOVE LOW-VALUE          TO XXDT-4747-LOWVALUE                        
024997                                                                          
024998     PERFORM IMS-ISRT-XXDT-KUL01                                          
024999                                                                          
025000     MOVE SPACE              TO XXDT-KUL-WDGX4747                         
025001     MOVE '1'                TO XXDT-KUL-KDSEGKEY                         
025002                                                                          
025003     MOVE MID-BEARTKUL-1     TO XXDT-KUL-BEARTKUL-1                       
025004     IF MID-BEARTKUL-2 NOT = ALL '+'                                      
025005       MOVE MID-BEARTKUL-2   TO XXDT-KUL-BEARTKUL-2                       
025006     ELSE                                                                 
025007       MOVE SPACE            TO XXDT-KUL-BEARTKUL-2                       
025008     END-IF                                                               
025009                                                                          
025010     MOVE MID-BELEVKUL-1     TO XXDT-KUL-BELEVKUL-1                       
025011     IF MID-BELEVKUL-2 NOT = ALL '+'                                      
025012       MOVE MID-BELEVKUL-2   TO XXDT-KUL-BELEVKUL-2                       
025013     ELSE                                                                 
025014       MOVE SPACE            TO XXDT-KUL-BELEVKUL-2                       
025015     END-IF                                                               
025016                                                                          
025017     MOVE WS-DIKULLAG-INNER  TO XXDT-KUL-DIKULLAG-INNER                   
025018     MOVE WS-DIKULLAG-YTTER  TO XXDT-KUL-DIKULLAG-YTTER                   
025019                                                                          
025020     MOVE MID-IDLEVKUL-1     TO XXDT-KUL-IDLEVKUL-1                       
025021     IF MID-IDLEVKUL-2 NOT = ALL '+'                                      
025022       MOVE MID-IDLEVKUL-2   TO XXDT-KUL-IDLEVKUL-2                       
025023     ELSE                                                                 
025024       MOVE SPACE            TO XXDT-KUL-IDLEVKUL-2                       
025025     END-IF                                                               
025026                                                                          
025027     MOVE MID-IDSTAKUL       TO XXDT-KUL-IDSTAKUL                         
025028     MOVE WS-VKARTNTO        TO XXDT-KUL-VKARTNTO                         
025029     MOVE MID-KDARTURS       TO XXDT-KUL-KDARTURS                         
025030                                                                          
025031     PERFORM IMS-ISRT-XXDT-KUL11                                          
025032     .                                                                    
025033     EJECT                                                                
025034 HB-REPLACE-BARN SECTION.                                                 
025035                                                                          
025036     PERFORM IMS-GET-XXDT-KUL11                                           
025037     IF SEGMENT-FINNS                                                     
025038                                                                          
025039       IF MID-KDCMD-BEARTKUL = 'R' AND                                    
025040          MID-BEARTKUL NOT = ALL '+'                                      
025041                                                                          
025042          IF MID-BEARTKUL-1 NOT = ALL '+'                                 
025043            MOVE MID-BEARTKUL-1 TO XXDT-KUL-BEARTKUL-1                    
025044          END-IF                                                          
025045          IF MID-BEARTKUL-2 NOT = ALL '+'                                 
025046            MOVE MID-BEARTKUL-2 TO XXDT-KUL-BEARTKUL-2                    
025047          END-IF                                                          
025048       ELSE                                                               
025049         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-BEARTKUL                     
025050         MOVE MFS-ROER-EJ-FAELT TO MOD-BEARTKUL-1-IN                      
025051         MOVE MFS-ROER-EJ-FAELT TO MOD-BEARTKUL-2-IN                      
025052       END-IF                                                             
025053                                                                          
025054       IF MID-KDCMD-BELEVKUL = 'R' AND                                    
025055          MID-BELEVKUL NOT = ALL '+'                                      
025056                                                                          
025057          IF MID-BELEVKUL-1 NOT = ALL '+'                                 
025058            MOVE MID-BELEVKUL-1 TO XXDT-KUL-BELEVKUL-1                    
025059          END-IF                                                          
025060          IF MID-BELEVKUL-2 NOT = ALL '+'                                 
025061            MOVE MID-BELEVKUL-2 TO XXDT-KUL-BELEVKUL-2                    
025062          END-IF                                                          
025063       ELSE                                                               
025064         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-BELEVKUL                     
025065         MOVE MFS-ROER-EJ-FAELT TO MOD-BELEVKUL-1-IN                      
025066         MOVE MFS-ROER-EJ-FAELT TO MOD-BELEVKUL-2-IN                      
025067       END-IF                                                             
025068                                                                          
025069       IF MID-KDCMD-DIKULLAG-INNER = 'R' AND                              
025070         MID-DIKULLAG-INNER NOT = ALL '+'                                 
025071         MOVE WS-DIKULLAG-INNER TO XXDT-KUL-DIKULLAG-INNER                
025072       ELSE                                                               
025073         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-DIKULLAG-INNER               
025074         MOVE MFS-ROER-EJ-FAELT TO MOD-DIKULLAG-INNER-IN                  
025075       END-IF                                                             
025076                                                                          
025077       IF MID-KDCMD-DIKULLAG-YTTER = 'R' AND                              
025078         MID-DIKULLAG-YTTER NOT = ALL '+'                                 
025079         MOVE WS-DIKULLAG-YTTER TO XXDT-KUL-DIKULLAG-YTTER                
025080       ELSE                                                               
025081         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-DIKULLAG-YTTER               
025082         MOVE MFS-ROER-EJ-FAELT TO MOD-DIKULLAG-YTTER-IN                  
025083       END-IF                                                             
025084                                                                          
025085       IF MID-KDCMD-IDLEVKUL = 'R' AND                                    
025086          MID-IDLEVKUL NOT = ALL '+'                                      
025087                                                                          
025088          IF MID-IDLEVKUL-1 NOT = ALL '+'                                 
025089            MOVE MID-IDLEVKUL-1 TO XXDT-KUL-IDLEVKUL-1                    
025090          END-IF                                                          
025091          IF MID-IDLEVKUL-2 NOT = ALL '+'                                 
025092            MOVE MID-IDLEVKUL-2 TO XXDT-KUL-IDLEVKUL-2                    
025093          END-IF                                                          
025094       ELSE                                                               
025095         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IDLEVKUL                     
025096         MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVKUL-1-IN                      
025097         MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVKUL-2-IN                      
025098       END-IF                                                             
025099                                                                          
025100       IF MID-KDCMD-IDSTAKUL = 'R' AND                                    
025101         MID-IDSTAKUL NOT = ALL '+'                                       
025102         MOVE MID-IDSTAKUL TO XXDT-KUL-IDSTAKUL                           
025103       ELSE                                                               
025104         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-IDSTAKUL                     
025105         MOVE MFS-ROER-EJ-FAELT TO MOD-IDSTAKUL-IN                        
025106       END-IF                                                             
025107                                                                          
025108       IF MID-KDCMD-VKARTNTO = 'R' AND                                    
025109         MID-VKARTNTO NOT = ALL '+'                                       
025110         MOVE WS-VKARTNTO TO XXDT-KUL-VKARTNTO                            
025111       ELSE                                                               
025112         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-VKARTNTO                     
025113         MOVE MFS-ROER-EJ-FAELT TO MOD-VKARTNTO-IN                        
025114       END-IF                                                             
025115                                                                          
025116       IF MID-KDCMD-KDARTURS = 'R' AND                                    
025117         MID-KDARTURS NOT = ALL '+'                                       
025118         MOVE MID-KDARTURS TO XXDT-KUL-KDARTURS                           
025119       ELSE                                                               
025120         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-KDARTURS                     
025121         MOVE MFS-ROER-EJ-FAELT TO MOD-KDARTURS-IN                        
025122       END-IF                                                             
025123                                                                          
025124       PERFORM IMS-REPL-XXDT-KUL11                                        
025125     END-IF                                                               
025126     .                                                                    
025127     EJECT                                                                
025128 S01-INFO-UPDATE SECTION.                                                 
025129                                                                          
025130     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
025131     CALL WMEDKONV USING MED-WMEDAREA                                     
025132     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
025133     PERFORM MFS-FORM-ATTR                                                
025134     PERFORM MFS-RENSA-FAELT-IN                                           
025135     MOVE MFS-RENSA-FAELT TO MOD-FLANNULL                                 
025136                                                                          
025137     .                                                                    
025138     EJECT                                                                
025139 MFS-RENSA-FAELT-UT SECTION.                                              
025140                                                                          
025150*    --- ALLA UTDATA-FÄLT                                                 
025200     MOVE MFS-RENSA-FAELT TO MOD-BEARTKUL-1                               
025300                             MOD-BEARTKUL-2                               
025310                             MOD-BELEVKUL-1                               
025320                             MOD-BELEVKUL-2                               
025330                             MOD-DIKULLAG-INNER                           
025331                             MOD-DIKULLAG-YTTER                           
025350                             MOD-IDLEVKUL-1                               
025360                             MOD-IDLEVKUL-2                               
025370                             MOD-IDSTAKUL                                 
025380                             MOD-VKARTNTO                                 
025390                             MOD-KDARTURS                                 
025400     .                                                                    
025600     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-BEARTKUL                           
026100                             MOD-BEARTKUL-1-IN                            
026110                             MOD-BEARTKUL-2-IN                            
026120                             MOD-KDCMD-BELEVKUL                           
026130                             MOD-BELEVKUL-1-IN                            
026140                             MOD-BELEVKUL-2-IN                            
026150                             MOD-KDCMD-DIKULLAG-INNER                     
026160                             MOD-DIKULLAG-INNER-IN                        
026161                             MOD-KDCMD-DIKULLAG-YTTER                     
026170                             MOD-DIKULLAG-YTTER-IN                        
026180                             MOD-KDCMD-IDLEVKUL                           
026190                             MOD-IDLEVKUL-1-IN                            
026191                             MOD-IDLEVKUL-2-IN                            
026192                             MOD-KDCMD-IDSTAKUL                           
026193                             MOD-IDSTAKUL-IN                              
026194                             MOD-KDCMD-VKARTNTO                           
026195                             MOD-VKARTNTO-IN                              
026196                             MOD-KDCMD-KDARTURS                           
026197                             MOD-KDARTURS-IN                              
026200     .                                                                    
026300     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500                                                                          
026600*    --- ALLA UTDATA-FÄLT                                                 
026800     MOVE MFS-ROER-EJ-FAELT TO MOD-BEARTKUL-1                             
027000                               MOD-BEARTKUL-2                             
027010                               MOD-BELEVKUL-1                             
027020                               MOD-BELEVKUL-2                             
027030                               MOD-DIKULLAG-INNER                         
027040                               MOD-DIKULLAG-YTTER                         
027050                               MOD-IDLEVKUL-1                             
027060                               MOD-IDLEVKUL-2                             
027070                               MOD-IDSTAKUL                               
027080                               MOD-VKARTNTO                               
027090                               MOD-KDARTURS                               
027100     .                                                                    
027200     SKIP3                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400                                                                          
027500*    --- ALLA INDATA-FÄLT                                                 
027600     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-BEARTKUL                         
027720                               MOD-BEARTKUL-1-IN                          
027730                               MOD-BEARTKUL-2-IN                          
027740                               MOD-KDCMD-BELEVKUL                         
027750                               MOD-BELEVKUL-1-IN                          
027760                               MOD-BELEVKUL-2-IN                          
027770                               MOD-KDCMD-DIKULLAG-INNER                   
027780                               MOD-DIKULLAG-INNER-IN                      
027790                               MOD-KDCMD-DIKULLAG-YTTER                   
027791                               MOD-DIKULLAG-YTTER-IN                      
027792                               MOD-KDCMD-IDLEVKUL                         
027793                               MOD-IDLEVKUL-1-IN                          
027794                               MOD-IDLEVKUL-2-IN                          
027795                               MOD-KDCMD-IDSTAKUL                         
027796                               MOD-IDSTAKUL-IN                            
027797                               MOD-KDCMD-VKARTNTO                         
027798                               MOD-VKARTNTO-IN                            
027799                               MOD-KDCMD-KDARTURS                         
027800                               MOD-KDARTURS-IN                            
027810     .                                                                    
027900     EJECT                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALLA INDATA-FÄLT                                                 
028300     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-BEARTKUL-ATTR                   
028400                                MOD-BEARTKUL-1-IN-ATTR                    
028410                                MOD-BEARTKUL-2-IN-ATTR                    
028420                                MOD-KDCMD-BELEVKUL-ATTR                   
028430                                MOD-BELEVKUL-1-IN-ATTR                    
028431                                MOD-BELEVKUL-2-IN-ATTR                    
028441                                MOD-KDCMD-DIKULLAG-INNER-ATTR             
028442                                MOD-DIKULLAG-INNER-IN-ATTR                
028443                                MOD-KDCMD-DIKULLAG-YTTER-ATTR             
028444                                MOD-DIKULLAG-YTTER-IN-ATTR                
028445                                MOD-KDCMD-IDLEVKUL-ATTR                   
028446                                MOD-IDLEVKUL-1-IN-ATTR                    
028448                                MOD-IDLEVKUL-2-IN-ATTR                    
028449                                MOD-KDCMD-IDSTAKUL-ATTR                   
028450                                MOD-IDSTAKUL-IN-ATTR                      
028460                                MOD-KDCMD-VKARTNTO-ATTR                   
028470                                MOD-VKARTNTO-IN-ATTR                      
028480                                MOD-KDCMD-KDARTURS-ATTR                   
028490                                MOD-KDARTURS-IN-ATTR                      
028500     .                                                                    
028600     SKIP2                                                                
028700 MFS-LAES-IN-IGEN SECTION.                                                
028800                                                                          
028900*    --- ALLA INDATA-FÄLT                                                 
029000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-BEARTKUL-ATTR                
029120                                   MOD-BEARTKUL-1-IN-ATTR                 
029130                                   MOD-BEARTKUL-2-IN-ATTR                 
029140                                   MOD-KDCMD-BELEVKUL-ATTR                
029150                                   MOD-BELEVKUL-1-IN-ATTR                 
029160                                   MOD-BELEVKUL-2-IN-ATTR                 
029170                                   MOD-KDCMD-DIKULLAG-INNER-ATTR          
029180                                   MOD-DIKULLAG-INNER-IN-ATTR             
029190                                   MOD-KDCMD-DIKULLAG-YTTER-ATTR          
029191                                   MOD-DIKULLAG-YTTER-IN-ATTR             
029192                                   MOD-KDCMD-IDLEVKUL-ATTR                
029193                                   MOD-IDLEVKUL-1-IN-ATTR                 
029194                                   MOD-IDLEVKUL-2-IN-ATTR                 
029195                                   MOD-KDCMD-IDSTAKUL-ATTR                
029196                                   MOD-IDSTAKUL-IN-ATTR                   
029197                                   MOD-KDCMD-VKARTNTO-ATTR                
029198                                   MOD-VKARTNTO-IN-ATTR                   
029199                                   MOD-KDCMD-KDARTURS-ATTR                
029200                                   MOD-KDARTURS-IN-ATTR                   
029210     .                                                                    
029300     EJECT                                                                
029400 MFS-STAENG-FAELT-IN SECTION.                                             
029401                                                                          
029402*    --- ALLA INDATA-FÄLT                                                 
029403     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDCMD-BEARTKUL-ATTR               
029404                                    MOD-BEARTKUL-1-IN-ATTR                
029405                                    MOD-BEARTKUL-2-IN-ATTR                
029406                                    MOD-KDCMD-BELEVKUL-ATTR               
029407                                    MOD-BELEVKUL-1-IN-ATTR                
029408                                    MOD-BELEVKUL-2-IN-ATTR                
029409                                    MOD-KDCMD-DIKULLAG-INNER-ATTR         
029410                                    MOD-DIKULLAG-INNER-IN-ATTR            
029411                                    MOD-KDCMD-DIKULLAG-YTTER-ATTR         
029412                                    MOD-DIKULLAG-YTTER-IN-ATTR            
029413                                    MOD-KDCMD-IDLEVKUL-ATTR               
029414                                    MOD-IDLEVKUL-1-IN-ATTR                
029415                                    MOD-IDLEVKUL-2-IN-ATTR                
029416                                    MOD-KDCMD-IDSTAKUL-ATTR               
029417                                    MOD-IDSTAKUL-IN-ATTR                  
029418                                    MOD-KDCMD-VKARTNTO-ATTR               
029419                                    MOD-VKARTNTO-IN-ATTR                  
029420                                    MOD-KDCMD-KDARTURS-ATTR               
029421                                    MOD-KDARTURS-IN-ATTR                  
029422                                    MOD-FLANNULL-ATTR                     
029423     .                                                                    
029424     EJECT                                                                
029425 MFS-STAENG-KDCMD SECTION.                                                
029426                                                                          
029427*    --- ALLA INDATA-FÄLT FÖR REPLACE ELLER ANNULLATION                   
029428     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDCMD-BEARTKUL-ATTR               
029429                                    MOD-KDCMD-BELEVKUL-ATTR               
029430                                    MOD-KDCMD-DIKULLAG-INNER-ATTR         
029431                                    MOD-KDCMD-DIKULLAG-YTTER-ATTR         
029432                                    MOD-KDCMD-IDLEVKUL-ATTR               
029433                                    MOD-KDCMD-IDSTAKUL-ATTR               
029434                                    MOD-KDCMD-VKARTNTO-ATTR               
029435                                    MOD-KDCMD-KDARTURS-ATTR               
029436                                    MOD-FLANNULL-ATTR                     
029437     .                                                                    
029438     EJECT                                                                
029440* --- IMS SEKTIONER ---                                                   
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
031502 IMS-GET-WLARTC01 SECTION.                                                
031503                                                                          
031504     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
031505          DELIMITED BY SIZE INTO SSA1                                     
031507     MOVE '  GE' TO GODK-STATUSKODER                                      
031508     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-WLARTC01 SSA1                 
031509     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031510     PERFORM IMS-STATUSKONTROLL                                           
031511     .                                                                    
031512     EJECT                                                                
031513 IMS-GET-XXDT-KUL01 SECTION.                                              
031514                                                                          
031515     STRING 'WLXXDT01(WDGXKEY  =' W-WDGX01KEY-X ')'                       
031516          DELIMITED BY SIZE INTO SSA1                                     
031517     MOVE '  GE' TO GODK-STATUSKODER                                      
031518     CALL CBLTDLI USING GHU XXDT-PCB DLI-IO-WLXXDT01 SSA1                 
031519     MOVE XXDT-STATUS-CODE TO STATUS-WS                                   
031520     PERFORM IMS-STATUSKONTROLL                                           
031521     .                                                                    
031522     SKIP3                                                                
031523 IMS-ISRT-XXDT-KUL01 SECTION.                                             
031524                                                                          
031525     MOVE 'WLXXDT01 ' TO SSA1                                             
031526     MOVE '  II' TO GODK-STATUSKODER                                      
031527     CALL CBLTDLI USING ISRT XXDT-PCB DLI-IO-WLXXDT01 SSA1                
031528     MOVE XXDT-STATUS-CODE TO STATUS-WS                                   
031529     PERFORM IMS-STATUSKONTROLL                                           
031530     .                                                                    
031531     SKIP3                                                                
031532 IMS-DLET-XXDT-KUL01 SECTION.                                             
031533                                                                          
031534     MOVE '  ' TO GODK-STATUSKODER                                        
031535     CALL CBLTDLI USING DLET XXDT-PCB DLI-IO-WLXXDT01                     
031536     MOVE XXDT-STATUS-CODE TO STATUS-WS                                   
031537     PERFORM IMS-STATUSKONTROLL                                           
031538     .                                                                    
031539     EJECT                                                                
031540 IMS-GET-XXDT-KUL11 SECTION.                                              
031541                                                                          
031542     STRING 'WLXXDT11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
031543          DELIMITED BY SIZE INTO SSA1                                     
031544     MOVE '  GE' TO GODK-STATUSKODER                                      
031545     CALL CBLTDLI USING GHNP XXDT-PCB DLI-IO-WLXXDT11 SSA1                
031546     MOVE XXDT-STATUS-CODE TO STATUS-WS                                   
031547     PERFORM IMS-STATUSKONTROLL                                           
031548     .                                                                    
031549     SKIP3                                                                
031550 IMS-ISRT-XXDT-KUL11 SECTION.                                             
031551                                                                          
031552     STRING 'WLXXDT01(WDGXKEY  =' W-WDGX01KEY-X ')'                       
031553          DELIMITED BY SIZE INTO SSA1                                     
031554     MOVE 'WLXXDT11 ' TO SSA2                                             
031555     MOVE '  II' TO GODK-STATUSKODER                                      
031556     CALL CBLTDLI USING ISRT XXDT-PCB DLI-IO-WLXXDT11 SSA1 SSA2           
031557     MOVE XXDT-STATUS-CODE TO STATUS-WS                                   
031558     PERFORM IMS-STATUSKONTROLL                                           
031559     .                                                                    
031560     SKIP3                                                                
031561 IMS-REPL-XXDT-KUL11 SECTION.                                             
031562                                                                          
031563     MOVE '  ' TO GODK-STATUSKODER                                        
031564     CALL CBLTDLI USING REPL XXDT-PCB DLI-IO-WLXXDT11                     
031565     MOVE XXDT-STATUS-CODE TO STATUS-WS                                   
031566     PERFORM IMS-STATUSKONTROLL                                           
031570     .                                                                    
031600     EJECT                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
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
