001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W6018400.                                                
001500 AUTHOR.         BODIL LINDAHL.                                           
001600 DATE-WRITTEN.   99/09/23.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        KOPPLAR ARTIKELNR TILL FÖRPACKNINGSINSTRUKTION.                  
002100*                                                                         
002201*        PROGRAMMET UPPDATERAR WDD1                                       
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W6T184                                              
002600*        MID:         W6I18401                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W6O18401                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6018400'.            
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004210 77  ARTNR-SOEK                  PIC X       VALUE SPACE.                 
004220 77  ARTNR-LEVNR-SOEK            PIC X       VALUE SPACE.                 
004221 77  LEVNR-SOEK                  PIC X       VALUE SPACE.                 
004222 77  REGNR-SOEK                  PIC X       VALUE SPACE.                 
004230 77  TRAEFF-SW                   PIC X       VALUE SPACE.                 
004402 77  RAD-IX                      PIC S9(3)   VALUE +0    COMP-3.          
004420 77  RAD-IX-MAX                  PIC S9(3)   VALUE +10   COMP-3.          
004440 77  SPAR-IDARTNR-DOLT           PIC S9(9)   VALUE ZERO.                  
004450 77  WS-IDFPINST                 PIC S9(9)   VALUE ZERO.                  
004460 77  WS-KOLL-IDLEVNR             PIC X       VALUE SPACE.                 
004470 77  WS-ART-FINNS                PIC X       VALUE SPACE.                 
004471                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004802     88  INDATA-OK                           VALUE 'J'.                   
004810     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '6184'.                
005600     88  GODK-MID                            VALUE '6181' '6182'          
005700                                                   '6183' '6184'          
005800                                                   '6185'.                
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
007300     EJECT                                                                
007400 01  MESSAGE-CODES.                                                       
007500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007501     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007502     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
007503     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007504     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007505     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
007506     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
007507     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007508     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007509     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007510 01  MESSAGE-TEXT1.                                                       
007520     03  INF-ARTIKEL-UNIK-TEXT                                            
007530                                 PIC X(20)   VALUE                        
007540         'ARTIKEL UNIK        '.                                          
007541*                                                                         
007550     03  INF-REGNR-UNIK-TEXT                                              
007560                                 PIC X(20)   VALUE                        
007570         'REGNR UNIK          '.                                          
007900*                                                                         
007901     03  INF-ARTIKEL-FINNS-TEXT                                           
007902                                 PIC X(30)   VALUE                        
007903         'ARTIKEL REDAN REGISTRERAD'.                                     
007904     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008503*                                                                         
008504 01  WS-SPAR-IDLEVNR               PIC X(5)  VALUE SPACE.                 
008505*                                                                         
008506 01  SPAR-AREA.                                                           
008507     03  SPAR-IDTRANS              PIC X(4)  VALUE '6184'.                
008508     03  SPAR-IDFPINST-ENTER       PIC 9(7)  VALUE ZERO.                  
008510     03  SPAR-IDFPINST-NEXT        PIC 9(7)  VALUE ZERO.                  
008520     03  SPAR-IDARTNR-ENTER        PIC 9(9)  VALUE ZERO.                  
008530     03  SPAR-IDARTNR-NEXT         PIC 9(9)  VALUE ZERO.                  
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W6I18401                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W6O18401                                                 
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
011005     03  W-IDARTNR-X.                                                     
011006         05  W-IDARTNR            PIC S9(9)  VALUE ZERO COMP-3.           
011007     03  W-IDARTNR-UPD-X.                                                 
011008         05  W-IDARTNR-UPD        PIC S9(9)  VALUE ZERO COMP-3.           
011009     03  W-IDLEVNR-X.                                                     
011010         05  W-IDLEVNR            PIC  X(5)  VALUE SPACE.                 
011011     03  W-IDFPINST-UPD-X.                                                
011012         05  W-IDFPINST-UPD       PIC S9(7)  VALUE ZERO COMP-3.           
011013     03  W-IDFPINST-X.                                                    
011014         05  W-IDFPINST           PIC S9(7)  VALUE ZERO COMP-3.           
011015     03  W-WDD1A1KY-MIN.                                                  
011016         05  W-SEQA-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.           
011020         05  W-SEQA-IDFPINST-MIN  PIC S9(7)  VALUE ZERO COMP-3.           
011030     03  W-WDD1A1KY-MAX.                                                  
011040         05  W-SEQA-IDARTNR-MAX   PIC S9(9)  VALUE ZERO COMP-3.           
011050         05  W-SEQA-IDFPINST-MAX  PIC S9(7)  VALUE ZERO COMP-3.           
011060     03  W-WDD1B1KY-MIN.                                                  
011070         05  W-SEQB-IDLEVNR-MIN   PIC  X(5)  VALUE SPACE.                 
011080         05  W-SEQB-IDFPINST-MIN  PIC S9(7)  VALUE ZERO COMP-3.           
011090     03  W-WDD1B1KY-MAX.                                                  
011091         05  W-SEQB-IDLEVNR-MAX   PIC  X(5)  VALUE SPACE.                 
011092         05  W-SEQB-IDFPINST-MAX  PIC S9(7)  VALUE ZERO COMP-3.           
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     EJECT                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD1A'.                       
013002 01  DLI-IO-WDD1A.                                                        
013003*    03  -COPY WDD1A1                                                     
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD1B'.                       
013006 01  DLI-IO-WDD1B.                                                        
013007*    03  -COPY WDD1B1                                                     
013008     EJECT                                                                
013009 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013010 01  DLI-IO-WDK601.                                                       
013011*    03  -COPY WDK601                                                     
013012     EJECT                                                                
013013 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD101'.                      
013014 01  DLI-IO-WDD101.                                                       
013015*    03  -COPY WDD101                                                     
013016     EJECT                                                                
013017 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD111'.                      
013018 01  DLI-IO-WDD111.                                                       
013020*    03  -COPY WDD111                                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013510     EJECT                                                                
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013802*01  -COPY W0008   -PRE WDD1A-                                            
013803     05  FILLER                  PIC X.                                   
013804     EJECT                                                                
013805*01  -COPY W0008   -PRE WDK6-                                             
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
013910*01  -COPY W0008   -PRE WDD1-                                             
013920     05  FILLER                  PIC X.                                   
013930     EJECT                                                                
014000*01  -COPY W0008   -PRE WDD1B-                                            
014001     05  FILLER                  PIC X.                                   
014002     EJECT                                                                
014003 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDD1A-PCB WDD1-PCB            
014004                           WDD1B-PCB WDK6-PCB.                            
014005 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDD1A-PCB WDD1-PCB            
014020                           WDD1B-PCB WDK6-PCB.                            
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
014901           IF MFS-FIRST                                                   
014902             PERFORM C-FOERSTA-SIDA                                       
014903           ELSE                                                           
014904             IF MFS-NEXT                                                  
014905               PERFORM D-NAESTA-SIDA                                      
014906             ELSE                                                         
014907               PERFORM E-SAMMA-SIDA                                       
014908             END-IF                                                       
014910           END-IF                                                         
015110         END-IF                                                           
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O18401 + 4                      
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
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I18401                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I18401                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W6O184N1' TO MFS-IDMOD                                         
018300     MOVE '6184' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018510     IF MSGI-IDLAND-SPR = 'SE'                                            
018520        MOVE '0' TO MFS-KDHUVOMR                                          
018530     END-IF                                                               
018540                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019110                                                                          
019200     MOVE LOW-VALUE  TO W-WDD1A1KY-MIN                                    
019210                        W-WDD1B1KY-MIN                                    
019300     MOVE HIGH-VALUE TO W-WDD1A1KY-MAX                                    
019310                        W-WDD1B1KY-MAX                                    
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '6184'            TO MSGI-IDTRANS                               
020300     IF EGEN-MID                                                          
020410         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
020420         MOVE MID-IDLEVNR-IN     TO MSGI-IDLEVNR                          
020430         MOVE MID-IDFPINST-IN    TO MSGI-IDFPINST                         
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020610     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
020700                                                                          
020710     MOVE ZERO TO SPAR-IDARTNR-DOLT                                       
020800     MOVE JA TO NYCKLAR-SW                                                
020900     MOVE NEJ TO ARTNR-SOEK                                               
021000                 ARTNR-LEVNR-SOEK                                         
021001                 LEVNR-SOEK                                               
021002                 REGNR-SOEK                                               
021003                                                                          
021004*    -- KONTROLL AV IDARTNR                                               
021005     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021006     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021007       MOVE '7'         TO MFS-IDPFK                                      
021008       MOVE SPACE       TO MFS-KDTRTYP                                    
021009     END-IF                                                               
021010     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021011     IF MSGI-IDARTNR NUMERIC                                              
021012       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
021013     ELSE                                                                 
021014       MOVE NEJ TO NYCKLAR-SW                                             
021020     END-IF                                                               
021101                                                                          
021102*    -- KONTROLL AV IDLEVNR                                               
021103     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
021104     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
021105       MOVE '7'         TO MFS-IDPFK                                      
021106       MOVE SPACE       TO MFS-KDTRTYP                                    
021107     END-IF                                                               
021110     MOVE MSGI-IDLEVNR TO W-IDLEVNR                                       
021114                                                                          
021115*    -- KONTROLL AV IDFPINST                                              
021116     MOVE MFS-RENSA-FAELT TO MOD-IDFPINST-IN                              
021117     IF MID-IDFPINST-IN NOT = ALL '+'                                     
021118       MOVE '7'         TO MFS-IDPFK                                      
021119       MOVE SPACE       TO MFS-KDTRTYP                                    
021120     END-IF                                                               
021121     INSPECT MSGI-IDFPINST REPLACING LEADING SPACE BY ZERO                
021122     IF MSGI-IDFPINST NUMERIC                                             
021123       MOVE MSGI-IDFPINST TO W-IDFPINST                                   
021124                             WS-IDFPINST                                  
021125     ELSE                                                                 
021126       MOVE NEJ TO NYCKLAR-SW                                             
021127     END-IF                                                               
021128                                                                          
021129     IF NYCKLAR-OK                                                        
021130        IF W-IDARTNR = ZERO                                               
021131           IF W-IDLEVNR = SPACE                                           
021132              IF W-IDFPINST = ZERO                                        
021133                 MOVE NEJ TO NYCKLAR-SW                                   
021134              ELSE                                                        
021135                 MOVE JA TO REGNR-SOEK                                    
021136              END-IF                                                      
021137           ELSE                                                           
021138              IF W-IDFPINST = ZERO                                        
021139                 MOVE JA TO LEVNR-SOEK                                    
021140              ELSE                                                        
021141                 MOVE NEJ TO NYCKLAR-SW                                   
021142              END-IF                                                      
021143           END-IF                                                         
021144        ELSE                                                              
021145           IF W-IDFPINST = ZERO                                           
021146              IF W-IDLEVNR = SPACE                                        
021147                 MOVE JA TO ARTNR-SOEK                                    
021148              ELSE                                                        
021149                 MOVE JA TO ARTNR-LEVNR-SOEK                              
021150              END-IF                                                      
021151           ELSE                                                           
021152              MOVE NEJ TO NYCKLAR-SW                                      
021153           END-IF                                                         
021154        END-IF                                                            
021155     END-IF                                                               
021156                                                                          
021166     IF EGEN-MID OR NYCKLAR-OK                                            
021167       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
021168       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021169       MOVE MSGI-IDLEVNR    TO MOD-IDLEVNR-UT                             
021171       MOVE MSGI-IDFPINST   TO MOD-IDFPINST-UT                            
021172       INSPECT MOD-IDFPINST-UT REPLACING LEADING ZERO BY SPACE            
021173     ELSE                                                                 
021174       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
021175                               MOD-IDLEVNR-UT                             
021176                               MOD-IDFPINST-UT                            
021180     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700       PERFORM MFS-RENSA-FAELT-IN                                         
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022108     PERFORM MFS-RENSA-FAELT-IN                                           
022109     .                                                                    
022110     EJECT                                                                
022111 D-NAESTA-SIDA SECTION.                                                   
022112                                                                          
022113     IF SPAR-IDTRANS = '6184'                                             
022114       MOVE SPAR-IDFPINST-NEXT TO W-SEQA-IDFPINST-MIN                     
022115                                  W-SEQB-IDFPINST-MIN                     
022116       MOVE SPAR-IDARTNR-NEXT  TO SPAR-IDARTNR-DOLT                       
022117     END-IF                                                               
022118     PERFORM MFS-RENSA-FAELT-IN                                           
022119     .                                                                    
022120     EJECT                                                                
022121 E-SAMMA-SIDA SECTION.                                                    
022122                                                                          
022123     IF SPAR-IDTRANS = '6184'                                             
022124       MOVE SPAR-IDFPINST-ENTER TO W-SEQA-IDFPINST-MIN                    
022125                                   W-SEQB-IDFPINST-MIN                    
022126       MOVE SPAR-IDARTNR-ENTER  TO SPAR-IDARTNR-DOLT                      
022127       IF MID-INDEL = ALL '+'                                             
022128         PERFORM MFS-RENSA-FAELT-IN                                       
022129       ELSE                                                               
022130         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022131         CALL WMEDKONV USING MED-WMEDAREA                                 
022132         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
022133         PERFORM EA-MID-INDATA-TILL-MOD                                   
022135       END-IF                                                             
022136     ELSE                                                                 
022137       PERFORM MFS-RENSA-FAELT-IN                                         
022138     END-IF                                                               
022139     .                                                                    
022140     EJECT                                                                
022141 EA-MID-INDATA-TILL-MOD SECTION.                                          
022142                                                                          
022143     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
022144        MOVE MID-IDARTNR-UPD       TO MOD-IDARTNR-UPD                     
022145        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR                    
022146        INSPECT MOD-IDARTNR-UPD REPLACING LEADING ZERO BY SPACE           
022147     ELSE                                                                 
022148        MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-UPD                     
022149     END-IF                                                               
022150                                                                          
022151     IF MID-IDFPINST-UPD NOT = ALL '+'                                    
022152        MOVE MID-IDFPINST-UPD      TO MOD-IDFPINST-UPD                    
022153        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFPINST-ATTR                   
022154        INSPECT MOD-IDFPINST-UPD REPLACING LEADING ZERO BY SPACE          
022155     ELSE                                                                 
022156        MOVE MFS-RENSA-FAELT       TO MOD-IDFPINST-UPD                    
022157     END-IF                                                               
022158                                                                          
022159     IF MID-FLBORT NOT = ALL '+'                                          
022160        MOVE MID-FLBORT            TO MOD-FLBORT                          
022161        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLBORT-ATTR                     
022162     ELSE                                                                 
022163        MOVE MFS-RENSA-FAELT       TO MOD-FLBORT-ATTR                     
022164     END-IF                                                               
022170     .                                                                    
022200     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022510     MOVE W-IDARTNR TO W-SEQA-IDARTNR-MIN                                 
022520                       W-SEQA-IDARTNR-MAX                                 
022521     MOVE W-IDLEVNR TO W-SEQB-IDLEVNR-MIN                                 
022522                       W-SEQB-IDLEVNR-MAX                                 
022523     MOVE NEJ TO TRAEFF-SW                                                
022530                                                                          
022600     PERFORM FA-LAES-GRUNDDATA                                            
022700                                                                          
022800     IF SEGMENT-SAKNAS                                                    
022810        MOVE URVAL-SAKNAS TO MED-IDMFSFEL                                 
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023200        PERFORM MFS-RENSA-FAELT-UT                                        
023300     ELSE                                                                 
023401        MOVE FPI-IDFPINST   TO SPAR-IDFPINST-ENTER                        
023402                               SPAR-IDFPINST-NEXT                         
023403        MOVE ZERO           TO SPAR-IDARTNR-ENTER                         
023404                               SPAR-IDARTNR-NEXT                          
023405        IF REGNR-SOEK = JA                                                
023406           IF TRAEFF-SW = JA                                              
023407             MOVE FPA-IDARTNR TO SPAR-IDARTNR-ENTER                       
023408                                 SPAR-IDARTNR-NEXT                        
023409           ELSE                                                           
023410             MOVE ZERO        TO SPAR-IDARTNR-ENTER                       
023411                                 SPAR-IDARTNR-NEXT                        
023412           END-IF                                                         
023413        END-IF                                                            
023414                                                                          
023415        MOVE +1 TO RAD-IX                                                 
023416        PERFORM UNTIL RAD-IX > RAD-IX-MAX                                 
023417           IF SEGMENT-FINNS OR TRAEFF-SW = JA                             
023418              IF ARTNR-SOEK = JA                                          
023419              OR ARTNR-LEVNR-SOEK = JA                                    
023420                 MOVE W-SEQA-IDARTNR-MIN   TO MOD-IDARTNR (RAD-IX)        
023421              ELSE                                                        
023422                 IF LEVNR-SOEK = JA                                       
023424                    MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR(RAD-IX)         
023425                 ELSE                                                     
023426                    IF REGNR-SOEK = JA                                    
023427                       IF TRAEFF-SW = JA                                  
023429                          MOVE FPA-IDARTNR TO MOD-IDARTNR(RAD-IX)         
023430                       ELSE                                               
023431                          MOVE MFS-RENSA-FAELT                            
023432                                           TO MOD-IDARTNR(RAD-IX)         
023433                       END-IF                                             
023434                    END-IF                                                
023435                 END-IF                                                   
023436              END-IF                                                      
023437              MOVE FPI-IDFPINST          TO MOD-IDFPINST(RAD-IX)          
023438              MOVE FPI-IDLEVNR           TO MOD-IDLEVNR (RAD-IX)          
023439              PERFORM FB-LAES-RADDATA                                     
023440           ELSE                                                           
023441              MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR (RAD-IX)          
023442                                            MOD-IDFPINST(RAD-IX)          
023443                                            MOD-IDLEVNR (RAD-IX)          
023444           END-IF                                                         
023445           ADD 1 TO RAD-IX                                                
023446        END-PERFORM                                                       
023447                                                                          
023448        IF SEGMENT-FINNS OR TRAEFF-SW = JA                                
023449           IF ARTNR-SOEK = JA                                             
023450           OR ARTNR-LEVNR-SOEK = JA                                       
023451           OR LEVNR-SOEK = JA                                             
023452              MOVE FPI-IDFPINST TO SPAR-IDFPINST-NEXT                     
023453              MOVE ZERO         TO SPAR-IDARTNR-NEXT                      
023454              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
023455              CALL WMEDKONV USING MED-WMEDAREA                            
023456              MOVE MED-TEMFSINF TO MOD-TEMFSINF                           
023457           ELSE                                                           
023458              IF REGNR-SOEK = JA                                          
023459                 IF TRAEFF-SW = JA                                        
023460                    MOVE FPI-IDFPINST TO SPAR-IDFPINST-NEXT               
023461                    MOVE FPA-IDARTNR  TO SPAR-IDARTNR-NEXT                
023462                    MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF             
023463                    CALL WMEDKONV USING MED-WMEDAREA                      
023464                    MOVE MED-TEMFSINF TO MOD-TEMFSINF                     
023465                 ELSE                                                     
023466                    MOVE ZERO TO SPAR-IDFPINST-NEXT                       
023467                                 SPAR-IDARTNR-NEXT                        
023468                 END-IF                                                   
023469              END-IF                                                      
023470           END-IF                                                         
023471        ELSE                                                              
023472           MOVE ZERO TO SPAR-IDFPINST-NEXT                                
023473                        SPAR-IDARTNR-NEXT                                 
023474        END-IF                                                            
023475                                                                          
023476        MOVE '002'    TO MSGI-KDCALL                                      
023477        MOVE '6184'   TO SPAR-IDTRANS                                     
023478        MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                 
023480        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 FA-LAES-GRUNDDATA SECTION.                                               
023900                                                                          
023901******************************************************************        
023902*  VID SÖKNING PÅ LEVNR VISAS BARA REGNR O LEVNR                          
023903*  (INGA KOPPLADE ARTIKLAR)                                               
023904******************************************************************        
023905                                                                          
023910     IF ARTNR-SOEK = JA                                                   
024000        PERFORM IMS-GET-WDD1A-FIRST                                       
024010        IF SEGMENT-FINNS                                                  
024020           MOVE SEQA-IDFPINST TO W-IDFPINST                               
024030           PERFORM IMS-GET-WDD101                                         
024040        END-IF                                                            
024041     ELSE                                                                 
024048        IF ARTNR-LEVNR-SOEK = JA                                          
024049           MOVE NEJ TO TRAEFF-SW                                          
024050           PERFORM IMS-GET-WDD1A-FIRST                                    
024051           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
024052              MOVE SEQA-IDFPINST TO W-IDFPINST                            
024053              PERFORM IMS-GET-WDD101                                      
024054              IF FPI-IDLEVNR = W-IDLEVNR                                  
024055                 MOVE JA TO TRAEFF-SW                                     
024056              ELSE                                                        
024057                 PERFORM IMS-GET-WDD1A-NEXT                               
024058              END-IF                                                      
024059           END-PERFORM                                                    
024060        ELSE                                                              
024061           IF LEVNR-SOEK = JA                                             
024062              PERFORM IMS-GET-WDD1B-FIRST                                 
024063              IF SEGMENT-FINNS                                            
024064                 MOVE SEQB-IDFPINST TO W-IDFPINST                         
024065                 PERFORM IMS-GET-WDD101                                   
024066              END-IF                                                      
024067           ELSE                                                           
024068              MOVE NEJ TO TRAEFF-SW                                       
024069              IF REGNR-SOEK = JA                                          
024070                 PERFORM IMS-GET-WDD101                                   
024071                 IF SEGMENT-FINNS                                         
024072                    PERFORM IMS-GNP-WDD111                                
024073                    PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA        
024074                       IF FPA-IDARTNR >= SPAR-IDARTNR-DOLT                
024075                          MOVE JA TO TRAEFF-SW                            
024076                       ELSE                                               
024077                          PERFORM IMS-GNP-WDD111                          
024078                       END-IF                                             
024079                    END-PERFORM                                           
024080                    IF TRAEFF-SW = NEJ                                    
024081                       MOVE SPACE TO STATUS-WS                            
024082                    END-IF                                                
024083                 END-IF                                                   
024084              END-IF                                                      
024085           END-IF                                                         
024086        END-IF                                                            
024087     END-IF                                                               
024088     .                                                                    
024089     EJECT                                                                
024090 FB-LAES-RADDATA SECTION.                                                 
024095                                                                          
024096     IF ARTNR-SOEK = JA                                                   
024097        PERFORM IMS-GET-WDD1A-NEXT                                        
024098        IF SEGMENT-FINNS                                                  
024099           MOVE SEQA-IDFPINST TO W-IDFPINST                               
024100           PERFORM IMS-GET-WDD101                                         
024101        END-IF                                                            
024103     ELSE                                                                 
024104        IF ARTNR-LEVNR-SOEK = JA                                          
024105           MOVE NEJ TO TRAEFF-SW                                          
024106           PERFORM IMS-GET-WDD1A-NEXT                                     
024107           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
024108              MOVE SEQA-IDFPINST TO W-IDFPINST                            
024109              PERFORM IMS-GET-WDD101                                      
024110              IF FPI-IDLEVNR = W-IDLEVNR                                  
024111                 MOVE JA TO TRAEFF-SW                                     
024112              ELSE                                                        
024113                 PERFORM IMS-GET-WDD1A-NEXT                               
024114              END-IF                                                      
024115           END-PERFORM                                                    
024116        ELSE                                                              
024117           IF LEVNR-SOEK = JA                                             
024119              PERFORM IMS-GET-WDD1B-NEXT                                  
024120              IF SEGMENT-FINNS                                            
024121                 MOVE SEQB-IDFPINST TO W-IDFPINST                         
024122                 PERFORM IMS-GET-WDD101                                   
024126              END-IF                                                      
024127           ELSE                                                           
024128              MOVE NEJ TO TRAEFF-SW                                       
024129              IF REGNR-SOEK = JA                                          
024132                 PERFORM IMS-GNP-WDD111                                   
024133                 PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA           
024134                    IF FPA-IDARTNR >= SPAR-IDARTNR-DOLT                   
024135                       MOVE JA TO TRAEFF-SW                               
024136                    ELSE                                                  
024137                       PERFORM IMS-GNP-WDD111                             
024138                    END-IF                                                
024139                 END-PERFORM                                              
024146              END-IF                                                      
024147           END-IF                                                         
024148        END-IF                                                            
024149     END-IF                                                               
024150     .                                                                    
024160     EJECT                                                                
024602 G-KOLLA-INPUT SECTION.                                                   
024603                                                                          
024604     MOVE JA   TO INDATA-SW                                               
024605     MOVE NEJ  TO TRAEFF-SW                                               
024607                  WS-ART-FINNS                                            
024608                  WS-KOLL-IDLEVNR                                         
024609                                                                          
024610     IF MID-INDEL = ALL '+'                                               
024611        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
024612        CALL WMEDKONV USING MED-WMEDAREA                                  
024613        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024614        MOVE NEJ TO INDATA-SW                                             
024615        PERFORM MFS-ROER-EJ-FAELT-IN                                      
024616     ELSE                                                                 
024617        IF REGNR-SOEK = NEJ                                               
024618           MOVE NEJ TO INDATA-SW                                          
024619           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFPINST-ATTR                    
024620        ELSE                                                              
024621           INSPECT MID-IDARTNR-UPD REPLACING LEADING SPACE                
024622                                         BY ZERO                          
024623           INSPECT MID-IDFPINST-UPD REPLACING LEADING SPACE               
024624                                         BY ZERO                          
024625                                                                          
024626           IF MID-IDARTNR-UPD NOT = ALL '+'                               
024627              IF MID-IDARTNR-UPD NOT NUMERIC                              
024628                 MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR             
024629                 MOVE NEJ TO INDATA-SW                                    
024630              ELSE                                                        
024631                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR             
024632              END-IF                                                      
024633           ELSE                                                           
024634              MOVE MFS-NUM-FAELT-FEL      TO MOD-IDARTNR-ATTR             
024635              MOVE NEJ TO INDATA-SW                                       
024636           END-IF                                                         
024637                                                                          
024638           IF MID-IDFPINST-UPD NOT = ALL '+'                              
024639              IF MID-IDFPINST-UPD NOT NUMERIC                             
024640                 MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFPINST-ATTR            
024641                 MOVE NEJ TO INDATA-SW                                    
024642              ELSE                                                        
024643                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFPINST-ATTR            
024644              END-IF                                                      
024646           ELSE                                                           
024647              MOVE MFS-NUM-FAELT-FEL      TO MOD-IDFPINST-ATTR            
024648              MOVE NEJ TO INDATA-SW                                       
024649           END-IF                                                         
024650                                                                          
024651           IF MID-FLBORT NOT = ALL '+'                                    
024652              IF MID-FLBORT = 'J' OR 'Y'                                  
024653                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBORT-ATTR             
024654              ELSE                                                        
024655                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLBORT-ATTR             
024656                 MOVE NEJ TO INDATA-SW                                    
024657              END-IF                                                      
024658           END-IF                                                         
024664                                                                          
024665           IF INDATA-OK                                                   
024672              INSPECT MID-IDARTNR-UPD REPLACING LEADING SPACE             
024673                                                    BY ZERO               
024674              INSPECT MID-IDFPINST-UPD REPLACING LEADING SPACE            
024675                                                   BY ZERO                
024676              MOVE MID-IDARTNR-UPD  TO W-IDARTNR-UPD                      
024677              MOVE MID-IDFPINST-UPD TO W-IDFPINST-UPD                     
024678                                                                          
024679              IF W-IDFPINST-UPD = W-IDFPINST                              
024680                 CONTINUE                                                 
024681              ELSE                                                        
024682                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFPINST-ATTR              
024684                 MOVE NEJ TO INDATA-SW                                    
024688              END-IF                                                      
024689                                                                          
024700                                                                          
024701              MOVE W-IDFPINST-UPD TO W-IDFPINST                           
024702              PERFORM IMS-GET-WDD101                                      
024703              IF SEGMENT-SAKNAS                                           
024704                 IF MID-FLBORT = 'J' OR 'Y'                               
024705                    MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR            
024706                    MOVE NEJ TO INDATA-SW                                 
024707                 ELSE                                                     
024708                    IF MID-FLBORT = ALL '+'                               
024709                       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR         
024710                       MOVE MFS-NUM-FAELT-FEL TO MOD-IDFPINST-ATTR        
024711                       MOVE NEJ TO INDATA-SW                              
024712                    END-IF                                                
024713                 END-IF                                                   
024714              ELSE                                                        
024715                 MOVE FPI-IDLEVNR TO WS-SPAR-IDLEVNR                      
024716********* KONTROLL KOPPLING TILL REGNR MED LEVNR 0                        
024717                 IF FPI-IDLEVNR = SPACE                                   
024718                    MOVE JA TO WS-KOLL-IDLEVNR                            
024719                 END-IF                                                   
024720                 PERFORM IMS-GET-WDD111                                   
024721                 IF SEGMENT-SAKNAS                                        
024722                    IF MID-FLBORT = 'J' OR 'Y'                            
024723                       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR         
024724                       MOVE NEJ TO INDATA-SW                              
024725                    END-IF                                                
024726                 ELSE                                                     
024727                    IF MID-FLBORT = ALL '+'                               
024728                       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR         
024729                       MOVE NEJ TO INDATA-SW                              
024730                    END-IF                                                
024731                 END-IF                                                   
024732              END-IF                                                      
024733                                                                          
024734********* KONTROLL OM REGNR MED LEVNR BLANK HAR KOPPLAD ARTIKEL           
024735              IF WS-KOLL-IDLEVNR = JA                                     
024736                 MOVE W-IDFPINST-UPD TO W-IDFPINST                        
024737                 PERFORM IMS-GET-WDD101                                   
024738                 PERFORM IMS-GNP-WDD111                                   
024739                 IF SEGMENT-FINNS                                         
024740                    MOVE JA TO WS-ART-FINNS                               
024741                 END-IF                                                   
024742              END-IF                                                      
024743                                                                          
024744              MOVE WS-IDFPINST TO W-IDFPINST                              
024745                                                                          
024746              IF MID-FLBORT = ALL '+'                                     
024747                                                                          
024748                 IF WS-ART-FINNS = JA                                     
024749                    MOVE NEJ TO INDATA-SW                                 
024750                    MOVE INF-REGNR-UNIK-TEXT TO MOD-TEMFSINF              
024751                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDFPINST-ATTR           
024752                    MOVE NEJ TO INDATA-SW                                 
024753                 END-IF                                                   
024754                                                                          
024755                 PERFORM IMS-GET-WDK601                                   
024756                 IF SEGMENT-SAKNAS                                        
024757                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR            
024758                    MOVE NEJ TO INDATA-SW                                 
024759                    MOVE ARTIKEL-SAKNAS TO MED-IDMFSINF                   
024760                    CALL WMEDKONV USING MED-WMEDAREA                      
024761                    MOVE MED-MFSINF TO MOD-TEMFSINF                       
024762                 ELSE                                                     
024763                    IF ART-KDERS-UTG > ZERO                               
024764                       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR         
024765                       MOVE NEJ TO INDATA-SW                              
024766                       MOVE ARTIKEL-UTGANGEN TO MED-IDMFSINF              
024767                       CALL WMEDKONV USING MED-WMEDAREA                   
024768                       MOVE MED-MFSINF TO MOD-TEMFSINF                    
024769                    END-IF                                                
024770                 END-IF                                                   
024771                                                                          
024772********* OM ARTIKEL SOM SKA KOPPLAS FINNS PÅ REGNR                       
024773*********                                   MED LEVNR = BLANK             
024774********* GODKÄNNES INTE KOPPLING TILL NYTT REGNR                         
024775*********                                   MED LEVNR = BLANK             
024776                                                                          
024777                 MOVE W-IDARTNR-UPD TO W-SEQA-IDARTNR-MIN                 
024778                                       W-SEQA-IDARTNR-MAX                 
024779                 PERFORM IMS-GET-WDD1A-FIRST                              
024780                 PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA           
024781                    MOVE SEQA-IDFPINST TO W-IDFPINST                      
024782                    PERFORM IMS-GET-WDD101                                
024783                    IF FPI-IDLEVNR = SPACE                                
024784                    AND WS-KOLL-IDLEVNR = JA                              
024785                       MOVE INF-ARTIKEL-UNIK-TEXT TO MOD-TEMFSINF         
024786                       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR         
024787                       MOVE NEJ TO INDATA-SW                              
024788                                                                          
024789*********     KONTROLLERA ATT ARTIKELN INTE ÄR KOPPLAD TILL               
024790*********     EN ANNAN FPINST PÅ SAMMA IDALEVNR                           
024791                    ELSE                                                  
024792                       IF WS-SPAR-IDLEVNR = FPI-IDLEVNR                   
024793                          MOVE INF-ARTIKEL-FINNS-TEXT                     
024794                                         TO MOD-TEMFSINF                  
024795                          MOVE MFS-NUM-FAELT-FEL                          
024796                                         TO MOD-IDARTNR-ATTR              
024797                          MOVE NEJ TO INDATA-SW                           
024798                       END-IF                                             
024799                    END-IF                                                
024800                                                                          
024801                    PERFORM IMS-GET-WDD1A-NEXT                            
024802                 END-PERFORM                                              
024803              END-IF                                                      
024804                                                                          
024805              MOVE WS-IDFPINST TO W-IDFPINST                              
024806           END-IF                                                         
024807        END-IF                                                            
024808                                                                          
024809        IF INDATA-FEL                                                     
024810           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
024811           CALL WMEDKONV USING MED-WMEDAREA                               
024812           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
024813           MOVE NEJ TO INDATA-SW                                          
024814           PERFORM MFS-ROER-EJ-FAELT-IN                                   
024815        END-IF                                                            
024816     END-IF                                                               
024817     .                                                                    
024818     EJECT                                                                
024819 H-UPPDATERA SECTION.                                                     
024820                                                                          
024821     INSPECT MID-IDARTNR-UPD REPLACING LEADING SPACE BY ZERO              
024822     INSPECT MID-IDFPINST-UPD REPLACING LEADING SPACE BY ZERO             
024823     MOVE MID-IDARTNR-UPD  TO W-IDARTNR-UPD                               
024824     MOVE MID-IDFPINST-UPD TO W-IDFPINST                                  
024825                                                                          
024826     IF MID-FLBORT = 'J' OR 'Y'                                           
024827        PERFORM HA-BORTTAG-KOPPLING                                       
024828     ELSE                                                                 
024829        PERFORM HB-UPPDATERA-KOPPLING                                     
024830     END-IF                                                               
024831                                                                          
024832     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
024833     CALL WMEDKONV USING MED-WMEDAREA                                     
024834     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
024835     PERFORM MFS-FORM-ATTR                                                
024836     PERFORM MFS-RENSA-FAELT-IN                                           
024837     .                                                                    
024838     EJECT                                                                
024839 HA-BORTTAG-KOPPLING SECTION.                                             
024840                                                                          
024841     PERFORM IMS-GET-WDD101                                               
024842     IF SEGMENT-FINNS                                                     
024843        PERFORM IMS-GET-WDD111                                            
024844        IF SEGMENT-FINNS                                                  
024845           PERFORM IMS-DLET-WDD111                                        
024846        END-IF                                                            
024847     END-IF                                                               
024848     .                                                                    
024849     EJECT                                                                
024850 HB-UPPDATERA-KOPPLING SECTION.                                           
024851                                                                          
024852     PERFORM IMS-GET-WDD101                                               
024853     IF SEGMENT-FINNS                                                     
024854        PERFORM IMS-GET-WDD111                                            
024855        IF SEGMENT-SAKNAS                                                 
024856           MOVE W-IDARTNR-UPD TO FPA-IDARTNR                              
024857           PERFORM IMS-ISRT-WDD111                                        
024858        END-IF                                                            
024859     END-IF                                                               
024860     .                                                                    
024861     EJECT                                                                
024870 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025100     MOVE +1 TO RAD-IX                                                    
025110     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
025200        MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR (RAD-IX)                      
025300                                MOD-IDFPINST(RAD-IX)                      
025310                                MOD-IDARTNR (RAD-IX)                      
025320        ADD +1 TO RAD-IX                                                  
025330     END-PERFORM                                                          
025400     .                                                                    
025501     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UPD                              
026100                             MOD-IDFPINST-UPD                             
026110                             MOD-FLBORT                                   
026200     .                                                                    
026300     EJECT                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400                                                                          
027500*    --- ALLA INDATA-FÄLT                                                 
027600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPD                            
027700                               MOD-IDFPINST-UPD                           
027710                               MOD-FLBORT                                 
027800     .                                                                    
027900     SKIP3                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALLA INDATA-FÄLT                                                 
028300     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-ATTR                          
028400                                MOD-IDFPINST-ATTR                         
028410                                MOD-FLBORT-ATTR                           
028500     .                                                                    
029300     EJECT                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030600     IF MSGI-IDLAND-SPR = 'SE'                                            
030700       MOVE '0' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GET-WDK601 SECTION.                                                  
031503     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-UPD-X ')'                     
031504          DELIMITED BY SIZE INTO SSA1                                     
031505     MOVE '  GE' TO GODK-STATUSKODER                                      
031506     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
031507     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031508     PERFORM IMS-STATUSKONTROLL                                           
031509     .                                                                    
031510     SKIP3                                                                
031511 IMS-GET-WDD101 SECTION.                                                  
031512     STRING 'WDD101  (IDFPINST =' W-IDFPINST-X ')'                        
031513          DELIMITED BY SIZE INTO SSA1                                     
031514     MOVE '  GE' TO GODK-STATUSKODER                                      
031515     CALL CBLTDLI USING GHU WDD1-PCB DLI-IO-WDD101 SSA1                   
031516     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
031517     PERFORM IMS-STATUSKONTROLL                                           
031518     .                                                                    
031519     SKIP3                                                                
031520 IMS-GET-WDD111 SECTION.                                                  
031521     STRING 'WDD111  (IDARTNR  =' W-IDARTNR-UPD-X ')'                     
031522          DELIMITED BY SIZE INTO SSA1                                     
031523     MOVE '  GE' TO GODK-STATUSKODER                                      
031524     CALL CBLTDLI USING GHU WDD1-PCB DLI-IO-WDD111 SSA1                   
031525     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
031526     PERFORM IMS-STATUSKONTROLL                                           
031527     .                                                                    
031528     EJECT                                                                
031529 IMS-GNP-WDD111 SECTION.                                                  
031530     MOVE 'WDD111 ' TO SSA1                                               
031531     MOVE '  GE' TO GODK-STATUSKODER                                      
031532     CALL CBLTDLI USING GNP WDD1-PCB DLI-IO-WDD111 SSA1                   
031533     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
031534     PERFORM IMS-STATUSKONTROLL                                           
031535     .                                                                    
031536     SKIP3                                                                
031537 IMS-ISRT-WDD111 SECTION.                                                 
031538     STRING 'WDD101  (IDFPINST =' W-IDFPINST-X ')'                        
031539            DELIMITED BY SIZE INTO SSA1                                   
031540     MOVE 'WDD111   ' TO SSA2                                             
031541     MOVE '  ' TO GODK-STATUSKODER                                        
031542     CALL CBLTDLI USING ISRT WDD1-PCB DLI-IO-WDD111 SSA1 SSA2             
031543     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
031550     PERFORM IMS-STATUSKONTROLL                                           
031580     .                                                                    
031590     SKIP3                                                                
031598 IMS-DLET-WDD111 SECTION.                                                 
031599     MOVE '  ' TO GODK-STATUSKODER                                        
031600     CALL CBLTDLI USING DLET WDD1-PCB DLI-IO-WDD111                       
031601     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
031602     PERFORM IMS-STATUSKONTROLL                                           
031603     .                                                                    
031604     EJECT                                                                
031605 IMS-GET-WDD1A-FIRST SECTION.                                             
031606     STRING 'WDD1A1  (WDD1A1KY=>' W-WDD1A1KY-MIN                          
031607                    '&WDD1A1KY=<' W-WDD1A1KY-MAX ')'                      
031608          DELIMITED BY SIZE INTO SSA1                                     
031609     MOVE '  GE' TO GODK-STATUSKODER                                      
031610     CALL CBLTDLI USING GU WDD1A-PCB DLI-IO-WDD1A SSA1                    
031611     MOVE WDD1A-STATUS-CODE TO STATUS-WS                                  
031612     PERFORM IMS-STATUSKONTROLL                                           
031613     .                                                                    
031614     SKIP3                                                                
031615 IMS-GET-WDD1A-NEXT SECTION.                                              
031616     STRING 'WDD1A1  (WDD1A1KY=>' W-WDD1A1KY-MIN                          
031617                    '&WDD1A1KY=<' W-WDD1A1KY-MAX ')'                      
031618          DELIMITED BY SIZE INTO SSA1                                     
031619     MOVE '  GE' TO GODK-STATUSKODER                                      
031620     CALL CBLTDLI USING GN WDD1A-PCB DLI-IO-WDD1A SSA1                    
031621     MOVE WDD1A-STATUS-CODE TO STATUS-WS                                  
031622     PERFORM IMS-STATUSKONTROLL                                           
031623     .                                                                    
031624     EJECT                                                                
031625 IMS-GET-WDD1B-FIRST SECTION.                                             
031626     STRING 'WDD1B1  (WDD1B1KY=>' W-WDD1B1KY-MIN                          
031627                    '&WDD1B1KY=<' W-WDD1B1KY-MAX ')'                      
031628          DELIMITED BY SIZE INTO SSA1                                     
031629     MOVE '  GE' TO GODK-STATUSKODER                                      
031630     CALL CBLTDLI USING GU WDD1B-PCB DLI-IO-WDD1B SSA1                    
031640     MOVE WDD1B-STATUS-CODE TO STATUS-WS                                  
031650     PERFORM IMS-STATUSKONTROLL                                           
031660     .                                                                    
031670     SKIP3                                                                
031680 IMS-GET-WDD1B-NEXT SECTION.                                              
031690     STRING 'WDD1B1  (WDD1B1KY=>' W-WDD1B1KY-MIN                          
031691                    '&WDD1B1KY=<' W-WDD1B1KY-MAX ')'                      
031692          DELIMITED BY SIZE INTO SSA1                                     
031693     MOVE '  GE' TO GODK-STATUSKODER                                      
031694     CALL CBLTDLI USING GN WDD1B-PCB DLI-IO-WDD1B SSA1                    
031695     MOVE WDD1B-STATUS-CODE TO STATUS-WS                                  
031696     PERFORM IMS-STATUSKONTROLL                                           
031697     .                                                                    
031698     EJECT                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
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
