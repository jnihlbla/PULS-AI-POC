001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4070800.                                                
001600 AUTHOR.         SUSANNE OLSSON.                                          
001700 DATE-WRITTEN.   02/09/04.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
001920*                                                                         
002000*    FUNKTION:                                                            
002100*        MPP SOM ADMINISTRERAR ANSVARIGTABELL FÖR UTSKRIFT RESPEK-        
002200*        TIVE ATTEST FÖR KREDITNOTOR.VÄRDE-INTERVALLER OCH                
002300*        DISTRIKT-INTERVALLER BESTÄMMER OM EN KN SKALL PRINTAS            
002400*        ELLER BARA TILL ON-DEMAND.TABELLEN VISAR OCKSÅ VEM SOM           
002500*        ÄR ATTESTANSVARIG.FÖR AUTOMATGODKÄNDA LEV.ANM. ÄR DETTA          
002600*        SAMMA PERSON SOM STÅR PÅ KN.FÖR ANDRA KODER ÄR DET NAMNET        
002700*        + ID'T PÅ DEN PERSON SOM GODKÄNT LEV.ANM. SOM SKRIVS UT          
002800*        PÅ KN.                                                           
002810*        PROGRAM MED PATH-CALL FÖR IDUSER                                 
002900*                                                                         
003001*        PROGRAMMET UPPDATERAR WDGX4124 (WDR1)                            
003100*                              WDGX4126                                   
003110*                                                                         
003200*    INDATA.                                                              
003300*        TRANSAKTION: W4T708                                              
003310*                     W4T708U                                             
003400*        MID:         W4I70801                                            
003500*                                                                         
003600*    UTDATA.                                                              
003700*        MOD:         W4O70801                                            
003800                                                                          
003900     SKIP3                                                                
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 DATA DIVISION.                                                           
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500 77  IDPGM                       PIC X(08)   VALUE 'W4070800'.            
004600                                                                          
004700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004900                                                                          
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005110 77  WS-IDDISTR-IN               PIC X(4)    VALUE ZERO.                  
005120 77  WS-IDANSTNR-IN              PIC X(5)    VALUE ZERO.                  
005130 77  WS-MID-IDDISTR-FOM-UPD      PIC S9(5)   VALUE ZERO COMP-3.           
005131 77  WS-MID-IDDISTR-TOM-UPD      PIC S9(5)   VALUE ZERO COMP-3.           
005132 77  WS-MID-SUKRENOT-FOM-UPD     PIC S9(7)   VALUE ZERO COMP-3.           
005133 77  WS-MID-SUKRENOT-TOM-UPD     PIC S9(7)   VALUE ZERO COMP-3.           
005200                                                                          
005301*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005302 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005310 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
005350                                                                          
005360 01  WS-IDUSER.                                                           
005371     03  FILLER                  PIC X(2)    VALUE 'PC'.                  
005372     03  WS-IDANSTNR             PIC 9(5)    VALUE ZERO.                  
005373     03  FILLER                  PIC X(1)    VALUE SPACE.                 
005374                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600                                                                          
005701 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005702     88  INDATA-OK                           VALUE 'J'.                   
005710     88  INDATA-FEL                          VALUE 'N'.                   
005800                                                                          
005900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006000     88  NYCKLAR-OK                          VALUE 'J'.                   
006100     88  NYCKLAR-FEL                         VALUE 'N'.                   
006200                                                                          
006210 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
006220     88  KDCMD-ALL-PLUS                      VALUE 'J'.                   
006230     88  KDCMD-NOT-ALL-PLUS                  VALUE 'N'.                   
006240                                                                          
006250 77  INTERVALL-SW                PIC X       VALUE 'J'.                   
006251     88  INTERVALL-OK                        VALUE 'J'.                   
006252     88  INTERVALL-FEL                       VALUE 'N'.                   
006260                                                                          
006300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006400     88  EGEN-MID                            VALUE '4708'.                
006500     88  GODK-MID                            VALUE '4701' '4702'          
006600                                                   '4703' '4704'          
006700                                                   '4705' '4706'          
006800                                                   '4707' '4708'          
006900                                                   '4709'.                
007000     88  HELP-MID                            VALUE '0551'.                
007001                                                                          
007100     EJECT                                                                
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008100*01 -COPY WMEDAREA                                                        
008200     SKIP3                                                                
008300 01  MESSAGE-CODES.                                                       
008401     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008402     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008403     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008410     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008501     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008502     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008510     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008700     03  ERR-UPPGIFTER-SAKNAS    PIC X(3)    VALUE '760'.                 
008710     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
008720     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '245'.                 
008730     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
008740     03  ERR-INTERVALL-KROCKAR   PIC X(3)    VALUE '723'.                 
008750     03  ERR-FOM-VARDE-FEL       PIC X(3)    VALUE '240'.                 
008760     03  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '007'.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009200     SKIP3                                                                
009300*01 -COPY WMSGINIT                                                        
009400     EJECT                                                                
009500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009600*                                                                         
009700 01  SPAR-AREA.                                                           
009800     03  SPAR-IDTRANS           PIC X(4)    VALUE '4708'.                 
009901     03  SPAR-IDDISTR-ENTER       PIC S9(5) VALUE ZERO COMP-3.            
009903     03  SPAR-IDDISTR-NEXT        PIC S9(5) VALUE ZERO COMP-3.            
009904     03  SPAR-IDUSER-ENTER        PIC X(8)  VALUE SPACE.                  
009905     03  SPAR-IDUSER-NEXT         PIC X(8)  VALUE SPACE.                  
009906     03  SPAR-IDDISTR-FOM-ENTER   PIC S9(5) VALUE ZERO COMP-3.            
009907     03  SPAR-IDDISTR-TOM-ENTER   PIC S9(5) VALUE ZERO COMP-3.            
009908     03  SPAR-IDDISTR-FOM-NEXT    PIC S9(5) VALUE ZERO COMP-3.            
009909     03  SPAR-IDDISTR-TOM-NEXT    PIC S9(5) VALUE ZERO COMP-3.            
009910     03  SPAR-SUKRENOT-FOM-ENTER  PIC S9(7) VALUE ZERO COMP-3.            
009911     03  SPAR-SUKRENOT-TOM-ENTER  PIC S9(7) VALUE ZERO COMP-3.            
009912     03  SPAR-SUKRENOT-FOM-NEXT   PIC S9(7) VALUE ZERO COMP-3.            
009913     03  SPAR-SUKRENOT-TOM-NEXT   PIC S9(7) VALUE ZERO COMP-3.            
009920     03  SPAR-MID-IDDISTR         PIC X(4)  VALUE SPACE.                  
009930     03  SPAR-MID-IDANSTNR        PIC X(5)  VALUE SPACE.                  
010000     EJECT                                                                
010100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010400     SKIP3                                                                
010500*01  MID -COPY W4I70801                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010800     SKIP3                                                                
010900*01  -COPY WMSGAREA                                                       
011000     EJECT                                                                
011100     03  MOD REDEFINES MSG-AREA.                                          
011200*      05  -COPY W4O70801                                                 
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011500     SKIP3                                                                
011600*01  -COPY WMFSAREA                                                       
011700     EJECT                                                                
011800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200 01  NYCKLAR-TILL-DLI.                                                    
012301*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
012302     03  W-IDDISTR-X.                                                     
012303         05  W-IDDISTR          PIC S9(5)    VALUE ZERO COMP-3.           
012304                                                                          
012305     03  W-IDUSER-X.                                                      
012306         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
012307                                                                          
012312     03  W-WDGXKEY-4123-X.                                                
012313         05  W-IDHTYP-4123       PIC  X(4)   VALUE '4123'.                
012314         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
012315                                                                          
012316     03  W-KEY4124-X.                                                     
012317         05  W-4124-IDDISTR-FOM  PIC S9(5)        COMP-3.                 
012318         05  W-4124-IDDISTR-TOM  PIC S9(5)        COMP-3.                 
012319                                                                          
012320     03  W-KEY4126-X.                                                     
012321         05  W-4126-SUKRENOT-FOM   PIC S9(7)      COMP-3.                 
012322         05  W-4126-SUKRENOT-TOM   PIC S9(7)      COMP-3.                 
012323                                                                          
012324     03  W-IDDISTR-FOM-X.                                                 
012325         05  W-IDDISTR-FOM       PIC S9(5)   VALUE ZERO COMP-3.           
012327                                                                          
012328     03  W-IDDISTR-TOM-X.                                                 
012329         05  W-IDDISTR-TOM       PIC S9(5)   VALUE ZERO COMP-3.           
012330                                                                          
012331     03  W-SUKRENOT-FOM-X.                                                
012332         05  W-SUKRENOT-FOM      PIC S9(7)   VALUE ZERO COMP-3.           
012333                                                                          
012334     03  W-SUKRENOT-TOM-X.                                                
012335         05  W-SUKRENOT-TOM      PIC S9(7)   VALUE ZERO COMP-3.           
012336                                                                          
012400     SKIP2                                                                
012500*    --- STATUS-KOD FRÅN IMS                                              
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FINNS                       VALUE '  '.                  
012800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013000     SKIP2                                                                
013100 01  GODK-STATUSKODER.                                                    
013200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(64).                               
013500 01  SSA2                        PIC X(64).                               
013510 01  SSA3                        PIC X(64).                               
013600     EJECT                                                                
013700*    --- IMS FUNKTIONSKODER                                               
013800*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200                                                                          
014301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR101'.                      
014302 01  DLI-IO-WDR101.                                                       
014303*    03  -COPY WDGX01                                                     
014304     EJECT                                                                
014305 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4124'.                    
014306 01  DLI-IO-WDGX4124.                                                     
014307*    03  -COPY WDGX4124                                                   
014308     EJECT                                                                
014309 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4126'.                    
014310 01  DLI-IO-WDGX4126.                                                     
014311*    03  -COPY WDGX4126                                                   
014600     EJECT                                                                
014601 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA01'.                      
014602 01  DLI-IO-AREA01.                                                       
014603*    03  -COPY WDGX01 -PRE USER-                                          
014604     EJECT                                                                
014610 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
014620 01  DLI-IO-AREA.                                                         
014621     03 WDGX4124.                                                         
014622*      05  -COPY WDGX4124   -PRE USER-                                    
014623     03 WDGX4126.                                                         
014624*      05  -COPY WDGX4126   -PRE USER-                                    
014692     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800*01  -COPY W0009   -PRE MSG-                                              
014900*01  -COPY W0008   -PRE WDP7-                                             
015000     05  FILLER                  PIC X.                                   
015101                                                                          
015102*01  -COPY W0008  -PRE 4123-                                              
015103     05  FILLER                  PIC X.                                   
015104                                                                          
015105*01  -COPY W0008  -PRE 4123-P-                                            
015106     05  FILLER                  PIC X.                                   
015107                                                                          
015200     EJECT                                                                
015301 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB 4123-PCB 4123-P-PCB.          
015302 MAIN SECTION.                                                            
015310     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB 4123-PCB 4123-P-PCB.          
015400                                                                          
015600     PERFORM IMS-GET-MSG                                                  
015700     IF SEGMENT-FINNS                                                     
015800       PERFORM A-INIT                                                     
015900       PERFORM B-KOLLA-NYCKLAR                                            
016000       IF NYCKLAR-OK                                                      
016101         IF MFS-UPDATE                                                    
016102           PERFORM G-KOLLA-INPUT                                          
016103           IF INDATA-OK                                                   
016104             PERFORM H-UPPDATERA                                          
016105           END-IF                                                         
016110         ELSE                                                             
016201           IF MFS-FIRST                                                   
016202             PERFORM C-FOERSTA-SIDA                                       
016203           ELSE                                                           
016204             IF MFS-NEXT                                                  
016205               PERFORM D-NAESTA-SIDA                                      
016206             ELSE                                                         
016207               PERFORM E-SAMMA-SIDA                                       
016208             END-IF                                                       
016210           END-IF                                                         
016410         END-IF                                                           
016420         IF INDATA-OK                                                     
016500           PERFORM F-LAES-VISA-INFO                                       
016600         END-IF                                                           
016700       END-IF                                                             
016900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O70801 + 4                      
017000       PERFORM IMS-INSERT-MSG                                             
017100     END-IF                                                               
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT SECTION.                                                          
017900                                                                          
018000     IF MSG-DUBBLA-TRANSKODER                                             
018100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I70801                 
018200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018400     ELSE                                                                 
018500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I70801                  
018600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018800     END-IF                                                               
018900                                                                          
019000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
019100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019300                                                                          
019400     MOVE LOW-VALUE TO MSG-AREA                                           
019500     MOVE 'W4O708N1' TO MFS-IDMOD                                         
019600     MOVE '4708' TO MOD-IDTRANS                                           
019700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019800                                                                          
019820     MOVE SPACE                       TO MED-IDMFSINF                     
019830     MOVE SPACE                       TO MED-IDMFSFEL                     
019840                                                                          
019900     IF EGEN-MID OR HELP-MID                                              
020000       CONTINUE                                                           
020100     ELSE                                                                 
020200       MOVE SPACE TO MFS-KDTRTYP                                          
020300       MOVE '7' TO MFS-IDPFK                                              
020400     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 B-KOLLA-NYCKLAR SECTION.                                                 
021000                                                                          
021100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021200     MOVE '001'             TO MSGI-KDCALL                                
021300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021500     MOVE '4708'            TO MSGI-IDTRANS                               
021600     IF EGEN-MID                                                          
021700        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
021701                                   WS-IDDISTR-IN                          
021710        MOVE MID-IDANSTNR-IN    TO MSGI-IDANSTNR                          
021720                                   WS-IDANSTNR-IN                         
021800     END-IF                                                               
021900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
022000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
022100                                                                          
022200*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
022300     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
022320     MOVE '2'               TO MFS-KDMFSFOR                               
022330                                                                          
022400                                                                          
022500     MOVE JA TO NYCKLAR-SW                                                
022600                                                                          
022800*    -- KONTROLL AV IDDISTR                                               
022810     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
022820                                                                          
022830     IF MID-IDDISTR-IN  NOT = ALL '+'                                     
022840       MOVE '7'         TO MFS-IDPFK                                      
022850       MOVE SPACE       TO MFS-KDTRTYP                                    
022870       INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO               
022880       IF MSGI-IDDISTR NUMERIC                                            
022890         MOVE MSGI-IDDISTR TO W-IDDISTR                                   
022891       ELSE                                                               
022892         MOVE NEJ TO NYCKLAR-SW                                           
022893       END-IF                                                             
022894     END-IF                                                               
022895                                                                          
022896*    -- KONTROLL AV IDANSTNR                                              
022897     MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR-IN                              
022898                                                                          
022899     IF MID-IDANSTNR-IN  NOT = ALL '+'                                    
022900       MOVE '7'         TO MFS-IDPFK                                      
022901       MOVE SPACE       TO MFS-KDTRTYP                                    
022903       INSPECT MSGI-IDANSTNR REPLACING LEADING SPACE BY ZERO              
022904       IF MSGI-IDANSTNR NUMERIC                                           
022905         MOVE MSGI-IDANSTNR TO WS-IDANSTNR                                
022906         MOVE WS-IDUSER     TO W-IDUSER                                   
022907       ELSE                                                               
022908         MOVE NEJ TO NYCKLAR-SW                                           
022909       END-IF                                                             
022910     END-IF                                                               
022911                                                                          
022912     IF EGEN-MID OR NYCKLAR-OK                                            
022913       IF MFS-FIRST                                                       
022917         IF  MID-IDDISTR-IN = ALL '+'                                     
022918           MOVE SPACE              TO MOD-IDDISTR-UT                      
022919         ELSE                                                             
022920           MOVE MSGI-IDDISTR       TO MOD-IDDISTR-UT                      
022921           INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE         
022922         END-IF                                                           
022923         IF  MID-IDANSTNR-IN = ALL '+'                                    
022924           MOVE SPACE              TO MOD-IDANSTNR-UT                     
022925         ELSE                                                             
022926           MOVE MSGI-IDANSTNR      TO MOD-IDANSTNR-UT                     
022927           INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE        
022928         END-IF                                                           
022929       ELSE                                                               
022930         IF SPAR-MID-IDDISTR = ALL '+'                                    
022931           MOVE SPACE             TO MOD-IDDISTR-UT                       
022932         ELSE                                                             
022933           MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                     
022934           INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE         
022935         END-IF                                                           
022936         IF SPAR-MID-IDANSTNR = ALL '+'                                   
022937           MOVE SPACE             TO MOD-IDANSTNR-UT                      
022938         ELSE                                                             
022939           MOVE MSGI-IDANSTNR       TO MOD-IDANSTNR-UT                    
022940           INSPECT MOD-IDANSTNR-UT REPLACING LEADING ZERO BY SPACE        
022941         END-IF                                                           
022942       END-IF                                                             
022943     ELSE                                                                 
022944       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
022945                               MOD-IDANSTNR-UT                            
022948     END-IF                                                               
022950                                                                          
023000     IF NYCKLAR-FEL                                                       
023100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
023200       CALL WMEDKONV USING MED-WMEDAREA                                   
023300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023400       PERFORM MFS-RENSA-FAELT-IN                                         
023500       PERFORM MFS-RENSA-FAELT-UT                                         
023600     END-IF                                                               
023700     .                                                                    
023801     EJECT                                                                
023802 C-FOERSTA-SIDA SECTION.                                                  
023803                                                                          
023804     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
023805     CALL WMEDKONV USING MED-WMEDAREA                                     
023806     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
023808                                                                          
023809     PERFORM MFS-RENSA-FAELT-IN                                           
023810                                                                          
023811*FIX FÖR ATT KUNNA TRYCKA PF7                                             
023813     IF  ( MID-IDDISTR-IN = ALL '+' ) AND                                 
023814         ( MID-IDANSTNR-IN = ALL '+' )                                    
023815                                                                          
023816       MOVE SPAR-MID-IDDISTR   TO WS-IDDISTR-IN                           
023817       IF MSGI-IDDISTR NUMERIC                                            
023818         MOVE MSGI-IDDISTR TO W-IDDISTR                                   
023819       END-IF                                                             
023820                                                                          
023821       MOVE SPAR-MID-IDANSTNR  TO WS-IDANSTNR-IN                          
023822       IF MSGI-IDANSTNR NUMERIC                                           
023823         MOVE MSGI-IDANSTNR TO WS-IDANSTNR                                
023824         MOVE WS-IDUSER     TO W-IDUSER                                   
023825       END-IF                                                             
023826     END-IF                                                               
023827     .                                                                    
023828     EJECT                                                                
023829 D-NAESTA-SIDA SECTION.                                                   
023830                                                                          
023831     IF SPAR-IDTRANS = '4708'                                             
023832       MOVE SPAR-IDDISTR-NEXT      TO W-IDDISTR                           
023833       MOVE SPAR-IDUSER-NEXT       TO W-IDUSER                            
023834       MOVE SPAR-IDDISTR-FOM-NEXT  TO W-IDDISTR-FOM                       
023835                                      W-4124-IDDISTR-FOM                  
023836       MOVE SPAR-IDDISTR-TOM-NEXT  TO W-IDDISTR-TOM                       
023837                                      W-4124-IDDISTR-TOM                  
023838       MOVE SPAR-SUKRENOT-FOM-NEXT TO W-SUKRENOT-FOM                      
023839                                      W-4126-SUKRENOT-FOM                 
023840       MOVE SPAR-SUKRENOT-TOM-NEXT TO W-SUKRENOT-TOM                      
023841                                      W-4126-SUKRENOT-TOM                 
023842                                                                          
023843       MOVE SPAR-MID-IDDISTR   TO WS-IDDISTR-IN                           
023844       MOVE SPAR-MID-IDANSTNR  TO WS-IDANSTNR-IN                          
023845     ELSE                                                                 
023846       PERFORM MFS-RENSA-FAELT-IN                                         
023847     END-IF                                                               
023848     .                                                                    
023849     EJECT                                                                
023850 E-SAMMA-SIDA SECTION.                                                    
023851                                                                          
023852     IF EGEN-MID OR HELP-MID                                              
023853       IF SPAR-IDTRANS = '4708' OR '0551'                                 
023854         MOVE SPAR-IDDISTR-ENTER      TO W-IDDISTR                        
023855         MOVE SPAR-IDUSER-ENTER       TO W-IDUSER                         
023856         MOVE SPAR-IDDISTR-FOM-ENTER  TO W-IDDISTR-FOM                    
023857                                         W-4124-IDDISTR-FOM               
023858         MOVE SPAR-IDDISTR-TOM-ENTER  TO W-IDDISTR-TOM                    
023859                                         W-4124-IDDISTR-TOM               
023860         MOVE SPAR-SUKRENOT-FOM-ENTER TO W-SUKRENOT-FOM                   
023861                                         W-4126-SUKRENOT-FOM              
023862         MOVE SPAR-SUKRENOT-TOM-ENTER TO W-SUKRENOT-TOM                   
023863                                         W-4126-SUKRENOT-TOM              
023864                                                                          
023865         MOVE SPAR-MID-IDDISTR   TO WS-IDDISTR-IN                         
023866         MOVE SPAR-MID-IDANSTNR  TO WS-IDANSTNR-IN                        
023867       END-IF                                                             
023868                                                                          
023869       MOVE JA TO KDCMD-SW                                                
023870                                                                          
023871       MOVE +1 TO INDX                                                    
023872       PERFORM UNTIL INDX > MAX-INDX                                      
023873         IF MID-KDCMD (INDX) =  '+'                                       
023874           CONTINUE                                                       
023875         ELSE                                                             
023876           MOVE NEJ TO KDCMD-SW                                           
023877         END-IF                                                           
023878         ADD +1 TO INDX                                                   
023879       END-PERFORM                                                        
023880                                                                          
023881       IF MID-INPUT = ALL '+' AND KDCMD-ALL-PLUS                          
023882         PERFORM MFS-RENSA-FAELT-IN                                       
023883       ELSE                                                               
023884         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
023885         CALL WMEDKONV USING MED-WMEDAREA                                 
023886         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
023887         PERFORM EA-MID-INDATA-TILL-MOD                                   
023888       END-IF                                                             
023889     ELSE                                                                 
023890       PERFORM MFS-RENSA-FAELT-IN                                         
023891     END-IF                                                               
023892     .                                                                    
023893     EJECT                                                                
023894 EA-MID-INDATA-TILL-MOD SECTION.                                          
023895                                                                          
023896* * * * * FÖR VARJE MID-FÄLT                                              
023897* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
023898* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
023899* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
023900                                                                          
023901     IF MID-KDCMD-RAD-19 = ALL '+'                                        
023902       MOVE MFS-RENSA-FAELT TO MOD-KDCMD-RAD-19-UPD                       
023903     ELSE                                                                 
023904       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-RAD-19-ATTR                
023905       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-RAD-19-UPD                     
023906     END-IF                                                               
023907                                                                          
023908     IF MID-IDDISTR-FOM-UPD = ALL '+'                                     
023909       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM-UPD                        
023910     ELSE                                                                 
023911       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-FOM-ATTR                 
023912       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM-UPD                      
023913     END-IF                                                               
023914                                                                          
023915     IF MID-IDDISTR-TOM-UPD = ALL '+'                                     
023916       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-TOM-UPD                        
023917     ELSE                                                                 
023918       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-TOM-ATTR                 
023919       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-TOM-UPD                      
023920     END-IF                                                               
023921                                                                          
023922     IF MID-SUKRENOT-FOM-UPD = ALL '+'                                    
023923       MOVE MFS-RENSA-FAELT TO MOD-SUKRENOT-FOM-UPD                       
023924     ELSE                                                                 
023925       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUKRENOT-FOM-ATTR                
023926       MOVE MFS-ROER-EJ-FAELT TO MOD-SUKRENOT-FOM-UPD                     
023927     END-IF                                                               
023928                                                                          
023929     IF MID-SUKRENOT-TOM-UPD = ALL '+'                                    
023930       MOVE MFS-RENSA-FAELT TO MOD-SUKRENOT-TOM-UPD                       
023931     ELSE                                                                 
023932       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUKRENOT-TOM-ATTR                
023933       MOVE MFS-ROER-EJ-FAELT TO MOD-SUKRENOT-TOM-UPD                     
023934     END-IF                                                               
023935                                                                          
023936     IF MID-IDANSTNR-UPD = ALL '+'                                        
023937       MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR-ADM-UPD                       
023938     ELSE                                                                 
023939       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSTNR-ADM-ATTR                
023940       MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR-ADM-UPD                     
023941     END-IF                                                               
023942                                                                          
023943     IF MID-BEANST-UPD = ALL '+'                                          
023944       MOVE MFS-RENSA-FAELT TO MOD-BEANST-UPD                             
023945     ELSE                                                                 
023946       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEANST-ATTR                      
023947       MOVE MFS-ROER-EJ-FAELT TO MOD-BEANST-UPD                           
023948     END-IF                                                               
023949                                                                          
023950     IF MID-FLKREPRT-UPD = ALL '+'                                        
023951       MOVE MFS-RENSA-FAELT TO MOD-FLKREPRT-UPD                           
023952     ELSE                                                                 
023953       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKREPRT-ATTR                    
023954       MOVE MFS-ROER-EJ-FAELT TO MOD-FLKREPRT-UPD                         
023955     END-IF                                                               
023956                                                                          
023957*- KOLLA KANTKODEN PÅ ALLA UT-RADERNA.                                    
023958                                                                          
023959     MOVE +1 TO INDX                                                      
023960     PERFORM UNTIL INDX > MAX-INDX                                        
023961       IF MID-KDCMD (INDX)  =  '+'                                        
023962         MOVE MFS-RENSA-FAELT  TO MOD-KDCMD-UPD (INDX)                    
023963       ELSE                                                               
023964         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)              
023965         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-UPD (INDX)                   
023966       END-IF                                                             
023967       ADD +1 TO INDX                                                     
023968     END-PERFORM                                                          
023969                                                                          
023970     .                                                                    
024000     EJECT                                                                
024100 F-LAES-VISA-INFO SECTION.                                                
024310*- MAN KAN FRÅGA PÅ BARA DISTR, BARA IDUSER, ELLER BÅDE OCH.              
024320                                                                          
024330     IF MFS-UPDATE                                                        
024334       PERFORM IMS-GU-WDR101                                              
024335       PERFORM IMS-GNP-WDGX4124-KVAL                                      
024336       IF SEGMENT-SAKNAS                                                  
024337         MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                            
024338         CALL WMEDKONV USING MED-WMEDAREA                                 
024339         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024340         PERFORM MFS-RENSA-FAELT-UT                                       
024341       ELSE                                                               
024342         MOVE W-IDDISTR         TO SPAR-IDDISTR-ENTER                     
024343         MOVE W-IDUSER          TO SPAR-IDUSER-ENTER                      
024344         MOVE WS-IDDISTR-IN     TO SPAR-MID-IDDISTR                       
024345         MOVE WS-IDANSTNR-IN    TO SPAR-MID-IDANSTNR                      
024346         MOVE 4124-IDDISTR-FOM  TO SPAR-IDDISTR-FOM-ENTER                 
024347         MOVE 4124-IDDISTR-TOM  TO SPAR-IDDISTR-TOM-ENTER                 
024348                                                                          
024349         PERFORM FB-LAES-DISTRIKT                                         
024350       END-IF                                                             
024351     ELSE                                                                 
024352       IF WS-IDDISTR-IN = ALL '+'                                         
024360         IF WS-IDANSTNR-IN NOT = ALL '+'                                  
024370           PERFORM FA-LAES-ALLA-IDUSER                                    
024380         END-IF                                                           
024390       ELSE                                                               
024391         PERFORM IMS-GU-WDR101                                            
024392         PERFORM IMS-GNP-WDGX4124                                         
024393                                                                          
024394         IF SEGMENT-SAKNAS                                                
024395           MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                          
024396           CALL WMEDKONV USING MED-WMEDAREA                               
024397           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
024399           PERFORM MFS-RENSA-FAELT-UT                                     
024400         ELSE                                                             
024401*- FLYTTA ENTER NYCKLAR.                                                  
024402           MOVE W-IDDISTR         TO SPAR-IDDISTR-ENTER                   
024403           MOVE W-IDUSER          TO SPAR-IDUSER-ENTER                    
024404           MOVE WS-IDDISTR-IN     TO SPAR-MID-IDDISTR                     
024405           MOVE WS-IDANSTNR-IN    TO SPAR-MID-IDANSTNR                    
024406           MOVE 4124-IDDISTR-FOM  TO SPAR-IDDISTR-FOM-ENTER               
024407           MOVE 4124-IDDISTR-TOM  TO SPAR-IDDISTR-TOM-ENTER               
024408                                                                          
024409           IF WS-IDANSTNR-IN = ALL '+'                                    
024410             PERFORM FB-LAES-DISTRIKT                                     
024411           ELSE                                                           
024412             PERFORM FC-LAES-DISTRIKT-IDUSER                              
024413           END-IF                                                         
024420         END-IF                                                           
024500       END-IF                                                             
024600     END-IF                                                               
025183                                                                          
025300     .                                                                    
025400     EJECT                                                                
025510 FA-LAES-ALLA-IDUSER SECTION.                                             
025600                                                                          
025601     PERFORM IMS-GU-PATH-WDR101                                           
025602                                                                          
025610     IF MFS-FIRST                                                         
025641       PERFORM IMS-GNP-PATH-WDGX4126                                      
025650     ELSE                                                                 
025660       PERFORM IMS-GNP-PATH-WDGX4126-KVAL                                 
025670     END-IF                                                               
025710                                                                          
025720     IF SEGMENT-SAKNAS                                                    
025730       MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                              
025740       CALL WMEDKONV USING MED-WMEDAREA                                   
025750       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025760       PERFORM MFS-RENSA-FAELT-UT                                         
025761     ELSE                                                                 
025770       MOVE W-IDDISTR         TO SPAR-IDDISTR-ENTER                       
025771       MOVE W-IDUSER          TO SPAR-IDUSER-ENTER                        
025772       MOVE USER-4124-IDDISTR-FOM  TO SPAR-IDDISTR-FOM-ENTER              
025773       MOVE USER-4124-IDDISTR-TOM  TO SPAR-IDDISTR-TOM-ENTER              
025774       MOVE USER-4126-SUKRENOT-FOM TO SPAR-SUKRENOT-FOM-ENTER             
025775       MOVE USER-4126-SUKRENOT-TOM TO SPAR-SUKRENOT-TOM-ENTER             
025776       MOVE WS-IDDISTR-IN     TO SPAR-MID-IDDISTR                         
025777       MOVE WS-IDANSTNR-IN    TO SPAR-MID-IDANSTNR                        
025788                                                                          
025789       MOVE +1 TO INDX                                                    
025790       PERFORM UNTIL INDX > MAX-INDX                                      
025791         IF SEGMENT-FINNS                                                 
025793           MOVE USER-4124-IDDISTR-FOM   TO MOD-IDDISTR-FOM (INDX)         
025794           MOVE USER-4124-IDDISTR-TOM   TO MOD-IDDISTR-TOM (INDX)         
025795           MOVE USER-4126-SUKRENOT-FOM  TO MOD-SUKRENOT-FOM(INDX)         
025796           MOVE USER-4126-SUKRENOT-TOM  TO MOD-SUKRENOT-TOM(INDX)         
025797           MOVE USER-4126-IDUSER-ADM(3:5) TO                              
025798                                     MOD-IDANSTNR-ADM  (INDX)             
025799           MOVE USER-4126-BEANST        TO MOD-BEANST  (INDX)             
025800           MOVE USER-4126-FLKREPRT      TO MOD-FLKREPRT(INDX)             
025801           MOVE USER-4126-IDUSER-ATTUPD(3:5) TO                           
025803                                     MOD-IDANSTNR-ATTUPD(INDX)            
025804           MOVE USER-4126-TIUPPDAT      TO MOD-TIUPPDAT (INDX)            
025805                                                                          
025806           MOVE USER-4124-IDDISTR-FOM   TO SPAR-IDDISTR-FOM-NEXT          
025807           MOVE USER-4124-IDDISTR-TOM   TO SPAR-IDDISTR-TOM-NEXT          
025808           MOVE USER-4126-SUKRENOT-FOM  TO SPAR-SUKRENOT-FOM-NEXT         
025809           MOVE USER-4126-SUKRENOT-TOM  TO SPAR-SUKRENOT-TOM-NEXT         
025810                                                                          
025811           PERFORM IMS-GNP-PATH-WDGX4126                                  
025812         ELSE                                                             
025813           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
025814           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR(INDX)           
025815         END-IF                                                           
025816         ADD 1 TO INDX                                                    
025817       END-PERFORM                                                        
025818                                                                          
025819*-- KOLLA OM DET FINNS ETT 11:E SEGMENT.                                  
025820                                                                          
025830       IF SEGMENT-FINNS                                                   
025840         MOVE W-IDDISTR         TO SPAR-IDDISTR-NEXT                      
025850         MOVE W-IDUSER          TO SPAR-IDUSER-NEXT                       
025860         MOVE USER-4124-IDDISTR-FOM   TO SPAR-IDDISTR-FOM-NEXT            
025870         MOVE USER-4124-IDDISTR-TOM   TO SPAR-IDDISTR-TOM-NEXT            
025880         MOVE USER-4126-SUKRENOT-FOM  TO SPAR-SUKRENOT-FOM-NEXT           
025890         MOVE USER-4126-SUKRENOT-TOM  TO SPAR-SUKRENOT-TOM-NEXT           
025891         MOVE WS-IDDISTR-IN     TO SPAR-MID-IDDISTR                       
025892         MOVE WS-IDANSTNR-IN    TO SPAR-MID-IDANSTNR                      
025893         IF MED-IDMFSINF = SPACE OR '006'                                 
025894           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
025895           CALL WMEDKONV USING MED-WMEDAREA                               
025896           MOVE MED-MFSINF TO MOD-TEMFSINF                                
025897         END-IF                                                           
025898       ELSE                                                               
025899         MOVE W-IDDISTR         TO SPAR-IDDISTR-NEXT                      
025900         MOVE W-IDUSER          TO SPAR-IDUSER-NEXT                       
025950         MOVE WS-IDDISTR-IN     TO SPAR-MID-IDDISTR                       
025960         MOVE WS-IDANSTNR-IN    TO SPAR-MID-IDANSTNR                      
025970         IF MED-IDMFSINF = SPACE                                          
025980           MOVE INF-LAST-PAGE  TO MED-IDMFSINF                            
025990           CALL WMEDKONV USING MED-WMEDAREA                               
025991           MOVE MED-MFSINF TO MOD-TEMFSINF                                
025992         END-IF                                                           
025993       END-IF                                                             
025994                                                                          
025995       MOVE '002'      TO MSGI-KDCALL                                     
025996       MOVE '4708'     TO SPAR-IDTRANS                                    
025997       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
025998       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
025999                                                                          
026000     END-IF                                                               
026100     .                                                                    
026201     EJECT                                                                
026202 FB-LAES-DISTRIKT SECTION.                                                
026203                                                                          
026204     MOVE 4124-IDDISTR-FOM TO W-4124-IDDISTR-FOM                          
026205     MOVE 4124-IDDISTR-TOM TO W-4124-IDDISTR-TOM                          
026206                                                                          
026238     IF MFS-FIRST OR MFS-UPDATE                                           
026241       PERFORM IMS-GNP-WDGX4126                                           
026242     ELSE                                                                 
026246       PERFORM IMS-GNP-WDGX4126-KVAL                                      
026247     END-IF                                                               
026248                                                                          
026249     IF SEGMENT-SAKNAS                                                    
026250       MOVE +0                   TO SPAR-SUKRENOT-FOM-ENTER               
026251       MOVE +9999999             TO SPAR-SUKRENOT-TOM-ENTER               
026254       MOVE ERR-UPPGIFTER-SAKNAS TO MED-IDMFSFEL                          
026255       CALL WMEDKONV USING MED-WMEDAREA                                   
026256       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026257       PERFORM MFS-RENSA-FAELT-UT                                         
026258     ELSE                                                                 
026259       MOVE 4126-SUKRENOT-FOM  TO SPAR-SUKRENOT-FOM-ENTER                 
026260       MOVE 4126-SUKRENOT-TOM  TO SPAR-SUKRENOT-TOM-ENTER                 
026261                                                                          
026262       MOVE +1 TO INDX                                                    
026263                                                                          
026264       PERFORM UNTIL INDX > MAX-INDX                                      
026265         IF SEGMENT-FINNS                                                 
026266           PERFORM S01-FLYTTA-DATA-TILL-MOD                               
026267           PERFORM IMS-GNP-WDGX4126                                       
026268         ELSE                                                             
026269           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
026270           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR(INDX)           
026271         END-IF                                                           
026272         ADD 1 TO INDX                                                    
026273       END-PERFORM                                                        
026274                                                                          
026275*-- KOLLA OM DET FINNS ETT 11:E SEGMENT.                                  
026276       PERFORM S02-FINNS-DET-FLERA-SEGMENT                                
026277     END-IF                                                               
026280     .                                                                    
026301     EJECT                                                                
026302 FC-LAES-DISTRIKT-IDUSER SECTION.                                         
026303                                                                          
026304     IF MFS-FIRST                                                         
026305       MOVE 4124-IDDISTR-FOM TO W-4124-IDDISTR-FOM                        
026306       MOVE 4124-IDDISTR-TOM TO W-4124-IDDISTR-TOM                        
026307       PERFORM IMS-GNP-WDGX4126-IDUSER                                    
026308     ELSE                                                                 
026309       PERFORM IMS-GNP-WDGX4126-KVAL                                      
026310     END-IF                                                               
026316                                                                          
026317     IF SEGMENT-SAKNAS                                                    
026318       MOVE +0                   TO SPAR-SUKRENOT-FOM-ENTER               
026319       MOVE +9999999             TO SPAR-SUKRENOT-TOM-ENTER               
026326       MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                              
026327       CALL WMEDKONV USING MED-WMEDAREA                                   
026328       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026329       PERFORM MFS-RENSA-FAELT-UT                                         
026330     ELSE                                                                 
026331       MOVE 4126-SUKRENOT-FOM TO SPAR-SUKRENOT-FOM-ENTER                  
026332       MOVE 4126-SUKRENOT-TOM TO SPAR-SUKRENOT-TOM-ENTER                  
026339                                                                          
026353       MOVE +1 TO INDX                                                    
026354                                                                          
026355       PERFORM UNTIL INDX > MAX-INDX                                      
026356         IF SEGMENT-FINNS                                                 
026357           PERFORM S01-FLYTTA-DATA-TILL-MOD                               
026358           PERFORM IMS-GNP-WDGX4126-IDUSER                                
026359         ELSE                                                             
026360           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
026361           MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR(INDX)           
026362         END-IF                                                           
026363         ADD 1 TO INDX                                                    
026364       END-PERFORM                                                        
026365                                                                          
026366*-- KOLLA OM DET FINNS ETT 11:E SEGMENT.                                  
026367         PERFORM S02-FINNS-DET-FLERA-SEGMENT                              
026369     END-IF                                                               
026370     .                                                                    
026371     EJECT                                                                
026372 G-KOLLA-INPUT SECTION.                                                   
026373                                                                          
026374     MOVE JA TO INDATA-SW                                                 
026376     MOVE JA TO KDCMD-SW                                                  
026377                                                                          
026378     MOVE +1 TO INDX                                                      
026379     PERFORM UNTIL INDX > MAX-INDX                                        
026380       IF MID-KDCMD (INDX) =  '+'                                         
026381         CONTINUE                                                         
026382       ELSE                                                               
026383         MOVE NEJ TO KDCMD-SW                                             
026384       END-IF                                                             
026385       ADD +1 TO INDX                                                     
026386     END-PERFORM                                                          
026387                                                                          
026388     IF MID-INPUT = ALL '+' AND KDCMD-ALL-PLUS                            
026390       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
026391       CALL WMEDKONV USING MED-WMEDAREA                                   
026392       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026393       PERFORM MFS-ROER-EJ-FAELT-IN                                       
026394       PERFORM MFS-ROER-EJ-FAELT-UT                                       
026395       MOVE NEJ TO INDATA-SW                                              
026396     ELSE                                                                 
026397                                                                          
026398       IF KDCMD-ALL-PLUS                                                  
026399         PERFORM GA-KOLLA-INPUT-RAD19                                     
026400       ELSE                                                               
026511         MOVE +1                           TO INDX                        
026512                                                                          
026513         PERFORM UNTIL INDX  >  MAX-INDX                                  
026514           IF MID-KDCMD (INDX)  NOT = ALL '+'                             
026515             IF MID-KDCMD (INDX)= 'D' OR 'B'                              
026516               IF MID-INPUT = ALL '+'                                     
026517                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)        
026519               ELSE                                                       
026520                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)         
026521                 MOVE NEJ TO INDATA-SW                                    
026522               END-IF                                                     
026523               IF MID-IDDISTR-FOM (INDX) = '9999'                         
026524                 MOVE ERR-UPDATE-NOT-OK  TO MED-IDMFSFEL                  
026525                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)         
026526                 MOVE NEJ TO INDATA-SW                                    
026527               ELSE                                                       
026528                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)        
026529               END-IF                                                     
026531             ELSE                                                         
026532               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)           
026533               MOVE NEJ TO INDATA-SW                                      
026534             END-IF                                                       
026535           END-IF                                                         
026536                                                                          
026537           ADD +1                          TO INDX                        
026538         END-PERFORM                                                      
026539       END-IF                                                             
026540                                                                          
026541       IF KDCMD-NOT-ALL-PLUS AND (MID-INPUT NOT = ALL '+')                
026542         MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                        
026543         MOVE NEJ TO INDATA-SW                                            
026544       END-IF                                                             
026545                                                                          
026546       IF INDATA-FEL                                                      
026547         IF MED-IDMFSFEL  = SPACE                                         
026548           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
026549         END-IF                                                           
026550         CALL WMEDKONV USING MED-WMEDAREA                                 
026551         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
026552         PERFORM MFS-ROER-EJ-FAELT-UT                                     
026553         PERFORM MFS-ROER-EJ-FAELT-IN                                     
026554       ELSE                                                               
026556         PERFORM GB-KOLLA-OM-OK-UPPDATERA                                 
026558                                                                          
026559         IF INDATA-FEL                                                    
026560           IF MED-IDMFSFEL  = SPACE                                       
026561             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
026562           END-IF                                                         
026563           CALL WMEDKONV USING MED-WMEDAREA                               
026564           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
026565           PERFORM MFS-ROER-EJ-FAELT-UT                                   
026566           PERFORM MFS-ROER-EJ-FAELT-IN                                   
026567         END-IF                                                           
026568       END-IF                                                             
026569     END-IF                                                               
026570     .                                                                    
026571     EJECT                                                                
026572 GA-KOLLA-INPUT-RAD19 SECTION.                                            
026573                                                                          
026574     IF MID-KDCMD-RAD-19 = ALL '+'                                        
026575        MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-RAD-19-ATTR                
026576        MOVE NEJ TO INDATA-SW                                             
026577     ELSE                                                                 
026579       IF MID-KDCMD-RAD-19 = 'C' OR 'Ä' OR 'I' OR 'N'                     
026580         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-RAD-19-ATTR               
026581       ELSE                                                               
026582         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-RAD-19-ATTR               
026583         MOVE NEJ TO INDATA-SW                                            
026584       END-IF                                                             
026585     END-IF                                                               
026586                                                                          
026587     IF MID-IDDISTR-FOM-UPD = ALL '+'                                     
026588        MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-ATTR                    
026589        MOVE NEJ TO INDATA-SW                                             
026590     ELSE                                                                 
026591       IF MID-IDDISTR-FOM-UPD NUMERIC                                     
026592         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-ATTR                 
026593       ELSE                                                               
026594         MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-ATTR                   
026595         MOVE NEJ TO INDATA-SW                                            
026596       END-IF                                                             
026597     END-IF                                                               
026598                                                                          
026599     IF MID-IDDISTR-TOM-UPD = ALL '+'                                     
026600        MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-TOM-ATTR                 
026601        MOVE NEJ TO INDATA-SW                                             
026602     ELSE                                                                 
026603       IF MID-IDDISTR-TOM-UPD NUMERIC                                     
026604         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-ATTR                 
026605       ELSE                                                               
026606         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-ATTR                 
026607         MOVE NEJ TO INDATA-SW                                            
026608       END-IF                                                             
026609     END-IF                                                               
026610                                                                          
026611     IF MID-SUKRENOT-FOM-UPD = ALL '+'                                    
026612       MOVE MFS-NUM-FAELT-FEL     TO MOD-SUKRENOT-FOM-ATTR                
026613       MOVE NEJ TO INDATA-SW                                              
026614     ELSE                                                                 
026615       IF MID-SUKRENOT-FOM-UPD NUMERIC                                    
026616         MOVE MFS-NUM-FAELT-RAETT TO MOD-SUKRENOT-FOM-ATTR                
026617       ELSE                                                               
026618         MOVE MFS-NUM-FAELT-FEL   TO MOD-SUKRENOT-FOM-ATTR                
026619         MOVE NEJ TO INDATA-SW                                            
026620       END-IF                                                             
026621     END-IF                                                               
026622                                                                          
026623     IF MID-SUKRENOT-TOM-UPD = ALL '+'                                    
026624       MOVE MFS-NUM-FAELT-FEL     TO MOD-SUKRENOT-TOM-ATTR                
026625       MOVE NEJ TO INDATA-SW                                              
026626     ELSE                                                                 
026627       IF MID-SUKRENOT-TOM-UPD NUMERIC                                    
026628         MOVE MFS-NUM-FAELT-RAETT TO MOD-SUKRENOT-TOM-ATTR                
026629       ELSE                                                               
026630        MOVE MFS-NUM-FAELT-FEL    TO MOD-SUKRENOT-TOM-ATTR                
026631        MOVE NEJ TO INDATA-SW                                             
026632       END-IF                                                             
026633     END-IF                                                               
026634                                                                          
026635     IF MID-IDANSTNR-UPD = ALL '+'                                        
026637       IF MID-KDCMD-RAD-19 = 'I' OR 'N'                                   
026638         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSTNR-ADM-ATTR                
026639         MOVE NEJ TO INDATA-SW                                            
026640       ELSE                                                               
026641         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSTNR-ADM-ATTR                
026644       END-IF                                                             
026645     ELSE                                                                 
026646       IF MID-IDANSTNR-UPD NUMERIC                                        
026647         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSTNR-ADM-ATTR                
026648       ELSE                                                               
026649         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSTNR-ADM-ATTR                
026650         MOVE NEJ TO INDATA-SW                                            
026651       END-IF                                                             
026652     END-IF                                                               
026653                                                                          
026654     IF MID-BEANST-UPD = ALL '+'                                          
026655       IF MID-KDCMD-RAD-19 = 'I' OR 'N'                                   
026656         MOVE MFS-ALFA-FAELT-FEL   TO MOD-BEANST-ATTR                     
026657         MOVE NEJ TO INDATA-SW                                            
026658       ELSE                                                               
026659         MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEANST-ATTR                     
026660       END-IF                                                             
026661     ELSE                                                                 
026662       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-BEANST-ATTR                     
026663     END-IF                                                               
026664                                                                          
026665     IF MID-FLKREPRT-UPD = ALL '+'                                        
026666       IF MID-KDCMD-RAD-19 = 'I' OR 'N'                                   
026668         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKREPRT-ATTR                   
026669         MOVE NEJ TO INDATA-SW                                            
026670       ELSE                                                               
026671         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKREPRT-ATTR                   
026672       END-IF                                                             
026673     ELSE                                                                 
026674       IF MID-FLKREPRT-UPD = 'J' or 'Y' or 'N'                            
026675         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKREPRT-ATTR                   
026678       ELSE                                                               
026679         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKREPRT-ATTR                   
026680         MOVE NEJ TO INDATA-SW                                            
026681       END-IF                                                             
026682     END-IF                                                               
026683                                                                          
026696     .                                                                    
026697     EJECT                                                                
026698 GB-KOLLA-OM-OK-UPPDATERA SECTION.                                        
026699                                                                          
026700     PERFORM IMS-GU-WDR101                                                
026701                                                                          
026702     IF MID-INPUT NOT = ALL '+'                                           
026703                                                                          
026704       MOVE MID-IDDISTR-FOM-UPD  TO W-4124-IDDISTR-FOM                    
026705       MOVE MID-IDDISTR-TOM-UPD  TO W-4124-IDDISTR-TOM                    
026706       MOVE MID-SUKRENOT-FOM-UPD TO W-4126-SUKRENOT-FOM                   
026708       IF MID-SUKRENOT-TOM-UPD = '999999'                                 
026709         MOVE '9999999'            TO W-4126-SUKRENOT-TOM                 
026710       ELSE                                                               
026711         MOVE MID-SUKRENOT-TOM-UPD TO W-4126-SUKRENOT-TOM                 
026712       END-IF                                                             
026713                                                                          
026714       PERFORM IMS-GNP-WDGX4126-KVAL                                      
026715                                                                          
026717       IF MID-KDCMD-RAD-19 = 'C' OR 'Ä'                                   
026718         IF SEGMENT-FINNS                                                 
026719           CONTINUE                                                       
026720         ELSE                                                             
026721           MOVE ERR-URVAL-SAKNAS   TO MED-IDMFSFEL                        
026722           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-FOM-ATTR                
026723           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-TOM-ATTR                
026724           MOVE MFS-NUM-FAELT-FEL  TO MOD-SUKRENOT-FOM-ATTR               
026725           MOVE MFS-NUM-FAELT-FEL  TO MOD-SUKRENOT-TOM-ATTR               
026727           MOVE NEJ TO INDATA-SW                                          
026728         END-IF                                                           
026729       END-IF                                                             
026730                                                                          
026731       IF MID-KDCMD-RAD-19 = 'I' OR 'N'                                   
026732         IF SEGMENT-FINNS                                                 
026733           MOVE ERR-RAD-FINNS-REDAN TO MED-IDMFSFEL                       
026734           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-RAD-19-ATTR              
026735           MOVE NEJ TO INDATA-SW                                          
026736         ELSE                                                             
026737           IF MID-IDDISTR-FOM-UPD > MID-IDDISTR-TOM-UPD                   
026738             MOVE ERR-FOM-VARDE-FEL       TO MED-IDMFSFEL                 
026739             MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-FOM-ATTR         
026740             MOVE NEJ                     TO INDATA-SW                    
026741           ELSE                                                           
026742             IF MID-SUKRENOT-FOM-UPD > MID-SUKRENOT-TOM-UPD               
026743               MOVE ERR-FOM-VARDE-FEL     TO MED-IDMFSFEL                 
026744               MOVE MFS-NUM-FAELT-FEL     TO MOD-SUKRENOT-FOM-ATTR        
026745               MOVE NEJ                   TO INDATA-SW                    
026746             ELSE                                                         
026747                                                                          
026748               PERFORM IMS-GNP-FIRST-WDGX4124-KVAL                        
026750               IF SEGMENT-SAKNAS                                          
026751                 PERFORM GBA-KOLLA-DISTR-INTERVALL                        
026752               ELSE                                                       
026753                 PERFORM GBB-KOLLA-VARDE-INTERVALL                        
026754               END-IF                                                     
026755             END-IF                                                       
026756           END-IF                                                         
026757         END-IF                                                           
026758       END-IF                                                             
026759                                                                          
026760     END-IF                                                               
026761     .                                                                    
026770     EJECT                                                                
026791 GBA-KOLLA-DISTR-INTERVALL SECTION.                                       
026792                                                                          
026793*- KOLLAR SÅ ATT INTERVALLET INTE KROCKAR MED BEFINTLIGA.                 
026794                                                                          
026795     MOVE JA  TO INTERVALL-SW                                             
026796                                                                          
026797     MOVE MID-IDDISTR-FOM-UPD  TO WS-MID-IDDISTR-FOM-UPD                  
026798     MOVE MID-IDDISTR-TOM-UPD  TO WS-MID-IDDISTR-TOM-UPD                  
026799                                                                          
026800     PERFORM IMS-GNP-FIRST-WDGX4124-OKVAL                                 
026801*-   PERFORM IMS-GNP-WDGX4124-OKVAL                                       
026802     PERFORM UNTIL SEGMENT-SAKNAS                                         
026803                                                                          
026804       IF ( 4124-IDDISTR-FOM >= WS-MID-IDDISTR-FOM-UPD AND                
026805            4124-IDDISTR-FOM <= WS-MID-IDDISTR-TOM-UPD ) OR               
026806                                                                          
026807          ( 4124-IDDISTR-TOM >= WS-MID-IDDISTR-FOM-UPD AND                
026808            4124-IDDISTR-TOM <= WS-MID-IDDISTR-TOM-UPD ) OR               
026809                                                                          
026810          ( 4124-IDDISTR-FOM >= WS-MID-IDDISTR-FOM-UPD AND                
026820            4124-IDDISTR-TOM <= WS-MID-IDDISTR-TOM-UPD ) OR               
026830                                                                          
026831          ( 4124-IDDISTR-FOM <= WS-MID-IDDISTR-FOM-UPD AND                
026832            4124-IDDISTR-TOM >= WS-MID-IDDISTR-FOM-UPD ) OR               
026833                                                                          
026834          ( 4124-IDDISTR-FOM <= WS-MID-IDDISTR-TOM-UPD AND                
026835            4124-IDDISTR-TOM >= WS-MID-IDDISTR-TOM-UPD ) OR               
026836                                                                          
026837          ( 4124-IDDISTR-FOM <  WS-MID-IDDISTR-FOM-UPD AND                
026838            4124-IDDISTR-TOM >  WS-MID-IDDISTR-TOM-UPD )                  
026840                                                                          
026841         MOVE NEJ  TO INTERVALL-SW                                        
026851                                                                          
026852       END-IF                                                             
026853                                                                          
026854       PERFORM IMS-GNP-WDGX4124-OKVAL                                     
026855     END-PERFORM                                                          
026856                                                                          
026857     IF INTERVALL-FEL                                                     
026858       MOVE ERR-INTERVALL-KROCKAR TO MED-IDMFSFEL                         
026859       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-FOM-ATTR                    
026860       MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-TOM-ATTR                    
026861       MOVE NEJ TO INDATA-SW                                              
026862     END-IF                                                               
026863                                                                          
026864     .                                                                    
026865     EJECT                                                                
026866 GBB-KOLLA-VARDE-INTERVALL SECTION.                                       
026867                                                                          
026868     MOVE JA  TO INTERVALL-SW                                             
026869                                                                          
026870     MOVE MID-SUKRENOT-FOM-UPD TO WS-MID-SUKRENOT-FOM-UPD                 
026872     IF MID-SUKRENOT-TOM-UPD = '999999'                                   
026873       MOVE '9999999'            TO WS-MID-SUKRENOT-TOM-UPD               
026874     ELSE                                                                 
026875       MOVE MID-SUKRENOT-TOM-UPD TO WS-MID-SUKRENOT-TOM-UPD               
026876     END-IF                                                               
026877                                                                          
026878     PERFORM IMS-GNP-WDGX4126                                             
026879     PERFORM UNTIL SEGMENT-SAKNAS                                         
026880                                                                          
026881       IF ( 4126-SUKRENOT-FOM >= WS-MID-SUKRENOT-FOM-UPD AND              
026882            4126-SUKRENOT-FOM <= WS-MID-SUKRENOT-TOM-UPD ) OR             
026883                                                                          
026884          ( 4126-SUKRENOT-TOM >= WS-MID-SUKRENOT-FOM-UPD AND              
026885            4126-SUKRENOT-TOM <= WS-MID-SUKRENOT-TOM-UPD ) OR             
026886                                                                          
026887          ( 4126-SUKRENOT-FOM >= WS-MID-SUKRENOT-FOM-UPD AND              
026888            4126-SUKRENOT-TOM <= WS-MID-SUKRENOT-TOM-UPD ) OR             
026889                                                                          
026890          ( 4126-SUKRENOT-FOM <= WS-MID-SUKRENOT-FOM-UPD AND              
026891            4126-SUKRENOT-TOM >= WS-MID-SUKRENOT-FOM-UPD ) OR             
026892                                                                          
026893          ( 4126-SUKRENOT-FOM <= WS-MID-SUKRENOT-TOM-UPD AND              
026894            4126-SUKRENOT-TOM >= WS-MID-SUKRENOT-TOM-UPD ) OR             
026895                                                                          
026896          ( 4126-SUKRENOT-FOM > WS-MID-SUKRENOT-FOM-UPD AND               
026897            4126-SUKRENOT-TOM < WS-MID-SUKRENOT-TOM-UPD )                 
026898                                                                          
026899         MOVE NEJ  TO INTERVALL-SW                                        
026900       END-IF                                                             
026901                                                                          
026902       PERFORM IMS-GNP-WDGX4126                                           
026903     END-PERFORM                                                          
026904                                                                          
026905     IF INTERVALL-FEL                                                     
026906       MOVE ERR-INTERVALL-KROCKAR TO MED-IDMFSFEL                         
026907       MOVE MFS-NUM-FAELT-FEL  TO MOD-SUKRENOT-FOM-ATTR                   
026908       MOVE MFS-NUM-FAELT-FEL  TO MOD-SUKRENOT-TOM-ATTR                   
026909       MOVE NEJ TO INDATA-SW                                              
026910     END-IF                                                               
026911                                                                          
026912     .                                                                    
026913     EJECT                                                                
026914 H-UPPDATERA SECTION.                                                     
026915                                                                          
026916     IF MID-INPUT = ALL '+'                                               
026917                                                                          
026918       MOVE +1 TO INDX                                                    
026919       PERFORM UNTIL INDX > MAX-INDX                                      
026920                                                                          
026921         IF MID-KDCMD (INDX) NOT = '+'                                    
026922           MOVE MID-IDDISTR-FOM (INDX)    TO W-IDDISTR-FOM                
026923                                             W-4124-IDDISTR-FOM           
026924           MOVE MID-IDDISTR-TOM (INDX)    TO W-IDDISTR-TOM                
026925                                             W-4124-IDDISTR-TOM           
026926           MOVE MID-SUKRENOT-FOM (INDX)   TO W-SUKRENOT-FOM               
026928           IF MID-SUKRENOT-TOM (INDX) = '999999'                          
026929             MOVE '9999999'               TO W-SUKRENOT-TOM               
026930           ELSE                                                           
026931             MOVE MID-SUKRENOT-TOM (INDX) TO W-SUKRENOT-TOM               
026932           END-IF                                                         
026933           PERFORM IMS-GHU-WDGX4126                                       
026934           IF SEGMENT-FINNS                                               
026935             PERFORM IMS-DLET-WDGX4126                                    
026936             PERFORM HC-KOLLA-DLET-WGDX4124                               
026937           END-IF                                                         
026938         END-IF                                                           
026939         ADD +1                          TO INDX                          
026940       END-PERFORM                                                        
026941                                                                          
026942       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
026943       CALL WMEDKONV USING MED-WMEDAREA                                   
026944       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
026945       PERFORM MFS-FORM-ATTR                                              
026946       PERFORM MFS-RENSA-FAELT-IN                                         
026947                                                                          
026948     ELSE                                                                 
026950       IF MID-KDCMD-RAD-19 = 'C' OR 'Ä'                                   
026951         PERFORM HA-REPLACE-TABELLRAD                                     
026952       ELSE                                                               
026953         IF MID-KDCMD-RAD-19 = 'I' OR 'N'                                 
026954           PERFORM HB-INSERT-TABELLRAD                                    
026956         END-IF                                                           
026957       END-IF                                                             
026958                                                                          
026959       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
026960       CALL WMEDKONV USING MED-WMEDAREA                                   
026961       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
026962       PERFORM MFS-FORM-ATTR                                              
026963       PERFORM MFS-RENSA-FAELT-IN                                         
026964* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
026965                                                                          
026966     END-IF                                                               
026967     .                                                                    
026968     EJECT                                                                
026969 HA-REPLACE-TABELLRAD SECTION.                                            
026970                                                                          
026971     MOVE MID-IDDISTR-FOM-UPD     TO W-IDDISTR-FOM                        
026972                                     w-4124-IDDISTR-FOM                   
026973     MOVE MID-IDDISTR-TOM-UPD     TO W-IDDISTR-TOM                        
026975                                     W-4124-IDDISTR-TOM                   
026976     MOVE MID-SUKRENOT-FOM-UPD    TO W-SUKRENOT-FOM                       
026978     IF MID-SUKRENOT-TOM-UPD = '999999'                                   
026979       MOVE '9999999'             TO W-SUKRENOT-TOM                       
026980     ELSE                                                                 
026981       MOVE MID-SUKRENOT-TOM-UPD  TO W-SUKRENOT-TOM                       
026982     END-IF                                                               
026983                                                                          
026984     PERFORM IMS-GHU-WDGX4126                                             
026985     IF SEGMENT-FINNS                                                     
026986                                                                          
026987       IF MID-IDANSTNR-UPD NOT = ALL '+'                                  
026988         MOVE MID-IDANSTNR-UPD    TO WS-IDANSTNR                          
026989         MOVE WS-IDUSER           TO 4126-IDUSER-ADM                      
026990       END-IF                                                             
026991                                                                          
026992       IF MID-BEANST-UPD NOT = ALL '+'                                    
026993         MOVE MID-BEANST-UPD      TO 4126-BEANST                          
026994       END-IF                                                             
026995                                                                          
026996       IF MID-FLKREPRT-UPD NOT = ALL '+'                                  
026997         MOVE MID-FLKREPRT-UPD    TO 4126-FLKREPRT                        
026998       END-IF                                                             
026999                                                                          
027000       MOVE MSG-SIGNON-USERID     TO 4126-IDUSER-ATTUPD                   
027001                                                                          
027002       ACCEPT 4126-TIUPPDAT FROM DATE                                     
027010                                                                          
027014       PERFORM IMS-REPL-WDGX4126                                          
027015                                                                          
027017     END-IF                                                               
027018                                                                          
027019     .                                                                    
027020     EJECT                                                                
027021 HB-INSERT-TABELLRAD SECTION.                                             
027022                                                                          
027023     MOVE MID-IDDISTR-FOM-UPD     TO 4124-IDDISTR-FOM                     
027024                                     W-4124-IDDISTR-FOM                   
027025     MOVE MID-IDDISTR-TOM-UPD     TO 4124-IDDISTR-TOM                     
027026                                     W-4124-IDDISTR-TOM                   
027027                                                                          
027028     PERFORM IMS-ISRT-WDGX4124                                            
027029                                                                          
027030     MOVE MID-SUKRENOT-FOM-UPD    TO 4126-SUKRENOT-FOM                    
027032     IF MID-SUKRENOT-TOM-UPD  = '999999'                                  
027033       MOVE '9999999'             TO 4126-SUKRENOT-TOM                    
027034     ELSE                                                                 
027035       MOVE MID-SUKRENOT-TOM-UPD  TO 4126-SUKRENOT-TOM                    
027036     END-IF                                                               
027037     MOVE MID-BEANST-UPD          TO 4126-BEANST                          
027038     MOVE MID-FLKREPRT-UPD        TO 4126-FLKREPRT                        
027039                                                                          
027040     MOVE MID-IDANSTNR-UPD        TO WS-IDANSTNR                          
027041     MOVE WS-IDUSER               TO 4126-IDUSER-ADM                      
027042                                                                          
027049     MOVE MSG-SIGNON-USERID       TO 4126-IDUSER-ATTUPD                   
027050                                                                          
027051     ACCEPT 4126-TIUPPDAT FROM DATE                                       
027065                                                                          
027066     PERFORM IMS-ISRT-WDGX4126                                            
027067                                                                          
027069     .                                                                    
027070     EJECT                                                                
027071 HC-KOLLA-DLET-WGDX4124 SECTION.                                          
027072*-- OM INGA VÄRDERADER FINNS KVAR SKALL DISTR.-INTERVALLET                
027073*-- TAS BORT.                                                             
027074                                                                          
027075     PERFORM IMS-GU-WDGX4124                                              
027076                                                                          
027077     PERFORM IMS-GNP-WDGX4126                                             
027078                                                                          
027079     IF SEGMENT-FINNS                                                     
027080       CONTINUE                                                           
027081     ELSE                                                                 
027082       PERFORM IMS-GHU-WDGX4124                                           
027083       PERFORM IMS-DLET-WDGX4124                                          
027084     END-IF                                                               
027085                                                                          
027086     .                                                                    
027087     EJECT                                                                
027088 S01-FLYTTA-DATA-TILL-MOD SECTION.                                        
027089                                                                          
027090     MOVE 4124-IDDISTR-FOM  TO MOD-IDDISTR-FOM (INDX)                     
027091     MOVE 4124-IDDISTR-TOM  TO MOD-IDDISTR-TOM (INDX)                     
027092     MOVE 4126-SUKRENOT-FOM TO MOD-SUKRENOT-FOM(INDX)                     
027093     MOVE 4126-SUKRENOT-TOM  TO MOD-SUKRENOT-TOM(INDX)                    
027094     MOVE 4126-IDUSER-ADM(3:5) TO MOD-IDANSTNR-ADM(INDX)                  
027095     MOVE 4126-BEANST       TO MOD-BEANST      (INDX)                     
027096     MOVE 4126-FLKREPRT     TO MOD-FLKREPRT    (INDX)                     
027098     MOVE 4126-IDUSER-ATTUPD(3:5) TO                                      
027099                               MOD-IDANSTNR-ATTUPD(INDX)                  
027100     MOVE 4126-TIUPPDAT     TO MOD-TIUPPDAT    (INDX)                     
027101                                                                          
027102     .                                                                    
027103     EJECT                                                                
027104 S02-FINNS-DET-FLERA-SEGMENT SECTION.                                     
027105                                                                          
027106     IF SEGMENT-FINNS                                                     
027107       MOVE W-IDDISTR          TO SPAR-IDDISTR-NEXT                       
027108       MOVE W-IDUSER           TO SPAR-IDUSER-NEXT                        
027109       MOVE 4124-IDDISTR-FOM   TO SPAR-IDDISTR-FOM-NEXT                   
027110       MOVE 4124-IDDISTR-TOM   TO SPAR-IDDISTR-TOM-NEXT                   
027111       MOVE 4126-SUKRENOT-FOM  TO SPAR-SUKRENOT-FOM-NEXT                  
027112       MOVE 4126-SUKRENOT-TOM  TO SPAR-SUKRENOT-TOM-NEXT                  
027113       MOVE WS-IDDISTR-IN      TO SPAR-MID-IDDISTR                        
027114       MOVE WS-IDANSTNR-IN     TO SPAR-MID-IDANSTNR                       
027115       IF MED-IDMFSINF = SPACE OR '006'                                   
027116         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
027117         CALL WMEDKONV USING MED-WMEDAREA                                 
027118         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
027119      END-IF                                                              
027120     ELSE                                                                 
027121       MOVE W-IDDISTR         TO SPAR-IDDISTR-NEXT                        
027122       MOVE W-IDUSER          TO SPAR-IDUSER-NEXT                         
027123       MOVE 4124-IDDISTR-FOM  TO SPAR-IDDISTR-FOM-NEXT                    
027124       MOVE 4124-IDDISTR-TOM  TO SPAR-IDDISTR-TOM-NEXT                    
027125       MOVE 4126-SUKRENOT-FOM TO SPAR-SUKRENOT-FOM-NEXT                   
027126       MOVE 4126-SUKRENOT-TOM  TO SPAR-SUKRENOT-TOM-NEXT                  
027127       MOVE WS-IDDISTR-IN     TO SPAR-MID-IDDISTR                         
027128       MOVE WS-IDANSTNR-IN    TO SPAR-MID-IDANSTNR                        
027129       IF MED-IDMFSINF = SPACE                                            
027130         MOVE INF-LAST-PAGE  TO MED-IDMFSINF                              
027131         CALL WMEDKONV USING MED-WMEDAREA                                 
027132         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
027133       END-IF                                                             
027134     END-IF                                                               
027135                                                                          
027136     MOVE '002'      TO MSGI-KDCALL                                       
027137     MOVE '4708'     TO SPAR-IDTRANS                                      
027138     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
027139     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
027140                                                                          
027141     .                                                                    
027142     EJECT                                                                
027154 MFS-RENSA-FAELT-UT SECTION.                                              
027158                                                                          
027159     MOVE +1 TO INDX                                                      
027160     PERFORM UNTIL INDX > MAX-INDX                                        
027161       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
027162       ADD +1 TO INDX                                                     
027170     END-PERFORM                                                          
027200     .                                                                    
027201     SKIP3                                                                
027202 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
027203                                                                          
027204*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027207     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM (INDX)                       
027208                             MOD-IDDISTR-TOM (INDX)                       
027209                             MOD-SUKRENOT-FOM(INDX)                       
027210                             MOD-SUKRENOT-TOM(INDX)                       
027211                             MOD-IDANSTNR-ADM(INDX)                       
027212                             MOD-BEANST      (INDX)                       
027213                             MOD-FLKREPRT    (INDX)                       
027215                             MOD-IDANSTNR-ATTUPD(INDX)                    
027216                             MOD-TIUPPDAT    (INDX)                       
027220     .                                                                    
027300     SKIP3                                                                
027400 MFS-RENSA-FAELT-IN SECTION.                                              
027500                                                                          
027600*    --- ALLA INDATA-FÄLT                                                 
027700     MOVE MFS-RENSA-FAELT TO MOD-KDCMD-RAD-19-UPD                         
027800                             MOD-IDDISTR-FOM-UPD                          
027801                             MOD-IDDISTR-TOM-UPD                          
027802                             MOD-SUKRENOT-FOM-UPD                         
027803                             MOD-SUKRENOT-TOM-UPD                         
027804                             MOD-IDANSTNR-ADM-UPD                         
027805                             MOD-BEANST-UPD                               
027806                             MOD-FLKREPRT-UPD                             
027808                                                                          
027810     MOVE +1 TO INDX                                                      
027820     PERFORM UNTIL INDX > MAX-INDX                                        
027830       MOVE MFS-RENSA-FAELT TO MOD-KDCMD-UPD (INDX)                       
027840       ADD +1 TO INDX                                                     
027850     END-PERFORM                                                          
027900     .                                                                    
028000     EJECT                                                                
028100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
028200                                                                          
028300*    --- ALLA UTDATA-FÄLT                                                 
028410*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
028701     MOVE +1 TO INDX                                                      
028702     PERFORM UNTIL INDX > MAX-INDX                                        
028704       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM (INDX)                   
028705                                 MOD-IDDISTR-TOM (INDX)                   
028706                                 MOD-SUKRENOT-FOM(INDX)                   
028707                                 MOD-SUKRENOT-TOM(INDX)                   
028708                                 MOD-IDANSTNR-ADM(INDX)                   
028709                                 MOD-BEANST      (INDX)                   
028710                                 MOD-FLKREPRT    (INDX)                   
028712                                 MOD-IDANSTNR-ATTUPD(INDX)                
028713                                 MOD-TIUPPDAT    (INDX)                   
028714       ADD +1 TO INDX                                                     
028715     END-PERFORM                                                          
028717     .                                                                    
028718     EJECT                                                                
029000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
029100                                                                          
029200*    --- ALLA INDATA-FÄLT                                                 
029300     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-RAD-19-UPD                       
029400                               MOD-IDDISTR-FOM-UPD                        
029500                               MOD-IDDISTR-TOM-UPD                        
029510                               MOD-SUKRENOT-FOM-UPD                       
029520                               MOD-SUKRENOT-TOM-UPD                       
029530                               MOD-IDANSTNR-ADM-UPD                       
029540                               MOD-BEANST-UPD                             
029550                               MOD-FLKREPRT-UPD                           
029601                                                                          
029602     MOVE +1 TO INDX                                                      
029603     PERFORM UNTIL INDX > MAX-INDX                                        
029604       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-UPD (INDX)                     
029605       ADD +1 TO INDX                                                     
029606     END-PERFORM                                                          
029610     .                                                                    
029620     EJECT                                                                
029700 MFS-FORM-ATTR SECTION.                                                   
029800                                                                          
029900*    --- ALLA INDATA-FÄLT                                                 
030110     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-RAD-19-ATTR                     
030120                                MOD-IDDISTR-FOM-ATTR                      
030130                                MOD-IDDISTR-TOM-ATTR                      
030140                                MOD-SUKRENOT-FOM-ATTR                     
030150                                MOD-SUKRENOT-TOM-ATTR                     
030160                                MOD-IDANSTNR-ADM-ATTR                     
030170                                MOD-BEANST-ATTR                           
030180                                MOD-FLKREPRT-ATTR                         
030191                                                                          
030192     MOVE +1 TO INDX                                                      
030193     PERFORM UNTIL INDX > MAX-INDX                                        
030194       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (INDX)                   
030195       ADD +1 TO INDX                                                     
030196     END-PERFORM                                                          
030200     .                                                                    
030300     SKIP2                                                                
031100* --- IMS SEKTIONER ---                                                   
031200     SKIP3                                                                
031300 IMS-GET-MSG SECTION.                                                     
031400                                                                          
031500     MOVE '  QC' TO GODK-STATUSKODER                                      
031600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
031700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031800     PERFORM IMS-STATUSKONTROLL                                           
031900     .                                                                    
032000     SKIP3                                                                
032100 IMS-INSERT-MSG SECTION.                                                  
032200                                                                          
032600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032700     MOVE SPACE TO GODK-STATUSKODER                                       
032800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033000     PERFORM IMS-STATUSKONTROLL                                           
033100     .                                                                    
033201     EJECT                                                                
033202 IMS-GU-WDR101 SECTION.                                                   
033203                                                                          
033204     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
033205            DELIMITED BY SIZE INTO SSA1                                   
033208     MOVE '  ' TO GODK-STATUSKODER                                        
033209     CALL CBLTDLI USING GU 4123-PCB DLI-IO-WDR101 SSA1                    
033210     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033211     PERFORM IMS-STATUSKONTROLL                                           
033212     .                                                                    
033213     EJECT                                                                
033214 IMS-GU-PATH-WDR101 SECTION.                                              
033215                                                                          
033216     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
033217            DELIMITED BY SIZE INTO SSA1                                   
033218     MOVE '  ' TO GODK-STATUSKODER                                        
033220     CALL CBLTDLI USING GU 4123-P-PCB DLI-IO-AREA01 SSA1                  
033222     MOVE 4123-P-STATUS-CODE TO STATUS-WS                                 
033223     PERFORM IMS-STATUSKONTROLL                                           
033224     .                                                                    
033225     EJECT                                                                
033237 IMS-GU-WDGX4124 SECTION.                                                 
033238                                                                          
033239     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
033240            DELIMITED BY SIZE INTO SSA1                                   
033241     STRING 'WDGX4124(IDDISTRF =' W-IDDISTR-FOM-X                         
033242                    '&IDDISTRT =' W-IDDISTR-TOM-X ')'                     
033243            DELIMITED BY SIZE INTO SSA2                                   
033248     MOVE '  GE' TO GODK-STATUSKODER                                      
033249     CALL CBLTDLI USING GU 4123-PCB DLI-IO-WDGX4124 SSA1 SSA2             
033250     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033251     PERFORM IMS-STATUSKONTROLL                                           
033252     .                                                                    
033253     SKIP3                                                                
033254 IMS-GNP-WDGX4124-OKVAL SECTION.                                          
033255                                                                          
033256     MOVE 'WDGX4124 '      TO SSA1                                        
033261     MOVE '  GE' TO GODK-STATUSKODER                                      
033262     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4124 SSA1                 
033263     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033264     PERFORM IMS-STATUSKONTROLL                                           
033265     .                                                                    
033266     SKIP3                                                                
033267 IMS-GNP-FIRST-WDGX4124-OKVAL SECTION.                                    
033268                                                                          
033269     MOVE 'WDGX4124*F'      TO SSA1                                       
033270     MOVE '  GE' TO GODK-STATUSKODER                                      
033271     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4124 SSA1                 
033272     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033273     PERFORM IMS-STATUSKONTROLL                                           
033274     .                                                                    
033275     SKIP3                                                                
033276 IMS-GNP-WDGX4124 SECTION.                                                
033277                                                                          
033278     STRING 'WDGX4124(IDDISTRF<=' W-IDDISTR-X                             
033279                    '&IDDISTRT>=' W-IDDISTR-X ')'                         
033280            DELIMITED BY SIZE INTO SSA1                                   
033281     MOVE '  GE' TO GODK-STATUSKODER                                      
033282     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4124 SSA1                 
033283     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033284     PERFORM IMS-STATUSKONTROLL                                           
033285     .                                                                    
033286     SKIP3                                                                
033287 IMS-GNP-WDGX4124-KVAL  SECTION.                                          
033288                                                                          
033289     STRING 'WDGX4124(KEY4124  =' W-KEY4124-X ')'                         
033290            DELIMITED BY SIZE INTO SSA1                                   
033291     MOVE '  GE' TO GODK-STATUSKODER                                      
033292     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4124 SSA1                 
033293     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033294     PERFORM IMS-STATUSKONTROLL                                           
033295     .                                                                    
033296     SKIP3                                                                
033297 IMS-GNP-FIRST-WDGX4124-KVAL  SECTION.                                    
033298                                                                          
033299     STRING 'WDGX4124*F(KEY4124  =' W-KEY4124-X ')'                       
033300            DELIMITED BY SIZE INTO SSA1                                   
033301     MOVE '  GE' TO GODK-STATUSKODER                                      
033302     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4124 SSA1                 
033303     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033304     PERFORM IMS-STATUSKONTROLL                                           
033305     .                                                                    
033306     SKIP3                                                                
033307 IMS-GNP-WDGX4126-KVAL SECTION.                                           
033308                                                                          
033309     STRING 'WDGX4124(KEY4124  =' W-KEY4124-X ')'                         
033310          DELIMITED BY SIZE INTO SSA1                                     
033311     STRING 'WDGX4126(KEY4126  =' W-KEY4126-X ')'                         
033312          DELIMITED BY SIZE INTO SSA2                                     
033313     MOVE '  GE' TO GODK-STATUSKODER                                      
033314     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4126 SSA1 SSA2            
033315     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033316     PERFORM IMS-STATUSKONTROLL                                           
033317     .                                                                    
033318     SKIP3                                                                
033319 IMS-GNP-PATH-WDGX4126-KVAL SECTION.                                      
033320                                                                          
033321     STRING 'WDGX4124*D(KEY4124  =' W-KEY4124-X ')'                       
033322          DELIMITED BY SIZE INTO SSA1                                     
033323     STRING 'WDGX4126(KEY4126  =' W-KEY4126-X ')'                         
033324          DELIMITED BY SIZE INTO SSA2                                     
033325     MOVE '  GE' TO GODK-STATUSKODER                                      
033326     CALL CBLTDLI USING GNP 4123-P-PCB DLI-IO-AREA SSA1 SSA2              
033327     MOVE 4123-P-STATUS-CODE TO STATUS-WS                                 
033328     PERFORM IMS-STATUSKONTROLL                                           
033329     .                                                                    
033330     SKIP3                                                                
033331 IMS-GNP-WDGX4126 SECTION.                                                
033332                                                                          
033333     STRING 'WDGX4124(KEY4124  =' W-KEY4124-X ')'                         
033334          DELIMITED BY SIZE INTO SSA1                                     
033335     MOVE 'WDGX4126 ' TO SSA2                                             
033336     MOVE '  GE' TO GODK-STATUSKODER                                      
033337     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4126 SSA1 SSA2            
033338     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033339     PERFORM IMS-STATUSKONTROLL                                           
033340     .                                                                    
033341     SKIP3                                                                
033342 IMS-GNP-WDGX4126-IDUSER SECTION.                                         
033343                                                                          
033344     STRING 'WDGX4124(KEY4124  =' W-KEY4124-X ')'                         
033345          DELIMITED BY SIZE INTO SSA1                                     
033346     STRING 'WDGX4126(IDUSERAD =' W-IDUSER-X ')'                          
033347          DELIMITED BY SIZE INTO SSA2                                     
033348     MOVE '  GE' TO GODK-STATUSKODER                                      
033349     CALL CBLTDLI USING GNP 4123-PCB DLI-IO-WDGX4126 SSA1 SSA2            
033350     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033351     PERFORM IMS-STATUSKONTROLL                                           
033352     .                                                                    
033353     SKIP3                                                                
033354 IMS-GNP-PATH-WDGX4126 SECTION.                                           
033355                                                                          
033356     MOVE 'WDGX4124*D' TO SSA1                                            
033357     STRING 'WDGX4126(IDUSERAD =' W-IDUSER-X ')'                          
033358          DELIMITED BY SIZE INTO SSA2                                     
033359     MOVE '  GE' TO GODK-STATUSKODER                                      
033360     CALL CBLTDLI USING GNP 4123-P-PCB DLI-IO-AREA SSA1 SSA2              
033361     MOVE 4123-P-STATUS-CODE TO STATUS-WS                                 
033362     PERFORM IMS-STATUSKONTROLL                                           
033363     .                                                                    
033364     SKIP3                                                                
033365 IMS-GHU-WDGX4124 SECTION.                                                
033366                                                                          
033367     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
033368            DELIMITED BY SIZE INTO SSA1                                   
033369     STRING 'WDGX4124(IDDISTRF =' W-IDDISTR-FOM-X                         
033370                    '&IDDISTRT =' W-IDDISTR-TOM-X ')'                     
033371            DELIMITED BY SIZE INTO SSA2                                   
033372     MOVE '  GE' TO GODK-STATUSKODER                                      
033373     CALL CBLTDLI USING GHU 4123-PCB DLI-IO-WDGX4124                      
033374                                               SSA1 SSA2                  
033375     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033376     PERFORM IMS-STATUSKONTROLL                                           
033377     .                                                                    
033378     SKIP3                                                                
033379 IMS-GHU-WDGX4126 SECTION.                                                
033380                                                                          
033381     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
033382            DELIMITED BY SIZE INTO SSA1                                   
033383     STRING 'WDGX4124(IDDISTRF =' W-IDDISTR-FOM-X                         
033384                    '&IDDISTRT =' W-IDDISTR-TOM-X ')'                     
033385            DELIMITED BY SIZE INTO SSA2                                   
033386     STRING 'WDGX4126(SUKRENOF =' W-SUKRENOT-FOM-X                        
033387                    '&SUKRENOT =' W-SUKRENOT-TOM-X ')'                    
033388            DELIMITED BY SIZE INTO SSA3                                   
033389     MOVE '  GE' TO GODK-STATUSKODER                                      
033390     CALL CBLTDLI USING GHU 4123-PCB DLI-IO-WDGX4126                      
033391                                               SSA1 SSA2 SSA3             
033392     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033393     PERFORM IMS-STATUSKONTROLL                                           
033394     .                                                                    
033395     SKIP3                                                                
033396 IMS-ISRT-WDGX4124 SECTION.                                               
033397                                                                          
033398     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
033399            DELIMITED BY SIZE INTO SSA1                                   
033400     MOVE 'WDGX4124 ' TO SSA2                                             
033401     MOVE '  II' TO GODK-STATUSKODER                                      
033402     CALL CBLTDLI USING ISRT 4123-PCB DLI-IO-WDGX4124 SSA1 SSA2           
033403     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033404     PERFORM IMS-STATUSKONTROLL                                           
033405     .                                                                    
033406     SKIP3                                                                
033415 IMS-DLET-WDGX4124 SECTION.                                               
033416                                                                          
033417     MOVE '  ' TO GODK-STATUSKODER                                        
033418     CALL CBLTDLI USING DLET 4123-PCB DLI-IO-WDGX4124                     
033419     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033420     PERFORM IMS-STATUSKONTROLL                                           
033421     .                                                                    
033422     EJECT                                                                
033433 IMS-ISRT-WDGX4126 SECTION.                                               
033434                                                                          
033435     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4123-X ')'                    
033436            DELIMITED BY SIZE INTO SSA1                                   
033437     STRING 'WDGX4124(KEY4124  =' W-KEY4124-X ')'                         
033438          DELIMITED BY SIZE INTO SSA2                                     
033439     MOVE 'WDGX4126 ' TO SSA3                                             
033440     MOVE '  II' TO GODK-STATUSKODER                                      
033441     CALL CBLTDLI USING ISRT 4123-PCB DLI-IO-WDGX4126 SSA1 SSA2           
033442                                                      SSA3                
033443     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033444     PERFORM IMS-STATUSKONTROLL                                           
033445     .                                                                    
033446     SKIP3                                                                
033447 IMS-REPL-WDGX4126 SECTION.                                               
033448                                                                          
033449     MOVE '  ' TO GODK-STATUSKODER                                        
033450     CALL CBLTDLI USING REPL 4123-PCB DLI-IO-WDGX4126                     
033451     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033452     PERFORM IMS-STATUSKONTROLL                                           
033453     .                                                                    
033454     SKIP3                                                                
033455 IMS-DLET-WDGX4126 SECTION.                                               
033456                                                                          
033457     MOVE '  ' TO GODK-STATUSKODER                                        
033458     CALL CBLTDLI USING DLET 4123-PCB DLI-IO-WDGX4126                     
033459     MOVE 4123-STATUS-CODE TO STATUS-WS                                   
033460     PERFORM IMS-STATUSKONTROLL                                           
033461     .                                                                    
033462     EJECT                                                                
033470 IMS-STATUSKONTROLL SECTION.                                              
033500                                                                          
033600     SET STATUS-IX TO 1                                                   
033700     SEARCH GODK-STATUS                                                   
033800       AT END                                                             
033900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
034000         DELIMITED BY SIZE INTO FELTEXT                                   
034100         CALL FELLOG                                                      
034200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034300         CONTINUE                                                         
034400     END-SEARCH                                                           
034500     .                                                                    
