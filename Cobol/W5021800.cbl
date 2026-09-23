001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021800.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/08/04.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMETS HUVUDFUNKTION ÄR ATT KOMPLETTERA OCH                 
002100*        ÄNDRA EKONOMISK PROFILDATA M.H.A INMATAD DATA FRÅN               
002200*        BILD 5218                                                        
002300*                                                                         
002410*        PROGRAMMET UPPDATERAR WDH5                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W5T218                                              
002800*        MID:         W5I21801                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W5O21801                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5021800'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004601*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004602 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004610 77  MAX-INDX                    PIC S9(4)  VALUE +1    COMP SYNC.        
004611                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005001     88  ALLT-OK                             VALUE 'J'.                   
005002                                                                          
005003 77  BYT-SW                      PIC X       VALUE 'J'.                   
005004     88  BYT-BILD                            VALUE 'J'.                   
005005     88  BYT-EJ-BILD                         VALUE 'N'.                   
005006                                                                          
005007 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005008     88  INDATA-OK                           VALUE 'J'.                   
005010     88  INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '5218'.                
005800     88  GODK-MID                            VALUE '5211' '5212'          
005900                                                   '5213' '5214'          
006000                                                   '5215' '5216'          
006100                                                   '5217' '5218'          
006200                                                   '5219'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007701     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007702     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007703     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007710     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007801     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007810     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     03  KDEKHHT-MISSING         PIC X(3)    VALUE '269'.                 
008010     03  KDEKSHT-MISSING         PIC X(3)    VALUE '270'.                 
008020     03  KDEKNIVA-MISSING        PIC X(3)    VALUE '271'.                 
008030     03  SYSTNAME-MISSING        PIC X(3)    VALUE '272'.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008701     EJECT                                                                
008702*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008703*                                                                         
008704 01  SPAR-AREA.                                                           
008705     03  SPAR-IDTRANS           PIC X(4)     VALUE '5218'.                
008706     03  SPAR-IDFTG             PIC 9(2)     VALUE ZERO.                  
008707     03  SPAR-KDEKHHT           PIC X(3)     VALUE SPACE.                 
008708     03  SPAR-KDEKSHT           PIC X(3)     VALUE SPACE.                 
008709     03  SPAR-KDEKNIVA          PIC X(5)     VALUE SPACE.                 
008710     03  SPAR-BILD              PIC X(4)     VALUE SPACE.                 
008711     03  SPAR-IDSYSMOT          PIC X(6)     VALUE SPACE.                 
008712     03  SPAR-IDPTYP            PIC X(3)     VALUE SPACE.                 
008713     03  SPAR-IDSYSMOT-ENTER    PIC X(6)     VALUE SPACE.                 
008714     03  SPAR-IDSYSMOT-NEXT     PIC X(6)     VALUE SPACE.                 
008715     03  SPAR-IDPTYP-ENTER      PIC X(3)     VALUE SPACE.                 
008716     03  SPAR-IDPTYP-NEXT       PIC X(3)     VALUE SPACE.                 
008717     03  SPAR-IDSEKVNR-ENTER    PIC 9(3)     VALUE ZERO.                  
008718     03  SPAR-IDSEKVNR-NEXT     PIC 9(3)     VALUE ZERO.                  
008719     03  SPAR-IDSEKVNR-ATTR     PIC X(2)     VALUE SPACE.                 
008720     03  SPAR-IDSEKVNR          PIC 9(3)     VALUE ZERO.                  
008721     03  SPAR-IDKONTO           PIC 9(10)    VALUE ZERO.                  
008730     03  SPAR-KDPOST            PIC X(2)     VALUE SPACE.                 
008740     03  SPAR-IDKST             PIC X(10)    VALUE SPACE.                 
008750     03  SPAR-KDDOKTYP          PIC X        VALUE SPACE.                 
008760     03  SPAR-IDANALYS          PIC X        VALUE SPACE.                 
008770     03  SPAR-KDANALYS          PIC X        VALUE SPACE.                 
008780     03  SPAR-FLALLOC           PIC X        VALUE SPACE.                 
008790     03  SPAR-FLPRSEGM          PIC X        VALUE SPACE.                 
008791     03  SPAR-IDPRCTR           PIC X(10)    VALUE SPACE.                 
008792     03  SPAR-BEEKHHT           PIC X(25)    VALUE SPACE.                 
008793     03  SPAR-BEEKSHT           PIC X(25)    VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W5I21801                                                   
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W5O21801                                                 
010100     EJECT                                                                
010110 01  W-PROG-TO-PROG-SW-5221.                                              
010120     03  M-SW-LL-5221            PIC S9(4)   VALUE +240 COMP SYNC.        
010130     03  M-SW-Z1-Z2-5221         PIC X(2)    VALUE LOW-VALUE.             
010140     03  M-SW-KDTRANS-5221       PIC X(8)    VALUE 'W5T221  '.            
010150     03  M-SW-IDTRANS-5221       PIC X(4)    VALUE '5218'.                
010160     03  M-SW-KDMFSTYP-5221      PIC X(1)    VALUE '2'.                   
010170                                                                          
010180*    03  MID -COPY W5I22101 -PRE 5221-                                    
010190     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300     SKIP3                                                                
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000     SKIP3                                                                
011100 01  NYCKLAR-TILL-DLI.                                                    
011201*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011208     03  W-WDH501KY-X.                                                    
011209         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011210         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
011211     03  W-KDEKSHT-X.                                                     
011212         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
011213     03  W-KDEKNIVA-X.                                                    
011214         05  W-KDEKNIVA          PIC X(5)    VALUE SPACE.                 
011215     03  W-WDH531KY-X.                                                    
011220         05  W-IDSYSMOT          PIC X(6)    VALUE SPACE.                 
011240         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
011250         05  W-IDSEKVNR          PIC S9(3)   COMP-3 VALUE +1.             
011260     03  W-WDH531KY-MIN-X.                                                
011270         05  W-IDSYSMOT-MIN      PIC X(6)    VALUE SPACE.                 
011280         05  W-IDPTYP-MIN        PIC X(3)    VALUE SPACE.                 
011290         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
011291     03  W-WDH531KY-MAX-X.                                                
011292         05  W-IDSYSMOT-MAX      PIC X(6)    VALUE SPACE.                 
011293         05  W-IDPTYP-MAX        PIC X(3)    VALUE SPACE.                 
011294         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
011300     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012410 01  SSA3                        PIC X(64).                               
012420 01  SSA4                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100                                                                          
013201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
013202 01  DLI-IO-WDH501.                                                       
013203*    03  -COPY WDH501                                                     
013204     EJECT                                                                
013205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
013206 01  DLI-IO-WDH511.                                                       
013207*    03  -COPY WDH511                                                     
013208     EJECT                                                                
013209 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
013210 01  DLI-IO-WDH521.                                                       
013211*    03  -COPY WDH521                                                     
013212     EJECT                                                                
013213 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
013214 01  DLI-IO-WDH531.                                                       
013220*    03  -COPY WDH531                                                     
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0009   -PRE MSG-                                              
013701     EJECT                                                                
013710*01  -COPY W0009   -PRE ALT-                                              
013720     EJECT                                                                
013800*01  -COPY W0008   -PRE USEA-                                             
013900     05  FILLER                  PIC X.                                   
014001                                                                          
014002*01  -COPY W0008  -PRE WDH5-                                              
014010     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014201 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDH5-PCB.             
014202 MAIN SECTION.                                                            
014210     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDH5-PCB.             
014300                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014800       PERFORM B-KOLLA-NYCKLAR                                            
014900       IF NYCKLAR-OK                                                      
015003         IF MFS-UPDATE                                                    
015004           PERFORM G-KOLLA-INPUT                                          
015005           IF INDATA-OK                                                   
015006             PERFORM H-UPPDATERA                                          
015007           END-IF                                                         
015010         ELSE                                                             
015101           IF MFS-FIRST                                                   
015102             PERFORM C-FOERSTA-SIDA                                       
015103           ELSE                                                           
015104             IF MFS-NEXT                                                  
015105               PERFORM D-NAESTA-SIDA                                      
015106             ELSE                                                         
015107               PERFORM E-SAMMA-SIDA                                       
015108             END-IF                                                       
015110           END-IF                                                         
015310         END-IF                                                           
015320         IF ALLT-OK                                                       
015321           PERFORM F-LAES-VISA-INFO                                       
015330         END-IF                                                           
015500       END-IF                                                             
015510       IF BYT-EJ-BILD                                                     
015800         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21801 + 4                    
015900         PERFORM IMS-INSERT-MSG                                           
016000       END-IF                                                             
016100     END-IF                                                               
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     IF MSG-DUBBLA-TRANSKODER                                             
017000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21801                 
017100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017300     ELSE                                                                 
017400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21801                  
017500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017700     END-IF                                                               
017800                                                                          
017900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018200                                                                          
018300     MOVE LOW-VALUE TO MSG-AREA                                           
018400     MOVE 'W5O218N1' TO MFS-IDMOD                                         
018500     MOVE '5218' TO MOD-IDTRANS                                           
018600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018700                                                                          
018800     IF EGEN-MID OR HELP-MID                                              
018900       CONTINUE                                                           
019000     ELSE                                                                 
019100       MOVE SPACE TO MFS-KDTRTYP                                          
019200       MOVE '7' TO MFS-IDPFK                                              
019300     END-IF                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 B-KOLLA-NYCKLAR SECTION.                                                 
019900                                                                          
020000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020100     MOVE '001'             TO MSGI-KDCALL                                
020200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020400     MOVE '5218'            TO MSGI-IDTRANS                               
021100                                                                          
021200     IF EGEN-MID OR GODK-MID                                              
021201       MOVE MID-KDEKHHT-IN  TO MSGI-KDEKHHT                               
021202       MOVE MID-KDEKSHT-IN  TO MSGI-KDEKSHT                               
021203       MOVE MID-KDEKNIVA-IN TO MSGI-KDEKNIVA                              
021204       MOVE MID-IDSYSMOT-IN TO MSGI-IDSYSMOT                              
021205       MOVE MID-IDPTYP-IN   TO MSGI-IDPTYP                                
021217     END-IF                                                               
021218     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021219     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
021220                                                                          
021221     IF MSGI-IDLAND-SPR = 'GB'                                            
021222       MOVE 'GB' TO MED-IDSKYLT                                           
021223     ELSE                                                                 
021224       MOVE 'S' TO MED-IDSKYLT                                            
021225     END-IF                                                               
021230                                                                          
021238     MOVE JA TO ALLT-SW                                                   
021239     MOVE JA TO NYCKLAR-SW                                                
021240     MOVE NEJ TO BYT-SW                                                   
021241     MOVE SPACE TO MED-IDMFSFEL                                           
021242     MOVE SPACE TO MED-IDMFSINF                                           
021244                                                                          
021245     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021246       MOVE MSGI-IDFTG      TO W-IDFTG                                    
021247                               SPAR-IDFTG                                 
021248     END-IF                                                               
021249                                                                          
021250*    -- KONTROLL AV KDEKHHT                                               
021251     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
021252                                                                          
021253     IF MSGI-KDEKHHT NUMERIC AND MSGI-KDEKHHT > ZERO                      
021254       MOVE MSGI-KDEKHHT    TO W-KDEKHHT                                  
021255                               SPAR-KDEKHHT                               
021256     ELSE                                                                 
021257       MOVE NEJ             TO NYCKLAR-SW                                 
021258     END-IF                                                               
021259                                                                          
021260*    -- KONTROLL AV KDEKSHT                                               
021261     MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT-IN                               
021265                                                                          
021266     IF MSGI-KDEKSHT NOT = ALL '+'                                        
021267       MOVE MSGI-KDEKSHT    TO W-KDEKSHT                                  
021268                               SPAR-KDEKSHT                               
021270     ELSE                                                                 
021271       MOVE NEJ             TO NYCKLAR-SW                                 
021272     END-IF                                                               
021273                                                                          
021274*    -- KONTROLL AV KDEKNIVA                                              
021275     MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-IN                              
021276                                                                          
021280     IF MSGI-KDEKNIVA NOT = ALL '+'                                       
021281       MOVE MSGI-KDEKNIVA   TO W-KDEKNIVA                                 
021282                               SPAR-KDEKNIVA                              
021284     ELSE                                                                 
021285       MOVE NEJ             TO NYCKLAR-SW                                 
021286     END-IF                                                               
021287                                                                          
021288*    -- KONTROLL AV IDSYSMOT                                              
021289     MOVE MFS-RENSA-FAELT TO MOD-IDSYSMOT-IN                              
021290                                                                          
021293     IF MSGI-IDSYSMOT NOT = ALL '+'                                       
021294       MOVE MSGI-IDSYSMOT TO W-IDSYSMOT                                   
021295                             W-IDSYSMOT-MIN                               
021296                             W-IDSYSMOT-MAX                               
021297                             SPAR-IDSYSMOT                                
021298     ELSE                                                                 
021299       MOVE NEJ           TO NYCKLAR-SW                                   
021300     END-IF                                                               
021301                                                                          
021302*    -- KONTROLL AV IDPTYP                                                
021303     MOVE MFS-RENSA-FAELT TO MOD-IDPTYP-IN                                
021304                                                                          
021309     IF MSGI-IDPTYP NOT = ALL '+'                                         
021310       MOVE MSGI-IDPTYP   TO W-IDPTYP                                     
021311                             W-IDPTYP-MIN                                 
021312                             W-IDPTYP-MAX                                 
021313                             SPAR-IDPTYP                                  
021314     ELSE                                                                 
021315       MOVE NEJ           TO NYCKLAR-SW                                   
021316     END-IF                                                               
021317                                                                          
021320                                                                          
021321     IF GODK-MID OR NYCKLAR-OK                                            
021322       MOVE MSGI-IDFTG      TO MOD-IDFTG-UT                               
021323       MOVE MSGI-KDEKHHT    TO MOD-KDEKHHT-UT                             
021324       MOVE MSGI-KDEKSHT    TO MOD-KDEKSHT-UT                             
021325       MOVE MSGI-KDEKNIVA   TO MOD-KDEKNIVA-UT                            
021326       MOVE MSGI-IDSYSMOT   TO MOD-IDSYSMOT-UT                            
021327       MOVE MSGI-IDPTYP     TO MOD-IDPTYP-UT                              
021328     ELSE                                                                 
021329       MOVE MFS-RENSA-FAELT TO MOD-IDFTG-UT                               
021330                               MOD-KDEKHHT-UT                             
021331                               MOD-KDEKSHT-UT                             
021332                               MOD-KDEKNIVA-UT                            
021333                               MOD-IDSYSMOT-UT                            
021334                               MOD-IDPTYP-UT                              
021340     END-IF                                                               
021400                                                                          
021500     IF NYCKLAR-FEL                                                       
021510*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021520*---GODKÄND BILD                                                          
021530       IF GODK-MID                                                        
021600         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021700         CALL WMEDKONV USING MED-WMEDAREA                                 
021800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021900         PERFORM MFS-RENSA-FAELT-IN                                       
022000         PERFORM MFS-RENSA-FAELT-UT                                       
022100       END-IF                                                             
022150     ELSE                                                                 
022151       PERFORM MFS-LAES-IN-IGEN                                           
022160     END-IF                                                               
022200     .                                                                    
022301     EJECT                                                                
022302 C-FOERSTA-SIDA SECTION.                                                  
022303                                                                          
022304     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
022305     CALL WMEDKONV USING MED-WMEDAREA                                     
022306     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
022308                                                                          
022309     PERFORM MFS-RENSA-FAELT-IN                                           
022310     .                                                                    
022311     EJECT                                                                
022312 D-NAESTA-SIDA SECTION.                                                   
022313                                                                          
022314     IF SPAR-IDTRANS = '5218'                                             
022318       MOVE SPAR-IDSYSMOT-NEXT TO W-IDSYSMOT                              
022319       MOVE SPAR-IDPTYP-NEXT   TO W-IDPTYP                                
022321       MOVE SPAR-IDSEKVNR-NEXT TO W-IDSEKVNR                              
022326     ELSE                                                                 
022327       PERFORM MFS-RENSA-FAELT-IN                                         
022328     END-IF                                                               
022329     .                                                                    
022330     EJECT                                                                
022331 E-SAMMA-SIDA SECTION.                                                    
022332                                                                          
022333     IF SPAR-IDTRANS = '5218' OR '0551'                                   
022334*---OM MID-KDEKHHT-IN ÄNDRATS OCH 'ENTER' AKTIVERATS                      
022335*---MÅSTE FÖLJANDE IF-SATS ANVÄNDAS FÖR ATT FÅ UT NÅGON DATA              
022336       IF MID-IDKONTO   NOT = ALL '+'                                     
022337        OR MID-KDPOST   NOT = ALL '+'                                     
022338        OR MID-KDTECKEN NOT = ALL ' '                                     
022339        OR MID-IDKST    NOT = ALL '+'                                     
022340        OR MID-KDDOKTYP NOT = ALL '+'                                     
022341        OR MID-IDANALYS NOT = ALL '+'                                     
022342        OR MID-KDANALYS NOT = ALL '+'                                     
022343        OR MID-FLALLOC  NOT = ALL '+'                                     
022344        OR MID-FLPRSEGM NOT = ALL '+'                                     
022345        OR MID-IDPRCTR  NOT = ALL '+'                                     
022351         MOVE '5218' TO SPAR-BILD                                         
022352       ELSE                                                               
022353         IF SPAR-BILD = '5221'                                            
022354          AND MID-KDEKHHT-IN  = ALL '+'                                   
022355          AND MID-KDEKSHT-IN  = ALL '+'                                   
022356          AND MID-KDEKNIVA-IN = ALL '+'                                   
022357          AND MID-IDSYSMOT-IN = ALL '+'                                   
022358          AND MID-IDPTYP-IN   = ALL '+'                                   
022359           PERFORM I-BYT-BILD                                             
022360         ELSE                                                             
022361           MOVE '5218' TO SPAR-BILD                                       
022362         END-IF                                                           
022363       END-IF                                                             
022364     ELSE                                                                 
022365       PERFORM MFS-RENSA-FAELT-IN                                         
022370     END-IF                                                               
022385     .                                                                    
022390     EJECT                                                                
022600 F-LAES-VISA-INFO SECTION.                                                
022601                                                                          
022610     PERFORM IMS-GU-HHT                                                   
022620                                                                          
022630     IF SEGMENT-SAKNAS                                                    
022640        IF MED-IDMFSFEL = SPACE                                           
022650          MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                            
022660        END-IF                                                            
022670        CALL WMEDKONV USING MED-WMEDAREA                                  
022680        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
022690        PERFORM MFS-RENSA-FAELT-UT                                        
022691     ELSE                                                                 
022693       MOVE HHT-BEEKHHT TO MOD-BEEKHHT                                    
022694                           SPAR-BEEKHHT                                   
022695       PERFORM FA-LAES-VISA-SHT                                           
022696       PERFORM FB-LAES-VISA-NIVA                                          
022697     END-IF                                                               
022707                                                                          
022708     MOVE '002'     TO MSGI-KDCALL                                        
022709     MOVE '5218'    TO SPAR-IDTRANS                                       
022710     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
022711     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022712     .                                                                    
022713     EJECT                                                                
022714 FA-LAES-VISA-SHT SECTION.                                                
022715                                                                          
022716       PERFORM IMS-GU-SHT                                                 
022717                                                                          
022718       IF SEGMENT-SAKNAS                                                  
022719          IF MED-IDMFSFEL = SPACE                                         
022720            MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                          
022721          END-IF                                                          
022722          CALL WMEDKONV USING MED-WMEDAREA                                
022723          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
022724          PERFORM MFS-RENSA-FAELT-UT                                      
022725       ELSE                                                               
022726         MOVE SHT-BEEKSHT TO MOD-BEEKSHT                                  
022727                             SPAR-BEEKSHT                                 
022728       END-IF                                                             
022729     .                                                                    
022730     EJECT                                                                
022731                                                                          
022740 FB-LAES-VISA-NIVA SECTION.                                               
022750                                                                          
022800     PERFORM IMS-GU-NIVA                                                  
022900                                                                          
023000     IF SEGMENT-SAKNAS                                                    
023100        IF MED-IDMFSFEL = SPACE                                           
023200          MOVE KDEKNIVA-MISSING TO MED-IDMFSFEL                           
023300        END-IF                                                            
023400        CALL WMEDKONV USING MED-WMEDAREA                                  
023500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023600        PERFORM MFS-RENSA-FAELT-UT                                        
023700     ELSE                                                                 
023800       MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA                                 
023900       PERFORM FBA-LAES-VISA-SYST                                         
024400     END-IF                                                               
024600                                                                          
024803     .                                                                    
024804     EJECT                                                                
024805 FBA-LAES-VISA-SYST SECTION.                                              
024806                                                                          
024807     PERFORM IMS-GHU-SYST                                                 
024809     IF SEGMENT-SAKNAS                                                    
024815       IF MFS-NEXT                                                        
024819         ADD +1             TO SPAR-IDSEKVNR                              
024820         MOVE SPAR-IDSEKVNR TO MOD-IDSEKVNR                               
024822         MOVE SPAR-IDSYSMOT TO MOD-IDSYSMOT                               
024824         MOVE SPAR-IDPTYP   TO MOD-IDPTYP                                 
024826         MOVE ZERO          TO MOD-IDKONTO                                
024828         MOVE SPACE         TO MOD-IDKST                                  
024830         MOVE SPACE         TO MOD-KDPOST                                 
024831         MOVE SPACE         TO MOD-KDTECKEN                               
024832         MOVE SPACE         TO MOD-KDDOKTYP                               
024834         MOVE SPACE         TO MOD-IDANALYS                               
024836         MOVE SPACE         TO MOD-KDANALYS                               
024838         MOVE SPACE         TO MOD-FLALLOC                                
024840         MOVE SPACE         TO MOD-FLPRSEGM                               
024842         MOVE SPACE         TO MOD-IDPRCTR                                
024846       ELSE                                                               
024847         MOVE SYSTNAME-MISSING TO MED-IDMFSFEL                            
024848         CALL WMEDKONV USING MED-WMEDAREA                                 
024849         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024850         PERFORM MFS-RENSA-FAELT-UT                                       
024851       END-IF                                                             
024852     ELSE                                                                 
024853       MOVE SYST-IDSYSMOT TO MOD-IDSYSMOT                                 
024854                             SPAR-IDSYSMOT-ENTER                          
024855                             SPAR-IDSYSMOT                                
024856       MOVE SYST-IDPTYP   TO MOD-IDPTYP                                   
024857                             SPAR-IDPTYP-ENTER                            
024858                             SPAR-IDPTYP                                  
024859       MOVE SYST-IDSEKVNR TO MOD-IDSEKVNR                                 
024860                             SPAR-IDSEKVNR                                
024861                             SPAR-IDSEKVNR-ENTER                          
024862       MOVE +1 TO INDX                                                    
024863       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
024864         IF SEGMENT-FINNS                                                 
024865           MOVE SYST-IDKONTO  TO MOD-IDKONTO                              
024866           MOVE SYST-KDPOST   TO MOD-KDPOST                               
024867           MOVE SYST-KDTECKEN TO MOD-KDTECKEN                             
024869           MOVE SYST-IDKST    TO MOD-IDKST                                
024870           MOVE SYST-KDDOKTYP TO MOD-KDDOKTYP                             
024871           MOVE SYST-IDANALYS TO MOD-IDANALYS                             
024872           MOVE SYST-KDANALYS TO MOD-KDANALYS                             
024873           MOVE SYST-FLALLOC  TO MOD-FLALLOC                              
024874           MOVE SYST-FLPRSEGM TO MOD-FLPRSEGM                             
024875           MOVE SYST-IDPRCTR  TO MOD-IDPRCTR                              
024876         ELSE                                                             
024877           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
024878         END-IF                                                           
024879         ADD +1 TO INDX                                                   
024880         PERFORM IMS-GN-SYST                                              
024881       END-PERFORM                                                        
024882                                                                          
024883       MOVE SYST-IDSYSMOT TO SPAR-IDSYSMOT-NEXT                           
024884       MOVE SYST-IDPTYP   TO SPAR-IDPTYP-NEXT                             
024885       MOVE SYST-IDSEKVNR TO SPAR-IDSEKVNR-NEXT                           
024887       IF MED-IDMFSINF = SPACE                                            
024888         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
024889         CALL WMEDKONV USING MED-WMEDAREA                                 
024890       END-IF                                                             
024891       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
024892                                                                          
024893       IF SEGMENT-SAKNAS                                                  
024894         ADD +1 TO SPAR-IDSEKVNR-NEXT                                     
024895       END-IF                                                             
024896     END-IF                                                               
024898     .                                                                    
024900     EJECT                                                                
024902 G-KOLLA-INPUT SECTION.                                                   
024903                                                                          
024904     MOVE JA  TO INDATA-SW                                                
024905     IF MID-IDKONTO    = ALL '+'                                          
024906      AND MID-KDPOST   = ALL '+'                                          
024907*     AND MID-KDTECKEN = ALL ' '                                          
024908      AND MID-IDKST    = ALL '+'                                          
024909      AND MID-KDDOKTYP = ALL '+'                                          
024910      AND MID-IDANALYS = ALL '+'                                          
024911      AND MID-KDANALYS = ALL '+'                                          
024912      AND MID-FLALLOC  = ALL '+'                                          
024913      AND MID-FLPRSEGM = ALL '+'                                          
024914      AND MID-IDPRCTR  = ALL '+'                                          
024915      AND MID-IDSEKVNR = ALL '+'                                          
024916       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024917       CALL WMEDKONV USING MED-WMEDAREA                                   
024918       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024919       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024920       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024921       MOVE NEJ TO INDATA-SW                                              
024922     ELSE                                                                 
024930                                                                          
024940       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRCTR-ATTR                     
024950       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLALLOC-ATTR                     
024960       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLPRSEGM-ATTR                    
024970       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKST-ATTR                       
024980       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKONTO-ATTR                     
024990       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPOST-ATTR                      
024991       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDTECKEN-ATTR                    
025000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDDOKTYP-ATTR                    
025001       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANALYS-ATTR                    
025002       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDANALYS-ATTR                    
025003                                                                          
025004       IF MID-IDPRCTR = ALL '+'                                           
025005         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPRCTR-ATTR                    
025006         MOVE NEJ TO INDATA-SW                                            
025007       ELSE                                                               
025009         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPRCTR-ATTR                    
025010       END-IF                                                             
025011                                                                          
025016       IF MID-FLALLOC = 'Y' OR 'N'                                        
025021         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLALLOC-ATTR                    
025030       ELSE                                                               
025040         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLALLOC-ATTR                    
025041         MOVE NEJ TO INDATA-SW                                            
025042       END-IF                                                             
025043                                                                          
025044       IF MID-FLPRSEGM = 'Y' OR 'N'                                       
025047         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPRSEGM-ATTR                   
025048       ELSE                                                               
025049         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLPRSEGM-ATTR                   
025050         MOVE NEJ TO INDATA-SW                                            
025051       END-IF                                                             
025052                                                                          
025053       IF MID-IDKST = ALL '+'                                             
025054         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKST-ATTR                       
025055         MOVE NEJ                 TO INDATA-SW                            
025056       ELSE                                                               
025063           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKST-ATTR                     
025065       END-IF                                                             
025066                                                                          
025067       IF MID-IDKONTO = ALL '+'                                           
025068         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKONTO-ATTR                     
025069         MOVE NEJ                 TO INDATA-SW                            
025070       ELSE                                                               
025071         INSPECT MID-IDKONTO REPLACING LEADING SPACE BY ZERO              
025072         IF MID-IDKONTO NUMERIC                                           
025074           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKONTO-ATTR                   
025077         END-IF                                                           
025078       END-IF                                                             
025079                                                                          
025080       IF MID-KDPOST = ALL '+'                                            
025081         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDPOST-ATTR                       
025083         MOVE NEJ TO INDATA-SW                                            
025084       ELSE                                                               
025086         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPOST-ATTR                     
025087       END-IF                                                             
025088                                                                          
025089       IF MID-KDTECKEN = ALL ' '                                          
025090         IF MID-KDPOST = '01' OR '21' OR '40'                             
025091                     OR  '11' OR '31' OR '50'                             
025092           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDTECKEN-ATTR                   
025093           MOVE NEJ TO INDATA-SW                                          
025094         ELSE                                                             
025095           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTECKEN-ATTR                 
025096         END-IF                                                           
025097       ELSE                                                               
025098         IF MID-KDTECKEN = '+'                                            
025099           IF MID-KDPOST = '01' OR '21' OR '40'                           
025100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTECKEN-ATTR               
025101           ELSE                                                           
025102             IF MID-KDPOST = '11' OR '31' OR '50'                         
025103               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDTECKEN-ATTR               
025104               MOVE NEJ TO INDATA-SW                                      
025105             ELSE                                                         
025106               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTECKEN-ATTR             
025107             END-IF                                                       
025108           END-IF                                                         
025109         END-IF                                                           
025110         IF MID-KDTECKEN = '-'                                            
025111           IF MID-KDPOST = '11' OR '31' OR '50'                           
025112             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTECKEN-ATTR               
025113           ELSE                                                           
025114             IF MID-KDPOST = '01' OR '21' OR '40'                         
025115               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDTECKEN-ATTR               
025116               MOVE NEJ TO INDATA-SW                                      
025117             ELSE                                                         
025118               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDTECKEN-ATTR             
025119             END-IF                                                       
025120           END-IF                                                         
025121         END-IF                                                           
025123       END-IF                                                             
025124                                                                          
025125       IF MID-KDDOKTYP = ALL '+'                                          
025126         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDDOKTYP-ATTR                     
025127         MOVE NEJ TO INDATA-SW                                            
025128       ELSE                                                               
025129         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDOKTYP-ATTR                   
025130       END-IF                                                             
025131                                                                          
025132       IF MID-IDANALYS = ALL '+'                                          
025133         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDANALYS-ATTR                     
025134         MOVE NEJ TO INDATA-SW                                            
025135       ELSE                                                               
025136         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDANALYS-ATTR                   
025137       END-IF                                                             
025138                                                                          
025139       IF MID-IDANALYS NOT = SPACE                                        
025140         IF MID-KDANALYS = 'R' OR 'S'                                     
025141           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDANALYS-ATTR                 
025142         ELSE                                                             
025143           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDANALYS-ATTR                 
025144           MOVE NEJ TO INDATA-SW                                          
025145         END-IF                                                           
025146       END-IF                                                             
025147                                                                          
025148       IF INDATA-FEL                                                      
025149         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
025150         CALL WMEDKONV USING MED-WMEDAREA                                 
025151         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
025152         PERFORM MFS-ROER-EJ-FAELT-UT                                     
025153         PERFORM MFS-ROER-EJ-FAELT-IN                                     
025154         MOVE NEJ TO ALLT-SW                                              
025155       ELSE                                                               
025156*---KOLLAR ATT KDEKHHT FINNS REGISTRERAD I BASEN                          
025157         PERFORM IMS-GU-HHT                                               
025158         IF SEGMENT-SAKNAS                                                
025159           IF MED-IDMFSFEL = SPACE                                        
025160             MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                         
025161           END-IF                                                         
025162         ELSE                                                             
025163*---KOLLAR ATT KDEKSHT FINNS REGISTRERAD I BASEN                          
025164           PERFORM IMS-GU-SHT                                             
025165           IF SEGMENT-SAKNAS                                              
025166             IF MED-IDMFSFEL = SPACE                                      
025167               MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                       
025168             END-IF                                                       
025169           ELSE                                                           
025170*---KOLLAR ATT KDEKNIVA FINNS REGISTRERAD I BASEN                         
025171             PERFORM IMS-GU-NIVA                                          
025172             IF SEGMENT-SAKNAS                                            
025173               IF MED-IDMFSFEL = SPACE                                    
025174                 MOVE KDEKNIVA-MISSING TO MED-IDMFSFEL                    
025175               END-IF                                                     
025176             END-IF                                                       
025177           END-IF                                                         
025178         END-IF                                                           
025179       END-IF                                                             
025180                                                                          
025181       IF SEGMENT-SAKNAS                                                  
025182         CALL WMEDKONV USING MED-WMEDAREA                                 
025183         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
025184         PERFORM MFS-RENSA-FAELT-IN                                       
025185         PERFORM MFS-RENSA-FAELT-UT                                       
025186*---OM SEGMENT SAKNAS KAN INGEN UPPDATERING SKE                           
025187*---DÄRFÖR ANVÄNDS DENNA SWITCH                                           
025188         MOVE NEJ TO INDATA-SW                                            
025189*---OM INTE SEGMENT FINNS BEHÖVER MAN EJ GÅ IN I                          
025190*---F-SECTIONEN. DÄRFÖR ANVÄNDS DENNA SWITCH                              
025191         MOVE NEJ TO ALLT-SW                                              
025192       END-IF                                                             
025193     END-IF                                                               
025194                                                                          
025195     .                                                                    
025196     EJECT                                                                
025197 H-UPPDATERA SECTION.                                                     
025198                                                                          
025199     MOVE SPAR-IDSEKVNR   TO W-IDSEKVNR                                   
025200                                                                          
025201     PERFORM IMS-GHU-SYST                                                 
025202     IF MID-IDKONTO NOT = ALL '+'                                         
025203       MOVE MID-IDKONTO  TO SYST-IDKONTO                                  
025204     END-IF                                                               
025205     IF MID-KDPOST NOT = ALL '+'                                          
025206       MOVE MID-KDPOST   TO SYST-KDPOST                                   
025207     END-IF                                                               
025208*    IF MID-KDTECKEN   NOT = ALL ' '                                      
025209       MOVE MID-KDTECKEN TO SYST-KDTECKEN                                 
025210*    END-IF                                                               
025211     IF MID-IDKST NOT = ALL '+'                                           
025212       MOVE MID-IDKST    TO SYST-IDKST                                    
025213     END-IF                                                               
025214     IF MID-KDDOKTYP NOT = ALL '+'                                        
025215       MOVE MID-KDDOKTYP TO SYST-KDDOKTYP                                 
025216     END-IF                                                               
025217     IF MID-IDANALYS NOT = ALL '+'                                        
025218       MOVE MID-IDANALYS TO SYST-IDANALYS                                 
025219     END-IF                                                               
025220     IF MID-KDANALYS NOT = ALL '+'                                        
025221       MOVE MID-KDANALYS TO SYST-KDANALYS                                 
025222     END-IF                                                               
025223     IF MID-FLALLOC NOT = ALL '+'                                         
025224       MOVE MID-FLALLOC  TO SYST-FLALLOC                                  
025225     END-IF                                                               
025226     IF MID-FLPRSEGM NOT = ALL '+'                                        
025227       MOVE MID-FLPRSEGM TO SYST-FLPRSEGM                                 
025228     END-IF                                                               
025229     IF MID-IDPRCTR  NOT = ALL '+'                                        
025230       MOVE MID-IDPRCTR  TO SYST-IDPRCTR                                  
025231     END-IF                                                               
025232     IF SEGMENT-FINNS                                                     
025233       PERFORM IMS-REPL-SYST                                              
025234     ELSE                                                                 
025235       MOVE MSGI-IDSYSMOT TO SYST-IDSYSMOT                                
025236       MOVE MSGI-IDPTYP   TO SYST-IDPTYP                                  
025237       MOVE SPAR-IDSEKVNR TO SYST-IDSEKVNR                                
025238       PERFORM IMS-ISRT-SYST                                              
025239     END-IF                                                               
025240                                                                          
025241     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
025242     CALL WMEDKONV USING MED-WMEDAREA                                     
025243     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
025244     PERFORM MFS-FORM-ATTR                                                
025245     PERFORM MFS-RENSA-FAELT-IN                                           
025246     .                                                                    
025247     EJECT                                                                
025248 I-BYT-BILD SECTION.                                                      
025249     SKIP2                                                                
025250     MOVE JA TO BYT-SW                                                    
025251     MOVE NEJ TO ALLT-SW                                                  
025252                                                                          
025253* ---SKICKAR VÄRDE TILL 5221-MID FÖR ATT SEDAN                            
025254* ---STARTA UPP 5221-BILDEN                                               
025255     MOVE LOW-VALUE            TO 5221-MID-W5I22101                       
025257     MOVE SPAR-KDEKHHT         TO 5221-MID-KDEKHHT-IN                     
025258     MOVE SPAR-KDEKSHT         TO 5221-MID-KDEKSHT-IN                     
025259     MOVE SPAR-KDEKNIVA        TO 5221-MID-KDEKNIVA-IN                    
025260     MOVE SPAR-IDSYSMOT        TO 5221-MID-IDSYSMOT-IN                    
025261     MOVE SPAR-IDPTYP          TO 5221-MID-IDPTYP-IN                      
025262     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21801 + 17                       
025263     PERFORM IMS-INSERT-ALT-MSG-5221                                      
025264     .                                                                    
025270     EJECT                                                                
025308 MFS-RENSA-FAELT-UT SECTION.                                              
025309                                                                          
025310*    --- ALLA UTDATA-FÄLT                                                 
025320*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025400     MOVE MFS-RENSA-FAELT TO MOD-BEEKHHT                                  
025500                             MOD-BEEKSHT                                  
025501                             MOD-KDEKNIVA                                 
025510                             MOD-IDSYSMOT                                 
025520                             MOD-IDPTYP                                   
025530                             MOD-IDKONTO                                  
025531                             MOD-IDSEKVNR                                 
025540                             MOD-KDPOST                                   
025541                             MOD-KDTECKEN                                 
025550                             MOD-IDKST                                    
025560                             MOD-KDDOKTYP                                 
025570                             MOD-IDANALYS                                 
025580                             MOD-KDANALYS                                 
025590                             MOD-FLALLOC                                  
025591                             MOD-FLPRSEGM                                 
025592                             MOD-IDPRCTR                                  
025600     .                                                                    
025701     SKIP3                                                                
025702 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025703                                                                          
025704*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025705     MOVE MFS-RENSA-FAELT TO MOD-IDKONTO                                  
025706                             MOD-KDPOST                                   
025707                             MOD-KDTECKEN                                 
025708                             MOD-IDKST                                    
025709                             MOD-KDDOKTYP                                 
025710                             MOD-IDANALYS                                 
025711                             MOD-KDANALYS                                 
025712                             MOD-FLALLOC                                  
025713                             MOD-FLPRSEGM                                 
025714                             MOD-IDPRCTR                                  
025720     .                                                                    
025800     SKIP3                                                                
025900 MFS-RENSA-FAELT-IN SECTION.                                              
026000                                                                          
026100*    --- ALLA INDATA-FÄLT                                                 
026200     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
026300                             MOD-KDEKSHT-IN                               
026310                             MOD-KDEKNIVA-IN                              
026320                             MOD-IDSYSMOT-IN                              
026330                             MOD-IDPTYP-IN                                
026340                             MOD-IDKONTO                                  
026341                             MOD-IDSEKVNR                                 
026350                             MOD-KDPOST                                   
026351                             MOD-KDTECKEN                                 
026360                             MOD-IDKST                                    
026370                             MOD-KDDOKTYP                                 
026380                             MOD-IDANALYS                                 
026390                             MOD-KDANALYS                                 
026391                             MOD-FLALLOC                                  
026392                             MOD-FLPRSEGM                                 
026393                             MOD-IDPRCTR                                  
026400     .                                                                    
026500     EJECT                                                                
026610 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026700                                                                          
026800*    --- ALLA UTDATA-FÄLT                                                 
026910*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
027000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFTG-UT                               
027010                               MOD-KDEKHHT-UT                             
027100                               MOD-KDEKSHT-UT                             
027200                               MOD-KDEKNIVA-UT                            
027201                               MOD-IDSYSMOT-UT                            
027202                               MOD-IDPTYP-UT                              
027203                               MOD-BEEKHHT                                
027204                               MOD-BEEKSHT                                
027205                               MOD-KDEKNIVA                               
027206                               MOD-IDSYSMOT                               
027207                               MOD-IDPTYP                                 
027208                               MOD-IDKONTO                                
027209                               MOD-IDSEKVNR                               
027210                               MOD-KDPOST                                 
027211                               MOD-KDTECKEN                               
027212                               MOD-IDKST                                  
027213                               MOD-KDDOKTYP                               
027214                               MOD-IDANALYS                               
027215                               MOD-KDANALYS                               
027216                               MOD-FLALLOC                                
027217                               MOD-FLPRSEGM                               
027218                               MOD-IDPRCTR                                
027219     .                                                                    
027220     SKIP2                                                                
027500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027600                                                                          
027700*    --- ALLA INDATA-FÄLT                                                 
027800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKHHT-IN                             
027900                               MOD-KDEKSHT-IN                             
027910                               MOD-KDEKNIVA-IN                            
027920                               MOD-IDSYSMOT-IN                            
027930                               MOD-IDPTYP-IN                              
027940                               MOD-IDKONTO                                
027941                               MOD-IDSEKVNR                               
027942                                   MOD-BEEKHHT                            
027943                                   MOD-BEEKSHT                            
027944                                   MOD-KDEKNIVA                           
027945                                   MOD-IDSYSMOT                           
027946                                   MOD-IDPTYP                             
027950                               MOD-KDPOST                                 
027951                               MOD-KDTECKEN                               
027960                               MOD-IDKST                                  
027970                               MOD-KDDOKTYP                               
027980                               MOD-IDANALYS                               
027990                               MOD-KDANALYS                               
027991                               MOD-FLALLOC                                
027992                               MOD-FLPRSEGM                               
027993                               MOD-IDPRCTR                                
028000     .                                                                    
028100     EJECT                                                                
028200 MFS-FORM-ATTR SECTION.                                                   
028300                                                                          
028400*    --- ALLA INDATA-FÄLT                                                 
028500     MOVE MFS-FORMATETS-ATTR TO MOD-IDKONTO-ATTR                          
028600                                MOD-KDPOST-ATTR                           
028601                                MOD-KDTECKEN-ATTR                         
028610                                MOD-IDKST-ATTR                            
028611                                MOD-IDSEKVNR-ATTR                         
028620                                MOD-KDDOKTYP-ATTR                         
028630                                MOD-IDANALYS-ATTR                         
028640                                MOD-KDANALYS-ATTR                         
028650                                MOD-FLALLOC-ATTR                          
028660                                MOD-FLPRSEGM-ATTR                         
028670                                MOD-IDPRCTR-ATTR                          
028700     .                                                                    
028800     SKIP2                                                                
028900 MFS-LAES-IN-IGEN SECTION.                                                
029000                                                                          
029100*    --- ALLA INDATA-FÄLT                                                 
029200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDKONTO-ATTR                       
029310                                   MOD-KDPOST-ATTR                        
029311                                   MOD-KDTECKEN-ATTR                      
029320                                   MOD-IDKST-ATTR                         
029321                                   MOD-IDSEKVNR-ATTR                      
029330                                   MOD-KDDOKTYP-ATTR                      
029340                                   MOD-IDANALYS-ATTR                      
029350                                   MOD-KDANALYS-ATTR                      
029360                                   MOD-FLALLOC-ATTR                       
029370                                   MOD-FLPRSEGM-ATTR                      
029380                                   MOD-IDPRCTR-ATTR                       
029391                                   MOD-BEEKHHT-ATTR                       
029392                                   MOD-BEEKSHT-ATTR                       
029393                                   MOD-KDEKNIVA-ATTR                      
029394                                   MOD-IDSYSMOT-ATTR                      
029395                                   MOD-IDPTYP-ATTR                        
029400     .                                                                    
029500     EJECT                                                                
029600* --- IMS SEKTIONER ---                                                   
029700     SKIP3                                                                
029800 IMS-GET-MSG SECTION.                                                     
029900                                                                          
030000     MOVE '  QC' TO GODK-STATUSKODER                                      
030100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030300     PERFORM IMS-STATUSKONTROLL                                           
030400     .                                                                    
030500     SKIP3                                                                
030600 IMS-INSERT-MSG SECTION.                                                  
030700                                                                          
031100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031200     MOVE SPACE TO GODK-STATUSKODER                                       
031300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031701     EJECT                                                                
031702 IMS-INSERT-ALT-MSG-5221 SECTION.                                         
031703                                                                          
031704     MOVE SPACE TO GODK-STATUSKODER                                       
031705     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-5221               
031706     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031707     PERFORM IMS-STATUSKONTROLL                                           
031708     .                                                                    
031709     EJECT                                                                
031710 IMS-GU-HHT SECTION.                                                      
031711                                                                          
031712     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031713          DELIMITED BY SIZE INTO SSA1                                     
031714     MOVE '  GE' TO GODK-STATUSKODER                                      
031715     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
031716     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031717     PERFORM IMS-STATUSKONTROLL                                           
031718     .                                                                    
031719     EJECT                                                                
031720 IMS-GU-SHT SECTION.                                                      
031721                                                                          
031722     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031723          DELIMITED BY SIZE INTO SSA1                                     
031724     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031725          DELIMITED BY SIZE INTO SSA2                                     
031726     MOVE '  GE' TO GODK-STATUSKODER                                      
031727     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH511 SSA1                    
031728                                                  SSA2                    
031729     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031730     PERFORM IMS-STATUSKONTROLL                                           
031731     .                                                                    
031732     EJECT                                                                
031733 IMS-GU-NIVA SECTION.                                                     
031734                                                                          
031735     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031736          DELIMITED BY SIZE INTO SSA1                                     
031737     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031738          DELIMITED BY SIZE INTO SSA2                                     
031739     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031740          DELIMITED BY SIZE INTO SSA3                                     
031741     MOVE '  GE' TO GODK-STATUSKODER                                      
031742     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1                    
031743                                                  SSA2                    
031744                                                  SSA3                    
031745     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031746     PERFORM IMS-STATUSKONTROLL                                           
031747     .                                                                    
031748     EJECT                                                                
031773 IMS-GHU-SYST SECTION.                                                    
031774                                                                          
031775     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031776          DELIMITED BY SIZE INTO SSA1                                     
031777     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031778          DELIMITED BY SIZE INTO SSA2                                     
031779     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031780          DELIMITED BY SIZE INTO SSA3                                     
031781     STRING 'WDH531  (WDH531KY =' W-WDH531KY-X ')'                        
031782          DELIMITED BY SIZE INTO SSA4                                     
031783     MOVE '  GE' TO GODK-STATUSKODER                                      
031784     CALL CBLTDLI USING GHU WDH5-PCB DLI-IO-WDH531 SSA1                   
031785                                                   SSA2                   
031786                                                   SSA3                   
031787                                                   SSA4                   
031788     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031789     PERFORM IMS-STATUSKONTROLL                                           
031790     .                                                                    
031791     EJECT                                                                
031838 IMS-GN-SYST SECTION.                                                     
031839                                                                          
031840     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031841          DELIMITED BY SIZE INTO SSA1                                     
031842     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031843          DELIMITED BY SIZE INTO SSA2                                     
031844     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031845          DELIMITED BY SIZE INTO SSA3                                     
031847     STRING 'WDH531  (WDH531KY>=' W-WDH531KY-MIN-X                        
031848                    '&WDH531KY<=' W-WDH531KY-MAX-X ')'                    
031850          DELIMITED BY SIZE INTO SSA4                                     
031851     MOVE '  GE' TO GODK-STATUSKODER                                      
031852     CALL CBLTDLI USING GN WDH5-PCB DLI-IO-WDH531 SSA1                    
031853                                                  SSA2                    
031854                                                  SSA3                    
031855                                                  SSA4                    
031856     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031857     PERFORM IMS-STATUSKONTROLL                                           
031858     .                                                                    
031859     EJECT                                                                
031879 IMS-REPL-SYST SECTION.                                                   
031880                                                                          
031881     MOVE '  ' TO GODK-STATUSKODER                                        
031882     CALL CBLTDLI USING REPL WDH5-PCB DLI-IO-WDH531                       
031883     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031884     PERFORM IMS-STATUSKONTROLL                                           
031885     .                                                                    
031886     EJECT                                                                
031887 IMS-ISRT-SYST SECTION.                                                   
031888                                                                          
031889     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031890          DELIMITED BY SIZE INTO SSA1                                     
031891     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031892          DELIMITED BY SIZE INTO SSA2                                     
031893     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031894          DELIMITED BY SIZE INTO SSA3                                     
031895     MOVE 'WDH531   ' TO SSA4                                             
031896     MOVE '  II' TO GODK-STATUSKODER                                      
031897     CALL CBLTDLI USING ISRT WDH5-PCB DLI-IO-WDH531 SSA1                  
031898                                                    SSA2                  
031899                                                    SSA3                  
031900                                                    SSA4                  
031901     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031902     PERFORM IMS-STATUSKONTROLL                                           
031903     .                                                                    
031904     EJECT                                                                
031910 IMS-STATUSKONTROLL SECTION.                                              
032000                                                                          
032100     SET STATUS-IX TO 1                                                   
032200     SEARCH GODK-STATUS                                                   
032300       AT END                                                             
032400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032500         DELIMITED BY SIZE INTO FELTEXT                                   
032600         CALL FELLOG                                                      
032700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032800         CONTINUE                                                         
032900     END-SEARCH                                                           
033000     .                                                                    
