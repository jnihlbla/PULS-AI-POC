001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021900.                                                
001500 AUTHOR.         MARKUS ASPFJÄLL.                                         
001600 DATE-WRITTEN.   98/08/11.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        UTFÖR ÄNDRAR/BORTTAG AV PRISDATA PÅ SEG 21                       
002100*        PÅ BAS WDH5                                                      
002200*        P-TO-P SWITCH EMOT BILD W5022100                                 
002300*                                                                         
002410*        PROGRAMMET UPPDATERAR WDH5                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W5T219                                              
002800*        MID:         W5I21901                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W5O21901                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5021900'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004601*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004602 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004610 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005001 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005002     88  INDATA-OK                           VALUE 'J'.                   
005010     88  INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005401                                                                          
005410 77  BYT-SW                      PIC X       VALUE 'N'.                   
005420     88  BYT-BILD                            VALUE 'J'.                   
005430     88  BYT-EJ-BILD                         VALUE 'N'.                   
005431                                                                          
005440 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005450     88  ALLT-OK                             VALUE 'J'.                   
005460     88  ALLT-NOT-OK                         VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '5219'.                
005800     88  GODK-MID                            VALUE '5211' '5212'          
005900                                                   '5213' '5214'          
006000                                                   '5215' '5216'          
006100                                                   '5217' '5218'          
006200                                                   '5219' '5221'.         
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
008705     03  SPAR-IDTRANS             PIC X(4)    VALUE '5219'.               
008706     03  SPAR-IDFTG               PIC 9(2)    VALUE ZERO.                 
008707     03  SPAR-KDEKHHT             PIC X(3).                               
008708     03  SPAR-KDEKSHT             PIC X(3).                               
008710     03  SPAR-KDEKNIVA            PIC X(5).                               
008720     03  SPAR-BILD                PIC X(4).                               
008721     03  SPAR-IDSYSMOT            PIC X(6) VALUE SPACE.                   
008722     03  SPAR-IDPTYP              PIC X(3) VALUE SPACE.                   
008723     03  SPAR-BEEKHHT             PIC X(25).                              
008724     03  SPAR-BEEKSHT             PIC X(25).                              
008730                                                                          
008800     EJECT                                                                
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W5I21901                                                   
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W5O21901                                                 
010100     EJECT                                                                
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
011201*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011202     03  W-WDH501KY-IN-X.                                                 
011203         05  W-IDFTG-MIN       PIC 9(2).                                  
011204         05  W-KDEKHHT-MIN     PIC X(3).                                  
011205                                                                          
011206     03  W-KDEKSHT-MIN-X.                                                 
011207         05  W-KDEKSHT-MIN     PIC X(3).                                  
011208                                                                          
011209     03  W-KDEKNIVA-MIN-X.                                                
011210         05  W-KDEKNIVA-MIN     PIC X(3).                                 
011211                                                                          
011212     03  W-WDH501KY-X.                                                    
011213         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011214         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
011215     03  W-BEEKHHT-X.                                                     
011216         05  W-BEEKHHT           PIC X(25)    VALUE SPACE.                
011217     03  W-KDEKSHT-X.                                                     
011218         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
011219     03  W-BEEKSHT-X.                                                     
011220         05  W-BEEKSHT           PIC X(25)    VALUE SPACE.                
011221     03  W-KDEKNIVA-X.                                                    
011222         05  W-KDEKNIVA          PIC X(5)    VALUE SPACE.                 
011230     03  W-FLLSBOK               PIC X       VALUE SPACE.                 
011231     03  W-FLARTNTO              PIC X       VALUE SPACE.                 
011232     03  W-FLARTSJK              PIC X       VALUE SPACE.                 
011233     03  W-FLARTSTD              PIC X       VALUE SPACE.                 
011234     03  W-FLAVCOST              PIC X       VALUE SPACE.                 
011235     03  W-FLINK                 PIC X       VALUE SPACE.                 
011236     03  W-FLDIRLON              PIC X       VALUE SPACE.                 
011237     03  W-FLDMTRL               PIC X       VALUE SPACE.                 
011238     03  W-FLOVRPAL              PIC X       VALUE SPACE.                 
011239     03  W-FLHEMTAG              PIC X       VALUE SPACE.                 
011250                                                                          
011300     SKIP2                                                                
011310 01  W-PROG-TO-PROG-SW-5221.                                              
011320     03  M-SW-LL-5221            PIC S9(4)   VALUE +240 COMP SYNC.        
011330     03  M-SW-Z1-Z2-5221         PIC X(2)    VALUE LOW-VALUE.             
011340     03  M-SW-KDTRANS-5221       PIC X(8)    VALUE 'W5T221  '.            
011350     03  M-SW-IDTRANS-5221       PIC X(4)    VALUE '5219'.                
011360     03  M-SW-KDMFSTYP-5221      PIC X(1)    VALUE '2'.                   
011370                                                                          
011380*    03  MID -COPY W5I22101 -PRE 5221-                                    
011390     EJECT                                                                
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
013220*    03  -COPY WDH521                                                     
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0009   -PRE MSG-                                              
013710*01  -COPY W0009   -PRE ALT1-                                             
013800*01  -COPY W0008   -PRE USEA-                                             
013900     05  FILLER                  PIC X.                                   
014001                                                                          
014002*01  -COPY W0008  -PRE WDH5-                                              
014010     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014201 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB USEA-PCB WDH5-PCB.            
014202 MAIN SECTION.                                                            
014210     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB USEA-PCB WDH5-PCB.            
014300                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014800       PERFORM B-KOLLA-NYCKLAR                                            
014900       IF NYCKLAR-OK AND ALLT-OK                                          
015001         IF MFS-UPDATE                                                    
015002           PERFORM G-KOLLA-INPUT                                          
015003           IF INDATA-OK                                                   
015004             PERFORM H-UPPDATERA                                          
015005           END-IF                                                         
015010         ELSE                                                             
015101           IF MFS-FIRST                                                   
015102             PERFORM C-FOERSTA-SIDA                                       
015103           ELSE                                                           
015107             IF SPAR-BILD = '5221' AND MID = '5221'                       
015108               PERFORM I-BYT-BILD                                         
015109             ELSE                                                         
015110               PERFORM E-SAMMA-SIDA                                       
015111             END-IF                                                       
015120           END-IF                                                         
015310         END-IF                                                           
015320         IF ALLT-OK                                                       
015400           PERFORM F-LAES-VISA-INFO                                       
015500         END-IF                                                           
015510       END-IF                                                             
015600*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015700*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
015710       IF BYT-EJ-BILD                                                     
015800         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21901 + 4                    
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
017000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21901                 
017100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017300     ELSE                                                                 
017400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21901                  
017500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017700     END-IF                                                               
017800                                                                          
017900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018200                                                                          
018300     MOVE LOW-VALUE TO MSG-AREA                                           
018400     MOVE 'W5O219N1' TO MFS-IDMOD                                         
018500     MOVE '5219' TO MOD-IDTRANS                                           
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
020400     MOVE '5219'            TO MSGI-IDTRANS                               
020500     IF EGEN-MID                                                          
020601         MOVE MID-KDEKHHT-IN     TO MSGI-KDEKHHT                          
020602         MOVE MID-KDEKSHT-IN     TO MSGI-KDEKSHT                          
020610         MOVE MID-KDEKNIVA-IN    TO MSGI-KDEKNIVA                         
020700     END-IF                                                               
020800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020900                                                                          
020910     IF GODK-MID                                                          
020920       MOVE MSGI-SPAR-AREA  TO SPAR-AREA                                  
020930     END-IF                                                               
021000     MOVE JA TO NYCKLAR-SW                                                
021100                                                                          
021200     IF EGEN-MID AND MID NOT = ALL '+'                                    
021201       IF MID-KDEKHHT-IN NUMERIC                                          
021202         MOVE MID-KDEKHHT-IN TO MSGI-KDEKHHT                              
021203       END-IF                                                             
021204       IF MID-KDEKSHT-IN NOT = ALL '+'                                    
021205         MOVE MID-KDEKSHT-IN TO MSGI-KDEKSHT                              
021206       END-IF                                                             
021207       IF MID-KDEKNIVA-IN NOT = ALL '+'                                   
021208         MOVE MID-KDEKNIVA-IN TO MSGI-KDEKNIVA                            
021209       END-IF                                                             
021226     END-IF                                                               
021227                                                                          
021228     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021229       MOVE MSGI-IDFTG      TO W-IDFTG                                    
021230     END-IF                                                               
021231                                                                          
021232*    -- KONTROLL AV KDEKHHT                                               
021233     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
021234                                                                          
021235     IF MID-KDEKHHT-IN NOT = ALL '+'                                      
021236       MOVE '7'         TO MFS-IDPFK                                      
021237       MOVE SPACE       TO MFS-KDTRTYP                                    
021238     END-IF                                                               
021239     MOVE MSGI-KDEKHHT TO W-KDEKHHT                                       
021240                                                                          
021241*    -- KONTROLL AV KDEKSHT                                               
021242     MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT-IN                               
021243                                                                          
021244     IF MID-KDEKSHT-IN NOT = ALL '+'                                      
021245       MOVE '7'         TO MFS-IDPFK                                      
021246       MOVE SPACE       TO MFS-KDTRTYP                                    
021247     END-IF                                                               
021248     MOVE MSGI-KDEKSHT TO W-KDEKSHT                                       
021249                                                                          
021250*    -- KONTROLL AV KDEKNIVA                                              
021251     MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-IN                              
021252                                                                          
021253     IF MID-KDEKNIVA-IN NOT = ALL '+'                                     
021254       MOVE '7'         TO MFS-IDPFK                                      
021255       MOVE SPACE       TO MFS-KDTRTYP                                    
021256     END-IF                                                               
021257     MOVE MSGI-KDEKNIVA TO W-KDEKNIVA                                     
021258                                                                          
021259*----HHT OCH SHT SKALL VARA IFYLLDA------*                                
021260                                                                          
021270     IF GODK-MID                                                          
021301      IF MSGI-KDEKHHT = ALL '+' AND                                       
021302         MSGI-KDEKSHT NOT = ALL '+'                                       
021303        MOVE NEJ TO NYCKLAR-SW                                            
021304      END-IF                                                              
021305      IF MSGI-KDEKSHT = ALL '+' AND MSGI-KDEKHHT NOT = ALL '+'            
021306        MOVE NEJ TO NYCKLAR-SW                                            
021307      END-IF                                                              
021308     END-IF                                                               
021317*----KONTOLL AV FLAGGOR-------------------------------------*             
021318                                                                          
021330     IF MFS-UPDATE                                                        
021331       IF MID-FLLSBOK-IN NOT = ALL '+'                                    
021332         IF MID-FLLSBOK-IN = 'N' OR 'Y'                                   
021333           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSBOK-ATTR                  
021334           MOVE MID-FLLSBOK-IN       TO W-FLLSBOK                         
021335         ELSE                                                             
021336           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLLSBOK-ATTR                  
021337           MOVE NEJ TO ALLT-SW                                            
021338         END-IF                                                           
021339       END-IF                                                             
021340       IF MID-FLARTNTO-IN NOT = ALL '+'                                   
021341         IF MID-FLARTNTO-IN = 'N' OR 'Y'                                  
021342           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLARTNTO-ATTR                 
021343           MOVE MID-FLARTNTO-IN      TO W-FLARTNTO                        
021344         ELSE                                                             
021345           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLARTNTO-ATTR                 
021346           MOVE NEJ TO ALLT-SW                                            
021347         END-IF                                                           
021348       END-IF                                                             
021349       IF MID-FLARTSJK-IN NOT = ALL '+'                                   
021350         IF MID-FLARTSJK-IN = 'N' OR 'Y'                                  
021351           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLARTSJK-ATTR                 
021352           MOVE MID-FLARTSJK-IN      TO W-FLARTSJK                        
021353         ELSE                                                             
021354           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLARTSJK-ATTR                 
021355           MOVE NEJ TO ALLT-SW                                            
021356         END-IF                                                           
021357       END-IF                                                             
021358       IF MID-FLARTSTD-IN NOT = ALL '+'                                   
021359         IF MID-FLARTSTD-IN = 'N' OR 'Y'                                  
021360           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLARTSTD-ATTR                 
021361           MOVE MID-FLARTSTD-IN      TO W-FLARTSTD                        
021362         ELSE                                                             
021363           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLARTSTD-ATTR                 
021364           MOVE NEJ TO ALLT-SW                                            
021365         END-IF                                                           
021366       END-IF                                                             
021367       IF MID-FLAVCOST-IN NOT = ALL '+'                                   
021368         IF MID-FLAVCOST-IN = 'N' OR 'Y'                                  
021369           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVCOST-ATTR                 
021370           MOVE MID-FLAVCOST-IN      TO W-FLAVCOST                        
021371         ELSE                                                             
021372           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAVCOST-ATTR                 
021373           MOVE NEJ TO ALLT-SW                                            
021374         END-IF                                                           
021375       END-IF                                                             
021376       IF MID-FLINK-IN NOT = ALL '+'                                      
021377         IF MID-FLINK-IN = 'N' OR 'Y'                                     
021378           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLINK-ATTR                    
021379           MOVE MID-FLINK-IN         TO W-FLINK                           
021380         ELSE                                                             
021381           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLINK-ATTR                    
021382           MOVE NEJ TO ALLT-SW                                            
021383         END-IF                                                           
021384       END-IF                                                             
021385       IF MID-FLDIRLON-IN NOT = ALL '+'                                   
021386         IF MID-FLDIRLON-IN = 'N' OR 'Y'                                  
021387           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLDIRLON-ATTR                 
021388           MOVE MID-FLDIRLON-IN      TO W-FLDIRLON                        
021389         ELSE                                                             
021390           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLDIRLON-ATTR                 
021391           MOVE NEJ TO ALLT-SW                                            
021392         END-IF                                                           
021393       END-IF                                                             
021394       IF MID-FLDMTRL-IN NOT = ALL '+'                                    
021395         IF MID-FLDMTRL-IN = 'N' OR 'Y'                                   
021396           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLDMTRL-ATTR                  
021397           MOVE MID-FLDMTRL-IN       TO W-FLDMTRL                         
021398         ELSE                                                             
021399           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLDMTRL-ATTR                  
021400           MOVE NEJ TO ALLT-SW                                            
021401         END-IF                                                           
021402       END-IF                                                             
021403       IF MID-FLOVRPAL-IN NOT = ALL '+'                                   
021404         IF MID-FLOVRPAL-IN = 'N' OR 'Y'                                  
021405           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLOVRPAL-ATTR                 
021406           MOVE MID-FLOVRPAL-IN      TO W-FLOVRPAL                        
021407         ELSE                                                             
021408           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLOVRPAL-ATTR                 
021409           MOVE NEJ TO ALLT-SW                                            
021410         END-IF                                                           
021411       END-IF                                                             
021412       IF MID-FLHEMTAG-IN NOT = ALL '+'                                   
021413         IF MID-FLHEMTAG-IN = 'N' OR 'Y'                                  
021414           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLHEMTAG-ATTR                 
021415           MOVE MID-FLHEMTAG-IN      TO W-FLHEMTAG                        
021416         ELSE                                                             
021417           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLHEMTAG-ATTR                 
021418           MOVE NEJ TO ALLT-SW                                            
021419         END-IF                                                           
021420       END-IF                                                             
021430       IF NOT ALLT-OK                                                     
021431         PERFORM MFS-ROER-EJ-FLAGGOR                                      
021432         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
021433         CALL WMEDKONV USING MED-WMEDAREA                                 
021434         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021435                                                                          
021436       END-IF                                                             
021440     END-IF                                                               
021441                                                                          
021442     IF GODK-MID OR NYCKLAR-OK                                            
021443       IF EGEN-MID AND MID = ALL '+' AND SPAR-BILD NOT = '5221'           
021444         PERFORM MFS-ROER-EJ-FLAGGOR                                      
021445         PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                 
021446         MOVE NEJ TO ALLT-SW                                              
021447       ELSE                                                               
021448         MOVE MSGI-IDFTG          TO MOD-IDFTG-UT                         
021449         MOVE MSGI-KDEKHHT        TO MOD-KDEKHHT-UT                       
021450         MOVE MSGI-KDEKSHT        TO MOD-KDEKSHT-UT                       
021451         MOVE MSGI-KDEKNIVA        TO MOD-KDEKNIVA-UT                     
021452         MOVE SPAR-BEEKHHT         TO MOD-BEEKHHT                         
021453         MOVE SPAR-BEEKSHT         TO MOD-BEEKSHT                         
021454       END-IF                                                             
021455     ELSE                                                                 
021456       MOVE NEJ TO ALLT-SW                                                
021457       MOVE MFS-RENSA-FAELT TO MOD-IDFTG-UT                               
021458       MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-UT                             
021459       MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT-UT                             
021460       MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-UT                            
021461                                                                          
021462     END-IF                                                               
021463                                                                          
021470     IF NOT GODK-MID                                                      
021480       MOVE NEJ TO ALLT-SW                                                
021481       PERFORM MFS-RENSA-FAELT-IN                                         
021482       PERFORM MFS-RENSA-FAELT-UT                                         
021490     END-IF                                                               
021491                                                                          
021500     IF NYCKLAR-FEL                                                       
021600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021700       CALL WMEDKONV USING MED-WMEDAREA                                   
021800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021900       PERFORM MFS-RENSA-FAELT-IN                                         
022000       PERFORM MFS-RENSA-FAELT-UT                                         
022100     END-IF                                                               
022200     .                                                                    
022301     EJECT                                                                
022302 C-FOERSTA-SIDA SECTION.                                                  
022303                                                                          
022304     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022305     CALL WMEDKONV USING MED-WMEDAREA                                     
022306     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022307                                                                          
022308     PERFORM MFS-RENSA-FAELT-IN                                           
022309     .                                                                    
022310     EJECT                                                                
022322 E-SAMMA-SIDA SECTION.                                                    
022323                                                                          
022324     IF EGEN-MID OR HELP-MID                                              
022328       IF MID       = ALL '+'                                             
022329         PERFORM MFS-RENSA-FAELT-IN                                       
022330       ELSE                                                               
022331         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022332         CALL WMEDKONV USING MED-WMEDAREA                                 
022333         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
022334         PERFORM EA-MID-INDATA-TILL-MOD                                   
022335         PERFORM MFS-LAES-IN-IGEN                                         
022336       END-IF                                                             
022337     ELSE                                                                 
022338       PERFORM MFS-RENSA-FAELT-IN                                         
022339     END-IF                                                               
022340     .                                                                    
022341     EJECT                                                                
022342 EA-MID-INDATA-TILL-MOD SECTION.                                          
022343                                                                          
022344* * * * * FÖR VARJE MID-FÄLT                                              
022345* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
022346* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
022347* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
022348     MOVE NEJ TO ALLT-SW                                                  
022349     MOVE W-IDFTG         TO MOD-IDFTG-UT                                 
022350     MOVE W-KDEKHHT       TO MOD-KDEKHHT-UT                               
022351     MOVE SPAR-BEEKHHT    TO MOD-BEEKHHT                                  
022352     MOVE W-KDEKSHT       TO MOD-KDEKSHT-UT                               
022353     MOVE SPAR-BEEKSHT    TO MOD-BEEKSHT                                  
022354     MOVE W-KDEKNIVA      TO MOD-KDEKNIVA-UT                              
022355                                                                          
022356     IF MID-FLLSBOK-IN NOT = ALL '+'                                      
022357       MOVE MID-FLLSBOK-IN  TO MOD-FLLSBOK                                
022358     ELSE                                                                 
022359       MOVE MFS-ROER-EJ-FAELT TO MOD-FLLSBOK                              
022360     END-IF                                                               
022361                                                                          
022362     IF MID-FLARTNTO-IN NOT = ALL '+'                                     
022363       MOVE MID-FLARTNTO-IN TO MOD-FLARTNTO                               
022364     ELSE                                                                 
022365       MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTNTO                             
022366     END-IF                                                               
022367                                                                          
022368     IF MID-FLARTSJK-IN NOT = ALL '+'                                     
022369       MOVE MID-FLARTSJK-IN TO MOD-FLARTSJK                               
022370     ELSE                                                                 
022371       MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTSJK                             
022372     END-IF                                                               
022373                                                                          
022374     IF MID-FLARTSTD-IN NOT = ALL '+'                                     
022375       MOVE MID-FLARTSTD-IN TO MOD-FLARTSTD                               
022376     ELSE                                                                 
022377       MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTSTD                             
022379     END-IF                                                               
022380                                                                          
022381     IF MID-FLAVCOST-IN NOT = ALL '+'                                     
022382       MOVE MID-FLAVCOST-IN TO MOD-FLAVCOST                               
022383     ELSE                                                                 
022384       MOVE MFS-ROER-EJ-FAELT TO MOD-FLAVCOST                             
022386     END-IF                                                               
022387                                                                          
022388     IF MID-FLINK-IN NOT = ALL '+'                                        
022389       MOVE MID-FLINK-IN    TO MOD-FLINK                                  
022390     ELSE                                                                 
022391       MOVE MFS-ROER-EJ-FAELT TO MOD-FLINK                                
022393     END-IF                                                               
022394                                                                          
022395     IF MID-FLDIRLON-IN NOT = ALL '+'                                     
022396       MOVE MID-FLDIRLON-IN TO MOD-FLDIRLON                               
022397     ELSE                                                                 
022398       MOVE MFS-ROER-EJ-FAELT TO MOD-FLDIRLON                             
022400     END-IF                                                               
022401                                                                          
022402     IF MID-FLDMTRL-IN NOT = ALL '+'                                      
022403       MOVE MID-FLDMTRL-IN  TO MOD-FLDMTRL                                
022404     ELSE                                                                 
022405       MOVE MFS-ROER-EJ-FAELT TO MOD-FLDMTRL                              
022407     END-IF                                                               
022408                                                                          
022409     IF MID-FLOVRPAL-IN NOT = ALL '+'                                     
022410       MOVE MID-FLOVRPAL-IN TO MOD-FLOVRPAL                               
022411     ELSE                                                                 
022412       MOVE MFS-ROER-EJ-FAELT TO MOD-FLOVRPAL                             
022414     END-IF                                                               
022415                                                                          
022416     IF MID-FLHEMTAG-IN NOT = ALL '+'                                     
022417       MOVE MID-FLHEMTAG-IN TO MOD-FLHEMTAG                               
022418     ELSE                                                                 
022419       MOVE MFS-ROER-EJ-FAELT TO MOD-FLHEMTAG                             
022420     END-IF                                                               
022430     .                                                                    
022500     EJECT                                                                
022600 F-LAES-VISA-INFO SECTION.                                                
022700                                                                          
022800     PERFORM IMS-GET-WDH5-HHT                                             
022900                                                                          
023000     IF SEGMENT-SAKNAS                                                    
023300        MOVE 'KDEKHHT SAKNAS' TO  MOD-TEMFSFEL                            
023500     ELSE                                                                 
023600       MOVE HHT-IDFTG          TO MOD-IDFTG-UT                            
023601       MOVE HHT-KDEKHHT        TO MOD-KDEKHHT-UT                          
023602       MOVE HHT-BEEKHHT        TO MOD-BEEKHHT                             
023603       MOVE HHT-BEEKHHT        TO SPAR-BEEKHHT                            
023604       PERFORM IMS-GET-WDH5-SHT                                           
023605       IF SEGMENT-SAKNAS                                                  
023606         MOVE 'KDEKSHT SAKNAS' TO MOD-TEMFSFEL                            
023607       ELSE                                                               
023608         MOVE SHT-KDEKSHT      TO MOD-KDEKSHT-UT                          
023609         MOVE SHT-BEEKSHT      TO MOD-BEEKSHT                             
023610         MOVE SHT-BEEKSHT      TO SPAR-BEEKSHT                            
023611         PERFORM IMS-GET-WDH5-NIVA                                        
023612         IF SEGMENT-SAKNAS                                                
023613           MOVE 'KDEKNIVA SAKNAS' TO MOD-TEMFSFEL                         
023615         ELSE                                                             
023616           MOVE NIVA-FLLSBOK  TO MOD-FLLSBOK                              
023617           MOVE NIVA-FLARTNTO TO MOD-FLARTNTO                             
023618           MOVE NIVA-FLARTSJK TO MOD-FLARTSJK                             
023619           MOVE NIVA-FLARTSTD TO MOD-FLARTSTD                             
023620           MOVE NIVA-FLAVCOST TO MOD-FLAVCOST                             
023621           MOVE NIVA-FLINK    TO MOD-FLINK                                
023622           MOVE NIVA-FLDIRLON TO MOD-FLDIRLON                             
023623           MOVE NIVA-FLDMTRL  TO MOD-FLDMTRL                              
023624           MOVE NIVA-FLOVRPAL TO MOD-FLOVRPAL                             
023625           MOVE NIVA-FLHEMTAG TO MOD-FLHEMTAG                             
023648         END-IF                                                           
023661       END-IF                                                             
023662                                                                          
023663       MOVE '002'      TO MSGI-KDCALL                                     
023664       MOVE '5219'   TO SPAR-IDTRANS                                      
023665       IF EGEN-MID                                                        
023666         MOVE SPACE   TO SPAR-BILD                                        
023667       END-IF                                                             
023668       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023670       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023700     END-IF                                                               
023800     .                                                                    
023900     EJECT                                                                
024802 G-KOLLA-INPUT SECTION.                                                   
024803                                                                          
024804     MOVE JA  TO INDATA-SW                                                
024805     IF MID       = ALL '+'                                               
024806       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024807       CALL WMEDKONV USING MED-WMEDAREA                                   
024808       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024809       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024811       MOVE NEJ TO INDATA-SW                                              
024812     ELSE                                                                 
024813       PERFORM IMS-GET-WDH5-21                                            
024833       IF SEGMENT-SAKNAS                                                  
024834         MOVE NEJ TO INDATA-SW                                            
024835       END-IF                                                             
024836                                                                          
024847                                                                          
024848       IF INDATA-FEL                                                      
024849         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024850         CALL WMEDKONV USING MED-WMEDAREA                                 
024851         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024853         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024864       END-IF                                                             
024865     END-IF                                                               
024866     .                                                                    
024867     EJECT                                                                
024868 H-UPPDATERA SECTION.                                                     
024869                                                                          
024871     IF SEGMENT-FINNS                                                     
024872       IF MID-FLLSBOK-IN NOT = ALL '+'                                    
024873         MOVE MID-FLLSBOK-IN TO NIVA-FLLSBOK MOD-FLLSBOK                  
024874         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLLSBOK-ATTR                   
024875       ELSE                                                               
024876         MOVE MFS-ROER-EJ-FAELT TO MOD-FLLSBOK                            
024877       END-IF                                                             
024878       IF MID-FLARTNTO-IN NOT = ALL '+'                                   
024879         MOVE MID-FLARTNTO-IN TO NIVA-FLARTNTO MOD-FLARTNTO               
024880         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLARTNTO-ATTR                  
024881       ELSE                                                               
024882         MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTNTO                           
024883       END-IF                                                             
024884       IF MID-FLARTSJK-IN NOT = ALL '+'                                   
024885         MOVE MID-FLARTSJK-IN TO NIVA-FLARTSJK MOD-FLARTSJK               
024886         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLARTSJK-ATTR                  
024887       ELSE                                                               
024888         MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTSJK                           
024889       END-IF                                                             
024890       IF MID-FLARTSTD-IN NOT = ALL '+'                                   
024891         MOVE MID-FLARTSTD-IN TO NIVA-FLARTSTD MOD-FLARTSTD               
024892         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLARTSTD-ATTR                  
024893       ELSE                                                               
024894         MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTSTD                           
024895       END-IF                                                             
024896       IF MID-FLAVCOST-IN NOT = ALL '+'                                   
024897         MOVE MID-FLAVCOST-IN TO NIVA-FLAVCOST MOD-FLAVCOST               
024898         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLAVCOST-ATTR                  
024899       ELSE                                                               
024900         MOVE MFS-ROER-EJ-FAELT TO MOD-FLAVCOST                           
024901       END-IF                                                             
024902       IF MID-FLINK-IN NOT = ALL '+'                                      
024903         MOVE MID-FLINK-IN TO NIVA-FLINK MOD-FLINK                        
024904         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLINK-ATTR                     
024905       ELSE                                                               
024906         MOVE MFS-ROER-EJ-FAELT TO MOD-FLINK                              
024907       END-IF                                                             
024908       IF MID-FLDIRLON-IN NOT = ALL '+'                                   
024909         MOVE MID-FLDIRLON-IN TO NIVA-FLDIRLON MOD-FLDIRLON               
024910         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLDIRLON-ATTR                  
024911       ELSE                                                               
024912         MOVE MFS-ROER-EJ-FAELT TO MOD-FLDIRLON                           
024913       END-IF                                                             
024914       IF MID-FLDMTRL-IN NOT = ALL '+'                                    
024915         MOVE MID-FLDMTRL-IN TO NIVA-FLDMTRL MOD-FLDMTRL                  
024916         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLDMTRL-ATTR                   
024917       ELSE                                                               
024918         MOVE MFS-ROER-EJ-FAELT TO MOD-FLDMTRL                            
024919       END-IF                                                             
024920       IF MID-FLOVRPAL-IN NOT = ALL '+'                                   
024921         MOVE MID-FLOVRPAL-IN TO NIVA-FLOVRPAL MOD-FLOVRPAL               
024922         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLOVRPAL-ATTR                  
024923       ELSE                                                               
024924         MOVE MFS-ROER-EJ-FAELT TO MOD-FLOVRPAL                           
024925       END-IF                                                             
024926       IF MID-FLHEMTAG-IN NOT = ALL '+'                                   
024927         MOVE MID-FLHEMTAG-IN TO NIVA-FLHEMTAG MOD-FLHEMTAG               
024928         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLHEMTAG-ATTR                  
024929       ELSE                                                               
024930         MOVE MFS-ROER-EJ-FAELT TO MOD-FLHEMTAG                           
024931       END-IF                                                             
024932                                                                          
024938       PERFORM IMS-REPL-WDH5-NIVA                                         
024939                                                                          
024940       MOVE SPACE TO SPAR-BILD                                            
024941       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
024942       CALL WMEDKONV USING MED-WMEDAREA                                   
024943       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
024944       PERFORM MFS-FORM-ATTR                                              
024945       PERFORM MFS-RENSA-FAELT-IN                                         
024946* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
024947     END-IF                                                               
024948     .                                                                    
024950     EJECT                                                                
024960 I-BYT-BILD SECTION.                                                      
024961     MOVE JA  TO BYT-SW                                                   
024962     MOVE NEJ TO ALLT-SW                                                  
024963     MOVE W-KDEKHHT      TO 5221-MID-KDEKHHT-IN                           
024964     MOVE W-KDEKSHT      TO 5221-MID-KDEKSHT-IN                           
024970     MOVE W-KDEKNIVA     TO 5221-MID-KDEKNIVA-IN                          
024980     MOVE SPAR-IDSYSMOT  TO 5221-MID-IDSYSMOT-IN                          
024981     MOVE SPAR-IDPTYP    TO 5221-MID-IDPTYP-IN                            
024984                                                                          
024985     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21901 + 17                       
024990     PERFORM IMS-INSERT-ALT1-MSG-5221                                     
024991     .                                                                    
024992     EJECT                                                                
025000 MFS-RENSA-FAELT-UT SECTION.                                              
025100                                                                          
025200*    --- ALLA UTDATA-FÄLT                                                 
025310*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025400     MOVE MFS-RENSA-FAELT TO MOD-IDFTG-UT                                 
025410                             MOD-KDEKHHT-UT                               
025500                             MOD-KDEKSHT-UT                               
025510                             MOD-KDEKNIVA-UT                              
025520                             MOD-BEEKHHT                                  
025530                             MOD-BEEKSHT                                  
025600     .                                                                    
025701     SKIP3                                                                
025900 MFS-RENSA-FAELT-IN SECTION.                                              
026000                                                                          
026100*    --- ALLA INDATA-FÄLT                                                 
026200     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
026300                             MOD-KDEKSHT-IN                               
026310                             MOD-KDEKNIVA-IN                              
026320                             MOD-BEEKHHT                                  
026330                             MOD-BEEKSHT                                  
026340                                                                          
026400     .                                                                    
026500     EJECT                                                                
026600 MFS-ROER-EJ-FLAGGOR   SECTION.                                           
026610     MOVE MFS-ROER-EJ-FAELT TO MOD-FLLSBOK                                
026620                               MOD-FLARTNTO                               
026700                               MOD-FLARTSJK                               
026800                               MOD-FLARTSTD                               
026910                               MOD-FLAVCOST                               
027201                               MOD-FLINK                                  
027202                               MOD-FLDIRLON                               
027203                               MOD-FLDMTRL                                
027204                               MOD-FLOVRPAL                               
027205                               MOD-FLHEMTAG                               
027208     .                                                                    
027209     SKIP2                                                                
027210 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027211                                                                          
027212*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027213     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFTG-UT                               
027214                               MOD-KDEKHHT-UT                             
027215                               MOD-KDEKSHT-UT                             
027216                               MOD-KDEKNIVA-UT                            
027217                               MOD-BEEKHHT                                
027218                               MOD-BEEKSHT                                
027220                                                                          
027300     .                                                                    
027400     SKIP3                                                                
027500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027600                                                                          
027700*    --- ALLA INDATA-FÄLT                                                 
027800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKHHT-IN                             
027900                               MOD-KDEKSHT-IN                             
027910                               MOD-KDEKNIVA-IN                            
027920                                                                          
028000     .                                                                    
028100     EJECT                                                                
028200 MFS-FORM-ATTR SECTION.                                                   
028300                                                                          
028400*    --- ALLA INDATA-FÄLT                                                 
028500     MOVE MFS-FORMATETS-ATTR TO MOD-FLLSBOK-ATTR                          
028600                                MOD-FLARTNTO-ATTR                         
028610                                MOD-FLARTSJK-ATTR                         
028620                                MOD-FLARTSTD-ATTR                         
028630                                MOD-FLAVCOST-ATTR                         
028640                                MOD-FLINK-ATTR                            
028650                                MOD-FLDIRLON-ATTR                         
028660                                MOD-FLDMTRL-ATTR                          
028670                                MOD-FLOVRPAL-ATTR                         
028680                                MOD-FLHEMTAG-ATTR                         
028700     .                                                                    
028800     SKIP2                                                                
028900 MFS-LAES-IN-IGEN SECTION.                                                
029000                                                                          
029100*    --- ALLA INDATA-FÄLT                                                 
029200     MOVE MFS-ADD-LAES-IN-FAELT TO  MOD-FLLSBOK-ATTR                      
029300                                    MOD-FLARTNTO-ATTR                     
029310                                    MOD-FLARTSJK-ATTR                     
029320                                    MOD-FLARTSTD-ATTR                     
029330                                    MOD-FLAVCOST-ATTR                     
029340                                    MOD-FLINK-ATTR                        
029350                                    MOD-FLDIRLON-ATTR                     
029360                                    MOD-FLDMTRL-ATTR                      
029370                                    MOD-FLOVRPAL-ATTR                     
029380                                    MOD-FLHEMTAG-ATTR                     
029391                                                                          
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
030600 IMS-INSERT-ALT1-MSG-5221 SECTION.                                        
030601                                                                          
030602     MOVE SPACE TO GODK-STATUSKODER                                       
030603     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW-5221              
030604     MOVE ALT1-STATUS-CODE  TO STATUS-WS                                  
030605     PERFORM IMS-STATUSKONTROLL                                           
030607     .                                                                    
030608     EJECT                                                                
030610 IMS-INSERT-MSG SECTION.                                                  
030700                                                                          
030800     IF MSGI-IDLAND-SPR = 'GB'                                            
030900       MOVE 'N' TO MFS-KDHUVOMR                                           
031000     END-IF                                                               
031100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031200     MOVE SPACE TO GODK-STATUSKODER                                       
031300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031701     EJECT                                                                
031702 IMS-GET-WDH5-HHT SECTION.                                                
031703                                                                          
031704     STRING 'WDH501  (WDH501KY= ' W-WDH501KY-X ')'                        
031705          DELIMITED BY SIZE INTO SSA1                                     
031706     MOVE '  GE' TO GODK-STATUSKODER                                      
031707     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
031708     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031709     PERFORM IMS-STATUSKONTROLL                                           
031710     .                                                                    
031711     EJECT                                                                
031712 IMS-GET-WDH5-SHT SECTION.                                                
031713                                                                          
031714     STRING 'WDH511  (KDEKSHT = ' W-KDEKSHT-X ')'                         
031715          DELIMITED BY SIZE INTO SSA1                                     
031716     MOVE '  GE' TO GODK-STATUSKODER                                      
031717     CALL CBLTDLI USING GU  WDH5-PCB DLI-IO-WDH511 SSA1                   
031718     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031719     PERFORM IMS-STATUSKONTROLL                                           
031720     .                                                                    
031721     EJECT                                                                
031722 IMS-GET-WDH5-NIVA SECTION.                                               
031723                                                                          
031724     STRING 'WDH521  (KDEKNIVA= ' W-KDEKNIVA-X ')'                        
031725          DELIMITED BY SIZE INTO SSA1                                     
031726     MOVE '  GE' TO GODK-STATUSKODER                                      
031727     CALL CBLTDLI USING GHU  WDH5-PCB DLI-IO-WDH521 SSA1                  
031728     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031729     PERFORM IMS-STATUSKONTROLL                                           
031730     .                                                                    
031731     SKIP3                                                                
031732 IMS-GET-WDH5-21   SECTION.                                               
031733                                                                          
031734     STRING 'WDH501  (WDH501KY= ' W-WDH501KY-X ')'                        
031735          DELIMITED BY SIZE INTO SSA1                                     
031736     STRING 'WDH511  (KDEKSHT = ' W-KDEKSHT-X ')'                         
031737          DELIMITED BY SIZE INTO SSA2                                     
031738     STRING 'WDH521  (KDEKNIVA= ' W-KDEKNIVA-X ')'                        
031739          DELIMITED BY SIZE INTO SSA3                                     
031740     MOVE '  GE' TO GODK-STATUSKODER                                      
031741     CALL CBLTDLI USING GHU  WDH5-PCB DLI-IO-WDH521 SSA1                  
031742                                                    SSA2 SSA3             
031743     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031744     PERFORM IMS-STATUSKONTROLL                                           
031745     .                                                                    
031746     SKIP3                                                                
031758 IMS-REPL-WDH5-NIVA SECTION.                                              
031759                                                                          
031760     MOVE '  ' TO GODK-STATUSKODER                                        
031761     CALL CBLTDLI USING REPL WDH5-PCB DLI-IO-WDH521                       
031762     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031763     PERFORM IMS-STATUSKONTROLL                                           
031764     .                                                                    
031765     SKIP3                                                                
031766                                                                          
031900 IMS-STATUSKONTROLL SECTION.                                              
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
