001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5022100.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/08/10.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMETS FUNKTIONER ÄR FÖLJANDE:                              
002100*        HANTERA ÄNDRING AV: EKONOMISK HUVUDHÄNDELSE                      
002200*                            EKONOMISK SUBHÄNDELSE                        
002300*                                                                         
002500*        HANTERA BORTTAG AV:                                              
002501*        EKONOMISK HUVUDHÄNDELSE                                          
002510*        (SAMT BORTTAG AV ALL UNDERLIGGANDE DATA)                         
002700*        EKONOMISK SUBHÄNDELSE                                            
002710*        (SAMT BORTTAG AV ALL UNDERLIGGANDE DATA)                         
002900*        EKONOMISK KLIENT+POSTTYP(SAMT ALL EKONOMISK PROFILDATA)          
002910*                                                                         
003000*        AKTIVERA BILD 5218 OM VIEW PROFILE MARKERATS+"ENTER"             
003100*        AKTIVERA BILD 5219 OM VIEW LEVELPRM MARKERATS + "ENTER"          
003200*                                                                         
003310*        PROGRAMMET UPPDATERAR WDH5                                       
003400*                                                                         
003500*    INDATA.                                                              
003600*        TRANSAKTION: W5T221                                              
003700*        MID:         W5I22101                                            
003800*                                                                         
003900*    UTDATA.                                                              
004000*        MOD:         W5O22101                                            
004100                                                                          
004200     SKIP3                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600 WORKING-STORAGE SECTION.                                                 
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(08)   VALUE 'W5022100'.            
004800                                                                          
004900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005100                                                                          
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005800                                                                          
005900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005901     88  ALLT-OK                             VALUE 'J'.                   
005902                                                                          
005903 77  BYT-SW                      PIC X       VALUE 'J'.                   
005904     88  BYT-BILD                            VALUE 'J'.                   
005905     88  BYT-EJ-BILD                         VALUE 'N'.                   
005906                                                                          
005907 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005908     88  INDATA-OK                           VALUE 'J'.                   
005910     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006200     88  NYCKLAR-OK                          VALUE 'J'.                   
006300     88  NYCKLAR-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006600     88  HOPP-MID                            VALUE '5213'.                
006610     88  EGEN-MID                            VALUE '5221'.                
006700     88  GODK-MID                            VALUE '5221' '5222'          
006800                                                   '5223' '5224'          
006900                                                   '5225' '5226'          
007000                                                   '5227' '5228'          
007100                                                   '5229'.                
007200     88  HELP-MID                            VALUE '0551'.                
007300     EJECT                                                                
007310*    ---AREA FÖR ATT HÄMTA UPP MSGI-SPAR-AREA TILL PGM                    
007320 01  SPAR-AREA.                                                           
007330     03  SPAR-IDTRANS            PIC X(4)    VALUE '5221'.                
007340     03  SPAR-IDFTG              PIC 9(2)    VALUE ZERO.                  
007350     03  SPAR-KDEKHHT            PIC X(3)    VALUE SPACE.                 
007360     03  SPAR-KDEKSHT            PIC X(3)    VALUE SPACE.                 
007380     03  SPAR-KDEKNIVA           PIC X(5)    VALUE SPACE.                 
007394     03  SPAR-BILD               PIC X(4).                                
007395     03  SPAR-IDSYSMOT           PIC X(6)    VALUE SPACE.                 
007396     03  SPAR-IDPTYP             PIC X(3).                                
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008300*01 -COPY WMEDAREA                                                        
008400     SKIP3                                                                
008500 01  MESSAGE-CODES.                                                       
008601     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008602     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008603     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008610     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008900     03  KDEKHHT-MISSING         PIC X(3)    VALUE '269'.                 
008910     03  KDEKSHT-MISSING         PIC X(3)    VALUE '270'.                 
008920     03  KDEKNIVA-MISSING        PIC X(3)    VALUE '271'.                 
008930     03  SYSTNAME-MISSING        PIC X(3)    VALUE '272'.                 
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009200*                                                                         
009300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009400     SKIP3                                                                
009500*01 -COPY WMSGINIT                                                        
009700     EJECT                                                                
009800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010100     SKIP3                                                                
010200*01  MID -COPY W5I22101                                                   
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010500     SKIP3                                                                
010600*01  -COPY WMSGAREA                                                       
010700     EJECT                                                                
010800     03  MOD REDEFINES MSG-AREA.                                          
010900*      05  -COPY W5O22101                                                 
011000     EJECT                                                                
011010 01  W-PROG-TO-PROG-SW-5213.                                              
011020     03  M-SW-LL-5213            PIC S9(4)   VALUE +240 COMP SYNC.        
011030     03  M-SW-Z1-Z2-5213         PIC X(2)    VALUE LOW-VALUE.             
011040     03  M-SW-KDTRANS-5213       PIC X(8)    VALUE 'W5T213  '.            
011050     03  M-SW-IDTRANS-5213       PIC X(4)    VALUE '5221'.                
011060     03  M-SW-KDMFSTYP-5213      PIC X(1)    VALUE '2'.                   
011070                                                                          
011080*    03  MID -COPY W5I21301 -PRE 5213-                                    
011090     EJECT                                                                
011091 01  W-PROG-TO-PROG-SW-5218.                                              
011092     03  M-SW-LL-5218            PIC S9(4)   VALUE +240 COMP SYNC.        
011093     03  M-SW-Z1-Z2-5218         PIC X(2)    VALUE LOW-VALUE.             
011094     03  M-SW-KDTRANS-5218       PIC X(8)    VALUE 'W5T218  '.            
011095     03  M-SW-IDTRANS-5218       PIC X(4)    VALUE '5221'.                
011096     03  M-SW-KDMFSTYP-5218      PIC X(1)    VALUE '2'.                   
011097                                                                          
011098*    03  MID -COPY W5I21801 -PRE 5218-                                    
011099     EJECT                                                                
011100 01  W-PROG-TO-PROG-SW-5219.                                              
011101     03  M-SW-LL-5219            PIC S9(4)   VALUE +240 COMP SYNC.        
011102     03  M-SW-Z1-Z2-5219         PIC X(2)    VALUE LOW-VALUE.             
011103     03  M-SW-KDTRANS-5219       PIC X(8)    VALUE 'W5T219  '.            
011104     03  M-SW-IDTRANS-5219       PIC X(4)    VALUE '5221'.                
011105     03  M-SW-KDMFSTYP-5219      PIC X(1)    VALUE '2'.                   
011106                                                                          
011107*    03  MID -COPY W5I21901 -PRE 5219-                                    
011108     EJECT                                                                
011110 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011200     SKIP3                                                                
011300*01  -COPY WMFSAREA                                                       
011400     EJECT                                                                
011500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011600*                                                                         
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  NYCKLAR-TILL-DLI.                                                    
012101     03  W-WDH501KY-X.                                                    
012102         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
012103         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
012104     03  W-KDEKSHT-X.                                                     
012105         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
012106     03  W-KDEKNIVA-X.                                                    
012107         05  W-KDEKNIVA          PIC X(5)    VALUE SPACE.                 
012108     03  W-WDH531KY-X.                                                    
012110         05  W-IDSYSMOT          PIC X(6)    VALUE SPACE.                 
012120         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
012130         05  W-IDSEKVNR          PIC S9(3)  COMP-3  VALUE +1.             
012200     SKIP2                                                                
012300*    --- STATUS-KOD FRÅN IMS                                              
012400 01  STATUS-WS                   PIC XX.                                  
012500     88  SEGMENT-FINNS                       VALUE '  '.                  
012600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012800     SKIP2                                                                
012900 01  GODK-STATUSKODER.                                                    
013000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013100     SKIP3                                                                
013200 01  SSA1                        PIC X(64).                               
013300 01  SSA2                        PIC X(64).                               
013310 01  SSA3                        PIC X(64).                               
013320 01  SSA4                        PIC X(64).                               
013400     EJECT                                                                
013500*    --- IMS FUNKTIONSKODER                                               
013600*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000                                                                          
014101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
014102 01  DLI-IO-WDH501.                                                       
014103*    03  -COPY WDH501                                                     
014104     EJECT                                                                
014105 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
014106 01  DLI-IO-WDH511.                                                       
014107*    03  -COPY WDH511                                                     
014108     EJECT                                                                
014109 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
014110 01  DLI-IO-WDH521.                                                       
014111*    03  -COPY WDH521                                                     
014112     EJECT                                                                
014113 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
014114 01  DLI-IO-WDH531.                                                       
014120*    03  -COPY WDH531                                                     
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600*01  -COPY W0009   -PRE MSG-                                              
014601     EJECT                                                                
014610*01  -COPY W0009   -PRE ALT1-                                             
014620     EJECT                                                                
014630*01  -COPY W0009   -PRE ALT2-                                             
014640     EJECT                                                                
014650*01  -COPY W0009   -PRE ALT3-                                             
014660     EJECT                                                                
014700*01  -COPY W0008   -PRE USEA-                                             
014800     05  FILLER                  PIC X.                                   
014901                                                                          
014902*01  -COPY W0008  -PRE WDH5-                                              
014910     05  FILLER                  PIC X.                                   
015000     EJECT                                                                
015101 PROCEDURE DIVISION  USING MSG-PCB  ALT1-PCB ALT2-PCB                     
015102                           ALT3-PCB USEA-PCB WDH5-PCB.                    
015103 MAIN SECTION.                                                            
015110     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB ALT2-PCB                     
015120                           ALT3-PCB USEA-PCB WDH5-PCB.                    
015200                                                                          
015400     PERFORM IMS-GET-MSG                                                  
015500     IF SEGMENT-FINNS                                                     
015600       PERFORM A-INIT                                                     
015700       PERFORM B-KOLLA-NYCKLAR                                            
015800       IF NYCKLAR-OK                                                      
015901         IF MFS-UPDATE                                                    
015902           PERFORM G-KOLLA-INPUT                                          
015903           IF INDATA-OK                                                   
015904             PERFORM H-UPPDATERA                                          
015905           END-IF                                                         
015910         ELSE                                                             
016101           IF MFS-FIRST                                                   
016102             PERFORM C-FOERSTA-SIDA                                       
016103           ELSE                                                           
016104             PERFORM E-SAMMA-SIDA                                         
016110           END-IF                                                         
016210         END-IF                                                           
016220         IF ALLT-OK                                                       
016300           PERFORM F-LAES-VISA-INFO                                       
016310         END-IF                                                           
016400       END-IF                                                             
016500       IF BYT-EJ-BILD                                                     
016510         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22101 + 4                    
016520         PERFORM IMS-INSERT-MSG                                           
016600       END-IF                                                             
016900     END-IF                                                               
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     IF MSG-DUBBLA-TRANSKODER                                             
017900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I22101                 
018000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018200     ELSE                                                                 
018300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I22101                  
018400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018600     END-IF                                                               
018700                                                                          
018800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019100                                                                          
019200     MOVE LOW-VALUE TO MSG-AREA                                           
019300     MOVE 'W5O221N1' TO MFS-IDMOD                                         
019400     MOVE '5221' TO MOD-IDTRANS                                           
019500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019600                                                                          
019700     IF EGEN-MID OR HELP-MID                                              
019800       CONTINUE                                                           
019900     ELSE                                                                 
020000       MOVE SPACE TO MFS-KDTRTYP                                          
020100       MOVE '7' TO MFS-IDPFK                                              
020200     END-IF                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 B-KOLLA-NYCKLAR SECTION.                                                 
020800                                                                          
020900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021000     MOVE '001'             TO MSGI-KDCALL                                
021100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021300     MOVE '5221'            TO MSGI-IDTRANS                               
022000                                                                          
022100     IF EGEN-MID OR HOPP-MID                                              
022101       MOVE MID-KDEKHHT-IN  TO MSGI-KDEKHHT                               
022102       MOVE MID-KDEKSHT-IN  TO MSGI-KDEKSHT                               
022103       MOVE MID-KDEKNIVA-IN TO MSGI-KDEKNIVA                              
022104       MOVE MID-IDSYSMOT-IN TO MSGI-IDSYSMOT                              
022105       MOVE MID-IDPTYP-IN   TO MSGI-IDPTYP                                
022116     END-IF                                                               
022117     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022118     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
022119                                                                          
022120     IF MSGI-IDLAND-SPR = 'GB'                                            
022121       MOVE 'GB' TO MED-IDSKYLT                                           
022122     ELSE                                                                 
022123       MOVE 'S' TO MED-IDSKYLT                                            
022124     END-IF                                                               
022130                                                                          
022134     MOVE JA TO ALLT-SW                                                   
022135     MOVE JA TO NYCKLAR-SW                                                
022136     MOVE NEJ TO BYT-SW                                                   
022137     MOVE SPACE TO MED-IDMFSFEL                                           
022138     MOVE SPACE TO MED-IDMFSINF                                           
022139                                                                          
022140     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
022141       MOVE MSGI-IDFTG      TO W-IDFTG                                    
022142                               SPAR-IDFTG                                 
022143     END-IF                                                               
022144                                                                          
022145*    -- KONTROLL AV KDEKHHT                                               
022146     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
022147                                                                          
022148     IF MSGI-KDEKHHT NUMERIC AND MSGI-KDEKHHT > ZERO                      
022149       MOVE MSGI-KDEKHHT    TO W-KDEKHHT                                  
022150                               SPAR-KDEKHHT                               
022151     ELSE                                                                 
022152       MOVE NEJ             TO NYCKLAR-SW                                 
022153     END-IF                                                               
022154                                                                          
022155*    -- KONTROLL AV KDEKSHT                                               
022156     MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT-IN                               
022157                                                                          
022158     IF MSGI-KDEKSHT NOT = ALL '+'                                        
022159       MOVE MSGI-KDEKSHT    TO W-KDEKSHT                                  
022160                               SPAR-KDEKSHT                               
022161     ELSE                                                                 
022162       MOVE NEJ             TO NYCKLAR-SW                                 
022163     END-IF                                                               
022164                                                                          
022165*    -- KONTROLL AV KDEKNIVA                                              
022166     MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-IN                              
022167                                                                          
022168     IF MSGI-KDEKNIVA NOT = ALL '+'                                       
022169       MOVE MSGI-KDEKNIVA   TO W-KDEKNIVA                                 
022170                               SPAR-KDEKNIVA                              
022171     ELSE                                                                 
022172       MOVE NEJ             TO NYCKLAR-SW                                 
022180     END-IF                                                               
022201                                                                          
022202*    -- KONTROLL AV IDSYSMOT                                              
022203     MOVE MFS-RENSA-FAELT TO MOD-IDSYSMOT-IN                              
022204                                                                          
022205     IF MSGI-IDSYSMOT NOT = ALL '+'                                       
022206       MOVE MSGI-IDSYSMOT   TO W-IDSYSMOT                                 
022207                               SPAR-IDSYSMOT                              
022208     ELSE                                                                 
022209       MOVE NEJ             TO NYCKLAR-SW                                 
022210     END-IF                                                               
022211                                                                          
022212*    -- KONTROLL AV IDPTYP                                                
022213     MOVE MFS-RENSA-FAELT TO MOD-IDPTYP-IN                                
022214                                                                          
022215     IF MSGI-IDPTYP NOT = ALL '+'                                         
022216       MOVE MSGI-IDPTYP   TO W-IDPTYP                                     
022217                             SPAR-IDPTYP                                  
022218     ELSE                                                                 
022219       MOVE NEJ             TO NYCKLAR-SW                                 
022220     END-IF                                                               
022221                                                                          
022222     IF GODK-MID OR NYCKLAR-OK                                            
022223       MOVE MSGI-IDFTG          TO MOD-IDFTG-UT                           
022224       MOVE MSGI-KDEKHHT        TO MOD-KDEKHHT-UT                         
022225       MOVE MSGI-KDEKSHT        TO MOD-KDEKSHT-UT                         
022226       MOVE MSGI-KDEKNIVA       TO MOD-KDEKNIVA-UT                        
022227       MOVE MSGI-IDSYSMOT       TO MOD-IDSYSMOT-UT                        
022228       MOVE MSGI-IDPTYP         TO MOD-IDPTYP-UT                          
022229     ELSE                                                                 
022230       MOVE MFS-RENSA-FAELT TO MOD-IDFTG-UT                               
022231                               MOD-KDEKHHT-UT                             
022232                               MOD-KDEKSHT-UT                             
022233                               MOD-KDEKNIVA-UT                            
022234                               MOD-IDSYSMOT-UT                            
022235                               MOD-IDPTYP-UT                              
022240     END-IF                                                               
022300                                                                          
022400     IF NYCKLAR-FEL                                                       
022410*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
022420*---GODKÄND BILD                                                          
022430       IF GODK-MID                                                        
022500         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
022600         CALL WMEDKONV USING MED-WMEDAREA                                 
022700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022800         PERFORM MFS-RENSA-FAELT-IN                                       
022900         PERFORM MFS-RENSA-FAELT-UT                                       
023000       END-IF                                                             
023010     END-IF                                                               
023100     .                                                                    
023300     EJECT                                                                
023401 C-FOERSTA-SIDA SECTION.                                                  
023402                                                                          
023403     PERFORM MFS-RENSA-FAELT-IN                                           
023404     .                                                                    
023405     EJECT                                                                
023406 E-SAMMA-SIDA SECTION.                                                    
023407                                                                          
023408     IF EGEN-MID OR HELP-MID                                              
023410       IF MID-DELHHT      = ALL '+'                                       
023411        AND MID-BEEKHHT   = ALL '+'                                       
023412        AND MID-BEEKSHT   = ALL '+'                                       
023413        AND MID-DELSHT    = ALL '+'                                       
023417        AND MID-DELNIVA   = ALL '+'                                       
023418        AND MID-DELKLIENT = ALL '+'                                       
023424         IF MID-KDEKHHT-IN    = ALL '+'                                   
023425          AND MID-KDEKSHT-IN  = ALL '+'                                   
023426          AND MID-KDEKNIVA-IN = ALL '+'                                   
023427          AND MID-IDSYSMOT-IN = ALL '+'                                   
023428          AND MID-IDPTYP-IN   = ALL '+'                                   
023429           IF MID-LEVELPRM = 'Y' OR MID-PROFIL = 'Y'                      
023431             PERFORM I-BYT-BILD                                           
023432           ELSE                                                           
023434             PERFORM I-5213-BYT-BILD                                      
023436           END-IF                                                         
023437*---HÄR NEDAN KOMMER HOPP TILL 5213-BILD                                  
023438         END-IF                                                           
023439*******  PERFORM MFS-RENSA-FAELT-IN                                       
023440       ELSE                                                               
023441*---KOLLAR OM NIVÅPARAMETER EL. PROFILDATA ÄR IFYLLT SAMTIDIGT            
023442*---MED NÅGOT DEL-FÄLT, ISÅFALL HAR NIVÅ OCH PROFIL FÖRETRÄDE             
023443         IF MID-LEVELPRM = 'Y' OR MID-PROFIL = 'Y'                        
023444           PERFORM I-BYT-BILD                                             
023445*        ELSE                                                             
023446*          MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                            
023447*          CALL WMEDKONV USING MED-WMEDAREA                               
023448*          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
023449*          PERFORM MFS-ROER-EJ-FAELT-IN                                   
023450*          PERFORM MFS-ROER-EJ-FAELT-UT2                                  
023451*          PERFORM MFS-LAES-IN-IGEN                                       
023452*          MOVE NEJ TO ALLT-SW                                            
023453         END-IF                                                           
023454       END-IF                                                             
023455     ELSE                                                                 
023456       PERFORM MFS-RENSA-FAELT-IN                                         
023457     END-IF                                                               
023458     .                                                                    
023460     EJECT                                                                
023500 F-LAES-VISA-INFO SECTION.                                                
023600                                                                          
023700     PERFORM IMS-GU-HHT                                                   
023800                                                                          
023900     IF SEGMENT-SAKNAS                                                    
024000        IF MED-IDMFSFEL = SPACE                                           
024100          MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                            
024200        END-IF                                                            
024300        CALL WMEDKONV USING MED-WMEDAREA                                  
024400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024500        PERFORM MFS-RENSA-FAELT-UT                                        
024600     ELSE                                                                 
024700       MOVE HHT-KDEKHHT TO MOD-KDEKHHT                                    
025000       MOVE HHT-BEEKHHT TO MOD-BEEKHHT                                    
025200       PERFORM FA-LAES-VISA-SHT                                           
025300       PERFORM FB-LAES-VISA-NIVA                                          
025310       PERFORM FC-LAES-VISA-SYST                                          
025400     END-IF                                                               
025500                                                                          
025600     MOVE '002'     TO MSGI-KDCALL                                        
025700     MOVE '5221'    TO SPAR-IDTRANS                                       
025701     MOVE '5221'    TO SPAR-BILD                                          
025702     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
025703     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025704     .                                                                    
025705     EJECT                                                                
025706 FA-LAES-VISA-SHT SECTION.                                                
025707                                                                          
025708       PERFORM IMS-GU-SHT                                                 
025709                                                                          
025710       IF SEGMENT-SAKNAS                                                  
025711          IF MED-IDMFSFEL = SPACE                                         
025712            MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                          
025713          END-IF                                                          
025714          CALL WMEDKONV USING MED-WMEDAREA                                
025715          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
025716          PERFORM MFS-RENSA-FAELT-UT                                      
025717       ELSE                                                               
025718         MOVE SHT-KDEKSHT TO MOD-KDEKSHT                                  
025720         MOVE SHT-BEEKSHT TO MOD-BEEKSHT                                  
025722       END-IF                                                             
025723     .                                                                    
025724     EJECT                                                                
025725 FB-LAES-VISA-NIVA SECTION.                                               
025726                                                                          
025727     PERFORM IMS-GU-NIVA                                                  
025728     IF SEGMENT-SAKNAS                                                    
025729       IF MED-IDMFSFEL = SPACE                                            
025730         MOVE KDEKNIVA-MISSING TO MED-IDMFSFEL                            
025731       END-IF                                                             
025732       CALL WMEDKONV USING MED-WMEDAREA                                   
025733       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025734       PERFORM MFS-RENSA-FAELT-UT                                         
025735     ELSE                                                                 
025736       MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA                                 
025740     END-IF                                                               
025741     .                                                                    
025742     EJECT                                                                
025743 FC-LAES-VISA-SYST SECTION.                                               
025744                                                                          
025747     PERFORM IMS-GU-SYST                                                  
025748                                                                          
025749     IF SEGMENT-SAKNAS                                                    
025750       IF MED-IDMFSFEL = SPACE                                            
025751         MOVE SYSTNAME-MISSING TO MED-IDMFSFEL                            
025752       END-IF                                                             
025753       CALL WMEDKONV USING MED-WMEDAREA                                   
025754       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025755       PERFORM MFS-RENSA-FAELT-UT                                         
025756     ELSE                                                                 
025757       MOVE SYST-IDSYSMOT TO MOD-IDSYSMOT                                 
025758       MOVE SYST-IDPTYP   TO MOD-IDPTYP                                   
025759     END-IF                                                               
025772     .                                                                    
025773     EJECT                                                                
025774 G-KOLLA-INPUT SECTION.                                                   
025775                                                                          
025776     MOVE JA  TO INDATA-SW                                                
025777     IF MID-BEEKHHT     = ALL '+'                                         
025778      AND MID-DELHHT    = ALL '+'                                         
025779      AND MID-BEEKSHT   = ALL '+'                                         
025780      AND MID-DELSHT    = ALL '+'                                         
025782      AND MID-LEVELPRM  = ALL '+'                                         
025783      AND MID-DELNIVA   = ALL '+'                                         
025784      AND MID-DELKLIENT = ALL '+'                                         
025785      AND MID-PROFIL    = ALL '+'                                         
025786       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
025787       CALL WMEDKONV USING MED-WMEDAREA                                   
025788       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
025789       PERFORM MFS-ROER-EJ-FAELT-IN                                       
025790       PERFORM MFS-ROER-EJ-FAELT-UT                                       
025791       MOVE NEJ TO INDATA-SW                                              
025792     ELSE                                                                 
025793                                                                          
025794       IF MID-DELHHT         NOT = ALL '+'                                
025795        AND (MID-DELSHT      NOT = ALL '+'                                
025802            OR MID-LEVELPRM  NOT = ALL '+'                                
025803            OR MID-DELNIVA   NOT = ALL '+'                                
025804            OR MID-DELKLIENT NOT = ALL '+'                                
025805            OR MID-PROFIL    NOT = ALL '+')                               
025806          MOVE MFS-ALFA-FAELT-FEL TO MOD-DELHHT-ATTR                      
025807          MOVE NEJ TO INDATA-SW                                           
025808       ELSE                                                               
025809          MOVE MFS-ALFA-FAELT-RAETT TO MOD-DELHHT-ATTR                    
025810       END-IF                                                             
025811                                                                          
025812       IF MID-DELSHT         NOT = ALL '+'                                
025813        AND (MID-DELHHT      NOT = ALL '+'                                
025816            OR MID-LEVELPRM  NOT = ALL '+'                                
025817            OR MID-DELNIVA   NOT = ALL '+'                                
025818            OR MID-DELKLIENT NOT = ALL '+'                                
025819            OR MID-PROFIL    NOT = ALL '+')                               
025820          MOVE MFS-ALFA-FAELT-FEL TO MOD-DELSHT-ATTR                      
025821          MOVE NEJ TO INDATA-SW                                           
025822       ELSE                                                               
025823          MOVE MFS-ALFA-FAELT-RAETT TO MOD-DELSHT-ATTR                    
025824       END-IF                                                             
025825                                                                          
025826       IF MID-DELNIVA        NOT = ALL '+'                                
025827        AND (MID-DELSHT      NOT = ALL '+'                                
025830            OR MID-LEVELPRM  NOT = ALL '+'                                
025831            OR MID-DELHHT    NOT = ALL '+'                                
025832            OR MID-DELKLIENT NOT = ALL '+'                                
025833            OR MID-PROFIL    NOT = ALL '+')                               
025834          MOVE MFS-ALFA-FAELT-FEL TO MOD-DELNIVA-ATTR                     
025835          MOVE NEJ TO INDATA-SW                                           
025836       ELSE                                                               
025837          MOVE MFS-ALFA-FAELT-RAETT TO MOD-DELNIVA-ATTR                   
025838       END-IF                                                             
025839                                                                          
025840       IF MID-DELKLIENT     NOT = ALL '+'                                 
025841        AND (MID-DELSHT     NOT = ALL '+'                                 
025844            OR MID-LEVELPRM NOT = ALL '+'                                 
025845            OR MID-DELNIVA  NOT = ALL '+'                                 
025846            OR MID-DELHHT   NOT = ALL '+'                                 
025847            OR MID-PROFIL   NOT = ALL '+')                                
025848          MOVE MFS-ALFA-FAELT-FEL TO MOD-DELKLIENT-ATTR                   
025849          MOVE NEJ TO INDATA-SW                                           
025850       ELSE                                                               
025851          MOVE MFS-ALFA-FAELT-RAETT TO MOD-DELKLIENT-ATTR                 
025852       END-IF                                                             
025853                                                                          
025854       IF MID-LEVELPRM       NOT = ALL '+'                                
025855        AND (MID-DELHHT      NOT = ALL '+'                                
025857            OR MID-DELSHT    NOT = ALL '+'                                
025860            OR MID-DELNIVA   NOT = ALL '+'                                
025861            OR MID-DELKLIENT NOT = ALL '+'                                
025862            OR MID-PROFIL    NOT = ALL '+')                               
025863          MOVE MFS-ALFA-FAELT-FEL TO MOD-LEVELPRM-ATTR                    
025864          MOVE NEJ TO INDATA-SW                                           
025865       ELSE                                                               
025866          MOVE MFS-ALFA-FAELT-RAETT TO MOD-LEVELPRM-ATTR                  
025867       END-IF                                                             
025868                                                                          
025884       IF MID-PROFIL         NOT = ALL '+'                                
025885        AND (MID-DELSHT      NOT = ALL '+'                                
025889            OR MID-LEVELPRM  NOT = ALL '+'                                
025890            OR MID-DELNIVA   NOT = ALL '+'                                
025891            OR MID-DELKLIENT NOT = ALL '+'                                
025892            OR MID-DELHHT    NOT = ALL '+')                               
025893          MOVE MFS-ALFA-FAELT-FEL TO MOD-PROFIL-ATTR                      
025894          MOVE NEJ TO INDATA-SW                                           
025895       ELSE                                                               
025896          MOVE MFS-ALFA-FAELT-RAETT TO MOD-PROFIL-ATTR                    
025897       END-IF                                                             
025898                                                                          
025919       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEKHHT-ATTR                      
025922                                                                          
025932       MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEKSHT-ATTR                      
025935                                                                          
025936       IF INDATA-FEL                                                      
025937         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
025938         CALL WMEDKONV USING MED-WMEDAREA                                 
025939         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
025940         PERFORM MFS-ROER-EJ-FAELT-UT                                     
025941         PERFORM MFS-ROER-EJ-FAELT-IN                                     
025942         PERFORM MFS-LAES-IN-IGEN3                                        
025943          MOVE NEJ TO ALLT-SW                                             
025944       ELSE                                                               
025945*---KOLLAR ATT KDEKHHT FINNS REGISTRERAD I BASEN                          
025946         PERFORM IMS-GU-HHT                                               
025947         IF SEGMENT-SAKNAS                                                
025948           IF MED-IDMFSFEL = SPACE                                        
025950             MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                         
025960           END-IF                                                         
025961         ELSE                                                             
025962*---KOLLAR ATT KDEKSHT FINNS REGISTRERAD I BASEN                          
025963           PERFORM IMS-GU-SHT                                             
025964           IF SEGMENT-SAKNAS                                              
025965             IF MED-IDMFSFEL = SPACE                                      
025966               MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                       
025967             END-IF                                                       
025968           ELSE                                                           
025969*---KOLLAR ATT KDEKNIVA FINNS REGISTRERAD I BASEN                         
025970             PERFORM IMS-GU-NIVA                                          
025971             IF SEGMENT-SAKNAS                                            
025972               IF MED-IDMFSFEL = SPACE                                    
025973                 MOVE KDEKNIVA-MISSING TO MED-IDMFSFEL                    
025974               END-IF                                                     
025975             END-IF                                                       
025976           END-IF                                                         
025977         END-IF                                                           
025978       END-IF                                                             
025980                                                                          
025981       IF SEGMENT-SAKNAS                                                  
025982         CALL WMEDKONV USING MED-WMEDAREA                                 
025983         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
025984         PERFORM MFS-RENSA-FAELT-IN                                       
025985         PERFORM MFS-RENSA-FAELT-UT                                       
025986*---OM SEGMENT SAKNAS KAN INGEN UPPDATERING SKE                           
025987*---DÄRFÖR ANVÄNDS DENNA SWITCH                                           
025988         MOVE NEJ TO INDATA-SW                                            
025989*---OM INTE SEGMENT FINNS BEHÖVER MAN EJ GÅ IN I                          
025990*---F-SECTIONEN. DÄRFÖR ANVÄNDS DENNA SWITCH                              
025991         MOVE NEJ TO ALLT-SW                                              
025992       END-IF                                                             
025993     END-IF                                                               
025994     .                                                                    
025995     EJECT                                                                
025996 I-BYT-BILD SECTION.                                                      
025997     SKIP2                                                                
025998     MOVE JA TO BYT-SW                                                    
026017                                                                          
026018*---TESTER FÖR ATT KOLLA OM 'Y' ÄR IFYLLT I DEL-FÄLT                      
026019*---SAMTIDIGT MED LEVEL EL. PROFIL-FÄLT                                   
026020     IF MID-LEVELPRM = 'Y' AND MID-DELHHT = 'Y'                           
026022       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELHHT-ATTR                         
026023       MOVE NEJ TO BYT-SW                                                 
026031     END-IF                                                               
026032                                                                          
026033     IF MID-LEVELPRM = 'Y' AND MID-DELSHT = 'Y'                           
026034       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELSHT-ATTR                         
026035       MOVE NEJ TO BYT-SW                                                 
026043     END-IF                                                               
026044                                                                          
026045     IF MID-LEVELPRM = 'Y' AND MID-DELNIVA = 'Y'                          
026046       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELNIVA-ATTR                        
026047       MOVE NEJ TO BYT-SW                                                 
026048     END-IF                                                               
026049                                                                          
026050     IF MID-LEVELPRM = 'Y' AND MID-DELKLIENT = 'Y'                        
026051       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELKLIENT-ATTR                      
026052       MOVE NEJ TO BYT-SW                                                 
026053     END-IF                                                               
026054                                                                          
026055     IF MID-PROFIL = 'Y' AND MID-DELHHT = 'Y'                             
026056       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELHHT-ATTR                         
026057       MOVE NEJ TO BYT-SW                                                 
026063     END-IF                                                               
026064                                                                          
026065     IF MID-PROFIL = 'Y' AND MID-DELSHT = 'Y'                             
026066       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELSHT-ATTR                         
026067       MOVE NEJ TO BYT-SW                                                 
026068     END-IF                                                               
026069                                                                          
026070     IF MID-PROFIL = 'Y' AND MID-DELNIVA = 'Y'                            
026071       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELNIVA-ATTR                        
026072       MOVE NEJ TO BYT-SW                                                 
026073     END-IF                                                               
026074                                                                          
026075     IF MID-PROFIL = 'Y' AND MID-DELKLIENT = 'Y'                          
026076       MOVE MFS-ALFA-FAELT-FEL TO MOD-DELKLIENT-ATTR                      
026077       MOVE NEJ TO BYT-SW                                                 
026078     END-IF                                                               
026079                                                                          
026080     IF BYT-BILD                                                          
026081*---OM MAN FÖRSÖKER ATT VISA BÅDE PROFIL OCH NIVÅBILD                     
026082*---LYSES FÄLT UPP FÖR ATT AVMARKERA ETT VAL                              
026083       IF MID-LEVELPRM = 'Y' AND MID-PROFIL = 'Y'                         
026084          MOVE MFS-ALFA-FAELT-FEL TO MOD-PROFIL-ATTR                      
026085          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
026086          CALL WMEDKONV USING MED-WMEDAREA                                
026087          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
026088          PERFORM MFS-ROER-EJ-FAELT-UT                                    
026089          PERFORM MFS-ROER-EJ-FAELT-IN                                    
026090          PERFORM MFS-LAES-IN-IGEN2                                       
026091          MOVE NEJ TO BYT-SW                                              
026092       ELSE                                                               
026093          MOVE JA TO BYT-SW                                               
026094          MOVE NEJ TO ALLT-SW                                             
026097         IF MID-LEVELPRM = 'Y'                                            
026098* ---SKICKAR VÄRDE TILL 5219-MID FÖR ATT SEDAN                            
026099* ---STARTA UPP 5219-BILDEN                                               
026100           MOVE LOW-VALUE       TO 5219-MID-W5I21901                      
026101           MOVE SPAR-KDEKHHT    TO 5219-MID-KDEKHHT-IN                    
026102           MOVE SPAR-KDEKSHT    TO 5219-MID-KDEKSHT-IN                    
026103           MOVE SPAR-KDEKNIVA   TO 5219-MID-KDEKNIVA-IN                   
026104           MOVE SPAR-AREA       TO MSGI-SPAR-AREA                         
026105           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
026106           COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22101 + 17                 
026107           PERFORM IMS-INSERT-ALT-MSG-5219                                
026117         ELSE                                                             
026118* ---SKICKAR VÄRDE TILL 5218-MID FÖR ATT SEDAN                            
026119* ---STARTA UPP 5218-BILDEN                                               
026120           MOVE LOW-VALUE       TO 5218-MID-W5I21801                      
026121           MOVE SPAR-KDEKHHT    TO 5218-MID-KDEKHHT-IN                    
026122           MOVE SPAR-KDEKSHT    TO 5218-MID-KDEKSHT-IN                    
026123           MOVE SPAR-KDEKNIVA   TO 5218-MID-KDEKNIVA-IN                   
026124           MOVE SPAR-IDSYSMOT   TO 5218-MID-IDSYSMOT-IN                   
026125           MOVE SPAR-IDPTYP     TO 5218-MID-IDPTYP-IN                     
026126           COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22101 + 17                 
026127           PERFORM IMS-INSERT-ALT-MSG-5218                                
026128         END-IF                                                           
026129       END-IF                                                             
026130     ELSE                                                                 
026131*---HÄNGER IHOP MED TESTERNA PÅ DEL-FÄLTEN                                
026132       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
026133       CALL WMEDKONV USING MED-WMEDAREA                                   
026134       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
026135       PERFORM MFS-ROER-EJ-FAELT-UT                                       
026136       PERFORM MFS-ROER-EJ-FAELT-IN                                       
026137       PERFORM MFS-LAES-IN-IGEN4                                          
026138       MOVE NEJ TO ALLT-SW                                                
026139     END-IF                                                               
026140     .                                                                    
026141     EJECT                                                                
026142 I-5213-BYT-BILD SECTION.                                                 
026143     SKIP2                                                                
026144     MOVE JA TO BYT-SW                                                    
026145     MOVE NEJ TO ALLT-SW                                                  
026146                                                                          
026147* ---SKICKAR VÄRDE TILL 5213-MID FÖR ATT SEDAN                            
026148* ---STARTA UPP 5213-BILDEN                                               
026149     MOVE LOW-VALUE            TO 5213-MID-W5I21301                       
026150     MOVE SPAR-KDEKHHT         TO 5213-MID-KDEKHHT-IN                     
026151*    MOVE SPAR-KDEKSHT         TO 5213-MID-KDEKSHT-IN                     
026152*    MOVE SPAR-KDEKNIVA        TO 5213-MID-KDEKNIVA-IN                    
026153*    MOVE SPAR-IDSYSMOT        TO 5213-MID-IDSYSMOT-IN                    
026154*    MOVE SPAR-IDPTYP          TO 5213-MID-IDPTYP-IN                      
026155     MOVE SPACE                TO 5213-MID-KDEKSHT-IN                     
026156                                  5213-MID-KDEKNIVA-IN                    
026157                                  5213-MID-IDSYSMOT-IN                    
026158                                  5213-MID-IDPTYP-IN                      
026159     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O22101 + 17                       
026160     PERFORM IMS-INSERT-ALT-MSG-5213                                      
026161     .                                                                    
026162     EJECT                                                                
026163 H-UPPDATERA SECTION.                                                     
026164                                                                          
026165     IF MID-DELKLIENT = 'Y'                                               
026166       PERFORM IMS-GHNP-SYST                                              
026167       PERFORM UNTIL SEGMENT-SAKNAS                                       
026168         IF SEGMENT-FINNS                                                 
026169           IF MSGI-IDSYSMOT = SYST-IDSYSMOT                               
026170             PERFORM IMS-DLET-SYST                                        
026171           END-IF                                                         
026172         END-IF                                                           
026173       PERFORM IMS-GHNP-SYST                                              
026174       END-PERFORM                                                        
026175     END-IF                                                               
026176                                                                          
026177     IF MID-DELNIVA = 'Y'                                                 
026178       PERFORM IMS-GU-NIVA                                                
026179       IF SEGMENT-FINNS                                                   
026180         PERFORM IMS-DLET-NIVA                                            
026181       END-IF                                                             
026182     END-IF                                                               
026183                                                                          
026184     IF MID-DELSHT = 'Y' OR MID-BEEKSHT NOT = ALL '+'                     
026185       PERFORM IMS-GU-SHT                                                 
026186       IF SEGMENT-FINNS                                                   
026187         IF MID-DELSHT = 'Y'                                              
026188           PERFORM IMS-DLET-SHT                                           
026189         ELSE                                                             
026190           IF MID-BEEKSHT NOT = ALL '+'                                   
026191             MOVE MID-BEEKSHT TO SHT-BEEKSHT                              
026192             PERFORM IMS-REPL-SHT                                         
026193           END-IF                                                         
026194         END-IF                                                           
026195       END-IF                                                             
026196     END-IF                                                               
026197                                                                          
026198     IF MID-DELHHT = 'Y' OR MID-BEEKHHT NOT = ALL '+'                     
026199       PERFORM IMS-GU-HHT                                                 
026200       IF SEGMENT-FINNS                                                   
026201         IF MID-DELHHT = 'Y'                                              
026202           PERFORM IMS-DLET-HHT                                           
026203         ELSE                                                             
026204           IF MID-BEEKHHT NOT = ALL '+'                                   
026205             MOVE MID-BEEKHHT TO HHT-BEEKHHT                              
026206             PERFORM IMS-REPL-HHT                                         
026207           END-IF                                                         
026208         END-IF                                                           
026209       END-IF                                                             
026210     END-IF                                                               
026211                                                                          
026212     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
026213     CALL WMEDKONV USING MED-WMEDAREA                                     
026214     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
026215     PERFORM MFS-FORM-ATTR                                                
026216     PERFORM MFS-RENSA-FAELT-IN                                           
026217     .                                                                    
026218     EJECT                                                                
026219 MFS-RENSA-FAELT-UT SECTION.                                              
026220                                                                          
026230*    --- ALLA UTDATA-FÄLT                                                 
026300     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT                                  
026400                             MOD-BEEKHHT                                  
026410                             MOD-DELHHT                                   
026420                             MOD-KDEKSHT                                  
026430                             MOD-BEEKSHT                                  
026440                             MOD-DELSHT                                   
026450                             MOD-KDEKNIVA                                 
026470                             MOD-LEVELPRM                                 
026480                             MOD-DELNIVA                                  
026490                             MOD-IDSYSMOT                                 
026491                             MOD-DELKLIENT                                
026492                             MOD-IDPTYP                                   
026493                             MOD-PROFIL                                   
026500     .                                                                    
026700     SKIP3                                                                
026800 MFS-RENSA-FAELT-IN SECTION.                                              
026900                                                                          
027000*    --- ALLA INDATA-FÄLT                                                 
027100     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
027200                             MOD-KDEKSHT-IN                               
027210                             MOD-KDEKNIVA-IN                              
027220                             MOD-IDSYSMOT-IN                              
027230                             MOD-IDPTYP-IN                                
027240                             MOD-BEEKHHT                                  
027250                             MOD-DELHHT                                   
027260                             MOD-BEEKSHT                                  
027270                             MOD-DELSHT                                   
027290                             MOD-LEVELPRM                                 
027291                             MOD-DELNIVA                                  
027292                             MOD-DELKLIENT                                
027293                             MOD-PROFIL                                   
027300     .                                                                    
027400     EJECT                                                                
028310 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
028320                                                                          
028330*    --- ALLA UTDATA-FÄLT                                                 
028340     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFTG-UT                               
028341                               MOD-KDEKHHT-UT                             
028350                               MOD-KDEKSHT-UT                             
028360                               MOD-KDEKNIVA-UT                            
028361                               MOD-IDSYSMOT-UT                            
028362                               MOD-IDPTYP-UT                              
028370                               MOD-KDEKHHT                                
028380                               MOD-BEEKHHT                                
028390                               MOD-DELHHT                                 
028391                               MOD-KDEKSHT                                
028392                               MOD-BEEKSHT                                
028393                               MOD-DELSHT                                 
028394                               MOD-KDEKNIVA                               
028395                               MOD-LEVELPRM                               
028396                               MOD-DELNIVA                                
028397                               MOD-IDSYSMOT                               
028398                               MOD-DELKLIENT                              
028399                               MOD-IDPTYP                                 
028400                               MOD-PROFIL                                 
028401     .                                                                    
028402     SKIP3                                                                
028410 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028500                                                                          
028600*    --- ALLA INDATA-FÄLT                                                 
028700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKHHT-IN                             
028710                               MOD-KDEKSHT-IN                             
028720                               MOD-KDEKNIVA-IN                            
028730                               MOD-IDSYSMOT-IN                            
028740                               MOD-IDPTYP-IN                              
028750                               MOD-BEEKHHT                                
028760                               MOD-DELHHT                                 
028770                               MOD-BEEKSHT                                
028780                               MOD-DELSHT                                 
028791                               MOD-LEVELPRM                               
028792                               MOD-DELNIVA                                
028793                               MOD-DELKLIENT                              
028794                               MOD-PROFIL                                 
028900     .                                                                    
029000     EJECT                                                                
029100 MFS-FORM-ATTR SECTION.                                                   
029200                                                                          
029300*    --- ALLA INDATA-FÄLT                                                 
029400     MOVE MFS-FORMATETS-ATTR TO MOD-DELHHT-ATTR                           
029500                                MOD-DELSHT-ATTR                           
029520                                MOD-LEVELPRM-ATTR                         
029530                                MOD-DELNIVA-ATTR                          
029540                                MOD-DELKLIENT-ATTR                        
029550                                MOD-PROFIL-ATTR                           
029600     .                                                                    
029700     SKIP2                                                                
029800 MFS-LAES-IN-IGEN2 SECTION.                                               
029900                                                                          
030000*    --- ALLA INDATA-FÄLT                                                 
030100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-LEVELPRM-ATTR                      
030300     .                                                                    
030400     EJECT                                                                
030401 MFS-LAES-IN-IGEN3 SECTION.                                               
030402                                                                          
030403*    --- ALLA INDATA-FÄLT                                                 
030404     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEEKHHT-ATTR                       
030405                                   MOD-BEEKSHT-ATTR                       
030406     .                                                                    
030407     EJECT                                                                
030408 MFS-LAES-IN-IGEN4 SECTION.                                               
030409                                                                          
030410*    --- ALLA INDATA-FÄLT                                                 
030411     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-LEVELPRM-ATTR                      
030412                                   MOD-PROFIL-ATTR                        
030413     .                                                                    
030494     EJECT                                                                
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
032000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
032100     MOVE SPACE TO GODK-STATUSKODER                                       
032200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032601     EJECT                                                                
032602 IMS-INSERT-ALT-MSG-5213 SECTION.                                         
032603                                                                          
032604     MOVE SPACE TO GODK-STATUSKODER                                       
032605     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW-5213              
032606     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
032607     PERFORM IMS-STATUSKONTROLL                                           
032608     .                                                                    
032609     EJECT                                                                
032610 IMS-INSERT-ALT-MSG-5218 SECTION.                                         
032611                                                                          
032612     MOVE SPACE TO GODK-STATUSKODER                                       
032613     CALL CBLTDLI USING ISRT ALT2-PCB W-PROG-TO-PROG-SW-5218              
032614     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
032615     PERFORM IMS-STATUSKONTROLL                                           
032616     .                                                                    
032617     EJECT                                                                
032618 IMS-INSERT-ALT-MSG-5219 SECTION.                                         
032619                                                                          
032620     MOVE SPACE TO GODK-STATUSKODER                                       
032621     CALL CBLTDLI USING ISRT ALT3-PCB W-PROG-TO-PROG-SW-5219              
032622     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
032623     PERFORM IMS-STATUSKONTROLL                                           
032624     .                                                                    
032625     EJECT                                                                
032626 IMS-GU-HHT SECTION.                                                      
032627                                                                          
032628     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
032629          DELIMITED BY SIZE INTO SSA1                                     
032630     MOVE '  GE' TO GODK-STATUSKODER                                      
032631     CALL CBLTDLI USING GHU WDH5-PCB DLI-IO-WDH501 SSA1                   
032632     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032633     PERFORM IMS-STATUSKONTROLL                                           
032634     .                                                                    
032635     SKIP3                                                                
032636 IMS-REPL-HHT SECTION.                                                    
032637                                                                          
032638     MOVE '  ' TO GODK-STATUSKODER                                        
032639     CALL CBLTDLI USING REPL WDH5-PCB DLI-IO-WDH501                       
032640     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032641     PERFORM IMS-STATUSKONTROLL                                           
032642     .                                                                    
032643     SKIP3                                                                
032644 IMS-DLET-HHT SECTION.                                                    
032645                                                                          
032648     MOVE '  ' TO GODK-STATUSKODER                                        
032649     CALL CBLTDLI USING DLET WDH5-PCB DLI-IO-WDH501                       
032650     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032651     PERFORM IMS-STATUSKONTROLL                                           
032652     .                                                                    
032653     EJECT                                                                
032654 IMS-GU-SHT SECTION.                                                      
032655                                                                          
032656     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
032657          DELIMITED BY SIZE INTO SSA1                                     
032658     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
032659          DELIMITED BY SIZE INTO SSA2                                     
032660     MOVE '  GE' TO GODK-STATUSKODER                                      
032661     CALL CBLTDLI USING GHU WDH5-PCB DLI-IO-WDH511 SSA1                   
032662                                                   SSA2                   
032663     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032664     PERFORM IMS-STATUSKONTROLL                                           
032665     .                                                                    
032666     SKIP3                                                                
032667 IMS-REPL-SHT SECTION.                                                    
032668                                                                          
032669     MOVE '  ' TO GODK-STATUSKODER                                        
032670     CALL CBLTDLI USING REPL WDH5-PCB DLI-IO-WDH511                       
032671     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032672     PERFORM IMS-STATUSKONTROLL                                           
032673     .                                                                    
032674     SKIP3                                                                
032675 IMS-DLET-SHT SECTION.                                                    
032676                                                                          
032677     MOVE '  ' TO GODK-STATUSKODER                                        
032678     CALL CBLTDLI USING DLET WDH5-PCB DLI-IO-WDH511                       
032679     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032680     PERFORM IMS-STATUSKONTROLL                                           
032681     .                                                                    
032682     EJECT                                                                
032683 IMS-GU-NIVA SECTION.                                                     
032684                                                                          
032685     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
032686          DELIMITED BY SIZE INTO SSA1                                     
032687     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
032688          DELIMITED BY SIZE INTO SSA2                                     
032689     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
032690          DELIMITED BY SIZE INTO SSA3                                     
032691     MOVE '  GE' TO GODK-STATUSKODER                                      
032692     CALL CBLTDLI USING GHU WDH5-PCB DLI-IO-WDH521 SSA1                   
032694                                                   SSA2                   
032695                                                   SSA3                   
032696     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032697     PERFORM IMS-STATUSKONTROLL                                           
032698     .                                                                    
032699     SKIP3                                                                
032717 IMS-DLET-NIVA SECTION.                                                   
032718                                                                          
032719     MOVE '  ' TO GODK-STATUSKODER                                        
032720     CALL CBLTDLI USING DLET WDH5-PCB DLI-IO-WDH521                       
032721     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032722     PERFORM IMS-STATUSKONTROLL                                           
032723     .                                                                    
032724     EJECT                                                                
032725 IMS-GU-SYST SECTION.                                                     
032726                                                                          
032727     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
032728          DELIMITED BY SIZE INTO SSA1                                     
032729     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
032730          DELIMITED BY SIZE INTO SSA2                                     
032731     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
032732          DELIMITED BY SIZE INTO SSA3                                     
032733     STRING 'WDH531  (WDH531KY =' W-WDH531KY-X ')'                        
032734          DELIMITED BY SIZE INTO SSA4                                     
032735     MOVE '  GE' TO GODK-STATUSKODER                                      
032737     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH531 SSA1                    
032738                                                  SSA2                    
032739                                                  SSA3                    
032740                                                  SSA4                    
032741     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032742     PERFORM IMS-STATUSKONTROLL                                           
032743     .                                                                    
032744     EJECT                                                                
032745 IMS-GHNP-SYST SECTION.                                                   
032746                                                                          
032747     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
032748          DELIMITED BY SIZE INTO SSA1                                     
032749     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
032750          DELIMITED BY SIZE INTO SSA2                                     
032751     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
032752          DELIMITED BY SIZE INTO SSA3                                     
032753     MOVE 'WDH531   ' TO SSA4                                             
032755     MOVE '  GE' TO GODK-STATUSKODER                                      
032756     CALL CBLTDLI USING GHNP WDH5-PCB DLI-IO-WDH531 SSA1                  
032757                                                    SSA2                  
032758                                                    SSA3                  
032759                                                    SSA4                  
032760     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032761     PERFORM IMS-STATUSKONTROLL                                           
032762     .                                                                    
032763     EJECT                                                                
032764 IMS-DLET-SYST SECTION.                                                   
032765                                                                          
032766     MOVE '  ' TO GODK-STATUSKODER                                        
032767     CALL CBLTDLI USING DLET WDH5-PCB DLI-IO-WDH531                       
032768     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
032769     PERFORM IMS-STATUSKONTROLL                                           
032770     .                                                                    
032780     EJECT                                                                
032800 IMS-STATUSKONTROLL SECTION.                                              
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
