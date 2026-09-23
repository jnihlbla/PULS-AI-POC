000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1052500.                                                
000300 AUTHOR.         STEFAN ANDREASSON FRONTEC.                               
000400 DATE-WRITTEN.   NOV 1995.                                                
000881 DATE-COMPILED.                                                           
000882*                KOPIERAT PROGRAM W00719.                                 
000883*    FUNKTION.   TP-UPPDATERINGSPROGRAM.                                  
000884*                STARTAR SOP VIA W00606 FÖR OMBRYTNING AV                 
000890*                KATALOG.                                                 
000900*     *------------------------------------------------------*            
001000*     *            OBS FÖRE KOMPILERING: SE W-STORAGE        *            
001100*     *                FÖR RÄTT DSOUT-KORT TEST/PROD         *            
001200*     *------------------------------------------------------*            
001210*                                                                         
001211*    ÄNDRINGAR:                                                           
001212*        2004-03-10                                                       
001213*        Nya SOP-symbol values som behövs för streamserve                 
001214*                                                                         
001220*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W1T525         ENTER                                
001500*                     W1T525U        PF11                                 
001600*        MID:         W1I52501                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W1O52501                                            
002000*                                                                         
002100*        FELLOG                                                           
002200*                                                                         
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 WORKING-STORAGE SECTION.                                                 
002710*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(8)    VALUE 'W1052500'.            
003200                                                                          
003300 01  FILLER                      PIC X(16)   VALUE 'SUBPROGRAM'.          
003400 01  DYNAMISKA-SUBPROGRAM.                                                
003500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
003800                                                                          
003900     EJECT                                                                
004000*01  -COPY WDATAREA                                                       
004100     EJECT                                                                
004200*- - - - - - - - - - - -   - - - PARAMETRAR TILL SOP                      
004300 01  W-PROG-TO-PROG-SW.                                                   
004400*03  -COPY WMSGSOP                                                        
004500                                                                          
004600 01  FILLER                      PIC X(16)   VALUE 'KONTROLLER'.          
004700 01  KONTROLL-FALT.                                                       
004900     03  INDATA-OK               PIC X       VALUE 'J'.                   
005000     03  STARTA-JOB              PIC X       VALUE 'N'.                   
005100     03  SPRAK-KOD-OK            PIC X       VALUE 'J'.                   
005200     03  JOB-STARTAT             PIC X       VALUE 'N'.                   
005300                                                                          
005400******   W-TEST-FAELT ANVÄNDS ENDAST I TEST                               
005500                                                                          
005600 01 W-TEST-FAELT.                                                         
005700     03 W-TEST-KATNR             PIC X(5)   VALUE SPACE.                  
005800     03 FILLER                   PIC X      VALUE '/'.                    
005900     03 W-TEST-ARTNR             PIC X(9)   VALUE SPACE.                  
006000     03 FILLER                   PIC X      VALUE '/'.                    
006100     03 W-TEST-IDSKYLT-1         PIC X(3)   VALUE SPACE.                  
006200     03 FILLER                   PIC X      VALUE '/'.                    
006300     03 W-TEST-KATALOGTYP        PIC X      VALUE SPACE.                  
006400     03 FILLER                   PIC X      VALUE '/'.                    
006500     03 W-TEST-KDCATPUB          PIC X(6)   VALUE SPACE.                  
006600     03 FILLER                   PIC X      VALUE '/'.                    
006700     03 W-TEST-TIAAVV-PUBL       PIC X(4)   VALUE SPACE.                  
006800     03 FILLER                   PIC X      VALUE '/'.                    
006900     03 W-TEST-FL-SPADAT         PIC X      VALUE SPACE.                  
007000     03 FILLER                   PIC X      VALUE '/'.                    
007100     03 W-TEST-FL-MASTER         PIC X      VALUE SPACE.                  
007200                                                                          
007300 01 W-TEST-FAELT2.                                                        
007400     03 W-TEST-BEKOM-1           PIC X(40)  VALUE SPACE.                  
007500     03 FILLER                   PIC X      VALUE '/'.                    
007600                                                                          
007700 01  ARBETSFALT.                                                          
007800     03  A-IDJOB                 PIC X(8).                                
007900     03  A-CURRENT-DATE.                                                  
007901         05  A-DAGENS-TIAAAA     PIC 9(4)   VALUE ZERO.                   
007902         05  FILLER              PIC 9(4)   VALUE ZERO.                   
007903         05  FILLER              PIC 9(6)   VALUE ZERO.                   
007904                                                                          
007905     03  FILLER REDEFINES A-CURRENT-DATE.                                 
007910         05  A-DAGENS-SEKEL      PIC 9(2).                                
007920         05  A-DAGENS-DATUM      PIC 9(6).                                
007921         05  A-DAGENS-TID.                                                
007922             07 A-DAGENS-TIMME   PIC 9(2).                                
007923             07 A-DAGENS-MINUT   PIC 9(2).                                
007924             07 A-DAGENS-SEKUND  PIC 9(2).                                
007930                                                                          
008100     03  A-TID                   PIC S9(9)   COMP-3.                      
008200                                                                          
008300     03  W-DAGENS-VECKA              PIC X(4).                            
008400     03  W-INMATAD-VECKA             PIC X(5).                            
008500     03  W-INMATAD-VECKA-NUM         REDEFINES W-INMATAD-VECKA            
008600                                     PIC 9(5).                            
008700     03  W-KDCATPUB                  PIC X(6).                            
008800     03  W-PUBL-AAMMDD               PIC 9(6).                            
008810     03  W-TIOMBRYT-6                PIC 9(6)  VALUE ZERO.                
008820     03  W-TIOMBRYT-7                PIC 9(7)  VALUE ZERO.                
008900     03  W-AAR                       PIC 9(2).                            
009000     03  W-AAVV                      PIC 9(4).                            
009010     03 WS-GILTIGA-AAR.                                                   
009020       04 WS-TIAAAA              PIC 9(4)    VALUE ZERO                   
009030                                 OCCURS 4.                                
009100 01  WS-IDCATNR                  PIC X(5)    VALUE SPACE.                 
009200                                                                          
009300 01  WS-KDFORDON                 PIC XX.                                  
009400     88  GODK-KDFORDON           VALUES  'PV'  'NL'  'RE'.                
009500     88  VCBV-KATALOG            VALUE   'NL'.                            
009600     88  REKTABELL-KROCKKATALOG  VALUE   'RE'.                            
009601                                                                          
009610 01  WS-KDCATPUB-R-AVV           PIC X(3)    VALUE SPACE.                 
009620 01  WS-KDCATPUB-AAAAVV          PIC X(6)    VALUE SPACE.                 
009700                                                                          
009800 77  TAB-IX                      PIC S9(9) COMP-3 VALUE ZERO.             
009810 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
009900                                                                          
010000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
010100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'KONSTANTER'.          
010400 01  KONSTANTER.                                                          
010500     03  JA                      PIC X       VALUE 'J'.                   
010600     03  NEJ                     PIC X       VALUE 'N'.                   
010700     03  OK                      PIC X       VALUE 'O'.                   
010800     03  FEL                     PIC X       VALUE 'F'.                   
010900                                                                          
011000 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
011100                                                                          
011200     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'TIAAVV-PUBL'.         
015000 01  TIAAVV-PUBL.                                                         
015100     03  FILLER                  PIC X.                                   
015200     03  TIAAVV-PUBL-X           PIC X(4).                                
015300                                                                          
015400*               ******     DATA SOM SKICKAS TILL SOP                      
015500 01  PARM-TESYMBV.                                                        
015600     03  FILLER                  PIC X(4)    VALUE 'KAT('.                
015700     03  PARM-IDCATNR            PIC X(5).                                
015800     03  FILLER                  PIC X       VALUE ')'.                   
015900     03  FILLER                  PIC X(4)    VALUE 'TYP('.                
016000     03  PARM-KDBEH-KATALOGTYP   PIC X.                                   
016100     03  FILLER                  PIC X       VALUE ')'.                   
016200     03  FILLER                  PIC X(5)    VALUE 'PUBL('.               
016300     03  PARM-TIAAVV-PUBL        PIC X(4).                                
016400     03  FILLER                  PIC X       VALUE ')'.                   
016500     03  FILLER                  PIC X(4)    VALUE 'MAS('.                
016600     03  PARM-FLJANEJ-MASTER     PIC X.                                   
016700     03  FILLER                  PIC X       VALUE ')'.                   
016800     03  FILLER                  PIC X(6)    VALUE 'SPKAT('.              
016900     03  PARM-FLJANEJ-SPRAKKAT   PIC X.                                   
017000     03  FILLER                  PIC X       VALUE ')'.                   
017100     03  FILLER                  PIC X(4)    VALUE 'ART('.                
017200     03  PARM-KAT-IDARTNR        PIC X(9).                                
017300     03  FILLER                  PIC X       VALUE ')'.                   
017400     03  FILLER                  PIC X(5)    VALUE 'SPR1('.               
017500     03  PARM-IDSKYLT-1          PIC X(3).                                
017600     03  FILLER                  PIC X       VALUE ')'.                   
017700     03  FILLER                  PIC X(5)    VALUE 'SPR2('.               
017800     03  PARM-IDSKYLT-2          PIC X(3).                                
017900     03  FILLER                  PIC X       VALUE ')'.                   
018000     03  FILLER                  PIC X(5)    VALUE 'SPR3('.               
018100     03  PARM-IDSKYLT-3          PIC X(3).                                
018200     03  FILLER                  PIC X       VALUE ')'.                   
018300     03  FILLER                  PIC X(5)    VALUE 'SPR4('.               
018400     03  PARM-IDSKYLT-4          PIC X(3).                                
018500     03  FILLER                  PIC X       VALUE ')'.                   
018600     03  FILLER                  PIC X(5)    VALUE 'SPR5('.               
018700     03  PARM-IDSKYLT-5          PIC X(3).                                
018800     03  FILLER                  PIC X       VALUE ')'.                   
018900     03  FILLER                  PIC X(5)    VALUE 'SPR6('.               
019000     03  PARM-IDSKYLT-6          PIC X(3).                                
019100     03  FILLER                  PIC X       VALUE ')'.                   
019200     03  FILLER                  PIC X(4)    VALUE 'PUB('.                
019300     03  PARM-KDCATPUB           PIC X(6).                                
019400     03  FILLER                  PIC X       VALUE ')'.                   
019410     03  FILLER                  PIC X(5)    VALUE 'KOM1('.               
019420     03  PARM-BEKOM-1            PIC X(40).                               
019430     03  FILLER                  PIC X       VALUE ')'.                   
019440                                                                          
019500     03  FILLER                  PIC X(5)    VALUE 'KOM2('.               
019600     03  PARM-BEKOM-2            PIC X(40).                               
019700     03  FILLER                  PIC X       VALUE ')'.                   
019701*    --- RTN åsätts värde efter vilken typ av körning det är              
019710     03  FILLER                  PIC X(4)    VALUE 'RTN('.                
019720     03  PARM-ROUTINE            PIC X(6)    VALUE 'w154bx'.              
019730     03  FILLER                  PIC X       VALUE ')'.                   
019740                                                                          
019750     03  FILLER                  PIC X(5)    VALUE 'TIME('.               
019760     03  PARM-TIME               PIC 9(14).                               
019770     03  FILLER                  PIC X       VALUE ')'.                   
019771                                                                          
019780     03  FILLER                  PIC X(4)    VALUE 'SP1('.                
019790     03  PARM-SP1                PIC X(3).                                
019791     03  FILLER                  PIC X       VALUE ')'.                   
019792                                                                          
019793     03  FILLER                  PIC X(4)    VALUE 'SP2('.                
019794     03  PARM-SP2                PIC X(3).                                
019795     03  FILLER                  PIC X       VALUE ')'.                   
019800                                                                          
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'NYCKLAR'.             
020100 01  NYCKLAR-TILL-DLI.                                                    
020200     03  W-IDCATNR-X.                                                     
020300         05  W-IDCATNR           PIC 9(5)    VALUE ZERO.                  
020400     03  W-TIAAAA-X.                                                      
020500         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
020600     EJECT                                                                
020700 01  FELMEDDELANDE.                                                       
020800     03  FEL-1                   PIC X(35)                                
020900         VALUE 'EJ AUKTORISERAD ANVÄNDARE'.                               
021000     03  FEL-2                   PIC X(35)                                
021100         VALUE 'UPPLYSTA FÄLT FEL        '.                               
021200     03  FEL-3                   PIC X(35)                                
021300         VALUE 'RUTIN FINNS EJ           '.                               
021400     03  FEL-4                   PIC X(35)                                
021500         VALUE 'INGEN UPPDAT. UTAN FRÅGA '.                               
021600     03  FEL-5                   PIC X(35)                                
021700         VALUE 'INGET URVAL FÖR KATALOG  '.                               
021800     03  FEL-6                   PIC X(35)                                
021900         VALUE 'KATALOG FINNS EJ         '.                               
022000     03  FEL-7                   PIC X(35)                                
022100         VALUE 'JOBNR SAKNAS             '.                               
022200                                                                          
022300 01  MEDDELANDE.                                                          
022400     03  MED-1                  PIC X(26)                                 
022500         VALUE 'UPPDATERING GJORD        '.                               
022600     03  MED-2                  PIC X(35)                                 
022700         VALUE 'TIDKOD OCH PUBLICERINGSVECKA SPARAD'.                     
022800     03  MED-3                  PIC X(39)                                 
022900         VALUE 'FÖRVALDA VÄRDEN FÖR KATALOG ''RE'' GÄLLER'.               
023000     03  MED-4                  PIC X(29)                                 
023100         VALUE 'TRYCK PF11 FÖR UPPDATERING'.                              
023200     03  MED-5                  PIC X(35)                                 
023300         VALUE 'KATALOGOMBRYTNING BESTÄLLD         '.                     
023400     03  MED-6.                                                           
023500         05 FILLER              PIC X(34)                                 
023600         VALUE 'KATALOGOMBRYTNING REDAN BESTÄLLD:'.                       
023700         05 MED-6-TIOMBRYT-ORD  PIC 9(6).                                 
023800     03  MED-7                   PIC X(35)                                
023900         VALUE 'Spara värden. Sen Katalogkontroll'.                       
023910     03  MED-8                   PIC X(35)                                
023930         VALUE 'Ny Pubkod. Spara + Katalogkontroll'.                      
024000     EJECT                                                                
024100*01  LAND-KONTROLL   -COPY WWLAND01                                       
024200     EJECT                                                                
024300*                        ****    TP-AREOR                                 
024400 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
024500*01  MID-AREA -COPY W1I52501                                              
024600     EJECT                                                                
024700*01  -COPY WMSGAREA                                                       
024800     EJECT                                                                
024900*03  MOD-AREA -COPY W1O52501  -RED MSG-AREA.                              
025000     EJECT                                                                
025100*01  -COPY WMFSAREA.                                                      
025200     EJECT                                                                
025300*****                                                                     
025400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025500*****                                                                     
025600 01  IMS-WS.                                                              
025700     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
025800     SKIP3                                                                
025900*****                    **** STATUS-KOD FRÅN IMS                         
026000     03  STATUS-WS               PIC X(2).                                
026100         88  SEGMENT-FINNS                   VALUE '  '.                  
026200         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
026300     SKIP3                                                                
026400     03  GODK-STATUSKODER.                                                
026500         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
026600     SKIP3                                                                
026700 01  SSA1                        PIC X(64).                               
026800 01  SSA2                        PIC X(64).                               
026900     EJECT                                                                
027000*                            IMS FUNKTIONSKODER                           
027100*01  -COPY W0003                                                          
027200     EJECT                                                                
027300 01  DLI-IO-AREA.                                                         
027400                                                                          
027500     03  FILLER                  PIC X(16)   VALUE 'WDN101-AREA'.         
027600     03  IO-AREA-1               PIC X(600)  VALUE SPACE.                 
027700     SKIP3                                                                
027800     03  WLKATM01 REDEFINES IO-AREA-1.                                    
027900*        05  -COPY WDN101  -PRE KAT-                                      
028000                                                                          
028100     03  FILLER                  PIC X(16)   VALUE 'WDN111-AREA'.         
028200     03  IO-AREA-2               PIC X(600)  VALUE SPACE.                 
028300     SKIP3                                                                
028400     03  WLKATM11 REDEFINES IO-AREA-2.                                    
028500*        05  -COPY WDN111  -PRE KAT-                                      
028600                                                                          
028700     EJECT                                                                
028800 LINKAGE SECTION.                                                         
028900     SKIP2                                                                
029000*01  -COPY W0009     -PRE MSG-                                            
029100     EJECT                                                                
029200*01  -COPY W0009     -PRE ALT-                                            
029300     EJECT                                                                
029400*01  -COPY W0008     -PRE WLKATM-                                         
029500         05  FILLER              PIC X.                                   
029600     EJECT                                                                
029700 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB                                
029800                           WLKATM-PCB.                                    
029810 MAIN SECTION.                                                            
029900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
030000                           WLKATM-PCB.                                    
030010                                                                          
030100     PERFORM IMS-GET-MSG                                                  
030200     IF SEGMENT-FINNS                                                     
030300        PERFORM A-INIT-SPARA-INPUT                                        
030400        IF MFS-IDTRANS = '1525'                                           
030500          IF MID-IDCATNR-IN = ALL '+'                                     
030600            IF MFS-UPDATE                                                 
030700                                                                          
030800               IF MID-FLJANEJ-SPADAT = JA                                 
030900                                                                          
031000**********  UPPDATERING FÖR ATT ENDAST SPARA       ************           
031100**********  PUBKOD SAMT PUBLICERINGSVECKA          ************           
031300                  PERFORM B-KOLLA-INDATA-SPARA                            
031400                  IF INDATA-OK = JA                                       
031410                      PERFORM IMS-GET-KATALOG-INFO                        
031500                      MOVE W-KDCATPUB                                     
031600                             TO KAT-KAT-KDCATPUB-FOM                      
031601                      MOVE W-PUBL-AAMMDD                                  
031602                             TO W-TIOMBRYT-6                              
031603                      PERFORM S51-Y2K-TIOMBRYT                            
031604                      MOVE W-TIOMBRYT-7                                   
031800                             TO KAT-KAT-TIOMBRYT-PUBL                     
031900                      PERFORM IMS-REPL-KATALOG                            
032000                      MOVE MED-2 TO MOD-TEMFSINF                          
032100                      PERFORM F-RENSA-BILD                                
032200                  ELSE                                                    
032300                      MOVE FEL-2 TO MOD-TEMFSFEL                          
032400                      PERFORM G-VISA-BILD-IGEN                            
032500                  END-IF                                                  
032600               ELSE                                                       
032700                                                                          
032800**********  KATALOGOMBRYTNING                      ************           
033000                  PERFORM C-KOLLA-INDATA-OMBRYT                           
033100                  IF INDATA-OK = JA                                       
033200                      IF MID-KDBEH-KATALOGTYP = 1                         
033210                        PERFORM IMS-GET-KATALOG-INFO                      
033300                        MOVE A-DAGENS-DATUM                               
033310                             TO W-TIOMBRYT-6                              
033320                        PERFORM S51-Y2K-TIOMBRYT                          
033330                        MOVE W-TIOMBRYT-7                                 
033400                               TO KAT-KAT-TIOMBRYT-ORD                    
033500                        PERFORM IMS-REPL-KATALOG                          
033600                      END-IF                                              
033700                      PERFORM D-UPPD-PARAMETER                            
033800                      PERFORM E-STARTA-JOB                                
033900                      MOVE MED-5 TO MOD-TEMFSINF                          
034000                      PERFORM K-VISA-KATALOGDATA                          
034100                  ELSE                                                    
034300                      MOVE FEL-2 TO MOD-TEMFSFEL                          
034400                      PERFORM G-VISA-BILD-IGEN                            
034500                  END-IF                                                  
034600               END-IF                                                     
034700            ELSE                                                          
034800              MOVE MED-4 TO MOD-TEMFSINF                                  
034900              PERFORM G-VISA-BILD-IGEN                                    
035000            END-IF                                                        
035100          ELSE                                                            
035200            PERFORM H-HAMTA-KATALOG                                       
035300          END-IF                                                          
035400        ELSE                                                              
035500            PERFORM F-RENSA-BILD                                          
035600        END-IF                                                            
035700        MOVE LENGTH OF MOD-AREA TO MSG-KVLL                               
035800        ADD +4                  TO MSG-KVLL                               
035900        PERFORM IMS-INSERT-MSG                                            
036000     END-IF                                                               
036100                                                                          
036200     MOVE ZERO TO RETURN-CODE                                             
036300     GOBACK                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 A-INIT-SPARA-INPUT SECTION.                                              
036700                                                                          
036800     IF MSG-DUBBLA-TRANSKODER                                             
036900         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I52501               
037000         MOVE MSG-IDTRANS-2   TO MFS-IDTRANS                              
037100         MOVE MSG-KDMFSFOR-2  TO MFS-KDMFSFOR                             
037200         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
037300     ELSE                                                                 
037400         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I52501                
037500         MOVE MSG-IDTRANS-1   TO MFS-IDTRANS                              
037600         MOVE MSG-KDMFSFOR-1  TO MFS-KDMFSFOR                             
037700         MOVE ' '             TO MFS-KDTRTYP                              
037800     END-IF                                                               
037900                                                                          
038000     MOVE LOW-VALUE TO MOD-W1O52501                                       
038100     MOVE 'W1O52501' TO MFS-IDMOD                                         
038200     MOVE '1525' TO MOD-IDTRANS                                           
038300                                                                          
038400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
038500                             MOD-TEMFSINF                                 
038600                             MOD-IDCATNR-IN                               
038700                                                                          
038800     MOVE SPACE           TO W-KDCATPUB                                   
038900     MOVE ZERO            TO W-PUBL-AAMMDD                                
039000                                                                          
039100     MOVE FUNCTION CURRENT-DATE TO A-CURRENT-DATE                         
039101     MOVE A-CURRENT-DATE  TO PARM-TIME                                    
039110                                                                          
039120     COMPUTE WS-TIAAAA(1) = A-DAGENS-TIAAAA - 1                           
039130     COMPUTE WS-TIAAAA(2) = A-DAGENS-TIAAAA                               
039140     COMPUTE WS-TIAAAA(3) = A-DAGENS-TIAAAA + 1                           
039150     COMPUTE WS-TIAAAA(4) = A-DAGENS-TIAAAA + 2                           
039160                                                                          
039170                                                                          
039180                                                                          
039190                                                                          
039200*    ACCEPT A-TID          FROM TIME                                      
039300*                                                                         
039400*    MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
039500*    MOVE A-DAGENS-DATUM                                                  
039600*                  TO DAT-I-TIDATUM                                       
039700*                                                                         
039800*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
039900*                    DAT-O-TIDATUM DAT-KDSVAR                             
040000*                                                                         
040100*    IF DAT-KDSVAR-OK                                                     
040200****             HÄMTA SEKELSIFFROR                                       
040300*                                                                         
040400*      MOVE DAT-TISEKEL    TO A-DAGENS-AAR(1:2)                           
040600*    ELSE                                                                 
040700*        STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
040800*        DELIMITED BY SIZE INTO FELTEXT                                   
040900*        CALL FELLOG                                                      
041000*    END-IF                                                               
041100*                                                                         
041200*    MOVE A-DAGENS-DATUM(1:2) TO A-DAGENS-AAR(3:2)                        
041300     .                                                                    
041400     EJECT                                                                
041500 B-KOLLA-INDATA-SPARA SECTION.                                            
041600     SKIP2                                                                
041700                                                                          
041800**********  KONTROLLERA KATALOG                    ************           
041900                                                                          
042000     INSPECT MID-IDCATNR-UT REPLACING LEADING SPACE BY ZERO               
042100     IF MID-IDCATNR-UT NUMERIC                                            
042200        MOVE MID-IDCATNR-UT         TO W-IDCATNR                          
042300        PERFORM IMS-GET-KATALOG-INFO                                      
042400        IF SEGMENT-FINNS                                                  
042500           IF KAT-KAT-TIOMBRYT-ORD = ZERO                                 
042600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDCATNR-IN-ATTR             
042700             MOVE KAT-KAT-BEMASTER     TO MOD-BEMASTER                    
042710             MOVE MFS-RENSA-FAELT      TO MOD-TIOMBRYT-ORD                
042800           ELSE                                                           
042900             MOVE KAT-KAT-TIOMBRYT-ORD TO MED-6-TIOMBRYT-ORD              
042910             MOVE MED-6-TIOMBRYT-ORD   TO MOD-TIOMBRYT-ORD                
043000             MOVE MED-6               TO MOD-TEMFSINF                     
043100             MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDCATNR-IN-ATTR              
043200             MOVE KAT-KAT-BEMASTER    TO MOD-BEMASTER                     
043300             MOVE NEJ TO INDATA-OK                                        
043400           END-IF                                                         
043500        ELSE                                                              
043600           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDCATNR-IN-ATTR                
043700           MOVE NEJ TO INDATA-OK                                          
043800        END-IF                                                            
043900     ELSE                                                                 
044000        MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDCATNR-IN-ATTR                
044100        MOVE NEJ TO INDATA-OK                                             
044200     END-IF                                                               
044300                                                                          
044400     PERFORM I-KONTR-PUBKOD                                               
044500     PERFORM J-KONTR-PUBL-VECKA                                           
044600                                                                          
044700     .                                                                    
044800     EJECT                                                                
044900 C-KOLLA-INDATA-OMBRYT SECTION.                                           
045000     SKIP2                                                                
045200**********  KONTROLLERA KATALOG                    ************           
045400     INSPECT MID-IDCATNR-UT REPLACING LEADING SPACE BY ZERO               
045500     IF MID-IDCATNR-UT NUMERIC                                            
045600        MOVE MID-IDCATNR-UT         TO W-IDCATNR                          
045700        PERFORM IMS-GET-KATALOG-INFO                                      
045800        IF SEGMENT-FINNS                                                  
045900           MOVE KAT-KAT-BEMASTER     TO MOD-BEMASTER                      
046000           IF MID-KDBEH-KATALOGTYP = 1                                    
046100             IF KAT-KAT-TIOMBRYT-ORD = ZERO                               
046200               MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDCATNR-IN-ATTR           
046300             ELSE                                                         
046400               MOVE KAT-KAT-TIOMBRYT-ORD TO MED-6-TIOMBRYT-ORD            
046401               MOVE MED-6-TIOMBRYT-ORD   TO MOD-TIOMBRYT-ORD              
046500               MOVE MED-6               TO MOD-TEMFSINF                   
046600               MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDCATNR-IN-ATTR            
046700               MOVE NEJ TO INDATA-OK                                      
046800             END-IF                                                       
046810*                                                                         
046900             IF KAT-KAT-TIOMBRYT-PUBL > ZERO                              
047000               MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIAAVV-PUBL-ATTR          
047100             ELSE                                                         
047200               MOVE MFS-ALFA-FAELT-FEL  TO MOD-TIAAVV-PUBL-ATTR           
047300               MOVE NEJ TO INDATA-OK                                      
047400             END-IF                                                       
047410*                                                                         
047500             IF KAT-KAT-KDCATPUB-FOM NOT = SPACE                          
047600               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCATPUB-R-ATTR           
047700             ELSE                                                         
047800               MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCATPUB-R-ATTR            
047900               MOVE NEJ TO INDATA-OK                                      
048000             END-IF                                                       
048100           END-IF                                                         
048200        ELSE                                                              
048300           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDCATNR-IN-ATTR                
048400           MOVE NEJ TO INDATA-OK                                          
048500        END-IF                                                            
048600     ELSE                                                                 
048700        MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDCATNR-IN-ATTR                
048800        MOVE NEJ TO INDATA-OK                                             
048900     END-IF                                                               
049000                                                                          
049100                                                                          
049200     IF MID-KAT-IDARTNR = ALL '+'                                         
049300        MOVE MFS-ALFA-FAELT-FEL TO MOD-KAT-IDARTNR-ATTR                   
049400        MOVE NEJ TO INDATA-OK                                             
049500     ELSE                                                                 
049600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KAT-IDARTNR-ATTR                 
049700        INSPECT MID-KAT-IDARTNR REPLACING LEADING SPACE BY ZERO           
049800     END-IF                                                               
049900                                                                          
050000     IF MID-IDSKYLT-1 = ALL '+'                                           
050100        MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDSKYLT-1-ATTR                
050200        MOVE NEJ TO INDATA-OK                                             
050300     ELSE                                                                 
050400        MOVE MID-IDSKYLT-1 TO IDSKYLT                                     
050500        PERFORM CA-KOLLA-SPRAK-KOD                                        
050600        IF SPRAK-KOD-OK = JA                                              
050700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-1-ATTR                
050800        ELSE                                                              
050900           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-1-ATTR                
051000           MOVE NEJ TO INDATA-OK                                          
051100        END-IF                                                            
051200     END-IF                                                               
051300                                                                          
051400     IF MID-IDSKYLT-2 = ALL '+' OR SPACE                                  
051500        MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDSKYLT-2-ATTR                
051600        INSPECT MID-IDSKYLT-2 REPLACING ALL '+' BY SPACE                  
051700     ELSE                                                                 
051800        MOVE MID-IDSKYLT-2 TO IDSKYLT                                     
051900        PERFORM CA-KOLLA-SPRAK-KOD                                        
052000        IF SPRAK-KOD-OK = JA                                              
052100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-2-ATTR                
052200        ELSE                                                              
052300           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-2-ATTR                
052400           MOVE NEJ TO INDATA-OK                                          
052500        END-IF                                                            
052600     END-IF                                                               
052700                                                                          
052800     IF MID-IDSKYLT-3 = ALL '+' OR SPACE                                  
052900        MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDSKYLT-3-ATTR                
053000        INSPECT MID-IDSKYLT-3 REPLACING ALL '+' BY SPACE                  
053100     ELSE                                                                 
053200        MOVE MID-IDSKYLT-3 TO IDSKYLT                                     
053300        PERFORM CA-KOLLA-SPRAK-KOD                                        
053400        IF SPRAK-KOD-OK = JA                                              
053500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-3-ATTR                
053600        ELSE                                                              
053700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-3-ATTR                
053800           MOVE NEJ TO INDATA-OK                                          
053900        END-IF                                                            
054000     END-IF                                                               
054100                                                                          
054200     IF MID-IDSKYLT-4 = ALL '+' OR SPACE                                  
054300        MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDSKYLT-4-ATTR                
054400        INSPECT MID-IDSKYLT-4 REPLACING ALL '+' BY SPACE                  
054500     ELSE                                                                 
054600        MOVE MID-IDSKYLT-4 TO IDSKYLT                                     
054700        PERFORM CA-KOLLA-SPRAK-KOD                                        
054800        IF SPRAK-KOD-OK = JA                                              
054900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-4-ATTR                
055000        ELSE                                                              
055100           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-4-ATTR                
055200           MOVE NEJ TO INDATA-OK                                          
055300        END-IF                                                            
055400     END-IF                                                               
055500                                                                          
055600     IF MID-IDSKYLT-5 = ALL '+' OR SPACE                                  
055700        MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDSKYLT-5-ATTR                
055800        INSPECT MID-IDSKYLT-5 REPLACING ALL '+' BY SPACE                  
055900     ELSE                                                                 
056000        MOVE MID-IDSKYLT-5 TO IDSKYLT                                     
056100        PERFORM CA-KOLLA-SPRAK-KOD                                        
056200        IF SPRAK-KOD-OK = JA                                              
056300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-5-ATTR                
056400        ELSE                                                              
056500           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-5-ATTR                
056600           MOVE NEJ TO INDATA-OK                                          
056700        END-IF                                                            
056800     END-IF                                                               
056900                                                                          
057000     IF MID-IDSKYLT-6 = ALL '+' OR SPACE                                  
057100        MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDSKYLT-6-ATTR                
057200        INSPECT MID-IDSKYLT-6 REPLACING ALL '+' BY SPACE                  
057300     ELSE                                                                 
057400        MOVE MID-IDSKYLT-6 TO IDSKYLT                                     
057500        PERFORM CA-KOLLA-SPRAK-KOD                                        
057600        IF SPRAK-KOD-OK = JA                                              
057700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-6-ATTR                
057800        ELSE                                                              
057900           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDSKYLT-6-ATTR                
058000           MOVE NEJ TO INDATA-OK                                          
058100        END-IF                                                            
058200     END-IF                                                               
058300                                                                          
058400     IF MID-KDBEH-KATALOGTYP = ALL '+'                                    
058500        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBEH-KATALOGTYP-ATTR              
058600        MOVE NEJ TO INDATA-OK                                             
058700     ELSE                                                                 
058800        IF MID-KDBEH-KATALOGTYP = 1                                       
058900*             OMBRYTNING                                                  
059000           MOVE MFS-ALFA-FAELT-RAETT TO                                   
059100                MOD-KDBEH-KATALOGTYP-ATTR                                 
059200        ELSE                                                              
059300           IF MID-KDBEH-KATALOGTYP = 2                                    
059400*             SPRÅK-KATALOG-KÖRNING                                       
059500              MOVE MFS-ALFA-FAELT-RAETT TO                                
059600                   MOD-KDBEH-KATALOGTYP-ATTR                              
059700           ELSE                                                           
059800*            --- KATALOGTYP 3 OCH 4 ÄR FÖRBERETT FÖR FRAMTIDEN            
059900*                                                                         
060000*            IF MID-KDBEH-KATALOGTYP = 3 OR 4                             
060100*               FÖRORD-KÖRNING OCH KORR-KÖRNING                           
060200*               MOVE NEJ TO MID-FLJANEJ-MASTER                            
060300*                           MOD-FLJANEJ-MASTER                            
060400*               MOVE MFS-ALFA-FAELT-RAETT TO                              
060500*                    MOD-KDBEH-KATALOGTYP-ATTR                            
060600*               IF MID-KDBEH-KATALOGTYP = 4                               
060700*                  MOVE 'KORR' TO MID-BEKOM-1  MOD-BEKOM-1                
060800*               END-IF                                                    
060900*            ELSE                                                         
061000                MOVE MFS-ALFA-FAELT-FEL TO                                
061100                     MOD-KDBEH-KATALOGTYP-ATTR                            
061200                MOVE NEJ TO INDATA-OK                                     
061300*            END-IF                                                       
061400           END-IF                                                         
061500        END-IF                                                            
061600     END-IF                                                               
061700                                                                          
061810     IF MID-KDCATPUB-R NOT = KAT-KAT-KDCATPUB-FOM (4:3)                   
061811       IF KAT-KAT-KDCATPUB-FOM = SPACE                                    
061812*        --- Man får inte starta en ombrytning utan att först             
061813*        --- ha sparat värden för den.                                    
061814*        --- Tanken är att man först skall köra katalogkontroll/CE        
061816         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-ATTR                   
061817         MOVE NEJ    TO INDATA-OK                                         
061818         MOVE MED-7  TO MOD-TEMFSINF                                      
061819*            VALUE 'Spara värden. Sen Katalogkontroll'.                   
061820       ELSE                                                               
061821*        --- Man håller på att byta pubkod inför ombrytningen./CE         
061823         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-ATTR                   
061824         MOVE NEJ TO INDATA-OK                                            
061825         MOVE MED-8  TO MOD-TEMFSINF                                      
061826*            VALUE 'Ny Pubkod. Spara + Katalogkontroll'.                  
061830       END-IF                                                             
061915     END-IF                                                               
061916                                                                          
062100     IF MID-FLJANEJ-MASTER = ALL '+'                                      
062200       MOVE JA TO  MID-FLJANEJ-MASTER  MOD-FLJANEJ-MASTER                 
062300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-MASTER-ATTR               
062400     ELSE                                                                 
062500       IF MID-FLJANEJ-MASTER = JA OR NEJ                                  
062600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-MASTER-ATTR             
062700       ELSE                                                               
062800         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLJANEJ-MASTER-ATTR               
062900         MOVE NEJ TO INDATA-OK                                            
063000       END-IF                                                             
063100     END-IF                                                               
063200                                                                          
063300     IF MID-FLJANEJ-SPRAKKAT = '+'                                        
063400       MOVE JA    TO MID-FLJANEJ-SPRAKKAT                                 
063500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-SPRAKKAT-ATTR             
063600     ELSE                                                                 
063700       IF MID-FLJANEJ-SPRAKKAT = JA                                       
063800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-SPRAKKAT-ATTR           
063900       ELSE                                                               
064000         IF MID-FLJANEJ-SPRAKKAT = NEJ                                    
064100           MOVE MFS-ALFA-FAELT-RAETT TO                                   
064200                                      MOD-FLJANEJ-SPRAKKAT-ATTR           
064300           IF MID-KDBEH-KATALOGTYP = '2'                                  
064400*                **************************************                   
064500*                * OM MAN VILL KÖRA EN SPRÅKKATALOG,  *                   
064600*                * MÅSTE SPRÅKKAT-FLAGGAN VARA 'J'    *                   
064700*                * ANNARS ABENDAR W154J372 I W154BI   *                   
064800*                **************************************                   
064900             MOVE JA TO MID-FLJANEJ-SPRAKKAT                              
065000                        MOD-FLJANEJ-SPRAKKAT                              
065100           END-IF                                                         
065200         ELSE                                                             
065300           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLJANEJ-SPRAKKAT-ATTR           
065400           MOVE NEJ TO INDATA-OK                                          
065500         END-IF                                                           
065600       END-IF                                                             
065700     END-IF                                                               
065800                                                                          
065900     IF MID-BEKOM-1 = ALL '+'                                             
066000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKOM-1-ATTR                     
066100        IF MID-BEKOM-2 = ALL '+'                                          
066200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKOM-2-ATTR                  
066300        ELSE                                                              
066400           MOVE MFS-ALFA-FAELT-FEL     TO MOD-BEKOM-2-ATTR                
066500           MOVE NEJ TO INDATA-OK                                          
066600        END-IF                                                            
066700     ELSE                                                                 
066800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEKOM-1-ATTR                     
066900                                     MOD-BEKOM-2-ATTR                     
067000     END-IF                                                               
067001                                                                          
067010     IF MID-BEKOM-1 (1:4) = 'KORR'                                        
067020     AND MID-KDBEH-KATALOGTYP = 1                                         
067021       MOVE NEJ      TO MID-FLJANEJ-MASTER                                
067022                        MOD-FLJANEJ-MASTER                                
067023       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-MASTER-ATTR               
067024       MOVE NEJ      TO MID-FLJANEJ-SPRAKKAT                              
067025                        MOD-FLJANEJ-SPRAKKAT                              
067026       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLJANEJ-SPRAKKAT-ATTR             
067030     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 CA-KOLLA-SPRAK-KOD SECTION.                                              
067400                                                                          
067500     MOVE JA TO SPRAK-KOD-OK                                              
067600                                                                          
067700     IF NOT TYSKA                                                         
067800        IF NOT ENGELSKA                                                   
067900           IF NOT AMERIKANSKA                                             
068000              IF NOT FRANSKA                                              
068100                 IF NOT SPANSKA                                           
068200                    IF NOT ITALENSKA                                      
068300                       IF NOT HOLLANDSKA                                  
068400                          IF NOT PORTUGISISKA                             
068500                              IF NOT SVENSKA                              
068600                                 IF NOT FINSKA                            
068700                                    MOVE NEJ TO SPRAK-KOD-OK              
068800                                 END-IF                                   
068900                              END-IF                                      
069000                          END-IF                                          
069100                       END-IF                                             
069200                    END-IF                                                
069300                 END-IF                                                   
069400              END-IF                                                      
069500           END-IF                                                         
069600        END-IF                                                            
069700     END-IF                                                               
069800                                                                          
069900     .                                                                    
070000     EJECT                                                                
070100 D-UPPD-PARAMETER SECTION.                                                
070110     SKIP2                                                                
070200     MOVE W-IDCATNR            TO PARM-IDCATNR                            
070201                                                                          
070210     IF MID-KDBEH-KATALOGTYP = '1' OR '2' OR '3' OR '4'                   
070300       MOVE MID-KDBEH-KATALOGTYP TO PARM-KDBEH-KATALOGTYP                 
070301                                     MOD-KDBEH-KATALOGTYP                 
070302     END-IF                                                               
070310                                                                          
070400     IF MID-FLJANEJ-MASTER NOT = ALL '+'                                  
070500       MOVE MID-FLJANEJ-MASTER TO PARM-FLJANEJ-MASTER                     
070501     END-IF                                                               
070502                                                                          
070510     IF MID-FLJANEJ-SPRAKKAT NOT = ALL '+'                                
070600       MOVE MID-FLJANEJ-SPRAKKAT TO PARM-FLJANEJ-SPRAKKAT                 
070610     END-IF                                                               
070620                                                                          
070630     IF MID-KAT-IDARTNR NOT = ALL '+'                                     
070700       MOVE MID-KAT-IDARTNR    TO PARM-KAT-IDARTNR                        
070701                                  MOD-KAT-IDARTNR                         
070702     END-IF                                                               
070703                                                                          
070704*    --- ETT SPRÅK ÄR MINIMUM                                             
070705     MOVE MID-IDSKYLT-1        TO PARM-IDSKYLT-1                          
070706                                  MOD-IDSKYLT-1                           
070707                                  PARM-SP1                                
070708     INSPECT PARM-SP1 REPLACING ALL SPACE BY '_'                          
070709                                                                          
070710     IF MID-IDSKYLT-2 NOT = ALL '+'                                       
070800       MOVE MID-IDSKYLT-2      TO PARM-IDSKYLT-2                          
070801                                  MOD-IDSKYLT-2                           
070802                                  PARM-SP2                                
070803     INSPECT PARM-SP2 REPLACING ALL SPACE BY '_'                          
070804                                                                          
070810     END-IF                                                               
070820     IF MID-IDSKYLT-3 NOT = ALL '+'                                       
070830       MOVE MID-IDSKYLT-3      TO PARM-IDSKYLT-3                          
070831                                  MOD-IDSKYLT-3                           
070840     END-IF                                                               
070850     IF MID-IDSKYLT-4 NOT = ALL '+'                                       
070860       MOVE MID-IDSKYLT-4      TO PARM-IDSKYLT-4                          
070861                                  MOD-IDSKYLT-4                           
070870     END-IF                                                               
070880     IF MID-IDSKYLT-5 NOT = ALL '+'                                       
070890       MOVE MID-IDSKYLT-5      TO PARM-IDSKYLT-5                          
070891                                  MOD-IDSKYLT-5                           
070900     END-IF                                                               
071000     IF MID-IDSKYLT-6 NOT = ALL '+'                                       
071100       MOVE MID-IDSKYLT-6      TO PARM-IDSKYLT-6                          
071110                                  MOD-IDSKYLT-6                           
071200     END-IF                                                               
071310                                                                          
071400     IF MID-BEKOM-1 NOT = ALL '+'                                         
071410       MOVE MID-BEKOM-1 (1:40) TO PARM-BEKOM-1                            
071411                                  MOD-BEKOM-1                             
071420       IF MID-BEKOM-2 NOT = ALL '+'                                       
071430         MOVE MID-BEKOM-2 (1:40)  TO PARM-BEKOM-2                         
071431                                  MOD-BEKOM-1                             
071440       END-IF                                                             
071450     END-IF                                                               
071460                                                                          
071510     MOVE KAT-KAT-KDCATPUB-FOM TO PARM-KDCATPUB                           
071511                                                                          
071520     MOVE KAT-KAT-TIOMBRYT-PUBL TO DAT-I-TIDATUM                          
071521*    --- FIXAR AAMMDD-DATUM FRÅN BASEN TILL AAVV                          
071530     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
071531     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
071532                     DAT-O-TIDATUM DAT-KDSVAR                             
071533                                                                          
071534     IF DAT-KDSVAR-OK                                                     
071535        MOVE DAT-TIAAVV-GRP     TO PARM-TIAAVV-PUBL                       
071538     ELSE                                                                 
071539         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
071540         DELIMITED BY SIZE INTO FELTEXT                                   
071541         CALL FELLOG                                                      
071542     END-IF                                                               
071600                                                                          
071700     .                                                                    
071800     EJECT                                                                
071900 E-STARTA-JOB SECTION.                                                    
072000                                                                          
072100     IF MID-KDBEH-KATALOGTYP = 1                                          
072200*******    NY KATALOG                                                     
072300                                                                          
072400        MOVE '1525'           TO MSGSOP-IDTRANS                           
072500        MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                          
072600        MOVE 'W1540H    '     TO MSGSOP-IDPROCESS                         
072610        MOVE 'w154bh'         TO PARM-ROUTINE                             
072700        MOVE 'O'              TO MSGSOP-KDSOPFUNK                         
072800        MOVE PARM-TESYMBV     TO MSGSOP-TESYMBV                           
072900        PERFORM IMS-INSERT-ALT-MSG                                        
073000                                                                          
073800     ELSE                                                                 
073810        IF MID-KDBEH-KATALOGTYP = 2                                       
074000*******    SPRÅKKATALOG                                                   
074200          MOVE '1525'         TO MSGSOP-IDTRANS                           
074300          MOVE MFS-KDMFSFOR   TO MSGSOP-KDMFSFOR                          
074400          MOVE 'W1540I    '   TO MSGSOP-IDPROCESS                         
074410          MOVE 'w154bi'       TO PARM-ROUTINE                             
074500          MOVE 'O'            TO MSGSOP-KDSOPFUNK                         
074600          MOVE PARM-TESYMBV   TO MSGSOP-TESYMBV                           
074700          PERFORM IMS-INSERT-ALT-MSG                                      
074800                                                                          
075600        END-IF                                                            
075610     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 F-RENSA-BILD SECTION.                                                    
076000     SKIP2                                                                
076100     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
076200                             MOD-IDCATNR-UT                               
076300                             MOD-KDCATPUB-R                               
076400                             MOD-BEMASTER                                 
076500                             MOD-FLJANEJ-SPADAT                           
076600                             MOD-TIAAVV-PUBL                              
076700                             MOD-KDBEH-KATALOGTYP                         
076800                             MOD-FLJANEJ-MASTER                           
076900                             MOD-FLJANEJ-SPRAKKAT                         
077000                             MOD-BEKOM-1                                  
077100                             MOD-BEKOM-2                                  
077200                             MOD-KAT-IDARTNR                              
077300                             MOD-IDSKYLT-1                                
077400                             MOD-IDSKYLT-2                                
077500                             MOD-IDSKYLT-3                                
077600                             MOD-IDSKYLT-4                                
077700                             MOD-IDSKYLT-5                                
077800                             MOD-IDSKYLT-6                                
077900     .                                                                    
078000     EJECT                                                                
078100 G-VISA-BILD-IGEN SECTION.                                                
078200                                                                          
078300     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCATPUB-R                             
078400                               MOD-IDCATNR-UT                             
078500                               MOD-BEMASTER                               
078600                               MOD-FLJANEJ-SPADAT                         
078700                               MOD-TIAAVV-PUBL                            
078800                               MOD-KDBEH-KATALOGTYP                       
078900                               MOD-FLJANEJ-MASTER                         
079000                               MOD-FLJANEJ-SPRAKKAT                       
079100                               MOD-BEKOM-1                                
079200                               MOD-BEKOM-2                                
079300                               MOD-KAT-IDARTNR                            
079400                               MOD-IDSKYLT-1                              
079500                               MOD-IDSKYLT-2                              
079600                               MOD-IDSKYLT-3                              
079700                               MOD-IDSKYLT-4                              
079800                               MOD-IDSKYLT-5                              
079900                               MOD-IDSKYLT-6                              
080000                                                                          
080100     .                                                                    
080200     EJECT                                                                
080300 H-HAMTA-KATALOG SECTION.                                                 
080400     SKIP2                                                                
080500     MOVE MFS-RENSA-FAELT TO MOD-KDCATPUB-R                               
080600                             MOD-BEMASTER                                 
080700                             MOD-FLJANEJ-SPADAT                           
080800                             MOD-TIAAVV-PUBL                              
080900                             MOD-KDBEH-KATALOGTYP                         
081000                             MOD-FLJANEJ-MASTER                           
081100                             MOD-FLJANEJ-SPRAKKAT                         
081200                             MOD-BEKOM-1                                  
081300                             MOD-BEKOM-2                                  
081400                             MOD-KAT-IDARTNR                              
081500                             MOD-IDSKYLT-1                                
081600                             MOD-IDSKYLT-2                                
081700                             MOD-IDSKYLT-3                                
081800                             MOD-IDSKYLT-4                                
081900                             MOD-IDSKYLT-5                                
082000                             MOD-IDSKYLT-6                                
082100     MOVE MID-IDCATNR-IN  TO WS-IDCATNR                                   
082200                             MOD-IDCATNR-UT                               
082300     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
082400     INSPECT MID-IDCATNR-IN REPLACING ALL '+' BY ZERO                     
082500     IF  WS-IDCATNR NUMERIC                                               
082600     AND WS-IDCATNR > ZERO                                                
082700        MOVE WS-IDCATNR             TO W-IDCATNR                          
082800        PERFORM IMS-GET-KATALOG-INFO                                      
082900        IF SEGMENT-FINNS                                                  
083000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDCATNR-IN-ATTR               
083100           MOVE KAT-KAT-BEMASTER    TO MOD-BEMASTER                       
083200           MOVE KAT-KAT-KDCATPUB-FOM (4:3)                                
083201                                     TO MOD-KDCATPUB-R                    
083210           MOVE KAT-KAT-TIOMBRYT-ORD TO MED-6-TIOMBRYT-ORD                
083220           MOVE MED-6-TIOMBRYT-ORD   TO MOD-TIOMBRYT-ORD                  
083300                                                                          
083400           IF KAT-KAT-TIOMBRYT-PUBL = ZERO                                
083500               MOVE SPACE          TO MOD-TIAAVV-PUBL                     
083600                                                                          
083700           ELSE                                                           
083800*******   KONVERTERA KAT-KAT-TIOMBRYT-PUBL                                
083900*******   ÅÅMMDD TILL ÅÅVV FÖR ATT LÄGGA UT I BILDEN                      
084000                                                                          
084100             MOVE 'AAMMDD'       TO DAT-KDDATFORM                         
084200             MOVE KAT-KAT-TIOMBRYT-PUBL                                   
084300                                 TO DAT-I-TIDATUM                         
084400                                                                          
084500             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
084600                             DAT-O-TIDATUM DAT-KDSVAR                     
084700                                                                          
084800             IF DAT-KDSVAR-OK                                             
084900                   MOVE DAT-TIAAVV-GRP TO MOD-TIAAVV-PUBL                 
085000             ELSE                                                         
085100                 STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS        
085200                 DELIMITED BY SIZE INTO FELTEXT                           
085300                 CALL FELLOG                                              
085400             END-IF                                                       
085500           END-IF                                                         
085600                                                                          
085700        ELSE                                                              
085800           MOVE FEL-5 TO MOD-TEMFSFEL                                     
085900           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDCATNR-IN-ATTR                
086000        END-IF                                                            
086100     ELSE                                                                 
086200        MOVE FEL-2 TO MOD-TEMFSFEL                                        
086300        MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDCATNR-IN-ATTR                
086400     END-IF                                                               
086500                                                                          
086600     .                                                                    
086700     EJECT                                                                
086800 I-KONTR-PUBKOD SECTION.                                                  
087100**********  KONTROLLERA PUBKOD                     ************           
087210     IF MID-KDCATPUB-R = ALL '+'                                          
087221       MOVE MOD-KDCATPUB-R   TO WS-KDCATPUB-R-AVV                         
087222       PERFORM S50-Y2K-KDCATPUB-R                                         
087223       MOVE WS-KDCATPUB-AAAAVV TO W-KDCATPUB                              
087230     ELSE                                                                 
087232       MOVE MID-KDCATPUB-R   TO WS-KDCATPUB-R-AVV                         
087233       PERFORM S50-Y2K-KDCATPUB-R                                         
087234       MOVE WS-KDCATPUB-AAAAVV TO W-KDCATPUB                              
087240     END-IF                                                               
087300                                                                          
092200     MOVE WS-TIAAAA(1)        TO W-TIAAAA                                 
092210*    --- förra året. (Man kan vilja köra t.ex. vecka 52 i jan.)           
092300     PERFORM IMS-GET-KATM11                                               
092301                                                                          
092310     IF SEGMENT-SAKNAS                                                    
092311       MOVE +13 TO TAB-IX                                                 
092313*      --- Katalogen kanske inte var "född" då (antagligen)               
092314*      --- Gå vidare till i år                                            
092320     ELSE                                                                 
092400       MOVE +1 TO TAB-IX                                                  
092600       PERFORM UNTIL KAT-TAB-KDCATPUB-FOM (TAB-IX) >= W-KDCATPUB          
092700                  OR TAB-IX > +12                                         
092800          ADD +1 TO TAB-IX                                                
092900       END-PERFORM                                                        
092901     END-IF                                                               
092910                                                                          
093000     IF TAB-IX > 12                                                       
093010*       --- pröva dagens år                                               
093100        ADD +1 TO W-TIAAAA                                                
093200        PERFORM IMS-GET-KATM11                                            
093210                                                                          
093220        IF SEGMENT-SAKNAS                                                 
093221          CONTINUE                                                        
093222*         --- Hoppsan ! Katalogen finns kanske bara nästa år              
093230        ELSE                                                              
093300          MOVE +1 TO TAB-IX                                               
093400          PERFORM UNTIL                                                   
093410                 KAT-TAB-KDCATPUB-FOM (TAB-IX) >= W-KDCATPUB              
093500                     OR TAB-IX > +12                                      
093700             ADD +1 TO TAB-IX                                             
093800          END-PERFORM                                                     
093801        END-IF                                                            
093802*                                                                         
093810        IF TAB-IX > 12                                                    
093820*          --- pröva nästa år då                                          
093900           ADD +1 TO W-TIAAAA                                             
093910           PERFORM IMS-GET-KATM11                                         
093911                                                                          
093912           IF SEGMENT-SAKNAS                                              
093913*            --- Nu gick vi i fällan !                                    
093914             CONTINUE                                                     
093915           ELSE                                                           
093920             MOVE +1 TO TAB-IX                                            
093930             PERFORM UNTIL                                                
093931                KAT-TAB-KDCATPUB-FOM (TAB-IX) >= W-KDCATPUB               
093940                OR TAB-IX > +12                                           
093950                   ADD +1 TO TAB-IX                                       
093960             END-PERFORM                                                  
093961           END-IF                                                         
093962*                                                                         
093970           IF KAT-TAB-KDCATPUB-FOM (TAB-IX) > W-KDCATPUB                  
093980           OR TAB-IX > +12                                                
093990               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-ATTR             
093991               MOVE NEJ TO INDATA-OK                                      
093992           END-IF                                                         
093993        END-IF                                                            
094400     ELSE                                                                 
094500        IF KAT-TAB-KDCATPUB-FOM (TAB-IX) > W-KDCATPUB                     
094510            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCATPUB-R-ATTR                
094520            MOVE NEJ TO INDATA-OK                                         
094800        END-IF                                                            
094900     END-IF                                                               
095100     .                                                                    
095200     EJECT                                                                
107300 J-KONTR-PUBL-VECKA SECTION.                                              
107500                                                                          
107600**********  KONTROLLERA PUBLICERINGSKOD            ************           
107700                                                                          
107800     IF MID-TIAAVV-PUBL = ALL '+'                                         
107900        MOVE MOD-TIAAVV-PUBL TO W-AAVV                                    
108000        MOVE W-AAVV         TO DAT-I-TIDATUM                              
108100        MOVE 'AAVV  '       TO DAT-KDDATFORM                              
108200                                                                          
108300        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
108400                        DAT-O-TIDATUM DAT-KDSVAR                          
108500                                                                          
108600        IF DAT-KDSVAR-OK                                                  
108700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIAAVV-PUBL-ATTR               
108800          MOVE DAT-TIAAMMDD TO W-PUBL-AAMMDD                              
108900        ELSE                                                              
109000          MOVE MFS-ALFA-FAELT-FEL TO MOD-TIAAVV-PUBL-ATTR                 
109100          MOVE NEJ TO INDATA-OK                                           
109200        END-IF                                                            
109300     ELSE                                                                 
109400        IF MID-TIAAVV-PUBL NUMERIC                                        
109500           MOVE MID-TIAAVV-PUBL TO W-AAVV                                 
109600           MOVE W-AAVV         TO DAT-I-TIDATUM                           
109700           MOVE 'AAVV  '       TO DAT-KDDATFORM                           
109800                                                                          
109900           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
110000                           DAT-O-TIDATUM DAT-KDSVAR                       
110100                                                                          
110200           IF DAT-KDSVAR-OK                                               
110300             MOVE DAT-TIAAMMDD TO W-PUBL-AAMMDD                           
110400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIAAVV-PUBL-ATTR            
110500           ELSE                                                           
110600             MOVE MFS-ALFA-FAELT-FEL TO MOD-TIAAVV-PUBL-ATTR              
110700             MOVE NEJ TO INDATA-OK                                        
110800           END-IF                                                         
110900        ELSE                                                              
111000           MOVE MFS-ALFA-FAELT-FEL TO MOD-TIAAVV-PUBL-ATTR                
111100           MOVE NEJ TO INDATA-OK                                          
111200        END-IF                                                            
111300     END-IF                                                               
111400                                                                          
111500     IF W-PUBL-AAMMDD = ZERO                                              
111600        MOVE MFS-ALFA-FAELT-FEL                                           
111700                             TO MOD-TIAAVV-PUBL-ATTR                      
111800        MOVE NEJ             TO INDATA-OK                                 
111900     END-IF                                                               
112000                                                                          
112100     .                                                                    
112200     EJECT                                                                
112210 K-VISA-KATALOGDATA   SECTION.                                            
112220     SKIP2                                                                
112300     PERFORM IMS-GET-KATALOG-INFO                                         
112420     MOVE W-IDCATNR              TO MOD-IDCATNR-UT                        
112421     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
112422     MOVE KAT-KAT-BEMASTER       TO MOD-BEMASTER                          
112430     MOVE KAT-KAT-KDCATPUB-FOM (4:3)                                      
112431                                 TO MOD-KDCATPUB-R                        
112432     MOVE KAT-KAT-TIOMBRYT-ORD   TO MED-6-TIOMBRYT-ORD                    
112433     MOVE MED-6-TIOMBRYT-ORD     TO MOD-TIOMBRYT-ORD                      
112440                                                                          
112450     IF KAT-KAT-TIOMBRYT-PUBL = ZERO                                      
112460       MOVE SPACE                TO MOD-TIAAVV-PUBL                       
112480     ELSE                                                                 
112490*   KONVERTERA KAT-KAT-TIOMBRYT-PUBL                                      
112491*   ÅÅMMDD TILL ÅÅVV FÖR ATT LÄGGA UT I BILDEN                            
112493       MOVE 'AAMMDD'  TO DAT-KDDATFORM                                    
112494       MOVE KAT-KAT-TIOMBRYT-PUBL                                         
112495                      TO DAT-I-TIDATUM                                    
112497       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
112498                           DAT-O-TIDATUM DAT-KDSVAR                       
112500       IF DAT-KDSVAR-OK                                                   
112501         MOVE DAT-TIAAVV-GRP     TO MOD-TIAAVV-PUBL                       
112502       ELSE                                                               
112503         STRING ' FEL FRÅN DATUMRUTIN I K-VISA... ' STATUS-WS             
112504         DELIMITED BY SIZE     INTO FELTEXT                               
112505         CALL FELLOG                                                      
112506       END-IF                                                             
112507     END-IF                                                               
112513     .                                                                    
112514     EJECT                                                                
112515*                                                                         
112516* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
112517* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
112518*                                                                         
112519*    -COPY W150Y2K1                                                       
112521     EJECT                                                                
112522*                                                                         
112523* SECTION S51-Y2K-TIOMBRYT LIGGER I                                       
112524* COPYTEXT W.PROD.COBOL.W150Y2K2                                          
112525*                                                                         
112526*    -COPY W150Y2K2                                                       
112528     EJECT                                                                
112530* IMS SEKTIONER                                                           
112600     SKIP3                                                                
112700 IMS-GET-MSG SECTION.                                                     
112800                                                                          
112900     MOVE '  QC' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
113100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
113200     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113410     SKIP3                                                                
113500 IMS-INSERT-MSG SECTION.                                                  
113600                                                                          
113700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
113800     MOVE SPACE TO GODK-STATUSKODER                                       
113900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
114000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
114100     PERFORM IMS-STATUSKONTROLL                                           
114300     .                                                                    
114310     SKIP3                                                                
114400 IMS-INSERT-ALT-MSG SECTION.                                              
114500                                                                          
114600     MOVE SPACE TO GODK-STATUSKODER                                       
114700     CALL CBLTDLI USING PURG ALT-PCB W-PROG-TO-PROG-SW                    
114800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
114900     PERFORM IMS-STATUSKONTROLL                                           
115000     .                                                                    
115100     EJECT                                                                
115200 IMS-GET-KATALOG-INFO SECTION.                                            
115300                                                                          
115400     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
115500             DELIMITED BY SIZE INTO SSA1                                  
115600     MOVE '  GE' TO GODK-STATUSKODER                                      
115700     CALL CBLTDLI USING GHU WLKATM-PCB IO-AREA-1 SSA1                     
115800     MOVE WLKATM-STATUS-CODE TO STATUS-WS                                 
115900     PERFORM IMS-STATUSKONTROLL                                           
116000     .                                                                    
116100     EJECT                                                                
116200 IMS-REPL-KATALOG SECTION.                                                
116300                                                                          
116400     MOVE ' ' TO GODK-STATUSKODER                                         
116500     CALL CBLTDLI USING REPL WLKATM-PCB IO-AREA-1                         
116600     MOVE WLKATM-STATUS-CODE TO STATUS-WS                                 
116700     PERFORM IMS-STATUSKONTROLL                                           
116800     .                                                                    
116900     EJECT                                                                
117000 IMS-GET-KATM11 SECTION.                                                  
117100                                                                          
117200     STRING 'WLKATM11(TIAAAA   =' W-TIAAAA-X ')'                          
117300          DELIMITED BY SIZE INTO SSA1                                     
117400     MOVE '  GE' TO GODK-STATUSKODER                                      
117500     CALL CBLTDLI USING GU WLKATM-PCB  IO-AREA-2 SSA1                     
117600     MOVE WLKATM-STATUS-CODE TO STATUS-WS                                 
117700     PERFORM IMS-STATUSKONTROLL                                           
117800     .                                                                    
117900     SKIP3                                                                
118000 IMS-STATUSKONTROLL SECTION.                                              
118100     SET STATUS-IX TO 1                                                   
118200     SEARCH GODK-STATUS AT END CALL FELLOG                                
118300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
118400         CONTINUE                                                         
118500     END-SEARCH                                                           
118600     .                                                                    
