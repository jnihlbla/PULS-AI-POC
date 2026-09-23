000100 IDENTIFICATION DIVISION.                                                 
000200     SKIP2                                                                
000300 PROGRAM-ID.    W2242000.                                                 
000400     SKIP2                                                                
000500 AUTHOR.        JAN PETTERSSON                                            
000600 DATE-WRITTEN.  FEBR 1977                                                 
000700     REMARKS.                                                             
000800*        INDATA                                                           
000900*                R09405 (LB) FRÅN DENNA VECKA                             
001000*                R09405 (LB) FRÅN FÖREGÅENDE VECKA                        
001100*        UTDATA                                                           
001200*                LISTA W22420-002                                         
001300*                FIL   W22421 INNEHÅLLANDE NYREG- OCH                     
001400*                             ERSATTA ARTIKLAR                            
001700*                FIL   W22424 INNEHÅLLANDE KONTO-ÄNDRINGAR                
001800*                             ARTIKLAR (PV)                               
001900*                FIL   W22425 INNEHÅLLANDE TRANSAR SOM SKALL              
002000*                             UPPDATERA KDSRA OCH KDARTURS                
002010*  ÄT DEC 93     FIL   W22428 INNEHÅLLANDE NYREG- OCH ERSATTA             
002020*                             ARTIKLAR FARLIGT GODS.                      
002030*                FIL   W22429 INNEHÅLLANDE NYREG  OCH LEVBYTEN            
002100*        FUNKTION                                                         
002200*                LÄS NYTT OCH GAMMALT LB                                  
002300*                A   SKRIV LISTA-002 MED ÄNDRING LEVERANTÖR               
002400*                B   SKRIV W22421 PÅ NYREG, ÄNDRAD ERS                    
002500*                    SKRIV W22425 TRANSAR SOM SKALL UPPDATERA KDSR        
002600*                    OCH KDARTURS                                         
002800*                C   SKRIV W22424 DÅ OLIKA KONTON FINNS (PV)              
002900*        OBS                                                              
003000*                VID FÖRSTA KÖRNING PÅ NYTT ÅR                            
003100*                (=DATUM-K-VECKA=1) SKAPAS TOM FIL.                       
003200*                DESSA KONTO-ÄNDRINGAR HAR BOKFÖRTS                       
003300*                I SYSTEM R544                                            
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800     SKIP2                                                                
003900 FILE-CONTROL.                                                            
004000     SKIP2                                                                
004100     SELECT  LB-NEW      ASSIGN  TO  UT-S-W22420D1.                       
004200     SELECT  LB-OLD      ASSIGN  TO  UT-S-W22420D2.                       
004300     SELECT  W22421      ASSIGN  TO  UT-S-W22420D4.                       
004400     SELECT  W22424      ASSIGN  TO  UT-S-W22420D5.                       
004500     SELECT  LISTA2      ASSIGN  TO  UT-S-W22420D6.                       
004600     SELECT  W22425      ASSIGN  TO  UT-S-W22420D7.                       
004700     SELECT  W22428      ASSIGN  TO  UT-S-W22420D8.                       
004710     SELECT  W22429      ASSIGN  TO  UT-S-W22420D9.                       
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000     SKIP2                                                                
005100 FILE SECTION.                                                            
005200     SKIP2                                                                
005300 FD  LB-NEW                                                               
005400     BLOCK 0 RECORDS                                                      
005500     RECORDING F                                                          
005600     LABEL RECORD STANDARD                                                
005700     DATA RECORD IN-LBNEW.                                                
005800*01  IN-LBNEW    -COPY W011100 -L                                         
006000     SKIP3                                                                
006100 FD  LB-OLD                                                               
006200     BLOCK 0 RECORDS                                                      
006300     RECORDING F                                                          
006400     LABEL RECORD STANDARD                                                
006500     DATA RECORD IN-LBOLD.                                                
006600*01  IN-LBOLD    -COPY W011100 -L                                         
006800     SKIP2                                                                
006900 FD  W22421                                                               
007000     BLOCK CONTAINS 0 RECORDS                                             
007100     RECORDING F                                                          
007200     LABEL RECORD STANDARD                                                
007300     DATA RECORD W22421-POST.                                             
007400*01  W22421-POST  -COPY W224972  -L                                       
008400     SKIP3                                                                
008500 FD  LISTA2                                                               
008600     BLOCK CONTAINS 0 RECORDS                                             
008700     LABEL RECORD IS STANDARD                                             
008800     RECORDING MODE IS V                                                  
008900     DATA RECORD IS UT-RAD2.                                              
009000     SKIP2                                                                
009100 01  UT-RAD2             PIC X(121).                                      
009200     SKIP3                                                                
009300 FD  W22425                                                               
009400     BLOCK CONTAINS 0 RECORDS                                             
009500     RECORDING F                                                          
009600     LABEL RECORD STANDARD                                                
009700     DATA RECORD W22425-POST.                                             
009800*01  W22425-POST  -COPY W223W002   -L                                     
010000     SKIP3                                                                
010100 FD  W22424                                                               
010200     BLOCK CONTAINS 0 RECORDS                                             
010300     RECORDING F                                                          
010400     LABEL RECORD STANDARD                                                
010500     DATA RECORD W22424-POST.                                             
010600*01  W22424-POST  -COPY W224971  -L                                       
010701     SKIP3                                                                
010710 FD  W22428                                                               
010720     BLOCK CONTAINS 0 RECORDS                                             
010730     RECORDING F                                                          
010740     LABEL RECORD STANDARD                                                
010750     DATA RECORD W22428-POST.                                             
010760*01  W22428-POST  -COPY W224281A -L                                       
010770     SKIP3                                                                
010780 FD  W22429                                                               
010790     BLOCK CONTAINS 0 RECORDS                                             
010791     RECORDING F                                                          
010792     LABEL RECORD STANDARD.                                               
010794*01  W22429-POST  -COPY W22429 -L                                         
010800     EJECT                                                                
010900 WORKING-STORAGE SECTION.                                                 
011000     SKIP2                                                                
011010                                                                          
011100*    -- CHECKED BY WY2000                                                 
011110 77  FILLER              PIC X(16)   VALUE 'AAAA********AAAA'.            
011200 77  RKOD                PIC S9(4)   COMP SYNC VALUE +0.                  
011500 77  RADREK-PV           PIC S9(3)   COMP-3    VALUE +99.                 
011600 77  SIDREK-PV           PIC S9(3)   COMP-3    VALUE +0.                  
011700 77  RADREK2             PIC S9(3)   COMP-3    VALUE +99.                 
011800 77  SIDREK2             PIC S9(3)   COMP-3    VALUE +0.                  
011900 77  MAXRAD-ANTAL        PIC S9(3)   COMP-3    VALUE +42.                 
012000     SKIP2                                                                
012100 01  VERDE-SWITCHAR.                                                      
012200     03 JA               PIC X       VALUE 'J'.                           
012300     03 NEJ              PIC X       VALUE 'N'.                           
012400     03 SANT             PIC XX      VALUE 'JJ'.                          
012500     03 FALSK            PIC XX      VALUE 'NN'.                          
012600     SKIP2                                                                
012700 01  SWITCHAR.                                                            
012800     03 EOF-ALLA-INFILER.                                                 
012900       05 EOF-LBOLD     PIC X       VALUE 'N'.                            
013000       05 EOF-LBNEW     PIC X       VALUE 'N'.                            
013100     SKIP2                                                                
013110 01  WS-AAVV            PIC 9(4).                                         
013120 01  FILLER REDEFINES WS-AAVV.                                            
013130     03  WS-AAR        PIC 9(2).                                          
013140     03  WS-VV         PIC 9(2).                                          
013150     SKIP2                                                                
013200 01  KONSTANTER.                                                          
013300     03  DUMMY-IDLEVNR       PIC X(5)    VALUE '99999'.                   
013400     03  DUMMY-IDARTNR       PIC 9(9)    VALUE 999999999.                 
013500     SKIP2                                                                
013600 01  SPAR-IDLKTO             PIC 9(7).                                    
013700 01  FILLER REDEFINES SPAR-IDLKTO.                                        
013800       03 IDLKTO-POS-1-2     PIC 9(2).                                    
013900       03 FILLER             PIC 9(5).                                    
014000     SKIP2                                                                
014100 01  W-TIFINLV-N             PIC 9(5).                                    
014200 01  FILLER REDEFINES W-TIFINLV-N.                                        
014300     03  W-TIFINLV1-4        PIC 9(4).                                    
014400     03  FILLER              PIC X.                                       
014500     EJECT                                                                
014600 01  GENERELLA-SUBPROGRAM.                                                
014700     03 POSTSUM          PIC X(8)    VALUE 'POSTSUM'.                     
014800     03 DATKORT          PIC X(8)    VALUE 'DATKORT'.                     
015000     SKIP2                                                                
015100 01  PARAMETRAR-DATKORT.                                                  
015200     03 KALLANDE-PGM     PIC X(6)    VALUE 'W22420'.                      
015300     03 SOEK-ID          PIC X(6)    VALUE 'WDATUM'.                      
015400     03 MOTTAG-FLT.                                                       
015500       05 NAMN-DATUM     PIC X(5).                                        
015600       05 MOTAGIT-SOEKID PIC X(6).                                        
015700        05 AAR           PIC 99.                                          
015800        05 MAAN          PIC 99.                                          
015900        05 DAG           PIC 99.                                          
016000        05 VECKA         PIC 99.                                          
016100        05 NRDAG         PIC 9.                                           
016200        05 PERIOD        PIC 9.                                           
016300        05 FILLER        PIC X(59).                                       
016400*    03  A  -COPY WDATKORT  -PRE DATUM -RED MOTTAG-FLT                    
016600     EJECT                                                                
016700 01  PARAMETRAR-POSTSUM.                                                  
016800     SKIP2                                                                
016900     03 PGM-NAMN         PIC X(6)    VALUE 'W22420'.                      
017000     03 AAD              PIC X       VALUE 'A'.                           
017100     03 SKRIV            PIC X       VALUE 'S'.                           
017200     SKIP2                                                                
017300     03 NY-TRANSID.                                                       
017400       05 NY-FDNAMN      PIC X(6)    VALUE 'LB-NEW'.                      
017500       05 NY-DDNAMN      PIC X(8)    VALUE 'W22420D1'.                    
017600       05 NY-TRANSTYP    PIC X(4)    VALUE 'INLN'.                        
017700     SKIP2                                                                
017800     03 G-TRANSID.                                                        
017900       05 G-FDNAMN       PIC X(6)    VALUE 'LB-OLD'.                      
018000       05 G-DDNAMN       PIC X(8)    VALUE 'W22420D2'.                    
018100       05 G-TRANSTYP     PIC X(4)    VALUE 'INLG'.                        
018700     SKIP2                                                                
018800     03  KONTO-ENDR-PV.                                                   
018900         05  K-FDNAMN        PIC X(6)    VALUE 'W22424'.                  
019000         05  K-DDNAMN        PIC X(8)    VALUE 'W22420D8'.                
019100         05  K-TRANSTYP      PIC X(4)    VALUE 'PKTO'.                    
019200     SKIP2                                                                
019300     03  BEREDAR.                                                         
019400         05  B-FDNAMN        PIC X(6)    VALUE 'W22421'.                  
019500         05  B-DDNAMN        PIC X(8)    VALUE 'W22420D4'.                
019600         05  B-TRANSTYP      PIC X(4)    VALUE 'BER'.                     
019700     SKIP2                                                                
019800     03  SRAURS.                                                          
019900         05  S-FDNAMN        PIC X(6)    VALUE 'W22425'.                  
020000         05  S-DDNAMN        PIC X(8)    VALUE 'W22420D7'.                
020100         05  S-TRANSTYP      PIC X(4)    VALUE 'SRA'.                     
020101     SKIP2                                                                
020110     03  FARLIGT.                                                         
020120         05  F-FDNAMN        PIC X(6)    VALUE 'W22428'.                  
020130         05  F-DDNAMN        PIC X(8)    VALUE 'W22420D8'.                
020140         05  F-TRANSTYP      PIC X(4)    VALUE 'FRL'.                     
020150     SKIP2                                                                
020160     03  NYLEVBY.                                                         
020170         05  F-FDNAMN        PIC X(6)    VALUE 'W22429'.                  
020180         05  F-DDNAMN        PIC X(8)    VALUE 'W22420D9'.                
020190         05  F-TRANSTYP      PIC X(4)    VALUE '   '.                     
020200     EJECT                                                                
020300*01  POST -COPY W223W002     -PRE U25-                                    
020500     EJECT                                                                
020800 01  FILLER              PIC X(16)   VALUE ALL 'A'.                       
020900     SKIP2                                                                
021000*01  -COPY W0005  -PRE  POSTSUM-                                          
021200 01  FILLER              PIC X(16)   VALUE ALL 'B'.                       
021300     EJECT                                                                
021400 01  SKRIV-STYR2.                                                         
021500     03 RADSTYR2         PIC 9       VALUE 0.                             
021600     03 ENRAD2           PIC 9       VALUE 1.                             
021700     03 TVARAD2          PIC 9       VALUE 2.                             
021800     SKIP2                                                                
021900 01  SKRIVRAD2           PIC X(121)  VALUE SPACE.                         
022000     SKIP2                                                                
022100 01  LISTA-002.                                                           
022200     SKIP2                                                                
022300     03 RUB-1-002.                                                        
022400     05 FILLER               PIC X(88)   VALUE '  VOLVO PARTS             
022500-                    'W22420-002    ÄNDRING AV LEVNR                      
022600-                    '                    VECKA '.                        
022700     05 AAR-002              PIC 99.                                      
022800     05 VECKA-002            PIC 99.                                      
022900     05 FILLER               PIC X(10)   VALUE '       SID'.              
023000     05 SID-NR-002           PIC ZZZ.                                     
023100     SKIP2                                                                
023200     03 RUB-2-002.                                                        
023300        05 FILLER            PIC X(121)  VALUE '   ART.NR ANSK SVE        
023400-                    'NSK BEN.     ENGELSK BEN.    G.LEVNR N.LEVNR        
023500-                   ' GK   HF  SRA URS ERS VVK 1:A IN SM'.                
023600     EJECT                                                                
023700     03 RAD-002.                                                          
023800         05 IDARTNR-002      PIC Z(9).                                    
023900         05 IDANSKNR-002     PIC BZ(3).                                   
024000         05 BEART-SVE-002    PIC BBX(15).                                 
024100         05 BEART-ENG-002    PIC BX(15).                                  
024200         05 FILLER           PIC X(3)    VALUE SPACE.                     
024210         05 IDLEVNR-002-G    PIC X(5).                                    
024220         05 FILLER           PIC X(3)    VALUE SPACE.                     
024300         05 IDLEVNR-002      PIC X(5).                                    
024400         05 KDGK-002         PIC BB9.                                     
024500         05 KDHF-002         PIC BBBB9.                                   
024600         05 KDSRA-BD-002     PIC BBB99.                                   
024700         05 KDARTURS-002     PIC BBXX.                                    
024800         05 KDERS-002        PIC BB99.                                    
024900         05 KDVVKL-002       PIC BB9.                                     
025000         05 TIFINLV-002      PIC B(4)9(4).                                
025100         05 K3150-002        PIC BX(2).                                   
025200     SKIP2                                                                
025300*01  POST  -COPY W224972  -PRE W-972-                                     
025500     SKIP2                                                                
025600*01  POST  -COPY W224971     -PRE W-                                      
025800     EJECT                                                                
025810*01  POST  -COPY W224281A    -PRE FARLIG-                                 
025820     EJECT                                                                
025821 01  FILLER                  PIC X(8) VALUE 'W22429  '.                   
025822*01  POST  -COPY W22429      -PRE NYLEV-                                  
025830     EJECT                                                                
025900 01  LISTNR-002              PIC X(11)   VALUE 'W22420-002'.              
026000     SKIP2                                                                
026100*01  -COPY W223W002                                                       
026300     EJECT                                                                
026400 01  FILLER                  PIC X(16)   VALUE ALL 'E'.                   
026500     SKIP2                                                                
026600 01  NY-LB-POST.                                                          
026700*    03  -PRE NY- -COPY W011100                                           
026900 01  FILLER                  PIC X(16)   VALUE ALL 'F'.                   
027000     EJECT                                                                
027100 01  GAMMAL-LB-POST.                                                      
027200*    03  -PRE G- -COPY W011100                                            
027400 01  FILLER                  PIC X(16)   VALUE ALL 'G'.                   
027500     EJECT                                                                
027600 PROCEDURE DIVISION.                                                      
027700                                                                          
027800 STYR SECTION.                                                            
027900                                                                          
028000******************************************************************        
028100*    TVÅ INFILER GAMLA-LB-IN OCH NYA-LB-IN.                      *        
028200*    002 LISTAN ÄR ÖVER FÖRÄNDRINGAR AVSEENDE LEVERANTÖRER       *        
028300*    (TILL UTLÄNDSK LEVERANTÖR)                                  *        
028400*    FIL W22421 AVSER NYREGISTERING ELLER ÄNDRING AV KDERS C1+C2 *        
028600*    FIL W22424 AVSER ÄNDRADE KONTON C1+C2  PV                   *        
028700*    FIL W22425 TRANSAR FÖR ATT UPPDATERA KDSRA OCH KDARTURS     *        
028710*    FIL W22428 NYREG OCH ÄNDRAD EK ARTIKLAR FARLIGT GODS        *        
028800******************************************************************        
028900     SKIP2                                                                
029000     PERFORM A-HK                                                         
029100     SKIP2                                                                
029200     MOVE FALSK TO EOF-ALLA-INFILER                                       
029300     PERFORM S10-LAES-NYA-LAGERBANDET                                     
029400     PERFORM S20-LAES-GAMLA-LAGERBANDET                                   
029500                                                                          
029600     PERFORM UNTIL EOF-ALLA-INFILER = SANT                                
029700       IF G-IDARTNR = NY-IDARTNR                                          
029800         PERFORM B-G-N-LIKA                                               
029900       ELSE                                                               
030000         IF G-IDARTNR < NY-IDARTNR                                        
030100           PERFORM S20-LAES-GAMLA-LAGERBANDET                             
030200         ELSE                                                             
030300           PERFORM D-N-NYREGISTRERING                                     
030400         END-IF                                                           
030500       END-IF                                                             
030600     END-PERFORM                                                          
030700     SKIP2                                                                
030800     PERFORM G-FINALRUTINEN                                               
030900                                                                          
031000     MOVE +0 TO RETURN-CODE                                               
031100     GOBACK                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 A-HK SECTION.                                                            
031500******************************************************************        
031600*                                                                *        
031700*    SECTION A-HK. INITIERINGAR.                                 *        
031800*                                                                *        
031900******************************************************************        
032000     SKIP2                                                                
032100     OPEN INPUT LB-NEW LB-OLD                                             
032200     OPEN OUTPUT W22421 LISTA2 W22425 W22424 W22428 W22429                
032300                                                                          
032600                                                                          
032700     CALL DATKORT USING KALLANDE-PGM SOEK-ID MOTTAG-FLT                   
032800     MOVE AAR   TO AAR-002                                                
032810                   WS-AAR                                                 
032900     MOVE VECKA TO VECKA-002                                              
032910                   WS-VV                                                  
033000     MOVE 0     TO SID-NR-002                                             
033100                                                                          
033200     MOVE PGM-NAMN TO POSTSUM-PROGNAMN                                    
033300     MOVE AAD      TO POSTSUM-OPKOD                                       
033400     MOVE +0       TO G-IDARTNR                                           
033500                      NY-IDARTNR                                          
033600     .                                                                    
033700     EJECT                                                                
033800 B-G-N-LIKA SECTION.                                                      
033900******************************************************************        
034000*    LISTA-002 SKRIVES OM LEVERANTÖRSBYTE SKETT, TILL            *        
034100*    UTLÄNDSK LEVERANTÖR                                         *        
034200*    HAR KDERS ÄNDRATS SKRIV  W22421 PÅ C1 ELLER C2              *        
034210*    HAR KDERS ÄNDRATS SKRIV  W22428 PÅ C1 ELLER C2              *        
034300*    HAR IDLKTO ÄNDRATS SKRIV I SÅ FALL W22424 PÅ C1 ELLER C2    *        
034400*    SKALL KDSRA OCH KDERS UPDATERAS SKRIVES W22425              *        
034500******************************************************************        
034600                                                                          
034700     IF G-KDERS NOT = NY-KDERS                                            
034800       MOVE SPACE    TO W-972-NYREG                                       
034900       MOVE NY-KDERS TO W-972-KDERS                                       
035000       PERFORM S70-SKRIV-W22421                                           
035010       IF NY-KDFARLIG = 4 OR 6 OR 7                                       
035020          IF NY-KDERS > 20                                                
035021             MOVE '002' TO FARLIG-IDPTYP                                  
035030             PERFORM S71-SKRIV-W22428                                     
035040          ELSE                                                            
035050             IF G-KDERS > 20 AND NY-KDERS < 10                            
035052                MOVE '002' TO FARLIG-IDPTYP                               
035060                PERFORM S71-SKRIV-W22428                                  
035070             END-IF                                                       
035080          END-IF                                                          
035090        END-IF                                                            
035100     END-IF                                                               
035200                                                                          
035210     IF (G-KDFARLIG = 4 OR 6 OR 7)                                        
035211     OR (NY-KDFARLIG = 4 OR 6 OR 7)                                       
035212        IF G-KDFARLIG NOT = NY-KDFARLIG                                   
035215           MOVE '001' TO FARLIG-IDPTYP                                    
035216           MOVE G-KDFARLIG TO FARLIG-KDFARLIG-OLD                         
035217           PERFORM S71-SKRIV-W22428                                       
035218        END-IF                                                            
035219     END-IF                                                               
035220                                                                          
035300     IF G-IDLKTO NOT = NY-IDLKTO                                          
035400       IF G-IDLKTO = ZERO                                                 
035500         MOVE NY-IDLKTO TO SPAR-IDLKTO                                    
035600       ELSE                                                               
035700         MOVE G-IDLKTO  TO SPAR-IDLKTO                                    
035800       END-IF                                                             
035900*****  KOLLA LAGERKONTOT                                                  
036000       IF IDLKTO-POS-1-2 = 57                                             
036400*******  PV                                                               
036500         PERFORM S81-SKRIV-W22424                                         
036600       END-IF                                                             
036700     END-IF                                                               
036800                                                                          
036900*------------------  SKAPAR LISTA-002                                     
037100       IF G-IDLEVNR NOT = NY-IDLEVNR                                      
037101*----------- FIL FÖR ATT SKAPA LISTA                                      
037110         MOVE 'LB' TO NYLEV-NYREG                                         
037111         MOVE G-IDLEVNR TO NYLEV-IDLEVNR-GAM                              
037120         PERFORM S72-SKRIV-W22429                                         
037130         IF NY-IDLEVNR = '1002' OR '1441'                                 
037140             PERFORM BB-BYGG-RAD-002-AV-NY-LB-POST                        
037150             PERFORM S32-SKRIV-LISTA-002                                  
037160         ELSE                                                             
037200         IF G-IDLEVNR = SPACE OR '9998 '                                  
037300           IF NY-IDLEVNR NOT = '9998 '                                    
037400             IF (NY-IDLEVNR > SPACE AND NY-IDLEVNR < '2700')              
037500                 OR (NY-IDLEVNR > '2999' AND NY-IDLEVNR < '3300')         
037600*------------- SVENSK LEVERANTÖR                                          
037700               CONTINUE                                                   
037800             ELSE                                                         
037900               PERFORM S90-SKRIV-W22425                                   
038000             END-IF                                                       
038100           END-IF                                                         
038200         ELSE                                                             
038300           IF (NY-IDLEVNR > SPACE AND NY-IDLEVNR < '2700')                
038400               OR (NY-IDLEVNR > '2999' AND NY-IDLEVNR < '3300')           
038500*----------- SVENSK LEVERANTÖR                                            
038600             CONTINUE                                                     
038700           ELSE                                                           
038800             PERFORM BB-BYGG-RAD-002-AV-NY-LB-POST                        
038900             PERFORM S32-SKRIV-LISTA-002                                  
039000           END-IF                                                         
039100         END-IF                                                           
039110       END-IF                                                             
039200       END-IF                                                             
039400                                                                          
039500     PERFORM S10-LAES-NYA-LAGERBANDET                                     
039600     PERFORM S20-LAES-GAMLA-LAGERBANDET                                   
039700     .                                                                    
039800     EJECT                                                                
039900 BB-BYGG-RAD-002-AV-NY-LB-POST SECTION.                                   
040000******************************************************************        
040100*    BYGG EN LISTRAD FÖR LISTA W22420-002                        *        
040200******************************************************************        
040300                                                                          
040700       IF NY-FLIART = JA                                                  
040800         MOVE '*' TO K3150-002                                            
040900       ELSE                                                               
041000         MOVE SPACE TO K3150-002                                          
041100       END-IF                                                             
041300                                                                          
041400     MOVE NY-IDARTNR   TO IDARTNR-002                                     
041500     MOVE NY-IDANSK    TO IDANSKNR-002                                    
041600     MOVE NY-BEART-SVE TO BEART-SVE-002                                   
041700     MOVE NY-BEART-ENG TO BEART-ENG-002                                   
041800     MOVE NY-IDLEVNR   TO IDLEVNR-002                                     
041900     MOVE G-IDLEVNR    TO IDLEVNR-002-G                                   
042000     MOVE NY-KDGK      TO KDGK-002                                        
042100     MOVE NY-KDHF      TO KDHF-002                                        
042200     MOVE NY-KDSRA     TO KDSRA-BD-002                                    
042300     MOVE NY-KDARTURS  TO KDARTURS-002                                    
042400     MOVE NY-KDERS     TO KDERS-002                                       
042500     MOVE NY-KDVVKL    TO KDVVKL-002                                      
042600     MOVE NY-TIFINLV   TO W-TIFINLV-N                                     
042700     MOVE W-TIFINLV1-4 TO TIFINLV-002                                     
042800     .                                                                    
042900     EJECT                                                                
043000 D-N-NYREGISTRERING SECTION.                                              
043100******************************************************************        
043200*    SKRIV W22421FÖR C1 ELLER C2                                          
043210*    SKRIV W22428 FARLIGT GODS                                            
043300******************************************************************        
043400                                                                          
043500     MOVE 'NY' TO W-972-NYREG                                             
043600     MOVE  +0  TO W-972-KDERS                                             
043700     PERFORM S70-SKRIV-W22421                                             
043701                                                                          
043702     MOVE 'NY' TO NYLEV-NYREG                                             
043703     MOVE SPACE TO NYLEV-IDLEVNR-GAM                                      
043704     PERFORM S72-SKRIV-W22429                                             
043705                                                                          
043710     IF NY-KDFARLIG = 4 OR 6 OR 7                                         
043720        MOVE '001' TO FARLIG-IDPTYP                                       
043730        MOVE ZERO TO FARLIG-KDFARLIG-OLD                                  
043800        PERFORM S71-SKRIV-W22428                                          
043810     END-IF                                                               
043820                                                                          
043860         IF NY-IDLEVNR NOT = '9998'                                       
043870             IF (NY-IDLEVNR > SPACE AND NY-IDLEVNR < '2700')              
043880                 OR (NY-IDLEVNR > '2999' AND NY-IDLEVNR < '3300')         
043890*------------- SVENSK LEVERANTÖR                                          
043891               CONTINUE                                                   
043892             ELSE                                                         
043893               PERFORM S90-SKRIV-W22425                                   
043894             END-IF                                                       
043895          END-IF                                                          
043897                                                                          
043900     PERFORM S10-LAES-NYA-LAGERBANDET                                     
044000     .                                                                    
044100     EJECT                                                                
044200 G-FINALRUTINEN SECTION.                                                  
044300******************************************************************        
044400*    FINALEN.                                                    *        
044500******************************************************************        
044600                                                                          
044700     MOVE SKRIV TO POSTSUM-OPKOD                                          
044800     PERFORM S60-CALL-POSTSUM                                             
044900                                                                          
045300                                                                          
045400     CLOSE LB-NEW LB-OLD W22421                                           
045500           LISTA2 W22425 W22424 W22428 W22429                             
045600     .                                                                    
045700     EJECT                                                                
045800 S10-LAES-NYA-LAGERBANDET SECTION.                                        
045900******************************************************************        
046000*    LÄS NYA-LAGERBANDET                                         *        
046100******************************************************************        
046200                                                                          
046300     IF NY-IDARTNR NOT = DUMMY-IDARTNR                                    
046400       READ LB-NEW INTO NY-W011100 AT END                                 
046500         MOVE DUMMY-IDARTNR TO NY-IDARTNR                                 
046600         MOVE JA            TO EOF-LBNEW                                  
046700       END-READ                                                           
046800     END-IF                                                               
046900                                                                          
047000     IF EOF-LBNEW = NEJ                                                   
047100       MOVE NY-TRANSID TO POSTSUM-TRANSID                                 
047200       PERFORM S60-CALL-POSTSUM                                           
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 S20-LAES-GAMLA-LAGERBANDET SECTION.                                      
047700******************************************************************        
047800*    LÄS GAMLA-LAGERBANDET                                       *        
047900******************************************************************        
048000                                                                          
048100     IF G-IDARTNR NOT = DUMMY-IDARTNR                                     
048200       READ LB-OLD INTO G-W011100 AT END                                  
048300         MOVE DUMMY-IDARTNR TO G-IDARTNR                                  
048400         MOVE JA            TO EOF-LBOLD                                  
048500       END-READ                                                           
048600     END-IF                                                               
048700                                                                          
048800     IF EOF-LBOLD = NEJ                                                   
048900       MOVE G-TRANSID TO POSTSUM-TRANSID                                  
049000       PERFORM S60-CALL-POSTSUM                                           
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 S32-SKRIV-LISTA-002 SECTION.                                             
049500******************************************************************        
049600*    SKRIV EN LISTRAD PÅ LISTA W22420-002                        *        
049700******************************************************************        
049800                                                                          
049900     IF RADREK2 > MAXRAD-ANTAL                                            
050000       MOVE +8      TO RADREK2                                            
050100       ADD  +1      TO SIDREK2                                            
050200       MOVE SIDREK2 TO SID-NR-002                                         
050300                                                                          
050400       MOVE RUB-1-002 TO SKRIVRAD2                                        
050500       PERFORM S53-SKRIV-EN-RAD-002-EFTER-SID                             
050600       MOVE TVARAD2   TO RADSTYR2                                         
050700                                                                          
050800       MOVE RUB-2-002 TO SKRIVRAD2                                        
050900       PERFORM S52-SKRIV-EN-RAD-002                                       
051000       MOVE TVARAD2   TO RADSTYR2                                         
051100     END-IF                                                               
051200                                                                          
051300     MOVE RAD-002     TO SKRIVRAD2                                        
051400     PERFORM S52-SKRIV-EN-RAD-002                                         
051500     ADD +1           TO RADREK2                                          
051600     .                                                                    
051700     EJECT                                                                
051800 S52-SKRIV-EN-RAD-002 SECTION.                                            
051900******************************************************************        
052000*    SKRIV EN LISTRAD PÅ LISTA W22420-002                        *        
052100******************************************************************        
052200                                                                          
052300     WRITE UT-RAD2 FROM SKRIVRAD2 AFTER ADVANCING RADSTYR2                
052400     MOVE ENRAD2 TO RADSTYR2                                              
052500     .                                                                    
052600     EJECT                                                                
052700 S53-SKRIV-EN-RAD-002-EFTER-SID SECTION.                                  
052800******************************************************************        
052900*    SKRIV EN LISTRAD PÅ LISTA W22420-002 EFTER SIDBYTE.         *        
053000******************************************************************        
053100                                                                          
053200     WRITE UT-RAD2 FROM SKRIVRAD2 AFTER ADVANCING PAGE                    
053300     MOVE ENRAD2 TO RADSTYR2                                              
053400     .                                                                    
053500     EJECT                                                                
053600 S60-CALL-POSTSUM  SECTION.                                               
053700******************************************************************        
053800*    CALL POSTSUM.                                               *        
053900******************************************************************        
054000                                                                          
054100     CALL POSTSUM USING POSTSUM-PARM                                      
054200     .                                                                    
054300     EJECT                                                                
054400 S70-SKRIV-W22421 SECTION.                                                
054500******************************************************************        
054600*****        SKRIVER POSTTYP 972 NYREG/ÄNDRAD KDERS          *****        
054700*****        EFTER KOMPLETTERING                             *****        
054800******************************************************************        
054900     MOVE NY-IDARTNR  TO W-972-IDARTNR                                    
055000     MOVE +1          TO W-972-KDCLAGER                                   
055100     MOVE +0          TO W-972-IDBERED                                    
055200                         W-972-IDANSK                                     
055300                         W-972-KDPRODSL                                   
055400     MOVE '972'       TO W-972-IDPTYP                                     
055500                                                                          
055600     WRITE W22421-POST FROM W-972-POST                                    
055700                                                                          
055800     MOVE BEREDAR TO POSTSUM-TRANSID                                      
055900     PERFORM S60-CALL-POSTSUM                                             
058500     .                                                                    
058600     EJECT                                                                
058610 S71-SKRIV-W22428 SECTION.                                                
058620******************************************************************        
058630*****        SKRIVER POSTTYP 001 NYREGISTRERING             ******        
058631*****                        002 ÄNDRAD KDERS               ******        
058640*****        ETER KOMPLETTERING                             ******        
058650******************************************************************        
058651                                                                          
058660     MOVE NY-IDARTNR     TO FARLIG-IDARTNR                                
058670     MOVE NY-BEART-SVE   TO FARLIG-BEART-SVE                              
058680     MOVE G-KDERS        TO FARLIG-KDERS-OLD                              
058690     MOVE NY-KDERS       TO FARLIG-KDERS-NEW                              
058692     MOVE NY-KDFARLIG    TO FARLIG-KDFARLIG-NEW                           
058693     MOVE NY-IDFKNGRP    TO FARLIG-IDFKNGRP                               
058694     MOVE NY-IDBERED     TO FARLIG-IDBERED                                
058695     MOVE NY-TIFINLV     TO FARLIG-TIFINLV                                
058696     MOVE NY-IDPROJ      TO FARLIG-IDPROJ                                 
058697     MOVE NY-KDLEVSP     TO FARLIG-KDLEVSP                                
058698     MOVE NY-TIREGDAT    TO FARLIG-TIREGDAT                               
058699     MOVE WS-AAVV        TO FARLIG-TIAAVV-AKT                             
058700                                                                          
058701     WRITE W22428-POST FROM FARLIG-POST                                   
058702     .                                                                    
058703     EJECT                                                                
058704 S72-SKRIV-W22429 SECTION.                                                
058705******************************************************************        
058706*****        SKRIVER             NYREGISTRERING             ******        
058707*****                            ÄNDRAD LEVERANTÖR          ******        
058708*****        EFTER KOMPLETTERING                            ******        
058709******************************************************************        
058710                                                                          
058711     MOVE NY-IDARTNR     TO NYLEV-IDARTNR                                 
058712     MOVE NY-BEART-SVE   TO NYLEV-BEART-SVE                               
058713     MOVE NY-IDFKNGRP    TO NYLEV-IDFKNGRP                                
058714     MOVE NY-IDLEVNR     TO NYLEV-IDLEVNR-NY                              
058715     MOVE NY-BEFT        TO NYLEV-BEFT                                    
058716     MOVE NY-TIFINLV     TO NYLEV-TIFINLV                                 
058717     MOVE NY-IDPROJ      TO NYLEV-IDPROJ                                  
058718                                                                          
058719     WRITE W22429-POST FROM NYLEV-POST                                    
058720                                                                          
058721     MOVE NYLEVBY TO POSTSUM-TRANSID                                      
058722     PERFORM S60-CALL-POSTSUM                                             
058723     .                                                                    
058724     EJECT                                                                
058730 S81-SKRIV-W22424 SECTION.                                                
058800******************************************************************        
058900*****      SKRIVER POSTTYP 971 KONTOÄNDRINGAR PV             *****        
059000*****      TOM FIL GENEREAS VID VECKONR = 1                  *****        
059100******************************************************************        
059200     IF DATUMK-VECKA NOT = +1                                             
059300       MOVE G-IDARTNR  TO W-IDARTNR                                       
059400       MOVE +1         TO W-KDCLAGER                                      
059500       MOVE G-KDKG     TO W-KDKG-OLD                                      
059600       MOVE G-IDLKTO   TO W-IDLKTO-OLD                                    
059700       MOVE G-KVLS     TO W-KVLS-OLD                                      
059800       MOVE G-KVEFRS   TO W-KVEFRS-OLD                                    
059900       MOVE G-KVAKS-CDC TO W-KVAKS-OLD                                    
059910       ADD  G-KVAKS-PAV TO W-KVAKS-OLD                                    
059920       ADD  G-KVAKS-T   TO W-KVAKS-OLD                                    
060000       MOVE G-PRARTSTD TO W-PRARTSTD-OLD                                  
060100       MOVE NY-KDKG    TO W-KDKG-NEW                                      
060200       MOVE NY-IDLKTO  TO W-IDLKTO-NEW                                    
060300       MOVE '971'      TO W-IDPTYP                                        
060400                                                                          
060500       WRITE W22424-POST FROM W-POST                                      
060600                                                                          
060700       MOVE KONTO-ENDR-PV TO POSTSUM-TRANSID                              
060800       PERFORM S60-CALL-POSTSUM                                           
060900     END-IF                                                               
061000     .                                                                    
061100     EJECT                                                                
061200 S90-SKRIV-W22425 SECTION.                                                
061300******************************************************************        
061400*****        SKRIVER POSTTYP UTL                             *****        
061500*****        LEVERANTÖRSBYTE SOM SKALL UPPDATERA KDSRA       *****        
061600*****        OCH KDARTURS                                    *****        
061700******************************************************************        
061800     MOVE 'UTL'      TO U25-IDPTYP                                        
061900     MOVE NY-IDARTNR TO U25-IDARTNR                                       
062000     MOVE NY-IDLEVNR TO U25-IDLEVNR-NYTT                                  
062100                                                                          
062200     WRITE W22425-POST FROM U25-W223W002                                  
062300                                                                          
062400     MOVE SRAURS TO POSTSUM-TRANSID                                       
062500     PERFORM S60-CALL-POSTSUM                                             
062600     .                                                                    
