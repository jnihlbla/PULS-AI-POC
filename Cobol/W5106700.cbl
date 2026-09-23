000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5106700.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   98/06/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER INFIL, MATCHAR DEN EMOT REGELVERKET OCH SKICKAR            
000900*        EN POST PER KLIENT.                                              
001000*        SKICKAR OCKSÅ EN FIL TILL LAGERVÄRDERINGSLISTOR                  
001100*        SKAPAR OCKSÅ EN FIL FÖR DIREKTLEVERANSLISTA                      
001200*                                                                         
001301*        PROGRAMMET LÄSER      WDH5                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL MED RÄTTA POSTER                                     
002600     SELECT W51063                     ASSIGN TO W51067D1.                
002700     SKIP2                                                                
002800*          --- UTFIL TILL KLIENTER                                        
002900     SELECT W51066                     ASSIGN TO W51067D2.                
003000     SKIP2                                                                
003100*          --- UTFIL TILL LAGERVÄRDERINGEN                                
003200     SELECT W5106D                     ASSIGN TO W51067D3.                
003300     SKIP2                                                                
003400*          --- UTFIL FÖR LAGERVÄRDERINGSLISTA                             
003500     SELECT W5106G                     ASSIGN TO W51067D4.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000                                                                          
004100 FD  W51063                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY WDR901      -L.                                                
004600                                                                          
004700 FD  W51066                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100 01  RATT-POST.                                                           
005200*    03   -COPY WDR901    -L.                                             
005300     03 FILLER                   PIC X(6).                                
005400                                                                          
005500 FD  W5106D                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900 01  UT-POST.                                                             
006000*    03   -COPY W51068    -L.                                             
006100                                                                          
006200 FD  W5106G                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600 01  UT-POST2.                                                            
006700*    03   -COPY W5106G    -L.                                             
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000                                                                          
007100 77  IDPGM                       PIC X(8)    VALUE 'W5106700'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007400                                                                          
007500 01  W-KLIENT                    PIC X       VALUE 'N'.                   
007600 01  SPAR-IDSYSMOT               PIC X(6)    VALUE SPACE.                 
007700                                                                          
007800                                                                          
007900 01  POST-SW                     PIC X      VALUE 'J'.                    
008000     88 POST-OK                             VALUE 'J'.                    
008100     88 POST-FEL                            VALUE 'N'.                    
008200                                                                          
008300 77  W51063-EOF-SW               PIC X       VALUE 'N'.                   
008400     88  END-OF-W51063                       VALUE 'J'.                   
008500     EJECT                                                                
008601                                                                          
008701 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
008801*01  -COPY WWIDFTG                                                        
008901     EJECT                                                                
009001                                                                          
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009700     SKIP2                                                                
009800*    --- PARAMETRAR TILL ABEND                                            
009900                                                                          
010000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010300     SKIP2                                                                
010400 01  FELTEXT.                                                             
010500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL POSTSUM                                          
010900*                                                                         
011000*01  -COPY W0005   -PRE  POSTSUM-                                         
011100     EJECT                                                                
011200 01  IN-AREA-START               PIC X(24)   VALUE                        
011300                                 'IN-AREA-START  '.                       
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY WDR901     -PRE IN-                                       
011700*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR901-DATA               
011800     EJECT                                                                
011900 01  RATT-AREA-START             PIC X(24)   VALUE                        
012000                                 'RATT-AREA-START  '.                     
012100     SKIP2                                                                
012200                                                                          
012300*01  AREA -COPY WDR901     -PRE RATT-                                     
012400*    05   -COPY W510EKHA   -PRE RATT- -RED RATT-FIL-WDR901-DATA           
012500     05   RATT-EKH-IDSYSMOT      PIC X(6).                                
012600     EJECT                                                                
012700 01  UT-AREA-START              PIC X(24)   VALUE                         
012800                                'UT-AREA-START  '.                        
012900*01  AREA -COPY W51068 -PRE UT-                                           
013000     EJECT                                                                
013100 01  DIR-AREA-START             PIC X(24)   VALUE                         
013200                                'DIR-AREA START '.                        
013300*01   -COPY W5106G                                                        
013400     EJECT                                                                
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800     SKIP3                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014001     03  W-WDH501KY-X.                                                    
014101         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
014200         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
014300     03  W-KDEKSHT-X.                                                     
014400         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
014500     03  W-KDEKNIVA-X.                                                    
014600         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
014700     03  W-IDSYSMOT-X.                                                    
014800         05  W-IDSYSMOT          PIC X(6)        VALUE SPACE.             
014900     SKIP2                                                                
015000*    --- STATUS-KOD FRÅN IMS                                              
015100 01  STATUS-WS                   PIC XX.                                  
015200     88  SEGMENT-FINNS                       VALUE '  '.                  
015300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015500     SKIP2                                                                
015600 01  GODK-STATUSKODER.                                                    
015700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015800     SKIP3                                                                
015900 01  SSA1                        PIC X(64).                               
016000 01  SSA2                        PIC X(64).                               
016100 01  SSA3                        PIC X(64).                               
016200     EJECT                                                                
016300*    --- IMS FUNKTIONSKODER                                               
016400*01  -COPY W0003                                                          
016500     EJECT                                                                
016600*    ---  DLI INPUT-OUTPUT AREA                                           
016701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
016801 01  DLI-IO-WDH501.                                                       
016900*    03  -COPY WDH501                                                     
017000     EJECT                                                                
017101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
017201 01  DLI-IO-WDH511.                                                       
017300*    03  -COPY WDH511                                                     
017400     EJECT                                                                
017501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
017601 01  DLI-IO-WDH521.                                                       
017700*    03  -COPY WDH521                                                     
017800     EJECT                                                                
017901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
018001 01  DLI-IO-WDH531.                                                       
018100*    03  -COPY WDH531                                                     
018200     EJECT                                                                
018300 LINKAGE SECTION.                                                         
018400                                                                          
018500                                                                          
018601*01  -COPY W0008  -PRE WDH5-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018901 PROCEDURE DIVISION USING WDH5-PCB .                                      
019000                                                                          
019100 MAIN SECTION.                                                            
019201     ENTRY 'DLITCBL' USING WDH5-PCB.                                      
019300                                                                          
019400                                                                          
019500     PERFORM A-INIT                                                       
019600                                                                          
019700     PERFORM S01-LAES-W51063                                              
019800     PERFORM UNTIL END-OF-W51063                                          
019900       PERFORM B-FLYTTA-DATA                                              
020000       PERFORM C-MATCHA-POST                                              
020100       PERFORM S01-LAES-W51063                                            
020200     END-PERFORM                                                          
020300                                                                          
020400                                                                          
020500     PERFORM Z-FINIT                                                      
020600                                                                          
020700     MOVE ZERO TO RETURN-CODE                                             
020800     GOBACK                                                               
020900     .                                                                    
021000     EJECT                                                                
021100 A-INIT SECTION.                                                          
021200                                                                          
021300     OPEN INPUT  W51063                                                   
021400                                                                          
021500     OPEN OUTPUT W51066                                                   
021600                 W5106D                                                   
021700                 W5106G                                                   
021800                                                                          
021900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022000     .                                                                    
022100     EJECT                                                                
022200 B-FLYTTA-DATA SECTION.                                                   
022300                                                                          
022401     MOVE WC-IDFTG-PV        TO W-IDFTG                                   
022500     MOVE IN-EKH-KDEKHHT     TO W-KDEKHHT                                 
022600     MOVE IN-EKH-KDEKSHT     TO W-KDEKSHT                                 
022700     MOVE IN-EKH-KDEKNIVA    TO W-KDEKNIVA                                
022800     .                                                                    
022900     EJECT                                                                
023000 C-MATCHA-POST SECTION.                                                   
023100                                                                          
023200     MOVE 'NEJ' TO W-KLIENT                                               
023300     MOVE SPACE  TO SPAR-IDSYSMOT                                         
023401     PERFORM IMS-GET-WDH5-ALL                                             
023500     IF SEGMENT-FINNS                                                     
023601       PERFORM IMS-GNP-WDH531                                             
023700       IF SEGMENT-FINNS                                                   
023800         PERFORM UNTIL SEGMENT-SAKNAS                                     
023900         IF SYST-IDSYSMOT NOT  = SPACE                                    
024000           MOVE SYST-IDSYSMOT TO W-IDSYSMOT                               
024100           IF W-IDSYSMOT NOT   = SPAR-IDSYSMOT                            
024200             MOVE 'JA'        TO W-KLIENT                                 
024300             PERFORM D-SKAPA-RATTPOST                                     
024400             PERFORM F-SKAPA-DIR-POST                                     
024500             IF SYST-IDSYSMOT = 'W511'                                    
024600               PERFORM E-SKAPA-LAGERVAERDERINGS-POST                      
024700             END-IF                                                       
024800             MOVE W-IDSYSMOT TO SPAR-IDSYSMOT                             
024900           END-IF                                                         
025000         END-IF                                                           
025101         PERFORM IMS-GNP-WDH531                                           
025200         END-PERFORM                                                      
025300       ELSE                                                               
025400         IF W-KDEKNIVA = 'MOMS' OR 'SUM'                                  
025500           PERFORM D-SKAPA-RATTPOST                                       
025600         END-IF                                                           
025700       END-IF                                                             
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 D-SKAPA-RATTPOST SECTION.                                                
026200                                                                          
026300     MOVE IN-AREA        TO RATT-AREA                                     
026400     MOVE W-IDSYSMOT     TO RATT-EKH-IDSYSMOT                             
026500                                                                          
026600*    PERFORM S11-SKRIV-W51066                                             
026610     IF ((IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '145'           
026620     AND IN-EKH-KDEKNIVA    = 'DDI'                                       
026630     AND IN-FIL-CT-IDSYSTEM = 'W510')                                     
026631     OR                                                                   
026632         (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '135'           
026633     AND IN-EKH-KDEKNIVA    = 'DDI'                                       
026634     AND IN-FIL-CT-IDSYSTEM = 'W510'))                                    
026640       CONTINUE                                                           
026650     ELSE                                                                 
026660       PERFORM S11-SKRIV-W51066                                           
026670     END-IF                                                               
026680                                                                          
026690     IF ((IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '145'           
026692     AND IN-EKH-KDEKNIVA    = 'SUM'                                       
026693     AND IN-FIL-CT-IDSYSTEM = 'W510')                                     
026694     OR                                                                   
026695        (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '135'            
026696     AND IN-EKH-KDEKNIVA    = 'SUM'                                       
026697     AND IN-FIL-CT-IDSYSTEM = 'W510')                                     
026698     OR                                                                   
026699        (IN-EKH-KDEKHHT     = '204' AND IN-EKH-KDEKSHT = '204'            
026700     AND IN-EKH-KDEKNIVA    = 'SUM'                                       
026701     AND IN-FIL-CT-IDSYSTEM = 'W510'))                                    
026702       MOVE 'TDDI'       TO RATT-EKH-KDEKNIVA                             
026703       MOVE 0.01         TO RATT-EKH-SUBEL                                
026704**** THIS IS TO ONLY GET ONE DDI POST FOR 102-145 SEPV TO                 
026705**** W51068 TO BE CALCULATED                                              
026706       PERFORM S11-SKRIV-W51066                                           
026707     END-IF                                                               
026710     .                                                                    
026800     EJECT                                                                
026900 E-SKAPA-LAGERVAERDERINGS-POST SECTION.                                   
027000                                                                          
027100     IF IN-EKH-KDEKNIVA = 'DET' AND IN-EKH-FLLSBOK = 'Y' OR 'J'           
027200       MOVE IN-FIL-DAREGDAT   TO UT-DAREGDAT                              
027300       MOVE IN-FIL-TIKLOCK    TO UT-TIKLOCK                               
027400       MOVE IN-EKH-KDEKHHT    TO UT-KDEKHHT                               
027500       MOVE IN-EKH-KDEKSHT    TO UT-KDEKSHT                               
027600       MOVE IN-EKH-KDEKNIVA   TO UT-KDEKNIVA                              
027700       MOVE IN-EKH-IDDC-SEND  TO UT-IDDC                                  
027800       MOVE IN-EKH-IDVERGL    TO UT-IDVERGL                               
027900       MOVE IN-EKH-IDARTNR    TO UT-IDARTNR                               
028000       MOVE IN-EKH-KDPRODSL   TO UT-KDPRODSL                              
028100       MOVE '1454040000'      TO UT-IDKONTO                               
028200       IF IN-EKH-KDEKHHT      = '202' AND IN-EKH-KDEKSHT = '201'          
028300          OR IN-EKH-KDEKHHT   = '102' AND IN-EKH-KDEKSHT = '106'          
028400         COMPUTE UT-KVANTAL   = IN-EKH-KVANTAL * -1                       
028500       ELSE                                                               
028600         MOVE IN-EKH-KVANTAL  TO UT-KVANTAL                               
028700       END-IF                                                             
028800       COMPUTE UT-SUBEL       =  UT-KVANTAL * IN-EKH-PRARTSTD             
028900       MOVE IN-EKH-FLLSBOK    TO UT-FLLSBOK                               
029000       MOVE IN-EKH-PRINK      TO UT-PRINK                                 
029100       MOVE IN-EKH-PRARTSTD   TO UT-PRARTSTD                              
029110       IF IN-FIL-IDPGM = 'W4183300'                                       
029120         MOVE ZERO            TO UT-DAVERDAT                              
029130       ELSE                                                               
029200         MOVE IN-EKH-DAAVIDAT TO UT-DAVERDAT                              
029210       END-IF                                                             
029300       PERFORM S12-SKRIV-W5106D                                           
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 F-SKAPA-DIR-POST SECTION.                                                
029800                                                                          
029900                                                                          
030000     IF IN-EKH-FLLSBOK = 'N' AND IN-EKH-KDEKNIVA = 'DET' AND              
030100        ((IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '301') OR           
030200         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '310') OR           
030300         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '311') OR           
030400         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '314') OR           
030500         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '351'))             
030600       MOVE IN-EKH-DAVERDAT  TO DIR-DAVERDAT                              
030700       MOVE IN-EKH-IDARTNR   TO DIR-IDARTNR                               
030800       MOVE IN-EKH-IDDC-SEND TO DIR-IDDC                                  
030900       MOVE IN-EKH-IDDISTR   TO DIR-IDDISTR                               
031000       MOVE IN-EKH-IDKUNDNR  TO DIR-IDKUNDNR                              
031100       MOVE IN-EKH-IDVERGL   TO DIR-IDVERGL                               
031200       MOVE IN-EKH-KDEKHHT   TO DIR-KDEKHHT                               
031300       MOVE IN-EKH-KDEKSHT   TO DIR-KDEKSHT                               
031400       MOVE IN-EKH-KDPRODSL  TO DIR-KDPRODSL                              
031500       MOVE IN-EKH-KVANTAL   TO DIR-KVANTAL                               
031600       MOVE IN-EKH-PRARTSTD  TO DIR-PRARTSTD                              
031700       MOVE IN-EKH-PRLANDCO  TO DIR-PRLANDCO                              
031800       MOVE IN-EKH-IDORDNR5  TO DIR-IDORDNR5                              
031900       PERFORM S13-SKRIV-W5106G                                           
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 Z-FINIT SECTION.                                                         
032400                                                                          
032500     CLOSE W51063                                                         
032600           W51066                                                         
032700           W5106D                                                         
032800           W5106G                                                         
032900                                                                          
033000     MOVE 'S' TO POSTSUM-OPKOD                                            
033100     CALL POSTSUM USING POSTSUM-PARM                                      
033200     .                                                                    
033300     EJECT                                                                
033400 S01-LAES-W51063  SECTION.                                                
033500                                                                          
033600     READ W51063 INTO IN-AREA                                             
033700     AT END                                                               
033800        MOVE HIGH-VALUE TO IN-AREA                                        
033900        SET END-OF-W51063 TO TRUE                                         
034000                                                                          
034100     NOT AT END                                                           
034200        MOVE 'W51063'   TO POSTSUM-FDNAMN                                 
034300        MOVE 'W51067D1' TO POSTSUM-DDNAMN2                                
034400        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
034500        CALL POSTSUM USING POSTSUM-PARM                                   
034600     END-READ                                                             
034700     .                                                                    
034800     EJECT                                                                
034900 S11-SKRIV-W51066 SECTION.                                                
035000                                                                          
035100     WRITE RATT-POST FROM RATT-AREA                                       
035200                                                                          
035300     MOVE 'RATT'     TO POSTSUM-TRANSTYP                                  
035400     MOVE 'W51066'   TO POSTSUM-FDNAMN                                    
035500     MOVE 'W51067D2' TO POSTSUM-DDNAMN2                                   
035600     CALL POSTSUM USING POSTSUM-PARM                                      
035700     .                                                                    
035800     EJECT                                                                
035900 S12-SKRIV-W5106D SECTION.                                                
036000                                                                          
036100     WRITE UT-POST FROM UT-AREA                                           
036200                                                                          
036300     MOVE 'LIST'     TO POSTSUM-TRANSTYP                                  
036400     MOVE 'W5106D'   TO POSTSUM-FDNAMN                                    
036500     MOVE 'W51067D3' TO POSTSUM-DDNAMN2                                   
036600     CALL POSTSUM USING POSTSUM-PARM                                      
036700     .                                                                    
036800     EJECT                                                                
036900 S13-SKRIV-W5106G SECTION.                                                
037000                                                                          
037100     WRITE UT-POST2 FROM DIR-W5106G                                       
037200                                                                          
037300     MOVE 'DIR '     TO POSTSUM-TRANSTYP                                  
037400     MOVE 'W5106G'   TO POSTSUM-FDNAMN                                    
037500     MOVE 'W51067D4' TO POSTSUM-DDNAMN2                                   
037600     CALL POSTSUM USING POSTSUM-PARM                                      
037700     .                                                                    
037800     EJECT                                                                
037900* --- IMS SEKTIONER ---                                                   
038000                                                                          
038101 IMS-GET-WDH5-ALL SECTION.                                                
038200                                                                          
038301     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
038400          DELIMITED BY SIZE INTO SSA1                                     
038501     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
038600          DELIMITED BY SIZE INTO SSA2                                     
038701     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
038800          DELIMITED BY SIZE INTO SSA3                                     
038900     MOVE '  GE' TO GODK-STATUSKODER                                      
039001     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2   SSA3        
039101     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
039200     PERFORM IMS-STATUSKONTROLL                                           
039300     .                                                                    
039400     EJECT                                                                
039501 IMS-GNP-WDH531   SECTION.                                                
039600                                                                          
039701     STRING 'WDH531   '                                                   
039800          DELIMITED BY SIZE INTO SSA1                                     
039900     MOVE '  GE' TO GODK-STATUSKODER                                      
040001     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
040101     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
040200     PERFORM IMS-STATUSKONTROLL                                           
040300     .                                                                    
040400     EJECT                                                                
040500                                                                          
040600 IMS-STATUSKONTROLL SECTION.                                              
040700                                                                          
040800     SET STATUS-IX TO 1                                                   
040900     SEARCH GODK-STATUS                                                   
041000       AT END                                                             
041100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041200           DELIMITED BY SIZE INTO FELTEXT                                 
041300         DISPLAY FELTEXT                                                  
041400         CALL FELLOG                                                      
041500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041600         CONTINUE                                                         
041700     END-SEARCH                                                           
041800     .                                                                    
