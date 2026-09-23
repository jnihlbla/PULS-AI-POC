000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5156700.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20170823.                                                
000500                                                                          
000600*    FUNKTION:                                                            
000700*        LÄSER INFIL, MATCHAR DEN EMOT REGELVERKET OCH SKICKAR            
000800*        EN POST PER KLIENT.                                              
000900*        SKICKAR OCKSÅ EN FIL TILL LAGERVÄRDERINGSLISTOR                  
001000*        SKAPAR OCKSÅ EN FIL FÖR DIREKTLEVERANSLISTA                      
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDH5                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INFIL MED RÄTTA POSTER                                     
002500     SELECT W51566                     ASSIGN TO W51567D1.                
002600     SKIP2                                                                
002700*          --- UTFIL TILL KLIENTER                                        
002800     SELECT W51567                     ASSIGN TO W51567D2.                
003200     SKIP2                                                                
003300*          --- UTFIL FÖR LAGERVÄRDERINGSLISTA                             
003400     SELECT W51569                     ASSIGN TO W51567D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900                                                                          
004000 FD  W51566                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  -COPY WDR801      -L.                                                
004400                                                                          
004500 FD  W51567                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800 01  RATT-POST.                                                           
004900*    03   -COPY WDR801    -L.                                             
005000     03 FILLER                   PIC X(6).                                
005700                                                                          
005800 FD  W51569                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100 01  UT-POST2.                                                            
006200*    03   -COPY W51569    -L.                                             
006300     EJECT                                                                
006400 WORKING-STORAGE SECTION.                                                 
006500                                                                          
006600 77  IDPGM                       PIC X(8)    VALUE 'W5156700'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 01  W-KLIENT                    PIC X       VALUE 'N'.                   
007100 01  SPAR-IDSYSMOT               PIC X(6)    VALUE SPACE.                 
007200                                                                          
007300                                                                          
007400 01  POST-SW                     PIC X      VALUE 'J'.                    
007500     88 POST-OK                             VALUE 'J'.                    
007600     88 POST-FEL                            VALUE 'N'.                    
007700                                                                          
007800 77  W51566-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W51566                       VALUE 'J'.                   
008000     EJECT                                                                
008100                                                                          
008200 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
008300*01  -COPY WWIDFTG                                                        
008400     EJECT                                                                
008500                                                                          
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009200     SKIP2                                                                
009300*    --- PARAMETRAR TILL ABEND                                            
009400                                                                          
009500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009800     SKIP2                                                                
009900 01  FELTEXT.                                                             
010000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                 'IN-AREA-START  '.                       
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY WDR801     -PRE IN-                                       
011200*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
011300     EJECT                                                                
011400 01  RATT-AREA-START             PIC X(24)   VALUE                        
011500                                 'RATT-AREA-START  '.                     
011600     SKIP2                                                                
011700                                                                          
011800*01  AREA -COPY WDR801     -PRE RATT-                                     
011900*    05   -COPY W510EKHA   -PRE RATT- -RED RATT-FIL-WDR801-DATA           
012000     05   RATT-EKH-IDSYSMOT      PIC X(6).                                
012500     EJECT                                                                
012600 01  DIR-AREA-START             PIC X(24)   VALUE                         
012700                                'DIR-AREA START '.                        
012800*01   -COPY W51569                                                        
012900     EJECT                                                                
013000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013100*                                                                         
013200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013300     SKIP3                                                                
013400 01  NYCKLAR-TILL-DLI.                                                    
013500     03  W-WDH501KY-X.                                                    
013600         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
013700         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
013800     03  W-KDEKSHT-X.                                                     
013900         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
014000     03  W-KDEKNIVA-X.                                                    
014100         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
014200     03  W-IDSYSMOT-X.                                                    
014300         05  W-IDSYSMOT          PIC X(6)        VALUE SPACE.             
014400     SKIP2                                                                
014500*    --- STATUS-KOD FRÅN IMS                                              
014600 01  STATUS-WS                   PIC XX.                                  
014700     88  SEGMENT-FINNS                       VALUE '  '.                  
014800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015000     SKIP2                                                                
015100 01  GODK-STATUSKODER.                                                    
015200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015300     SKIP3                                                                
015400 01  SSA1                        PIC X(64).                               
015500 01  SSA2                        PIC X(64).                               
015600 01  SSA3                        PIC X(64).                               
015700     EJECT                                                                
015800*    --- IMS FUNKTIONSKODER                                               
015900*01  -COPY W0003                                                          
016000     EJECT                                                                
016100*    ---  DLI INPUT-OUTPUT AREA                                           
016200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
016300 01  DLI-IO-WDH501.                                                       
016400*    03  -COPY WDH501                                                     
016500     EJECT                                                                
016600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
016700 01  DLI-IO-WDH511.                                                       
016800*    03  -COPY WDH511                                                     
016900     EJECT                                                                
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
017100 01  DLI-IO-WDH521.                                                       
017200*    03  -COPY WDH521                                                     
017300     EJECT                                                                
017400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
017500 01  DLI-IO-WDH531.                                                       
017600*    03  -COPY WDH531                                                     
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900                                                                          
018000                                                                          
018100*01  -COPY W0008  -PRE WDH5-                                              
018200     05  FILLER                  PIC X.                                   
018300     EJECT                                                                
018400 PROCEDURE DIVISION USING WDH5-PCB .                                      
018500                                                                          
018600 MAIN SECTION.                                                            
018700     ENTRY 'DLITCBL' USING WDH5-PCB.                                      
018800                                                                          
018900                                                                          
019000     PERFORM A-INIT                                                       
019100                                                                          
019200     PERFORM S01-LAES-W51566                                              
019300     PERFORM UNTIL END-OF-W51566                                          
019400       PERFORM B-FLYTTA-DATA                                              
019500       PERFORM C-MATCHA-POST                                              
019600       PERFORM S01-LAES-W51566                                            
019700     END-PERFORM                                                          
019800                                                                          
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     OPEN INPUT  W51566                                                   
020900                                                                          
021000     OPEN OUTPUT W51567                                                   
021200                 W51569                                                   
021300                                                                          
021400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021500     .                                                                    
021600     EJECT                                                                
021700                                                                          
021800 B-FLYTTA-DATA SECTION.                                                   
021910     MOVE WC-IDFTG-IN        TO W-IDFTG                                   
022000     MOVE IN-EKH-KDEKHHT     TO W-KDEKHHT                                 
022100     MOVE IN-EKH-KDEKSHT     TO W-KDEKSHT                                 
022200     MOVE IN-EKH-KDEKNIVA    TO W-KDEKNIVA                                
022300     .                                                                    
022400     EJECT                                                                
022500 C-MATCHA-POST SECTION.                                                   
022600                                                                          
022700     MOVE 'NEJ' TO W-KLIENT                                               
022800     MOVE SPACE  TO SPAR-IDSYSMOT                                         
022900     PERFORM IMS-GET-WDH5-ALL                                             
023000     IF SEGMENT-FINNS                                                     
023100       PERFORM IMS-GNP-WDH531                                             
023200       IF SEGMENT-FINNS                                                   
023300         PERFORM UNTIL SEGMENT-SAKNAS                                     
023400         IF SYST-IDSYSMOT NOT  = SPACE                                    
023500           MOVE SYST-IDSYSMOT TO W-IDSYSMOT                               
023600           IF W-IDSYSMOT NOT   = SPAR-IDSYSMOT                            
023700             MOVE 'JA'        TO W-KLIENT                                 
023800             PERFORM D-SKAPA-RATTPOST                                     
023900             PERFORM F-SKAPA-DIR-POST                                     
024300             MOVE W-IDSYSMOT TO SPAR-IDSYSMOT                             
024400           END-IF                                                         
024500         END-IF                                                           
024600         PERFORM IMS-GNP-WDH531                                           
024700         END-PERFORM                                                      
024800       ELSE                                                               
024900         IF W-KDEKNIVA = 'MOMS' OR 'SUM'                                  
025000           PERFORM D-SKAPA-RATTPOST                                       
025100         END-IF                                                           
025200       END-IF                                                             
025300     END-IF                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 D-SKAPA-RATTPOST SECTION.                                                
025700                                                                          
025800     MOVE IN-AREA        TO RATT-AREA                                     
025900     MOVE W-IDSYSMOT     TO RATT-EKH-IDSYSMOT                             
026000                                                                          
026100     PERFORM S11-SKRIV-W51567                                             
026200     .                                                                    
029200     EJECT                                                                
029300 F-SKAPA-DIR-POST SECTION.                                                
029400                                                                          
029500                                                                          
029600     IF IN-EKH-FLLSBOK = 'N' AND IN-EKH-KDEKNIVA = 'DET' AND              
029700        ((IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '301') OR           
029800         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '310') OR           
029900         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '311'))             
030200       MOVE IN-EKH-DAVERDAT  TO DIR-DAVERDAT                              
030300       MOVE IN-EKH-IDARTNR   TO DIR-IDARTNR                               
030400       MOVE IN-EKH-IDDC-SEND TO DIR-IDDC                                  
030500       MOVE IN-EKH-IDDISTR   TO DIR-IDDISTR                               
030600       MOVE IN-EKH-IDKUNDNR  TO DIR-IDKUNDNR                              
030700       MOVE IN-EKH-IDVERGL   TO DIR-IDVERGL                               
030800       MOVE IN-EKH-KDEKHHT   TO DIR-KDEKHHT                               
030900       MOVE IN-EKH-KDEKSHT   TO DIR-KDEKSHT                               
031000       MOVE IN-EKH-KDPRODSL  TO DIR-KDPRODSL                              
031010       MOVE IN-EKH-KDTRADP   TO DIR-KDTRADP                               
031100       MOVE IN-EKH-KVANTAL   TO DIR-KVANTAL                               
031200       MOVE IN-EKH-PRARTSTD  TO DIR-PRARTSTD                              
031300       MOVE IN-EKH-PRLANDCO  TO DIR-PRLANDCO                              
031400       MOVE IN-EKH-IDORDNR5  TO DIR-IDORDNR5                              
031500       PERFORM S13-SKRIV-W51569                                           
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 Z-FINIT SECTION.                                                         
032000                                                                          
032100     CLOSE W51566                                                         
032200           W51567                                                         
032400           W51569                                                         
032500                                                                          
032600     MOVE 'S' TO POSTSUM-OPKOD                                            
032700     CALL POSTSUM USING POSTSUM-PARM                                      
032800     .                                                                    
032900     EJECT                                                                
033000 S01-LAES-W51566  SECTION.                                                
033100                                                                          
033200     READ W51566 INTO IN-AREA                                             
033300     AT END                                                               
033400        MOVE HIGH-VALUE TO IN-AREA                                        
033500        SET END-OF-W51566 TO TRUE                                         
033600                                                                          
033700     NOT AT END                                                           
033800        MOVE 'W51566'   TO POSTSUM-FDNAMN                                 
033900        MOVE 'W51567D1' TO POSTSUM-DDNAMN2                                
034000        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
034100        CALL POSTSUM USING POSTSUM-PARM                                   
034200     END-READ                                                             
034300     .                                                                    
034400     EJECT                                                                
034500 S11-SKRIV-W51567 SECTION.                                                
034600                                                                          
034700     WRITE RATT-POST FROM RATT-AREA                                       
034800                                                                          
034900     MOVE 'RATT'     TO POSTSUM-TRANSTYP                                  
035000     MOVE 'W51567'   TO POSTSUM-FDNAMN                                    
035100     MOVE 'W51567D2' TO POSTSUM-DDNAMN2                                   
035200     CALL POSTSUM USING POSTSUM-PARM                                      
035300     .                                                                    
036400     EJECT                                                                
036500 S13-SKRIV-W51569 SECTION.                                                
036600                                                                          
036700     WRITE UT-POST2 FROM DIR-W51569                                       
036800                                                                          
036900     MOVE 'DIR '     TO POSTSUM-TRANSTYP                                  
037000     MOVE 'W51569'   TO POSTSUM-FDNAMN                                    
037100     MOVE 'W51567D3' TO POSTSUM-DDNAMN2                                   
037200     CALL POSTSUM USING POSTSUM-PARM                                      
037300     .                                                                    
037400     EJECT                                                                
037500* --- IMS SEKTIONER ---                                                   
037600                                                                          
037700 IMS-GET-WDH5-ALL SECTION.                                                
037800                                                                          
037900     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
038000          DELIMITED BY SIZE INTO SSA1                                     
038100     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
038200          DELIMITED BY SIZE INTO SSA2                                     
038300     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
038400          DELIMITED BY SIZE INTO SSA3                                     
038500     MOVE '  GE' TO GODK-STATUSKODER                                      
038600     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
038700     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
038800     PERFORM IMS-STATUSKONTROLL                                           
038900     .                                                                    
039000     EJECT                                                                
039100 IMS-GNP-WDH531   SECTION.                                                
039200                                                                          
039300     STRING 'WDH531   '                                                   
039400          DELIMITED BY SIZE INTO SSA1                                     
039500     MOVE '  GE' TO GODK-STATUSKODER                                      
039600     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
039700     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040000     EJECT                                                                
040100                                                                          
040200 IMS-STATUSKONTROLL SECTION.                                              
040300                                                                          
040400     SET STATUS-IX TO 1                                                   
040500     SEARCH GODK-STATUS                                                   
040600       AT END                                                             
040700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040800           DELIMITED BY SIZE INTO FELTEXT                                 
040900         DISPLAY FELTEXT                                                  
041000         CALL FELLOG                                                      
041100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041200         CONTINUE                                                         
041300     END-SEARCH                                                           
041400     .                                                                    
