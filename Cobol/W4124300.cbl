000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4124300.                                    
000300 AUTHOR.                     GERRY CARMICHAEL.                            
000400     DATE-WRITTEN.           JULI 2000.                                   
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    LÄSER ORDERHUVUDETSREGISTRET (WDQ2) MED SB.                          
001010*                            OCH   WDB2.                                  
001100*    SKRIVER FIL MED TACDIS ORDRAR.                                       
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600*                                                                         
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*    ---- UT-FIL W41243    OUTPUT                                         
002000                                                                          
002100     SELECT W41243           ASSIGN TO      W41243D1.                     
002200     EJECT                                                                
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W41243                                                               
002900     LABEL RECORD STANDARD                                                
003000     RECORDING F                                                          
003100     BLOCK CONTAINS 0.                                                    
003200                                                                          
003210*01  POST -COPY W41243 -PRE  UT- -L.                                      
003400     EJECT                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP2                                                                
003701                                                                          
003702*    -- CHECKED BY WY2000                                                 
003704     SKIP3                                                                
003800*    ---- GENERELLA KONSTANTER                                            
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  SPAR-IDDC-LDC               PIC X(2).                                
004101 77  SPAR-IDORDER                PIC 9(7).                                
004110 77  SPAR-IDDISTR                PIC 9(4).                                
004120 77  SPAR-IDKUNDNR               PIC 9(6).                                
004130 77  SPAR-IDORDNR7               PIC 9(7).                                
004131 77  SPAR-TIREPDAT               PIC 9(6).                                
004132 77  SPAR-NEXT-WORKDAY           PIC 9(6).                                
004140                                                                          
004150 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
004160     88  SKRIV                               VALUE 'J'.                   
004200                                                                          
004300*    ---- EOF-SWITCHAR                                                    
004400 77  W41243-EOF                  PIC X       VALUE 'N'.                   
004500                                                                          
004600*    ---- ARBETSFÄLT                                                      
004700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004800 01  FILLER REDEFINES DAGENS-DATUM.                                       
004900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     SKIP3                                                                
005310 01  WS-TIRFS                    PIC 9(10)   VALUE ZERO.                  
005320 01  FILLER REDEFINES WS-TIRFS.                                           
005340     03  WS-TIRFS-DATUM          PIC 9(6).                                
005350     03  WS-TIRFS-TID            PIC 9(4).                                
005360 01  WS-DARFS                    PIC 9(12)   VALUE ZERO.                  
005370 01  FILLER REDEFINES WS-DARFS.                                           
005380     03  WS-DARFS-SEKEL          PIC 9(2).                                
005381     03  WS-DARFS-DATUM-TID      PIC 9(10).                               
005390                                                                          
005400     EJECT                                                                
005410*      --- VALID IDDC CODES                                               
005420*                                                                         
005430*01    -COPY WWDCKONS                                                     
005440       EJECT                                                              
005500                                                                          
005510 01  FELTEXT.                                                             
005520     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005530     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005540     EJECT                                                                
005550                                                                          
005600 01  RETURKODER.                                                          
005700   03  RKOD                      PIC S9(4) COMP SYNC VALUE ZERO.          
005800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4) COMP SYNC VALUE +16.           
005810   03  RKOD-ABEND-MED-DUMP       PIC S9(4) COMP SYNC VALUE +33.           
005900                                                                          
006000*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
006100                                                                          
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
006710   03  WORKDAY                   PIC X(8)    VALUE 'WORKDAY '.            
006800     SKIP3                                                                
006900                                                                          
007000 01  FILLER                  PIC X(16)   VALUE 'WORKAREA   '.             
007100*   -COPY WORKAREA                                                        
007200     EJECT                                                                
007700                                                                          
007800*    ---- PARAMETRAR TILL POSTSUM                                         
007900*01  -COPY W0005      -PRE POSTSUM-.                                      
008000     EJECT                                                                
008200                                                                          
008300*01  FILLER                      PIC X(8)    VALUE 'UT-AREA'.             
008400                                                                          
008510*01  AREA -COPY W41243     -PRE UT-                                       
008600     EJECT                                                                
008700                                                                          
008800*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
008900                                                                          
009000 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
009100                                                                          
009200*    ---- STATUSKOD FRÅN IMS                                              
009300                                                                          
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                      VALUE '  '.                   
009510     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
009600     88  SEGMENT-SLUT                       VALUE 'GB'.                   
009700     SKIP3                                                                
009800 01  GODK-STATUSKODER.                                                    
009900   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010200     SKIP3                                                                
010300                                                                          
010400*01      -COPY W0003.                                                     
010401                                                                          
010402     EJECT                                                                
010410 01  NYCKLAR-TILL-DLI.                                                    
010420   03  W-IDGMT-X.                                                         
010430     05  W-IDDISTR           PIC S9(5)   COMP-3.                          
010440     05  W-IDKUNDNR          PIC S9(7)   COMP-3.                          
010500     EJECT                                                                
010600 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDB201'.           
010610 01  DLI-IO-WDB201.                                                       
010620*    03  -COPY WDB201                                                     
010630     EJECT                                                                
010700 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-AREA'.             
010800 01  DLI-IO-AREA.                                                         
010900   03  IO-AREA               PIC X(5000)  VALUE SPACE.                    
010920     SKIP2                                                                
010930*  03  WLORQI01     -COPY WDQ201             -RED IO-AREA.                
010940     EJECT                                                                
010950*  03  WLORQI12     -COPY WDQ212             -RED IO-AREA.                
010960     EJECT                                                                
011500                                                                          
011600 LINKAGE SECTION.                                                         
011700     SKIP2                                                                
011800*    -COPY W0008  -PRE WDQ2-.                                             
011900    05  FILLER                   PIC X(1).                                
012000     EJECT                                                                
012010*01  -COPY W0008  -PRE WDB2-                                              
012020     05  FILLER                  PIC X.                                   
012030     EJECT                                                                
012100                                                                          
012200 PROCEDURE DIVISION  USING WDQ2-PCB WDB2-PCB.                             
012300     ENTRY 'DLITCBL' USING WDQ2-PCB WDB2-PCB.                             
012400 STYR SECTION.                                                            
012500     PERFORM A-INIT                                                       
012600     PERFORM IMS-GET-WDQ2                                                 
012700     PERFORM UNTIL SEGMENT-SLUT                                           
012800       EVALUATE WDQ2-SEG-NAME-FB                                          
012900         WHEN 'WDQ201  '                                                  
013000           PERFORM B-KOLLA-ORDER                                          
013100         WHEN 'WDQ212  '                                                  
013200           PERFORM C-EV-SKAPA-UTPOST                                      
013300       END-EVALUATE                                                       
013400       PERFORM IMS-GET-WDQ2                                               
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400     OPEN OUTPUT W41243                                                   
014500     MOVE 'W4124300'         TO POSTSUM-PROGNAMN                          
014600     MOVE 'W41243D1'         TO POSTSUM-DDNAMN2                           
014700     MOVE 'W41243  '         TO POSTSUM-FDNAMN                            
014900                                                                          
014910     MOVE SPACE TO UT-AREA                                                
015000     ACCEPT DAGENS-DATUM FROM DATE                                        
015010                                                                          
015610     .                                                                    
015700     EJECT                                                                
015800 B-KOLLA-ORDER  SECTION.                                                  
016400                                                                          
016500     MOVE SPACE TO UT-AREA                                                
016600     MOVE NEJ   TO SKRIV-SW                                               
016700     IF OHUV-IDSYSTEM = 'LDC '                                            
016800       MOVE JA                        TO SKRIV-SW                         
016801       MOVE OHUV-IDDC-PRIM            TO SPAR-IDDC-LDC                    
016810       MOVE OHUV-IDORDER              TO SPAR-IDORDER                     
016900       MOVE OHUV-IDDISTR              TO SPAR-IDDISTR                     
016910                                         W-IDDISTR                        
017000       MOVE OHUV-IDKUNDNR             TO SPAR-IDKUNDNR                    
017010                                         W-IDKUNDNR                       
017100       MOVE OHUV-IDORDNR7             TO SPAR-IDORDNR7                    
017101       MOVE OHUV-TIREPDAT             TO SPAR-TIREPDAT                    
017102                                                                          
017110       PERFORM IMS-GU-WDB201                                              
017200       PERFORM BA-BERAKNA-CLEARING-DATUM                                  
017400       MOVE WORK-TIAAMMDD-NEXT-WORKDAY                                    
017500                                      TO SPAR-NEXT-WORKDAY                
017603     END-IF                                                               
017604     .                                                                    
017605     EJECT                                                                
017606 BA-BERAKNA-CLEARING-DATUM SECTION.                                       
017607                                                                          
017609** RÄKNAR X+8 ARBETSDAGAR FRAMÅT (JÄMFÖRT MED W41238)                     
017610     MOVE WC-CDC-SE             TO WORK-IDDC                              
017611     MOVE +002                  TO WORK-KDCALL                            
017612*    ADD  1 TO GMT-KVDAGAR-CDC  GIVING WORK-KVWORKD                       
017613     ADD  8 TO GMT-KVDAGAR-CDC  GIVING WORK-KVWORKD                       
017614     MOVE DAGENS-DATUM          TO WORK-TIAAMMDD-FOM                      
017615     CALL WORKDAY               USING WORK-KDCALL                         
017616                                      WORK-DATE-AREA                      
017617                                      WORK-KDSVAR                         
017618     IF WORK-KDSVAR-FEL                                                   
017619        MOVE 'SECT BA-, DATUM SAKNAS I WORKDAY'                           
017620                                TO    FELTEXT-STR                         
017621        CALL ABEND              USING RKOD-ABEND-MED-DUMP                 
017622     END-IF                                                               
017623     .                                                                    
017624     EJECT                                                                
017625                                                                          
017626 BB-BERAKNA-CLEARING-DATUM SECTION.                                       
017627                                                                          
017629** RÄKNAR X-2 ARBETSDAGAR BAKÅT                                           
017630     MOVE WC-CDC-SE             TO WORK-IDDC                              
017631     MOVE +003                  TO WORK-KDCALL                            
017633     ADD  2 TO GMT-KVDAGAR-CDC  GIVING WORK-KVWORKD                       
017634     MOVE DAGENS-DATUM          TO WORK-TIAAMMDD-FOM                      
017635     CALL WORKDAY               USING WORK-KDCALL                         
017636                                      WORK-DATE-AREA                      
017637                                      WORK-KDSVAR                         
017638     IF WORK-KDSVAR-FEL                                                   
017639        MOVE 'SECT BB-, DATUM SAKNAS I WORKDAY'                           
017640                                TO    FELTEXT-STR                         
017641        CALL ABEND              USING RKOD-ABEND-MED-DUMP                 
017642     END-IF                                                               
017643     .                                                                    
017644     EJECT                                                                
017645 C-EV-SKAPA-UTPOST SECTION.                                               
017646                                                                          
017647     IF SKRIV                                                             
017648         IF ARB-TIRFS > ZERO                                              
017649           MOVE ARB-TIRFS TO WS-TIRFS                                     
017653           IF  WS-TIRFS-DATUM <= SPAR-NEXT-WORKDAY                        
017655                                                                          
017656             MOVE 20                  TO WS-DARFS-SEKEL                   
017657             MOVE WS-TIRFS            TO WS-DARFS-DATUM-TID               
017658             MOVE WS-DARFS            TO UT-DARFS                         
017659                                                                          
017660             MOVE SPAR-IDORDER        TO UT-IDORDER                       
017661             MOVE SPAR-IDDC-LDC       TO UT-IDDC-LDC                      
017662             MOVE ARB-IDDC            TO UT-IDDC                          
017663             MOVE SPAR-IDDISTR        TO UT-IDDISTR                       
017664             MOVE SPAR-IDKUNDNR       TO UT-IDKUNDNR                      
017665             MOVE SPAR-IDORDNR7       TO UT-IDORDNR7                      
017666             MOVE SPAR-TIREPDAT       TO UT-TIREPDAT                      
017667             PERFORM S11-SKRIV-W41243                                     
017668           END-IF                                                         
017669         END-IF                                                           
017670     END-IF                                                               
017700     .                                                                    
017800     EJECT                                                                
017810 S11-SKRIV-W41243 SECTION.                                                
017820                                                                          
017830     WRITE UT-POST FROM UT-AREA                                           
017840                                                                          
017850     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
017860     MOVE 'W41243' TO POSTSUM-FDNAMN                                      
017870     MOVE 'W41243D1' TO POSTSUM-DDNAMN2                                   
017880     CALL POSTSUM USING POSTSUM-PARM                                      
017890     .                                                                    
017891     EJECT                                                                
017900 Z-FINIT SECTION.                                                         
018000     SKIP2                                                                
018100     CLOSE W41243                                                         
018200     MOVE 'S' TO POSTSUM-OPKOD                                            
018300     CALL POSTSUM USING POSTSUM-PARM                                      
018400     .                                                                    
018500     EJECT                                                                
018600*    ---- IMS SEKTIONER                                                   
018700 IMS-GET-WDQ2 SECTION.                                                    
018800                                                                          
018900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
019000     CALL CBLTDLI USING GN WDQ2-PCB DLI-IO-AREA                           
019100     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
019200     PERFORM IMS-STATUSKONTROLL                                           
019300     .                                                                    
019400     SKIP3                                                                
019410 IMS-GU-WDB201 SECTION.                                                   
019420                                                                          
019430     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
019440          DELIMITED BY SIZE INTO SSA1                                     
019450     MOVE '  ' TO GODK-STATUSKODER                                        
019460     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
019470     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
019480     PERFORM IMS-STATUSKONTROLL                                           
019490     .                                                                    
019491     SKIP3                                                                
019492                                                                          
019500 IMS-STATUSKONTROLL SECTION.                                              
019600                                                                          
019700     SET STATUS-IX TO 1                                                   
019800     SEARCH GODK-STATUS                                                   
019900       AT END CALL FELLOG                                                 
020000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
020100     END-SEARCH                                                           
020200     .                                                                    
020210     EJECT                                                                
