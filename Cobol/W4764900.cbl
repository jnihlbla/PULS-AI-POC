000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4764900.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   02/12/17.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER W47650 , SORTERADE POSTER                       
000900*        SKRIVS PÅ UTFILEN SAMT KOPIOR TILL ANDRA SYSTEM                  
001000*        VILKA SYSTEM SKALL UPPDATERAS MED FAKTURAUPPGIFTER ?             
001100*                                                                         
001200*        PROGRAMMET LÄSER     WDB2  KUNDREG                               
001300*                             WDE4 PROD.NR                                
001400*                                                                         
001500* ETRACKER 1796339                                                        
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*     --- INPOSTER FRÅN W4765000 SOM SKALL                                
002500     SELECT W47650                     ASSIGN TO W47649D1.                
002600     SKIP2                                                                
002700*     --- UTFIL MED TRANS FÖR OLIKA SYSTEM                                
002800     SELECT W47649                     ASSIGN TO W47649D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W47650                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W4765001     -L.                                               
003900     SKIP3                                                                
004000 FD  W47649                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  UT-POST   -COPY W4765001 -L.                                         
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W4764900'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  E4POST                      PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  W47650-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W47650                       VALUE 'J'.                   
005400                                                                          
005500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005600                                                                          
005700 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
005800 01  WS-SEKTION                  PIC X(32)   VALUE SPACE.                 
005900                                                                          
006000 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
006100 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
006200 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
006300 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
006400 77  WS-IMPORTER                 PIC 9(2)    VALUE 4.                     
006500                                                                          
006600 77  W-KDSOFT-RU                 PIC S9(3)   VALUE 0 COMP-3.              
006700                                                                          
006800 77  SKRIV-VIPS-SW               PIC X       VALUE 'N'.                   
006900     88  SKRIV-VIPS-POST                     VALUE 'J'.                   
007000     88  EJ-SKRIV-VIPS                       VALUE 'N'.                   
007100                                                                          
007200 01  TEST-IDDISTR                PIC 9(5)  COMP-3.                        
007300*01  FILLER    -COPY WWDIST03   -RED TEST-IDDISTR.                        
007400     EJECT                                                                
007500*01  FILLER    -COPY WWDIST07   -RED TEST-IDDISTR.                        
007600     EJECT                                                                
007700*01  FILLER    -COPY WWDIST18   -RED TEST-IDDISTR.                        
007800     EJECT                                                                
007900*01  FILLER    -COPY WWDIST19   -RED TEST-IDDISTR.                        
008000     EJECT                                                                
008100*01  FILLER    -COPY WWDIST24   -RED TEST-IDDISTR.                        
008200     EJECT                                                                
008300*01  FILLER    -COPY WWDIST35   -RED TEST-IDDISTR.                        
008400     EJECT                                                                
008500*01  FILLER    -COPY WWDIST38   -RED TEST-IDDISTR.                        
008600     EJECT                                                                
008700*01  FILLER    -COPY WWDIS105   -RED TEST-IDDISTR.                        
008800     EJECT                                                                
008900*01  FILLER    -COPY WWDIS130   -RED TEST-IDDISTR.                        
009000     EJECT                                                                
009100 01  TEST-ARTIKEL                PIC 9(9)  COMP-3.                        
009200*01  FILLER    -COPY WWART04    -RED TEST-ARTIKEL.                        
009300                                                                          
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
010000     03  W460LISO                PIC X(8)    VALUE 'W460LISO'.            
010100                                                                          
010200 01  FILLER                      PIC X(8)  VALUE 'W460DIS1'.              
010300*   -COPY W460DIS1                                                        
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16) VALUE 'ISO-KODER'.             
010600*    -COPY  W460LISO                                                      
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL ABEND                                            
010900                                                                          
011000 77  RKOD-ABEND                  PIC S9(4) COMP VALUE +0.                 
011100     EJECT                                                                
011200                                                                          
011300*    --- PARAMETRAR TILL POSTSUM                                          
011400*                                                                         
011500*01  -COPY W0005   -PRE  POSTSUM-                                         
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
011800                                                                          
011900 01  IN-AREA.                                                             
012000*    03  -COPY W4765001   -PRE IN-                                        
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
012300                                                                          
012400 01  UT-AREA.                                                             
012500*    03  -COPY W4765001   -PRE UT-                                        
012600     EJECT                                                                
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  NYCKLAR-TILL-DLI.                                                    
013100                                                                          
013200     03  W-IDGMT-X.                                                       
013300         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
013400         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
013500                                                                          
013600     03  W-IDGMT-MIN-X.                                                   
013700         05  W-IDDISTR-MIN-B2    PIC S9(5)   VALUE ZERO COMP-3.           
013800         05  W-IDKUNDNR-MIN-B2   PIC S9(7)   VALUE ZERO COMP-3.           
013900                                                                          
014000     03  W-IDGMT-MAX-X.                                                   
014100         05  W-IDDISTR-MAX-B2    PIC S9(5)   VALUE ZERO COMP-3.           
014200         05  W-IDKUNDNR-MAX-B2   PIC S9(7)   VALUE ZERO COMP-3.           
014300                                                                          
014400     03  W-WDE4ESEQ-X.                                                    
014500         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
014600                                                                          
014700     03  W-WDE4BSEQ-X.                                                    
014800         05  W-IDPRODNR-BSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
014900         05  W-IDPURAD-BSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
015000                                                                          
015100     03  W-IDPURAD-X.                                                     
015200         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
015300                                                                          
015400     03  W-IDDC-B6-X.                                                     
015500         05 W-IDDC-B6            PIC X(2).                                
015600                                                                          
015700*    --- STATUS-KOD FRÅN IMS                                              
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FINNS                       VALUE '  '.                  
016000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017200     88  IMS-EJ-OK                           VALUE 'XD'.                  
017300     SKIP2                                                                
017400 01  GODK-STATUSKODER.                                                    
017500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017600     SKIP3                                                                
017700 01  SSA1                        PIC X(64).                               
017800 01  SSA2                        PIC X(64).                               
017900     EJECT                                                                
018000*    --- IMS FUNKTIONSKODER                                               
018100*01  -COPY W0003                                                          
018200     EJECT                                                                
018300                                                                          
018400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
018500 01  DLI-IO-WDB201.                                                       
018600*    03  -COPY WDB201                                                     
018700                                                                          
018800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE401'.         
018900 01  DLI-IO-WDE401.                                                       
019000*    03  -COPY WDE401                                                     
019100                                                                          
019200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE411'.         
019300 01  DLI-IO-WDE411.                                                       
019400*    03  -COPY WDE411                                                     
019500                                                                          
019600 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
019700 01   DLI-IO-AREA-B601.                                                   
019800*     03  -COPY WDB601                                                    
019900                                                                          
020000 LINKAGE SECTION.                                                         
021000*01  -COPY W0009         -PRE MSG-                                        
021100                                                                          
021200*01  -COPY W0008  -PRE  WDB2-                                             
021300     05  FILLER                  PIC X.                                   
021400                                                                          
021500*01  -COPY W0008  -PRE  WDE4-                                             
021600     05  FILLER                  PIC X.                                   
021700                                                                          
021800*01  -COPY W0008  -PRE  WDB6-                                             
021900     05  FILLER                  PIC X.                                   
022000                                                                          
022100*01  -COPY W0008  -PRE  WDE4B-                                            
022200     05  FILLER                  PIC X.                                   
022300                                                                          
022400     EJECT                                                                
022500 PROCEDURE DIVISION       USING   MSG-PCB                                 
022600                                 WDB2-PCB                                 
022700                                 WDE4-PCB                                 
022800                                 WDB6-PCB                                 
022900                                 WDE4B-PCB.                               
023000 MAIN SECTION.                                                            
023100     ENTRY 'DLITCBL'      USING   MSG-PCB                                 
023200                                 WDB2-PCB                                 
023300                                 WDE4-PCB                                 
023400                                 WDB6-PCB                                 
023500                                 WDE4B-PCB.                               
023600                                                                          
023700     PERFORM A-INIT                                                       
023800     PERFORM IMS-RESTART                                                  
023900                                                                          
024000     PERFORM S01-LAES-W47650                                              
024100     PERFORM UNTIL END-OF-W47650                                          
024200                                                                          
024300       PERFORM D-TESTA-SKAPA-POSTER                                       
024400                                                                          
024500       PERFORM S01-LAES-W47650                                            
024600     END-PERFORM                                                          
024700                                                                          
024800     PERFORM Z-FINIT                                                      
024900                                                                          
025000     MOVE ZERO TO RETURN-CODE                                             
025100     GOBACK                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 A-INIT SECTION.                                                          
025500                                                                          
025600     OPEN INPUT  W47650                                                   
025700     OPEN OUTPUT W47649                                                   
025800                                                                          
025900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026000                                                                          
026100     MOVE LOW-VALUE              TO W-IDGMT-MIN-X                         
026200     MOVE HIGH-VALUE             TO W-IDGMT-MAX-X                         
026300     .                                                                    
026400     EJECT                                                                
026500 D-TESTA-SKAPA-POSTER  SECTION.                                           
026600                                                                          
026700*    TRANS TILL 500-SYSTEM,  EKONOMI                                      
026800                                                                          
026900     MOVE IN-W4765001            TO UT-W4765001                           
027000                                                                          
028000     MOVE IN-IDDISTR             TO TEST-IDDISTR                          
028100                                    DIS1-IDDISTR                          
028200                                    W-IDDISTR-B2                          
028300                                    W-IDDISTR-MIN-B2                      
028400                                    W-IDDISTR-MAX-B2                      
028500                                                                          
028600     IF IN-IDDC NOT = W-IDDC-B6                                           
028700        MOVE IN-IDDC             TO W-IDDC-B6                             
028800        PERFORM IMS-GU-WDB601                                             
028900     END-IF                                                               
029000                                                                          
029100     MOVE IN-IDKUNDNR            TO W-IDKUNDNR-B2                         
029200     IF IN-IDKUNDNR NOT = ZERO                                            
029300       PERFORM IMS-GU-WDB201                                              
029400     ELSE                                                                 
029500       PERFORM IMS-GU-WDB201A                                             
029600     END-IF                                                               
029700                                                                          
029800     IF DIST35-NA-TRANSFER OR DCS-NDC-NA                                  
029900       IF NOT DIST35-NONVCC-REFILL                                        
030000         CONTINUE                                                         
030100       ELSE                                                               
030200*          MAN SKAPAR FIL FÖR DE NYA FLÖDEN FRÅN USA                      
030300*          REFILL TILL CN OCH REFILL TILL SE                              
030400*          510-TRANS                                                      
030500         PERFORM S11-SKRIV-W47649                                         
030600       END-IF                                                             
030700       IF DCS-USA AND GMT-KDKUNDKAT = WS-IMPORTER                         
030800*        MAN SKAPAR FIL FÖR DE NYA FLÖDEN FRÅN USA                        
030900*        GLOBAL EXPORT TILL IMPORTÖRER                                    
031000*        510-TRANS                                                        
031100         PERFORM S11-SKRIV-W47649                                         
031200       END-IF                                                             
031300     ELSE                                                                 
031400*        510-TRANS                                                        
031500       PERFORM S11-SKRIV-W47649                                           
031600     END-IF                                                               
031700                                                                          
031800     MOVE IN-IDPRODNR            TO W-IDPRODNR                            
031900                                    W-IDPRODNR-BSEQ                       
032000     MOVE IN-IDPURAD             TO W-IDPURAD-BSEQ                        
032100     PERFORM IMS-GU-WDE401-ESEQ                                           
032200     IF IN-IDKUNDNR = ZERO AND KORD-IDKUNDNR NOT = ZERO                   
032300       MOVE KORD-IDKUNDNR        TO W-IDKUNDNR-B2                         
032400       PERFORM IMS-GU-WDB201                                              
032500     END-IF                                                               
032600     CALL W460DIS1 USING DIS1-W460DIS1                                    
032700                                                                          
032800     IF KORD-KDFAKTYP =  'R' OR 'G' OR 'K'                                
032900*         TILLÅTNA FAKTURATYPER                                           
033000                                                                          
033100       IF IN-FLCOD = JA                                                   
033200         CONTINUE                                                         
033300       ELSE                                                               
033400         IF GMT-FLNC = JA        OR                                       
033500            DIS1-KDSVAR = JA     OR                                       
033600            DIS130-NOAC          OR                                       
033700            DIST35-EES-REFILL    OR                                       
033710            DIST35-CDC-BR-REFILL                                          
033800           MOVE JA                  TO SKRIV-VIPS-SW                      
033900           MOVE ZERO                TO W-KDSOFT-RU                        
034000           PERFORM S03-KOLLA-SOFTW-RU                                     
034100           IF W-KDSOFT-RU NOT = ZERO                                      
034200             MOVE NEJ               TO SKRIV-VIPS-SW                      
034300           END-IF                                                         
034400                                                                          
034500           IF SKRIV-VIPS-POST                                             
034600*                                                                         
034700*---         -VANLIGT FLÖDE (UTAN STUDS)  --> KDFAKSTA-EXP = 0            
034800*                                                                         
034900*            -VOR-STUDS (DC11 TILL CN/IN) --> KDFAKSTA-EXP = 2            
035000*             * OBS : -INGA VIPS TRANSAR EFTER FÖRSTA FAKT.               
035100*                     DC11->DC71/67 (KDFAKSTA-EXP=1)                      
035200*                     -UTAN BARA EFTER ANDRA FAKTURAN                     
035300*                     DCXX->DEALER (KDFAKSTA-EXP=2)                       
035400*---                                                                      
035500*                                                                         
035600             IF IN-KDFAKSTA-EXP = 0 OR 2                                  
035700                                                                          
035800*              TRANS FÖR INVOICE INFO VIPS/IMPORTÖR                       
035900               MOVE 'RIK'             TO UT-IDPTYP                        
036000               PERFORM S11-SKRIV-W47649                                   
036100             END-IF                                                       
036200           END-IF                                                         
036300         END-IF                                                           
036400       END-IF                                                             
036500     END-IF                                                               
036600                                                                          
036700*      TRANS FÖR SALES STATISTICS                                         
036800     IF KORD-KDFAKTYP =  'R'                                              
036900**** WHEN IT'S NOT A VIPS MARKET (EX DUBAI),                              
037000**** NO LOGIC SHOULD BE ADDED.                                            
037100**** THEN PULS NEED TO SEND SALES INFO TO S&T.                            
037200       IF (DIST35-REFILL AND NOT  (DIST35-REFILL-NA     OR                
037300                                   DIST35-REFILL-CN     OR                
037400                                   DIST35-CDC-IN-REFILL OR                
037500                                   DIST35-CDC-KR-REFILL OR                
037600                                   DIST35-CDC-MY-REFILL OR                
037700                                   DIST35-CDC-TH-REFILL OR                
037800                                   DIST35-CDC-TW-REFILL OR                
037810                                   DIST35-CDC-MX-REFILL OR                
037820                                   DIST35-CDC-BR-REFILL OR                
037830                                   DIST35-CDC-ZA-REFILL))                 
037900                                          OR                              
038000          DIST35-NONVCC-CDC-REFILL        OR                              
038100          DIST35-NONVCC-VCC-REFILL        OR                              
038110          DIST35-NONVCC-VCC-TRANSFER      OR                              
038200          DIST35-RETUR                    OR                              
038300          DIST35-RETUR                    OR                              
038400          DCS-NDC-NA                      OR                              
038500          DCS-CHINA                       OR                              
038600          DCS-INDIA                       OR                              
038700          DCS-KOREA                       OR                              
038800          DCS-TURKEY                      OR                              
038900          DCS-MALAYSIA                    OR                              
039000          DCS-THAILAND                    OR                              
039100          DCS-TAIWAN                      OR                              
039110          DCS-MEXICO                      OR                              
039120          DCS-BRASIL                      OR                              
039130          DCS-SOUTH-AFRICA                                                
039200         CONTINUE                                                         
039300       ELSE                                                               
039400         IF IN-KDFAKSTA-EXP = 0 OR 1                                      
039500           MOVE '330'                TO UT-IDPTYP                         
039600                                                                          
039700           PERFORM S11-SKRIV-W47649                                       
039800         ELSE                                                             
039900           CONTINUE                                                       
040000         END-IF                                                           
040100       END-IF                                                             
040200                                                                          
040300     END-IF                                                               
040400                                                                          
040500     IF DCS-DDC                                                           
040600*      TRANS FÖR INFO OM DDGS-LEVERANS                                    
040700       MOVE '463'                TO UT-IDPTYP                             
040800                                                                          
040900       PERFORM S11-SKRIV-W47649                                           
041000     END-IF                                                               
041100                                                                          
041200     IF  (KORD-KDFAKTYP  = 'R' OR 'G' OR 'K' OR 'N')                      
041300**     TRANS FÖR INFO TILL VR-SYSTEMET                                    
041400       IF GMT-FLVR     = JA                                               
041500                                                                          
041600           IF DIS1-IDLANDX2 =                                             
041700              ISO-SVERIGE   OR                                            
041800              ISO-DANMARK   OR                                            
041900              ISO-NORGE     OR                                            
042000              ISO-PORTUGAL  OR                                            
042100              ISO-HOLLAND   OR                                            
042200              ISO-OSTERRIKE OR                                            
042300              ISO-TJECKIEN  OR                                            
042400              ISO-UNGERN    OR                                            
042500              ISO-TYSKLAND  OR                                            
042600              ISO-AUSTRALIEN OR                                           
042700              DIST24-NORGE                                                
042800             CONTINUE                                                     
042900           ELSE                                                           
043000             MOVE 'VR '            TO UT-IDPTYP                           
043100                                                                          
043200             PERFORM S11-SKRIV-W47649                                     
043300           END-IF                                                         
043400       END-IF                                                             
043500     END-IF                                                               
043600                                                                          
043700*    IF KORD-KDFAKTYP = 'R' OR 'G' OR 'K' OR 'N'                          
043800**     TRANS FÖR FAKTURAINFO  OM FAKTURASTATISTIK                         
043900*      MOVE 'FAK'                  TO UT-IDPTYP                           
044000**  W421V1  BORTTAGET                                                     
044100*      PERFORM S11-SKRIV-W47649                                           
044200*    END-IF                                                               
044300                                                                          
044400     IF  (KORD-KDFAKTYP  = 'R' OR 'G' )                                   
044500        AND DIST03-NORGE                                                  
044600**     TRANS FÖR NORSK TULL-SUMMERING                                     
044700       IF KORD-KDFRAKT = +65                                              
044800         OR (DCS-SDC AND NOT DCS-SWEDEN)                                  
044900         OR DCS-NDC                                                       
045000         OR (DCS-DDC AND DCS-NORWAY)                                      
045100         CONTINUE                                                         
045200       ELSE                                                               
045300         MOVE 'TUL'                  TO UT-IDPTYP                         
045400                                                                          
045500         PERFORM S11-SKRIV-W47649                                         
045600       END-IF                                                             
045700     END-IF                                                               
045800                                                                          
045900     IF ((DCS-CDC OR DCS-DDC) AND                                         
046000         (DIST07-USA-RETAILER OR DIST07-CAN-RETAILER))                    
046100         OR                                                               
046200        ((DCS-NDC-NA) AND                                                 
046300         (KORD-FLORDSPE = NEJ))                                           
046400**     NDC GOODS RECEIVING,  NDC HISTORY, W33                             
046500         MOVE 'W33'                  TO UT-IDPTYP                         
046600                                                                          
046700         PERFORM S11-SKRIV-W47649                                         
046800     END-IF                                                               
046900                                                                          
047000     IF (DIST35-NA-CDC-QUAL-RETURN OR                                     
047100         DIST35-NA-CDC-BB-RETURN   OR                                     
047200         DIST35-CN-CDC-RETURNS     OR                                     
047300         DIST35-IN-CDC-RETURNS     OR                                     
047400         DIST35-KR-CDC-RETURNS     OR                                     
047500         DIST35-AE-CDC-RETURNS     OR                                     
047600         DIST35-TR-CDC-RETURNS     OR                                     
047700         DIST35-MY-CDC-RETURNS     OR                                     
047800         DIST35-TH-CDC-RETURNS     OR                                     
047900         DIST35-TW-CDC-RETURNS     OR                                     
047910         DIST35-MX-CDC-RETURNS     OR                                     
047920         DIST35-BR-CDC-RETURNS     OR                                     
047930         DIST35-ZA-CDC-RETURNS     OR                                     
048000         DIST18-SCRAP-NDC-QUAL)                                           
048100**     LEVERANSANM NDC,2359 OCH 6321                                      
048200         MOVE 'LEV'                  TO UT-IDPTYP                         
048300                                                                          
048400         PERFORM S11-SKRIV-W47649                                         
048500     END-IF                                                               
048600                                                                          
048700     IF (DIST35-REFILL-NA                                                 
048800     OR  DIST35-REFILL-NA-JAP                                             
048900     OR  DIST35-NA-CDC-RETURN                                             
049000     OR  DIST35-NA-NDC-RETURNS                                            
049100     OR  DIST35-NA-TRANSFER                                               
049200     OR  DIST35-REFILL-INOM-NA                                            
049300     OR  DIST07-USA-RETAILER OR DIST07-USA-RET-DISCR                      
049400     OR  DIST07-CAN-RETAILER OR DIST07-CAN-RET-DISCR                      
049500     OR (DIST18-SCRAP-NDC-SC-LOCAL AND (DCS-USA))                         
049600     OR (DIST18-SCRAP-NDC AND (DCS-USA))                                  
049700     OR (DIST18-SCRAP-NDC AND (DCS-CANADA))                               
049800     OR  DIS105-NA-SPEC   )                                               
049900**     LAB  TRANSPORTER TILL AMER. LAGER                                  
050000         IF (KORD-KDFAKTYP  = 'G' OR 'K' OR 'R' OR 'N')                   
050100           MOVE 'LAB'                TO UT-IDPTYP                         
050200           PERFORM S11-SKRIV-W47649                                       
050300         END-IF                                                           
050400     END-IF                                                               
050500                                                                          
050600*      TRANS FÖR SATS TILL INLEVERANS FLYTTAT TILL W47668 I S5            
050700                                                                          
050800                                                                          
050900     .                                                                    
051000     EJECT                                                                
051100 Z-FINIT SECTION.                                                         
051200                                                                          
051300     CLOSE W47650                                                         
051400           W47649                                                         
051500                                                                          
051600     MOVE 'S' TO POSTSUM-OPKOD                                            
051700     CALL POSTSUM USING POSTSUM-PARM                                      
051800     .                                                                    
051900     EJECT                                                                
052000 S01-LAES-W47650  SECTION.                                                
052100                                                                          
052200     READ W47650 INTO IN-AREA                                             
052300     AT END                                                               
052400        SET END-OF-W47650 TO TRUE                                         
052500     NOT AT END                                                           
052600        MOVE 'W47649'       TO POSTSUM-FDNAMN                             
052700        MOVE 'W47649D1'     TO POSTSUM-DDNAMN2                            
052800        MOVE SPACE          TO POSTSUM-TRANSTYP                           
052900        CALL POSTSUM USING POSTSUM-PARM                                   
053000     END-READ                                                             
053100     .                                                                    
053200     EJECT                                                                
053300 S03-KOLLA-SOFTW-RU SECTION.                                              
053400     IF DIST38-PER-RU                                                     
053500        PERFORM IMS-GU-WDE411-BSEQ                                        
053600        IF SEGMENT-FINNS                                                  
053700           IF ORAD-IDBIL > SPACE AND  ORAD-IDSYSTEM  = 'VDI '             
053800             MOVE +1                  TO W-KDSOFT-RU                      
053900           ELSE                                                           
054000             IF ORAD-IDSYSTEM = 'SOFT'                                    
054100               MOVE +2                TO W-KDSOFT-RU                      
054200             ELSE                                                         
054300               MOVE ORAD-IDARTNR      TO TEST-ARTIKEL                     
054400               IF ART04-SOFTWARE                                          
054500                 MOVE +3              TO W-KDSOFT-RU                      
054600               ELSE                                                       
054700                 IF ORAD-IDSYSTEM = 'W371' OR 'W37A'                      
054800                   MOVE +4            TO W-KDSOFT-RU                      
054900                 ELSE                                                     
055000                   MOVE +0            TO W-KDSOFT-RU                      
055100                 END-IF                                                   
055200               END-IF                                                     
055300             END-IF                                                       
055400           END-IF                                                         
055500        END-IF                                                            
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900 S11-SKRIV-W47649 SECTION.                                                
056000                                                                          
056100     WRITE UT-POST          FROM UT-W4765001                              
056200                                                                          
056300     MOVE UT-IDPTYP         TO POSTSUM-TRANSTYP                           
056400     MOVE 'W47649'          TO POSTSUM-FDNAMN                             
056500     MOVE 'W47649D2'        TO POSTSUM-DDNAMN2                            
056600     CALL POSTSUM USING POSTSUM-PARM                                      
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000******  IMS-LÄSNINGAR  *******                                            
057100                                                                          
057200 IMS-RESTART  SECTION.                                                    
057300     MOVE 'IMS-RESTART'         TO WS-SEKTION                             
057400                                                                          
057500     MOVE SPACE TO MSG-IO-AREA-1                                          
057600     MOVE '  ' TO GODK-STATUSKODER                                        
057700     CALL CBLTDLI USING XRST MSG-PCB                                      
057800                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
057900                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
058000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058100     PERFORM IMS-STATUSKONTROLL                                           
058200     .                                                                    
058300                                                                          
058400 IMS-GU-WDB201  SECTION.                                                  
058500     MOVE 'IMS-GU-WDB201'       TO WS-SEKTION                             
058600                                                                          
058700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
058800          DELIMITED BY SIZE INTO SSA1                                     
058900     MOVE '    ' TO GODK-STATUSKODER                                      
059000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
059100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     .                                                                    
059400     SKIP3                                                                
059500 IMS-GU-WDB201A  SECTION.                                                 
059600     MOVE 'IMS-GU-WDB201A'       TO WS-SEKTION                            
059700                                                                          
059800     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
059900                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
060000          DELIMITED BY SIZE INTO SSA1                                     
060100     MOVE '    ' TO GODK-STATUSKODER                                      
060200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
060300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
060400     PERFORM IMS-STATUSKONTROLL                                           
060500     .                                                                    
060600     SKIP3                                                                
060700 IMS-GU-WDE401-ESEQ SECTION.                                              
060800     MOVE 'IMS-GU-WDE401-ESEQ'   TO WS-SEKTION                            
060900                                                                          
061000     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ') '                       
061100          DELIMITED BY SIZE INTO SSA1                                     
061200     MOVE '    ' TO GODK-STATUSKODER                                      
061300     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
061400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
061500     PERFORM IMS-STATUSKONTROLL                                           
061600     .                                                                    
061700     SKIP3                                                                
061800 IMS-GU-WDE411-BSEQ SECTION.                                              
061900     MOVE 'IMS-GU-WDE411-BSEQ'   TO WS-SEKTION                            
062000                                                                          
062100     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ') '                       
062200          DELIMITED BY SIZE INTO SSA1                                     
062300     MOVE '    ' TO GODK-STATUSKODER                                      
062400     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-WDE411 SSA1                   
062500     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
062600     PERFORM IMS-STATUSKONTROLL                                           
062700     .                                                                    
062800     SKIP3                                                                
062900 IMS-GU-WDB601    SECTION.                                                
063000     MOVE 'IMS-GU-WDB601'        TO WS-SEKTION                            
063100                                                                          
063200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
063300          DELIMITED BY SIZE INTO SSA1                                     
063400     MOVE '  GE' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
063600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     IF SEGMENT-SAKNAS                                                    
063900         MOVE SPACE TO DCS-KDDC                                           
064000     END-IF                                                               
064100     .                                                                    
064200     SKIP3                                                                
064300 IMS-STATUSKONTROLL SECTION.                                              
064400                                                                          
064500     SET STATUS-IX TO 1                                                   
064600     SEARCH GODK-STATUS                                                   
064700       AT END                                                             
064800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
064900         DELIMITED BY SIZE INTO FELTEXT                                   
065000         CALL FELLOG                                                      
065100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066000         CONTINUE                                                         
066100     END-SEARCH                                                           
066200     .                                                                    
066300     SKIP2                                                                
