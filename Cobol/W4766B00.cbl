000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4766B00.                                                
000300 AUTHOR.         MOGREN STINA.                                            
000400 DATE-WRITTEN.   03/10/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER W47669 , SORTERADE POSTER                       
000900*        SKRIVS PÅ UTFILEN SAMT HÄMTAR PRAVCOST FÖR USA/CAN               
001000*                                                                         
001100*        PROGRAMMET LÄSER     WDK7 ARTIKELREG                             
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*     --- INPOSTER FRÅN W4766800                                          
002100     SELECT W47669                     ASSIGN TO W4766BD1.                
002200     SKIP2                                                                
002300*     --- UTFIL MED TRANS TILL REFILL-SYSTEM                              
002400     SELECT W4766B                     ASSIGN TO W4766BD2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W47669                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W461RIO2     -L.                                               
003500     SKIP3                                                                
003600 FD  W4766B                                                               
003700     RECORDING       V                                                    
003800     BLOCK CONTAINS  0.                                                   
003900*01  UT-POST   -COPY W461RIO2 -L.                                         
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4766B00'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  W47669-EOF-SW               PIC X       VALUE 'N'.                   
004800     88  END-OF-W47669                       VALUE 'J'.                   
004900                                                                          
005000 77  W-POST                      PIC X(3)    VALUE SPACE.                 
005100                                                                          
005200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
005300                                                                          
005400 01  FILLER                      PIC X(16)   VALUE 'WS-SEKTION'.          
005500 01  WS-SEKTION                  PIC X(32)   VALUE SPACE.                 
005600                                                                          
005700 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
005800 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
005900 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
006000 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
006100                                                                          
006200*    ----  VALID IDDC CODES                                               
006300*01    -COPY  WWDC99                                                      
006400*01    -COPY  WWDCKONS                                                    
006500     EJECT                                                                
006600 01  TEST-IDDISTR                PIC 9(5)  COMP-3.                        
006700*01  FILLER    -COPY WWDIST07   -RED TEST-IDDISTR.                        
006800     EJECT                                                                
006900                                                                          
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500*    --- PARAMETRAR TILL ABEND                                            
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4) COMP VALUE +0.                 
007800     EJECT                                                                
007900                                                                          
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IN-AREA'.             
008500                                                                          
008600 01  IN-AREA.                                                             
008700*    03  -COPY W461RIO2   -PRE IN-                                        
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'UT-AREA'.             
009000                                                                          
009100 01  UT-AREA.                                                             
009200*    03  -COPY W461RIO2   -PRE UT-                                        
009300     EJECT                                                                
009400 01  W-AREA.                                                              
009500*    03  -COPY W461RIK1                                                   
009600*    03  -COPY W461RILN                                                   
009700*    03  -COPY W461RIM2                                                   
009800*    03  -COPY W461RINN                                                   
009900*    03  -COPY W461RIO2                                                   
010000*    03  -COPY W461RIPN                                                   
010100*                                                                         
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  NYCKLAR-TILL-DLI.                                                    
010500                                                                          
010600     03  W-IDARTNR-X.                                                     
010700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010800                                                                          
010900     03  W-IDDC-X.                                                        
011000         05  W-IDDC              PIC XX      VALUE SPACE.                 
011010     03  W-WDE4FSEQ-X.                                                    
011020         05 W-IDPRODNR           PIC S9(7) VALUE +0 COMP-3.               
011030         05 W-IDKOLLI            PIC S9(5) VALUE +0 COMP-3.               
011100                                                                          
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011800     88  IMS-EJ-OK                           VALUE 'XD'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012800     EJECT                                                                
012900                                                                          
013000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
013100 01  DLI-IO-WDK711.                                                       
013200*    03  -COPY WDK711                                                     
013210 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE401'.         
013220 01  DLI-IO-WDE401.                                                       
013230*    03  -COPY WDE401                                                     
013240     EJECT                                                                
013250 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE411'.         
013260 01  DLI-IO-WDE411.                                                       
013270*    03  -COPY WDE411                                                     
013280     EJECT                                                                
013300                                                                          
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0008  -PRE  WDK7-                                             
013800     05  FILLER                  PIC X.                                   
013810*01  -COPY W0008  -PRE  WDE4-                                             
013820     05  FILLER                  PIC X.                                   
013900                                                                          
014000                                                                          
014100 PROCEDURE DIVISION       USING                                           
014200                                 WDK7-PCB WDE4-PCB.                       
014300 MAIN SECTION.                                                            
014400     ENTRY 'DLITCBL'      USING                                           
014500                                 WDK7-PCB WDE4-PCB.                       
014600                                                                          
014700                                                                          
014800     PERFORM A-INIT                                                       
014900*    PERFORM IMS-RESTART                                                  
015000                                                                          
015100     PERFORM S01-LAES-W47669                                              
015200     PERFORM UNTIL END-OF-W47669                                          
015300                                                                          
015400       PERFORM D-TESTA-UPPDATERA-POSTER                                   
015500                                                                          
015600       PERFORM S01-LAES-W47669                                            
015700     END-PERFORM                                                          
015800                                                                          
015900     PERFORM Z-FINIT                                                      
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     OPEN INPUT  W47669                                                   
016800     OPEN OUTPUT W4766B                                                   
016900                                                                          
017000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017100                                                                          
017200     .                                                                    
017300     EJECT                                                                
017400 D-TESTA-UPPDATERA-POSTER  SECTION.                                       
017500                                                                          
017600*    TRANS TILL REFILL-SYSTEM,                                            
017700                                                                          
017800     MOVE IN-RIO-IDPTYP          TO W-POST                                
017900                                                                          
018000     EVALUATE W-POST                                                      
018100       WHEN 'RIK'                                                         
018200         MOVE IN-AREA            TO RIK-W461RIK1                          
018300       WHEN 'RIL'                                                         
018400         MOVE IN-AREA            TO RIL-W461RILN-CTX                      
018500       WHEN 'RIM'                                                         
018600         MOVE IN-AREA            TO RIM-W461RIM2-CTX                      
018700       WHEN 'RIN'                                                         
018800         MOVE IN-AREA            TO RIN-W461RINN-CTX                      
018900       WHEN 'RIO'                                                         
019000         MOVE IN-AREA            TO RIO-W461RIO2                          
019100         PERFORM DA-HAMTA-PRAVCOST                                        
019200       WHEN 'RIP'                                                         
019300         MOVE IN-AREA            TO RIP-W461RIPN-CTX                      
019400     END-EVALUATE                                                         
019500                                                                          
019600     PERFORM S11-SKRIV-W4766B                                             
019700     .                                                                    
019800     EJECT                                                                
019900 DA-HAMTA-PRAVCOST  SECTION.                                              
020000                                                                          
020100     MOVE RIK-IDDISTR            TO TEST-IDDISTR                          
020200     MOVE RIK-IDDC               TO WS-IDDC                               
020300                                    W-IDDC                                
020310     MOVE RIN-IDPRODNR           TO W-IDPRODNR                            
020320     MOVE RIN-IDKOLLI            TO W-IDKOLLI                             
020400                                                                          
020500     MOVE RIO-IDARTNR            TO W-IDARTNR                             
020600     IF NDC-US OR NDC-CA OR XDC-NON-VCC-OWNED OR                          
020700        ((CDC-SE OR GOOD-DDC) AND (DIST07-USA-RETAILER)) OR               
020800        ((CDC-SE OR GOOD-DDC) AND (DIST07-CAN-RETAILER)) OR               
020900        ((CDC-SE OR GOOD-DDC) AND (DIST07-NON-VCC-OWNED))                 
020920                                                                          
021000       IF (CDC-SE OR GOOD-DDC) AND (DIST07-USA-RETAILER)                  
021100         MOVE  WC-NDC-US-RU         TO W-IDDC                             
021200       END-IF                                                             
021300       IF (CDC-SE OR GOOD-DDC) AND DIST07-CAN-RETAILER                    
021400         MOVE WC-NDC-CA           TO W-IDDC                               
021500       END-IF                                                             
021510       IF (CDC-SE OR GOOD-DDC) AND DIST07-KINA                            
021520         MOVE WC-NDC-CN-71      TO W-IDDC                                 
021531       END-IF                                                             
021532       IF (CDC-SE OR GOOD-DDC) AND DIST07-INDIEN                          
021533         MOVE WC-NDC-IN       TO W-IDDC                                   
021534       END-IF                                                             
021535       IF (CDC-SE OR GOOD-DDC) AND DIST07-KOREA                           
021536         MOVE WC-NDC-KR    TO W-IDDC                                      
021538       END-IF                                                             
021539       IF (CDC-SE OR GOOD-DDC) AND DIST07-TURKEY                          
021540         MOVE WC-NDC-TR   TO W-IDDC                                       
021543       END-IF                                                             
021544       IF (CDC-SE OR GOOD-DDC) AND DIST07-MALAYSIA                        
021545         MOVE WC-NDC-MY TO W-IDDC                                         
021546       END-IF                                                             
021547       IF (CDC-SE OR GOOD-DDC) AND DIST07-THAILAND                        
021548         MOVE WC-NDC-TH TO W-IDDC                                         
021549       END-IF                                                             
021550       IF (CDC-SE OR GOOD-DDC) AND DIST07-TAIWAN                          
021560         MOVE WC-NDC-TW TO W-IDDC                                         
021600       END-IF                                                             
021610       IF (CDC-SE OR GOOD-DDC) AND DIST07-MEXICO                          
021620         MOVE WC-NDC-MX TO W-IDDC                                         
021630       END-IF                                                             
021631       IF (CDC-SE OR GOOD-DDC) AND DIST07-BRAZIL                          
021632         MOVE WC-NDC-BR TO W-IDDC                                         
021633       END-IF                                                             
021640       IF (CDC-SE OR GOOD-DDC) AND DIST07-S-AFRICA                        
021650         MOVE WC-NDC-ZA TO W-IDDC                                         
021660       END-IF                                                             
021700                                                                          
021701       IF DIST07-NON-VCC-OWNED                                            
021710         PERFORM IMS-GU-WDE411-FSEQ                                       
021720         IF SEGMENT-FINNS                                                 
021721           IF ORAD-IDARTNR = W-IDARTNR                                    
021732             MOVE ORAD-PRAVCOST     TO RIO-PRAVCOST                       
021741           END-IF                                                         
021742         ELSE                                                             
021743           MOVE ZERO                TO RIO-PRAVCOST                       
021744         END-IF                                                           
021745       END-IF                                                             
021746                                                                          
021750       IF RIO-PRAVCOST = ZERO                                             
021760       OR NOT DIST07-NON-VCC-OWNED                                        
021800         PERFORM IMS-GU-WDK711                                            
021900         IF SEGMENT-FINNS                                                 
022000           MOVE SLAG-PRAVCOST     TO RIO-PRAVCOST                         
022100         END-IF                                                           
022110       END-IF                                                             
022200       PERFORM S30-JA                                                     
022300     END-IF                                                               
022400                                                                          
022500     .                                                                    
022600     EJECT                                                                
022700 Z-FINIT SECTION.                                                         
022800                                                                          
022900     CLOSE W47669                                                         
023000           W4766B                                                         
023100                                                                          
023200     MOVE 'S' TO POSTSUM-OPKOD                                            
023300     CALL POSTSUM USING POSTSUM-PARM                                      
023400     .                                                                    
023500     EJECT                                                                
023600 S01-LAES-W47669  SECTION.                                                
023700                                                                          
023800     READ W47669 INTO IN-AREA                                             
023900     AT END                                                               
024000        SET END-OF-W47669 TO TRUE                                         
024100     NOT AT END                                                           
024200        MOVE 'W47669'       TO POSTSUM-FDNAMN                             
024300        MOVE 'W4766BD1'     TO POSTSUM-DDNAMN2                            
024400        MOVE SPACE          TO POSTSUM-TRANSTYP                           
024500        CALL POSTSUM USING POSTSUM-PARM                                   
024600     END-READ                                                             
024700     .                                                                    
024800     EJECT                                                                
024900 S11-SKRIV-W4766B SECTION.                                                
025000                                                                          
025100     EVALUATE W-POST                                                      
025200       WHEN 'RIK'                                                         
025300         WRITE UT-POST FROM RIK-W461RIK1                                  
025400       WHEN 'RIL'                                                         
025500         WRITE UT-POST FROM RIL-W461RILN-CTX                              
025600       WHEN 'RIM'                                                         
025700         WRITE UT-POST FROM RIM-W461RIM2-CTX                              
025800       WHEN 'RIN'                                                         
025900         WRITE UT-POST FROM RIN-W461RINN-CTX                              
026000       WHEN 'RIO'                                                         
026100         WRITE UT-POST FROM RIO-W461RIO2                                  
026200       WHEN 'RIP'                                                         
026300         WRITE UT-POST FROM RIP-W461RIPN-CTX                              
026400     END-EVALUATE                                                         
026500                                                                          
026600     MOVE W-POST            TO POSTSUM-TRANSTYP                           
026700     MOVE 'W4766B'          TO POSTSUM-FDNAMN                             
026800     MOVE 'W4766BD2'        TO POSTSUM-DDNAMN2                            
026900     CALL POSTSUM USING POSTSUM-PARM                                      
027000     .                                                                    
027100     EJECT                                                                
027200 S30-JA  SECTION.                                                         
027300                                                                          
027400     IF RIO-FLINVEST = 'J'                                                
027500       MOVE 'Y'                  TO RIO-FLINVEST                          
027600     END-IF                                                               
027700     IF RIO-FLPRTILL = 'J'                                                
027800       MOVE 'Y'                  TO RIO-FLPRTILL                          
027900     END-IF                                                               
028000     IF RIO-FLDIRLEV = 'J'                                                
028100       MOVE 'Y'                  TO RIO-FLDIRLEV                          
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500                                                                          
028600******  IMS-LÄSNINGAR  *******                                            
028700                                                                          
028800*IMS-RESTART  SECTION.                                                    
028900*    MOVE 'IMS-RESTART'         TO WS-SEKTION                             
029000*                                                                         
029100*    MOVE SPACE TO MSG-IO-AREA-1                                          
029200*    MOVE '  ' TO GODK-STATUSKODER                                        
029300*    CALL CBLTDLI USING XRST MSG-PCB                                      
029400*                       MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
029500*                       CHKP-AREA-1-LENGTH CHKP-AREA-1                    
029600*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029700*    PERFORM IMS-STATUSKONTROLL                                           
029800*    .                                                                    
029900*                                                                         
030000 IMS-GU-WDK711 SECTION.                                                   
030100     MOVE 'IMS-GU-WDK711'       TO WS-SEKTION                             
030200                                                                          
030300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
030400          DELIMITED BY SIZE INTO SSA1                                     
030500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
030600          DELIMITED BY SIZE INTO SSA2                                     
030700     MOVE '  GE' TO GODK-STATUSKODER                                      
030800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
030900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
031000     PERFORM IMS-STATUSKONTROLL                                           
031100     .                                                                    
031200     SKIP3                                                                
031210 IMS-GU-WDE411-FSEQ            SECTION.                                   
031220                                                                          
031230     STRING 'WDE411  (WDE4FSEQ =' W-WDE4FSEQ-X ')'                        
031240            DELIMITED BY SIZE INTO SSA1                                   
031250     MOVE '  GE' TO GODK-STATUSKODER                                      
031260     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE411 SSA1                    
031270     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
031280     PERFORM IMS-STATUSKONTROLL                                           
031290     .                                                                    
031291     SKIP3                                                                
031300 IMS-STATUSKONTROLL SECTION.                                              
031400                                                                          
031500     SET STATUS-IX TO 1                                                   
031600     SEARCH GODK-STATUS                                                   
031700       AT END                                                             
031800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031900         DELIMITED BY SIZE INTO FELTEXT                                   
032000         CALL FELLOG                                                      
032100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032200         CONTINUE                                                         
032300     END-SEARCH                                                           
032400     .                                                                    
032500     SKIP2                                                                
