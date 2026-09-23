000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017600.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   04/10/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.INVENTORYREQUEST                            
000800*    WEB-LDC: WL017600 PROGRAM IS A REPLICA OF W5030400 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        INVENTERINGEN - INRAPPORTERING AV RE1-UPPGIFTER.                 
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: WL0176T                                             
001700*        REQUEST:     WL0176I1                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        RESPONSE:    WL0176O1                                            
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500 FILE-CONTROL.                                                            
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800                                                                          
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'WL017600'.            
003100                                                                          
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400 77  KDRC-DISPLAY                PIC Z(5).                                
003500 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
003600 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
003700                                                                          
003800 77  YES                         PIC X       VALUE 'J'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
004000 77  INV-DC-EJ-KLAR              PIC X       VALUE 'N'.                   
004100 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77  KOLLIND                     PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77  MAX-GRAENS                  PIC S9(9)  VALUE +24   COMP SYNC.        
       77    CD-IX                     PIC S9(3)  VALUE ZERO  COMP-3.           
004400                                                                          
004500                                                                          
004600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004700     88  KEYS-OK                             VALUE 'J'.                   
004800     88  KEYS-WRONG                          VALUE 'N'.                   
004900                                                                          
005000 01  DIVERSE.                                                             
005100   03  DAGENS-DATUM              PIC S9(6).                               
005200   03  LAGRA-VVKL     OCCURS 24  PIC S9      COMP-3.                      
005300   03  LAGRA-OMR      OCCURS 24  PIC S9(3)   COMP-3.                      
005400   03  LAGRA-GANG     OCCURS 24  PIC S9(3)   COMP-3.                      
005500   03  LAGRA-PLATS    OCCURS 24  PIC S9(5)   COMP-3.                      
005600   03  LAGRA-IDFKNGRP OCCURS 24  PIC S9(5)   COMP-3.                      
005700   03  LAGRA-KDPRODSL OCCURS 24  PIC S9(3)   COMP-3.                      
005800   03  LAGRA-KDPSLLOC OCCURS 24  PIC S9(3)   COMP-3.                      
005900   03  FELFLAGGA                 PIC X       VALUE 'N'.                   
006000   03  RAPP-KOLL                 PIC X.                                   
006100       88  REPORT-EXISTS                     VALUE 'J'.                   
006200       88  REPORT-MISSING                    VALUE 'N'.                   
006300   03  W-ADARTADR                PIC 9(11).                               
006400   03  W-ADRESS REDEFINES W-ADARTADR .                                    
006500     05  W-ADLAGOMR              PIC 9(3).                                
006600     05  W-ADGANG                PIC 9(3).                                
006700     05  W-ADPLATS               PIC 9(5).                                
006800   03  ARTIKEL-FINNS             PIC X       VALUE 'N'.                   
006900   03  ADRESS-FINNS              PIC X       VALUE 'N'.                   
007000   03  STANDARDPRIS-FINNS        PIC X       VALUE 'N'.                   
007100   03  INVENT-FINNS OCCURS 24    PIC X.                                   
007200   03  UPPDATERING               PIC X       VALUE 'N'.                   
007300                                                                          
007400 01  WS-INV-DAREGDAT-AREA.                                                
007500     03  WS-INV-DAREGDAT     PIC 9(9) VALUE ZERO.                         
007600     03  FILLER REDEFINES WS-INV-DAREGDAT.                                
007700       05  WS-INV-NOLL         PIC 9(1).                                  
007800       05  WS-INV-SEKEL        PIC 9(2).                                  
007900       05  WS-INV-AAMMDD       PIC 9(6).                                  
008000                                                                          
008100 01  WS-TISEGKEYAREA.                                                     
008200     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
008300     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
008400         05  WS-AAR          PIC 9(2).                                    
008500         05  WS-TIAAMMDD     PIC 9(6).                                    
008600         05  WS-LOPNR        PIC 9(1).                                    
008700     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
008800                                                                          
008900 01  FILLER                      PIC X(16)   VALUE 'OBJEKT-TEST'.         
009000 01  TEST-IDARTNR                PIC 9(9)    COMP-3.                      
009100*01  FILLER -COPY WWBYT09    -RED TEST-IDARTNR                            
009200                                                                          
009210*01  -COPY WWDC99                                                         
009220*                                                                         
009300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009400 01  GENERAL-SUBPROGRAMS.                                                 
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900                                                                          
010000*    --- PARAMETERS TO ABEND                                              
010100                                                                          
010200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010500                                                                          
010600 01  MESSAGE-CODES.                                                       
010700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010800                                                                          
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011100                                                                          
011200*01  -COPY WZ01SUB                                                        
011300                                                                          
011400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011500                                                                          
011600 01  REQU-AREA.                                                           
011700*    03  -COPY WZ01REQU                                                   
011800*    03  -COPY WL0176I1                                                   
011900                                                                          
012000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012100                                                                          
012200 01  RESP-AREA.                                                           
012300*    03  -COPY WZ01RESP                                                   
012400*    03  -COPY WL0176O1                                                   
012500                                                                          
012600******************************************************************        
012700*****                                                                     
012800*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012900*****                                                                     
013000 01  IMS-WS.                                                              
013100   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
013200     SKIP3                                                                
013300*****                    **** STATUS-KOD FRÅN IMS                         
013400   03    STATUS-WS       PIC XX.                                          
013500         88  SEGMENT-FOUND       VALUE '  '.                              
013600         88  SEGMENT-MISSING     VALUE 'GE'.                              
013700         88  SEGMENT-EXISTS      VALUE 'II'.                              
013800         88  INDEX-EXISTS        VALUE 'NI'.                              
013900                                                                          
014000   03    GOOD-STATUSCODES.                                                
014100     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200                                                                          
014300 01      SSA1            PIC X(128) VALUE SPACE.                          
014400 01      SSA2            PIC X(128) VALUE SPACE.                          
014500                                                                          
014600*                            IMS FUNKTIONSKODER                           
014700*01      -COPY W0003                                                      
014800                                                                          
014900 01    FILLER                    PIC X(12) VALUE 'DLINYCKLAR'.            
015000 01    NYCKLAR-TILL-DLI.                                                  
015100   03    W-IDARTNR-X.                                                     
015200     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
015300                                                                          
015400   03    W-KDSEGKEY-X.                                                    
015500     05    W-KDSEGKEY            PIC X(1)    VALUE '1'.                   
015600                                                                          
015700   03    W-IDDC-X.                                                        
015800     05    W-IDDC                PIC X(2)    VALUE SPACE.                 
015900                                                                          
016000   03    W-WDH111KY-X.                                                    
016100     05    W-IDDC-WDH1           PIC X(2)  VALUE SPACE.                   
016200     05    W-KDINVKAT-WDH1       PIC S9(3) COMP-3.                        
016300     05    W-TISEGKEY-WDH1       PIC S9(9) COMP-3.                        
016400     05    W-DAREGDAT-SORT       PIC  9(8).                               
016500                                                                          
016600   03    W-WDH111KY-MIN-X.                                                
016700     05    W-IDDC-WDH1-MIN     PIC X(2)  VALUE SPACE.                     
016800     05    W-KDINVKAT-WDH1-MIN PIC S9(3) VALUE ZERO       COMP-3.         
016900     05    W-TISEGKEY-WDH1-MIN PIC S9(9) VALUE ZERO       COMP-3.         
017000     05    W-DAREGDAT-SORT-MIN    PIC  9(8) VALUE ZERO.                   
017100                                                                          
017200   03    W-WDH111KY-MAX-X.                                                
017300     05    W-IDDC-WDH1-MAX     PIC X(2)  VALUE SPACE.                     
017400     05    W-KDINVKAT-WDH1-MAX PIC S9(3) VALUE +049       COMP-3.         
017500     05    W-TISEGKEY-WDH1-MAX PIC S9(9) VALUE +999999999 COMP-3.         
017600     05    W-DAREGDAT-SORT-MAX PIC    9(8) VALUE 99999999.                
017700                                                                          
017800                                                                          
017900*                            DLI INPUT-OUTPUT AREA                        
018000*01  -COPY WDH101       -PRE INV-.                                        
018100     EJECT                                                                
018200*01  -COPY WDH111                                                         
018300     EJECT                                                                
018400*01  -COPY WDH121                                                         
018500     EJECT                                                                
018510*-------- WDK6-ARTIKELREG                                                 
018520                                                                          
018530 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
018540 01  DLI-IO-WDK601.                                                       
018550*  03  -COPY WDK601.                                                      
018560                                                                          
018570 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
018580 01  DLI-IO-WDK611.                                                       
018590*  03  -COPY WDK611.                                                      
019300                                                                          
019310*-------- WDK7-ARTIKELREG                                                 
019320                                                                          
019330 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK701'.           
019340 01  DLI-IO-WDK701.                                                       
019350*  03  -COPY WDK701.                                                      
019360                                                                          
019370 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK711'.           
019380 01  DLI-IO-WDK711.                                                       
019390*  03  -COPY WDK711.                                                      
020200                                                                          
020300 LINKAGE SECTION.                                                         
020400*01    -COPY W0009           -PRE MSG-                                    
020500                                                                          
020600*01    -COPY W0008           -PRE INV-                                    
020700    05  FILLER        PIC X.                                              
020800                                                                          
020900*01    -COPY W0008           -PRE WDK6-                                   
021000    05  FILLER        PIC X.                                              
021100                                                                          
021200*01    -COPY W0008           -PRE WDK7-                                   
021300    05  FILLER        PIC X.                                              
021400                                                                          
021500 PROCEDURE DIVISION USING  MSG-PCB   INV-PCB                              
021600                           WDK6-PCB  WDK7-PCB.                            
021700     ENTRY 'DLITCBL' USING MSG-PCB   INV-PCB                              
021800                           WDK6-PCB  WDK7-PCB.                            
021900                                                                          
022000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
022100     IF SUB-KDRC = 0                                                      
022200        PERFORM A-INIT                                                    
022300        PERFORM B-CHECK-KEYS                                              
022400        IF KEYS-OK                                                        
022500           PERFORM C-CHECK-BEFRAPP                                        
022600           IF REPORT-EXISTS                                               
022700              PERFORM D-KONTROLLERA-INDATA                                
022800              IF FELFLAGGA = NOO                                          
022900                 PERFORM E-UPPDATERA-BAS                                  
023000              END-IF                                                      
023100           END-IF                                                         
023200        END-IF                                                            
023300        PERFORM S02-RETURN-RESPONSE                                       
023400     END-IF                                                               
023500                                                                          
023600     MOVE ZERO TO RETURN-CODE                                             
023700     GOBACK                                                               
023800     .                                                                    
023900                                                                          
024000 A-INIT SECTION.                                                          
024100     MOVE 'A-INIT'      TO CURR-SECTION                                   
024200                                                                          
024300     MOVE ALL '+'        TO RESP-AREA                                     
024400     MOVE SPACE          TO RESP-IDMSG-ERROR                              
024500                            RESP-IDMSG-INFO                               
024600                            RESP-IDELMT-ERROR                             
024700     MOVE 001            TO RESP-IDMSGVER                                 
024800                                                                          
024900     MOVE REQU-IDDC-KEY  TO RESP-IDDC-KEY                                 
025000                                                                          
025100     ACCEPT  DAGENS-DATUM  FROM DATE                                      
025200     .                                                                    
025300                                                                          
025400 B-CHECK-KEYS SECTION.                                                    
025500     MOVE 'B-CHECK-KEYS'    TO CURR-SECTION                               
025600                                                                          
025700     MOVE YES TO KEYS-SW                                                  
025800                                                                          
025900     IF REQU-KDPGMACT = 'E'                                               
026000        CONTINUE                                                          
026100     ELSE                                                                 
026200       MOVE '023'              TO RESP-IDMSG-ERROR                        
026300*      WRONG ACTION KEY ***                                               
026400       MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                       
026500       MOVE NOO                TO KEYS-SW                                 
026600     END-IF                                                               
026700     .                                                                    
026800                                                                          
026900 C-CHECK-BEFRAPP   SECTION.                                               
027000     MOVE 'C-CHECK-BEFRAPP'    TO CURR-SECTION                            
027100*                                                                         
027200*  KONTROLL OM NÅGON RAPPORTERING HAR GJORTS PÅ BILDEN                    
027300*                                                                         
027400     MOVE +1  TO INDX                                                     
027500     MOVE NOO TO RAPP-KOLL                                                
027600                                                                          
027700     PERFORM UNTIL INDX > MAX-GRAENS OR REPORT-EXISTS                     
027800        IF REQU-IDARTNR (INDX)   NOT = ALL '+'  OR                        
027900           REQU-KDINVPRIO (INDX) NOT = ALL '+'  OR                        
028000           REQU-TEINVANM (INDX)  NOT = ALL '+'                            
028100           MOVE YES TO RAPP-KOLL                                          
028200        ELSE                                                              
028300           ADD +1 TO INDX                                                 
028400        END-IF                                                            
028500     END-PERFORM                                                          
028600     .                                                                    
028700 D-KONTROLLERA-INDATA  SECTION.                                           
028800     MOVE 'D-KONTROLLERA-INDATA'    TO CURR-SECTION                       
028900*                                                                         
029000*  INDATAKONTROLLER                                                       
029100*  FORMELLA KONTROLLER                                                    
029200*                                                                         
029300     MOVE NOO TO FELFLAGGA                                                
029400     MOVE +1  TO INDX                                                     
029500                                                                          
029600     PERFORM UNTIL INDX > MAX-GRAENS                                      
029700        IF (REQU-INV-RE1-GRP (INDX) NOT = ALL '+')                        
029800           IF REQU-IDARTNR (INDX) = ALL '+'                               
029900              MOVE YES              TO FELFLAGGA                          
030000              MOVE '026'            TO RESP-IDMSG-ERROR                   
030100              MOVE '026'            TO RESP-IDMSG-ERROR-LINE(INDX)        
030200*             PART NUMBER MISSING ***                                     
030300              MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                  
030400           END-IF                                                         
030500           IF FELFLAGGA = NOO                                             
030600              IF REQU-IDARTNR (INDX) NOT NUMERIC                          
030700                 MOVE YES           TO FELFLAGGA                          
030800                 MOVE '024'         TO RESP-IDMSG-ERROR                   
030900                 MOVE '024'         TO RESP-IDMSG-ERROR-LINE(INDX)        
031000*                NOT NUMERIC ***                                          
031100                 MOVE 'IDARTNR'     TO RESP-IDELMT-ERROR                  
031200              END-IF                                                      
031300           END-IF                                                         
031400           IF FELFLAGGA = NOO                                             
031500              MOVE REQU-IDARTNR(INDX) TO W-IDARTNR TEST-IDARTNR           
031600                                                                          
031700              IF BYT09-OBJEKT                                             
031800                 MOVE YES           TO FELFLAGGA                          
031900                 MOVE '218'         TO RESP-IDMSG-ERROR                   
032000                 MOVE '218'         TO RESP-IDMSG-ERROR-LINE(INDX)        
032100*     BAD CORE WILL NOT BE UPDATED ***                                    
032200              END-IF                                                      
032300           END-IF                                                         
032400           IF FELFLAGGA = NOO                                             
032500              PERFORM DA-LAES-ARTBAS-K6-K7                                
032600              IF ARTIKEL-FINNS = NOO                                      
032700                 MOVE YES           TO FELFLAGGA                          
032800                 MOVE '041'         TO RESP-IDMSG-ERROR                   
032900                 MOVE '041'         TO RESP-IDMSG-ERROR-LINE(INDX)        
033000*                PART MISSING ***                                         
033100                 MOVE 'IDARTNR'     TO RESP-IDELMT-ERROR                  
033200              END-IF                                                      
033300           END-IF                                                         
033400           IF FELFLAGGA = NOO                                             
033500              IF STANDARDPRIS-FINNS = YES                                 
033600                 IF ADRESS-FINNS = NOO                                    
033700                    MOVE YES        TO FELFLAGGA                          
033800                    MOVE '025'      TO RESP-IDMSG-ERROR                   
033900                    MOVE '025'      TO RESP-IDMSG-ERROR-LINE(INDX)        
034000*                   ADRESS MISSING  ***                                   
034100                    MOVE 'ADFLGEO'  TO RESP-IDELMT-ERROR                  
034200                 END-IF                                                   
034300              ELSE                                                        
034400                 MOVE YES           TO FELFLAGGA                          
034500                 MOVE '025'         TO RESP-IDMSG-ERROR                   
034600                 MOVE '025'         TO RESP-IDMSG-ERROR-LINE(INDX)        
034700*                STD-PRIS SAKNAS ***                                      
034800                 MOVE 'PRARTSTD'    TO RESP-IDELMT-ERROR                  
034900              END-IF                                                      
035000           END-IF                                                         
035100        END-IF                                                            
035200        IF REQU-KDINVPRIO (INDX) NOT = ALL '+'                            
035300           IF REQU-KDINVPRIO (INDX) NOT NUMERIC                           
035400              MOVE YES            TO FELFLAGGA                            
035500              MOVE '024'          TO RESP-IDMSG-ERROR                     
035600              MOVE '024'          TO RESP-IDMSG-ERROR-LINE(INDX)          
035700*             NOT NUMERIC ***                                             
035800              MOVE 'KDINVPRIO'    TO RESP-IDELMT-ERROR                    
035900           END-IF                                                         
036000           IF FELFLAGGA = NOO                                             
036100             IF REQU-KDINVPRIO (INDX) = 1 OR 2 OR 5 OR 6                  
036200                CONTINUE                                                  
036300             ELSE                                                         
036400                MOVE YES            TO FELFLAGGA                          
036500                MOVE '023'          TO RESP-IDMSG-ERROR                   
036600                MOVE '023'          TO RESP-IDMSG-ERROR-LINE(INDX)        
036700*               FEL VÄRDE   ***                                           
036800                MOVE 'KDINVPRIO'    TO RESP-IDELMT-ERROR                  
036900             END-IF                                                       
037000           END-IF                                                         
037100        END-IF                                                            
037200        ADD +1 TO INDX                                                    
037300     END-PERFORM                                                          
037400                                                                          
037500     IF FELFLAGGA = NOO                                                   
037600       PERFORM DB-KOLLA-DUBLETTER                                         
037700     END-IF                                                               
037800     .                                                                    
037900 DA-LAES-ARTBAS-K6-K7 SECTION.                                            
038000     MOVE 'DA-LAES-ARTBAS-K6-K7' TO CURR-SECTION                          
038100                                                                          
038200*  LÄS WDK7                                                               
038300*  ÄVEN WDK6 LÄSES HÄR FÖR ATT HÄMTA VISSA VÄRDEN SOM                     
038400*  BARA FINNS DÄR.                                                        
038500                                                                          
038600     MOVE NOO TO ARTIKEL-FINNS                                            
038700     MOVE NOO TO ADRESS-FINNS                                             
042600     MOVE REQU-IDDC-KEY     TO W-IDDC                                     
042610                               WS-IDDC                                    
038800                                                                          
038900     PERFORM IMS-01-GET-ARTIKEL-ROT                                       
039000     IF SEGMENT-FOUND AND ART-KDERS-UTG = +0                              
039100       MOVE ART-IDFKNGRP TO LAGRA-IDFKNGRP(INDX)                          
039200       MOVE ART-KDPRODSL TO LAGRA-KDPRODSL(INDX)                          
039300       PERFORM IMS-02-GNP-WDK6ARTIKEL                                     
039400       IF SEGMENT-FOUND                                                   
                IF CDC-SE                                                       
                 PERFORM DAA-LAES-ARTBAS-K6                                     
                ELSE                                                            
039500           PERFORM DAB-LAES-ARTBAS-K7                                     
                END-IF                                                          
039600       ELSE                                                               
039700          MOVE YES            TO FELFLAGGA                                
039800          MOVE '025'          TO RESP-IDMSG-ERROR                         
039900          MOVE '025'          TO RESP-IDMSG-ERROR-LINE(INDX)              
040000*         PART MISSING ***                                                
040100          MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR                        
040200       END-IF                                                             
040300     ELSE                                                                 
040400        MOVE YES            TO FELFLAGGA                                  
040500        MOVE '025'          TO RESP-IDMSG-ERROR                           
040600        MOVE '025'          TO RESP-IDMSG-ERROR-LINE(INDX)                
040700*       PART MISSING ***                                                  
040800        MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR                          
040900     END-IF                                                               
041000     .                                                                    
041100 DAA-LAES-ARTBAS-K6 SECTION.                                              
           MOVE YES TO ARTIKEL-FINNS                                            
041400     MOVE CLAG-KDVVKL       TO LAGRA-VVKL (INDX)                          
041500     MOVE CLAG-KDPSLLOC     TO LAGRA-KDPSLLOC(INDX)                       
041600     IF CLAG-PRARTSTD = 0                                                 
041700        MOVE YES            TO FELFLAGGA                                  
041800        MOVE '025'          TO RESP-IDMSG-ERROR                           
041900        MOVE '025'          TO RESP-IDMSG-ERROR-LINE(INDX)                
042000*       PARTNO MISSES STDPRICE ***                                        
042100        MOVE 'PRARTSTD'     TO RESP-IDELMT-ERROR                          
042200     ELSE                                                                 
042300        MOVE YES            TO STANDARDPRIS-FINNS                         
042400     END-IF                                                               
042500                                                                          
                                                                                
           MOVE CLAG-ADLAGOMR TO W-ADLAGOMR                                     
           MOVE CLAG-ADGANG   TO W-ADGANG                                       
           MOVE CLAG-ADPLATS  TO W-ADPLATS                                      
                                                                                
           IF W-ADARTADR > 0                                                    
            MOVE YES TO ADRESS-FINNS                                            
            MOVE CLAG-ADLAGOMR     TO LAGRA-OMR   (INDX)                        
            MOVE CLAG-ADGANG       TO LAGRA-GANG  (INDX)                        
            MOVE CLAG-ADPLATS      TO LAGRA-PLATS (INDX)                        
           ELSE                                                                 
            MOVE CLAG-ADLAGOMR-SVS TO W-ADLAGOMR                                
            MOVE CLAG-ADGANG-SVS   TO W-ADGANG                                  
            MOVE CLAG-ADPLATS-SVS  TO W-ADPLATS                                 
            IF W-ADARTADR > 0                                                   
             MOVE YES TO ADRESS-FINNS                                           
             MOVE CLAG-ADLAGOMR-SVS TO LAGRA-OMR   (INDX)                       
             MOVE CLAG-ADGANG-SVS   TO LAGRA-GANG  (INDX)                       
             MOVE CLAG-ADPLATS-SVS  TO LAGRA-PLATS (INDX)                       
            ELSE                                                                
             MOVE +1 TO CD-IX                                                   
             PERFORM UNTIL CD-IX > 4                                            
                IF CLAG-ADLAGOMR-CD(CD-IX) = ZERO                               
                  CONTINUE                                                      
                ELSE                                                            
                  MOVE CLAG-ADLAGOMR-CD(CD-IX)                                  
                                           TO LAGRA-OMR(INDX)                   
                                              W-ADLAGOMR                        
                  MOVE CLAG-ADGANG-CD(CD-IX)                                    
                                           TO LAGRA-GANG(INDX)                  
                                              W-ADGANG                          
                  MOVE CLAG-ADPLATS-CD(CD-IX)                                   
                                           TO LAGRA-PLATS(INDX)                 
                                              W-ADPLATS                         
                  MOVE +4 TO CD-IX                                              
               END-IF                                                           
               ADD +1 TO CD-IX                                                  
             END-PERFORM                                                        
            END-IF                                                              
           IF W-ADARTADR > 0                                                    
             MOVE YES TO ADRESS-FINNS                                           
           END-IF                                                               
           .                                                                    
041100 DAB-LAES-ARTBAS-K7 SECTION.                                              
041200     MOVE 'DAB-LAES-ARTBAS-K6-K7' TO CURR-SECTION                         
041300                                                                          
041400     MOVE CLAG-KDVVKL       TO LAGRA-VVKL (INDX)                          
041500     MOVE CLAG-KDPSLLOC     TO LAGRA-KDPSLLOC(INDX)                       
041600     IF CLAG-PRARTSTD = 0                                                 
041700        MOVE YES            TO FELFLAGGA                                  
041800        MOVE '025'          TO RESP-IDMSG-ERROR                           
041900        MOVE '025'          TO RESP-IDMSG-ERROR-LINE(INDX)                
042000*       PARTNO MISSES STDPRICE ***                                        
042100        MOVE 'PRARTSTD'     TO RESP-IDELMT-ERROR                          
042200     ELSE                                                                 
042300        MOVE YES            TO STANDARDPRIS-FINNS                         
042400     END-IF                                                               
042500                                                                          
042600     MOVE REQU-IDDC-KEY     TO W-IDDC                                     
042610                               WS-IDDC                                    
042700     PERFORM IMS-03-GET-WDK7ARTIKEL                                       
042800      IF SEGMENT-FOUND                                                    
042900        MOVE YES TO ARTIKEL-FINNS                                         
043000        MOVE SLAG-ADLAGOMR TO W-ADLAGOMR                                  
043100        MOVE SLAG-ADGANG     TO W-ADGANG                                  
043200        MOVE SLAG-ADPLATS    TO W-ADPLATS                                 
043301        IF (XDC-NON-VCC-OWNED OR LDC-CN OR NDC-NA) AND                    
043302           SLAG-PRAVCOST = +0                                             
043401          MOVE YES           TO FELFLAGGA                                 
043501          MOVE '260'         TO RESP-IDMSG-ERROR                          
043601          MOVE '260'         TO RESP-IDMSG-ERROR-LINE(INDX)               
043701***       ARTIKELN SAKNAR AVERAGE COST ***                                
043801          MOVE 'IDARTNR'     TO RESP-IDELMT-ERROR                         
043901        END-IF                                                            
044001        IF W-ADARTADR > 0                                                 
044101           MOVE YES             TO ADRESS-FINNS                           
044201           MOVE SLAG-ADLAGOMR   TO LAGRA-OMR   (INDX)                     
044301           MOVE SLAG-ADGANG     TO LAGRA-GANG  (INDX)                     
044401           MOVE SLAG-ADPLATS    TO LAGRA-PLATS (INDX)                     
044501        ELSE                                                              
044601          IF SLAG-KVLS      NOT = 0 OR                                    
044701             SLAG-KVEFRS    NOT = 0 OR                                    
044801             SLAG-KVAKS-SDC NOT = 0                                       
044901           MOVE YES             TO ADRESS-FINNS                           
045001           MOVE SLAG-ADLAGOMR   TO LAGRA-OMR   (INDX)                     
045101           MOVE SLAG-ADGANG     TO LAGRA-GANG  (INDX)                     
045201           MOVE SLAG-ADPLATS    TO LAGRA-PLATS (INDX)                     
045301          ELSE                                                            
045401           MOVE YES            TO FELFLAGGA                               
045501           MOVE '023'          TO RESP-IDMSG-ERROR                        
045601           MOVE '023'          TO RESP-IDMSG-ERROR-LINE(INDX)             
045701***        ARTIKELN SAKNAR ADRESS ***                                     
045801           MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR                       
045901          END-IF                                                          
046001        END-IF                                                            
046101      ELSE                                                                
046201        MOVE YES            TO FELFLAGGA                                  
046301        MOVE '025'          TO RESP-IDMSG-ERROR                           
046401        MOVE '025'          TO RESP-IDMSG-ERROR-LINE(INDX)                
046501*       PART MISSING ***                                                  
046601        MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR                          
046701      END-IF                                                              
046801     .                                                                    
046901 DB-KOLLA-DUBLETTER  SECTION.                                             
047001     MOVE 'DB-KOLLA-DUBLETTER'   TO CURR-SECTION                          
047101*                                                                         
047201*  KONTROLL OM SAMMA ARTIKELNUMMER HAR RAPPORTERATS                       
047301*  TVÅ GÅNGER PÅ SAMMA BILD                                               
047401                                                                          
047501     MOVE +1 TO INDX                                                      
047601     MOVE +1 TO KOLLIND                                                   
047701                                                                          
047801     PERFORM UNTIL KOLLIND > MAX-GRAENS                                   
047901        IF REQU-IDARTNR (KOLLIND) NOT = ALL '+'                           
048001           MOVE +1 TO INDX                                                
048101           PERFORM UNTIL INDX > KOLLIND                                   
048201                      OR INDX = KOLLIND                                   
048301                      OR FELFLAGGA = YES                                  
048401              IF REQU-IDARTNR (INDX) NOT = ALL '+'                        
048501                 IF REQU-IDARTNR (INDX) = REQU-IDARTNR (KOLLIND)          
048601                    MOVE YES        TO FELFLAGGA                          
048701                    MOVE '023'      TO RESP-IDMSG-ERROR                   
048801                    MOVE '023'      TO RESP-IDMSG-ERROR-LINE(INDX)        
048901*                   DUBLETT  ***                                          
049001                    MOVE 'IDARTNR'  TO RESP-IDELMT-ERROR                  
049101                 ELSE                                                     
049201                    ADD +1 TO INDX                                        
049301                 END-IF                                                   
049401              ELSE                                                        
049501                 ADD +1 TO INDX                                           
049601              END-IF                                                      
049701           END-PERFORM                                                    
049801        END-IF                                                            
049901        ADD +1 TO KOLLIND                                                 
050001     END-PERFORM                                                          
050101     .                                                                    
050201 E-UPPDATERA-BAS  SECTION.                                                
050301     MOVE 'E-UPPDATERA-BAS'   TO CURR-SECTION                             
050401*                                                                         
050501*  UPPDATERING AV WDH1                                                    
050601*  UPPDATERING GÖRS OM INVENTERING INTE REDAN FINNS ELLER                 
050701*  OM DEN NYA RAPPORTEN HAR PRIORITET                                     
050801*                                                                         
050901     MOVE +1 TO INDX                                                      
051001     MOVE REQU-IDDC-KEY  TO W-IDDC-WDH1                                   
051101                            W-IDDC-WDH1-MIN                               
051201                            W-IDDC-WDH1-MAX                               
051301     MOVE YES            TO UPPDATERING                                   
051401                                                                          
051501     PERFORM UNTIL INDX > MAX-GRAENS                                      
051601       MOVE NOO TO INVENT-FINNS (INDX)                                    
051701       IF REQU-IDARTNR (INDX) NOT = ALL '+'                               
051801         MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                            
051901                                                                          
052001*** BESTÄM OM AKTUELLT DC HAR NÅGON EJ AVSLUTAD INVENTERING               
052101         MOVE NOO    TO INV-DC-EJ-KLAR                                    
052201         PERFORM IMS-04-GHU-WDH101                                        
052301         IF SEGMENT-FOUND                                                 
052401           PERFORM IMS-05-GHNP-WDH111                                     
052501           PERFORM UNTIL SEGMENT-MISSING                                  
052601             IF INV-FLINVBEH = NOO                                        
052701               MOVE YES TO INV-DC-EJ-KLAR                                 
052801             END-IF                                                       
052901             PERFORM IMS-05-GHNP-WDH111                                   
053001           END-PERFORM                                                    
053101*** KVALIFICERA ROTEN IGEN                                                
053201           PERFORM IMS-04-GHU-WDH101                                      
053301         END-IF                                                           
053401                                                                          
053501         IF INV-DC-EJ-KLAR = YES                                          
053601           PERFORM IMS-05-GHNP-WDH111                                     
053701           PERFORM UNTIL SEGMENT-MISSING                                  
053801             MOVE YES TO INVENT-FINNS (INDX)                              
053901             IF REQU-KDINVPRIO (INDX) = ALL '+'                           
054001               MOVE NOO TO UPPDATERING                                    
054101               IF INV-FLINVSKR = 'J'                                      
054201                 MOVE YES           TO FELFLAGGA                          
054301                 MOVE '271'         TO RESP-IDMSG-ERROR                   
054401                 MOVE '271'         TO RESP-IDMSG-ERROR-LINE(INDX)        
054501*                INVENTERING FINNS REDAN, UTSKRIVEN *** )                 
054601               ELSE                                                       
054701                 MOVE YES           TO FELFLAGGA                          
054801                 MOVE '272'         TO RESP-IDMSG-ERROR                   
054901                 MOVE '272'         TO RESP-IDMSG-ERROR-LINE(INDX)        
055001*                INVENTERING FINNS REDAN, EJ UTSKRIVEN ***                
055101               END-IF                                                     
055201             ELSE                                                         
055301               IF REQU-KDINVPRIO (INDX) NOT = 5 AND 6                     
055401                 MOVE NOO TO UPPDATERING                                  
055501                   IF INV-FLINVSKR = 'J'                                  
055601                   MOVE YES         TO FELFLAGGA                          
055701                   MOVE '271'       TO RESP-IDMSG-ERROR                   
055801                   MOVE '271'       TO RESP-IDMSG-ERROR-LINE(INDX)        
055901*                  INVENTERING FINNS REDAN, UTSKRIVEN ***                 
056001                 ELSE                                                     
056101                   MOVE YES         TO FELFLAGGA                          
056201                   MOVE '272'       TO RESP-IDMSG-ERROR                   
056301                   MOVE '272'       TO RESP-IDMSG-ERROR-LINE(INDX)        
056401*                  INVENTERING FINNS REDAN, EJ UTSKRIVEN ***              
056501                 END-IF                                                   
056601               END-IF                                                     
056701             END-IF                                                       
056801             PERFORM IMS-05-GHNP-WDH111                                   
056901           END-PERFORM                                                    
057001         ELSE                                                             
057101           IF REQU-KDINVPRIO (INDX) NOT = ALL '+'                         
057201             IF REQU-KDINVPRIO (INDX) NOT = 1 AND 2                       
057301               MOVE NOO TO UPPDATERING                                    
057401               MOVE YES            TO FELFLAGGA                           
057501               MOVE '023'          TO RESP-IDMSG-ERROR                    
057601               MOVE '023'          TO RESP-IDMSG-ERROR-LINE(INDX)         
057701*              UPPLYSTA FÄLT FEL   ***                                    
057801               MOVE 'KDINVPRIO'    TO RESP-IDELMT-ERROR                   
057901             END-IF                                                       
058001           END-IF                                                         
058101         END-IF                                                           
058201       END-IF                                                             
058301       ADD +1 TO INDX                                                     
058401     END-PERFORM                                                          
058501                                                                          
058601     IF UPPDATERING = YES                                                 
058701       MOVE +1 TO INDX                                                    
058801       PERFORM UNTIL INDX > MAX-GRAENS                                    
058901         IF REQU-IDARTNR (INDX) NOT = ALL '+'                             
059001           MOVE REQU-IDARTNR (INDX) TO W-IDARTNR                          
059101           PERFORM EA-UPPDATERA-WDH1                                      
059201           MOVE SPACE TO RESP-RAD (INDX)                                  
059301         END-IF                                                           
059401         ADD +1 TO INDX                                                   
059501       END-PERFORM                                                        
059601       MOVE '001'             TO RESP-IDMSG-INFO                          
059701     END-IF                                                               
059801     .                                                                    
059901     EJECT                                                                
060001 EA-UPPDATERA-WDH1  SECTION.                                              
060101     MOVE 'EA-UPPDATERA-WDH1   '   TO CURR-SECTION                        
060201*                                                                         
060301*  UPPDATERING AV WDH1                                                    
060401*                                                                         
060501     IF INVENT-FINNS (INDX) = YES                                         
060601       MOVE REQU-IDARTNR(INDX)  TO W-IDARTNR                              
060701       PERFORM IMS-04-GHU-WDH101                                          
060801       IF SEGMENT-FOUND                                                   
060901         PERFORM IMS-05-GHNP-WDH111                                       
061001         PERFORM UNTIL SEGMENT-MISSING                                    
061101          IF INV-FLINVBEH = NOO                                           
061201           IF REQU-KDINVPRIO(INDX) NOT = ALL '+'                          
061301             IF INV-KDINVPRIO = 5                                         
061401               MOVE 1 TO INV-KDINVPRIO                                    
061501             ELSE                                                         
061601               IF INV-KDINVPRIO = 6                                       
061701                 MOVE 2 TO INV-KDINVPRIO                                  
061801               END-IF                                                     
061901             END-IF                                                       
062001           END-IF                                                         
062101           MOVE 'N'     TO INV-FLINVSKR                                   
062201           IF REQU-TEINVANM(INDX) NOT = ALL '+'                           
062301             MOVE REQU-TEINVANM(INDX) TO INV-TEINVANM                     
062401           END-IF                                                         
062501           PERFORM IMS-06-REPL-WDH111                                     
062601          END-IF                                                          
062701          PERFORM IMS-05-GHNP-WDH111                                      
062801         END-PERFORM                                                      
062901       END-IF                                                             
063001     ELSE                                                                 
063101       PERFORM IMS-04-GHU-WDH101                                          
063201       IF SEGMENT-MISSING                                                 
063301         MOVE REQU-IDARTNR(INDX)  TO INV-ART-IDARTNR                      
063401         PERFORM IMS-07-ISRT-WDH1-ROT                                     
063501       END-IF                                                             
063601                                                                          
063701       MOVE REQU-IDDC-KEY        TO INV-IDDC                              
063801       MOVE +1                   TO INV-KDINVKAT                          
063901       MOVE ZERO                 TO INV-KDINVKAT-OLD                      
064001       MOVE LAGRA-OMR (INDX)     TO INV-ADLAGOMR                          
064101       MOVE LAGRA-GANG (INDX)    TO INV-ADGANG                            
064201       MOVE LAGRA-PLATS (INDX)   TO INV-ADPLATS                           
064301       MOVE NOO                  TO INV-FLINVBEH                          
064401       MOVE NOO                  TO INV-FLINVSKR                          
064501       MOVE NOO                  TO INV-FLINV2B                           
064601       MOVE NOO                  TO INV-FLINV2C                           
064701       MOVE NOO                  TO INV-FLINV2D                           
064801       MOVE NOO                  TO INV-FLINV3E                           
064901       MOVE NOO                  TO INV-FLINV4N                           
065001       MOVE NOO                  TO INV-FLINV4P                           
065101       MOVE NOO                  TO INV-FLINV4R                           
065201       MOVE NOO                  TO INV-FLINV85                           
065301       MOVE SPACE                TO INV-FILLER1                           
065401                                    INV-FILLER2                           
065501       MOVE LAGRA-IDFKNGRP(INDX) TO INV-IDFKNGRP                          
065601       IF REQU-KDINVPRIO(INDX) NOT = ALL '+'                              
065701         MOVE REQU-KDINVPRIO(INDX) TO INV-KDINVPRIO                       
065801       ELSE                                                               
065901         MOVE +2                 TO INV-KDINVPRIO                         
066001       END-IF                                                             
066101       MOVE LAGRA-KDPRODSL(INDX) TO INV-KDPRODSL                          
066201       MOVE LAGRA-KDPSLLOC(INDX) TO INV-KDPSLLOC                          
066301       MOVE LAGRA-VVKL (INDX)    TO INV-KDVVKL                            
066401                                                                          
066501       MOVE ZERO                 TO INV-KVJUSTKV                          
066601       IF REQU-TEINVANM (INDX) NOT = ALL '+'                              
066701         MOVE REQU-TEINVANM (INDX) TO INV-TEINVANM                        
066801       ELSE                                                               
066901         MOVE SPACE              TO INV-TEINVANM                          
067001       END-IF                                                             
067101       MOVE ZERO                 TO INV-IDPRTOMG                          
067201                                    INV-IDLOPNR                           
067301                                    INV-KVAKS-OLD                         
067401                                    INV-KVEFRS-OLD                        
067501                                    INV-KVLS-OLD                          
067601       MOVE 20                   TO WS-AAR                                
067701       MOVE WS-AAR               TO WS-INV-SEKEL                          
067801       MOVE DAGENS-DATUM         TO WS-INV-AAMMDD                         
067901       MOVE WS-INV-DAREGDAT      TO INV-DAREGDAT-CRE                      
068001                                    INV-DAREGDAT                          
068101***    COMPUTE INV-DAREGDAT-SORT =                                        
068201***      99999999 - WS-INV-DAREGDAT                                       
068301       MOVE 99999999             TO INV-DAREGDAT-SORT                     
068401       MOVE ZERO                 TO INV-DAREGDAT-PR1                      
068501                                    INV-DAREGDAT-PR2                      
068601                                    INV-DAREGDAT-PR3                      
068701*      MOVE SPACE                TO INV-IDUSER-PR1                        
068801*                                   INV-IDUSER-PR2                        
068901*                                   INV-IDUSER-PR3                        
069001       MOVE DAGENS-DATUM         TO WS-TIAAMMDD                           
069101       MOVE 0                    TO WS-LOPNR                              
069201       MOVE WS-TIAAAAMMDDL       TO INV-TISEGKEY                          
069301                                                                          
069401       PERFORM IMS-08-ISRT-WDH111                                         
069501                                                                          
069601       PERFORM UNTIL SEGMENT-FOUND                                        
069701         IF SEGMENT-EXISTS OR INDEX-EXISTS                                
069801           ADD 1 TO INV-TISEGKEY                                          
069901           PERFORM IMS-08-ISRT-WDH111                                     
070001         END-IF                                                           
070101       END-PERFORM                                                        
070201                                                                          
070301*** INSERT PÅ WDH21 SEGMENTET ***                                         
070401       MOVE REQU-IDUSER TO INVL-IDUSER                                    
070501       MOVE '0'         TO INVL-KDSEGKEY                                  
070601       PERFORM IMS-INSERT-WDH121                                          
070701       MOVE SPACE       TO INVL-IDUSER                                    
070801       MOVE '1'         TO INVL-KDSEGKEY                                  
070901       PERFORM IMS-INSERT-WDH121                                          
071001       MOVE SPACE       TO INVL-IDUSER                                    
071101       MOVE '2'         TO INVL-KDSEGKEY                                  
071201       PERFORM IMS-INSERT-WDH121                                          
071301       MOVE SPACE       TO INVL-IDUSER                                    
071401       MOVE '3'         TO INVL-KDSEGKEY                                  
071501       PERFORM IMS-INSERT-WDH121                                          
071601                                                                          
071701**** SLUT PÅ INSERT PÅ WDH21 SEGMENT                                      
071801     END-IF                                                               
071901     .                                                                    
072001                                                                          
072101*    --- DISPATCHER SECTIONS                                              
072201 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
072301     MOVE 'S01-FETCH-REQUEST-ARGUMENT' TO CURR-SECTION                    
072401                                                                          
072501     MOVE 'GETARG'                        TO SUB-KDFUNC                   
072601     MOVE 'CARPARTS.LDC.INVENTORYREQUEST' TO SUB-ADDISPABS                
072701     MOVE LENGTH OF REQU-AREA             TO SUB-KVDLEN                   
072801                                                                          
072901     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
073001                                                                          
073101     IF SUB-KDRC > 0                                                      
073201       MOVE SUB-KDRC       TO KDRC-DISPLAY                                
073301       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
073401       DELIMITED BY SIZE INTO ERROR-TEXT                                  
073501       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
073601     END-IF                                                               
073701     .                                                                    
073801                                                                          
073901 S02-RETURN-RESPONSE SECTION.                                             
074001     MOVE 'S02-RETURN-RESPONSE       ' TO CURR-SECTION                    
074101                                                                          
074201     MOVE 'RETURN'                   TO SUB-KDFUNC                        
074301     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
074401                                                                          
074501     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
074601                                                                          
074701     IF SUB-KDRC > 0                                                      
074801       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
074901       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
075001       DELIMITED BY SIZE INTO ERROR-TEXT                                  
075101       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
075201     END-IF                                                               
075301     .                                                                    
075401 IMS-01-GET-ARTIKEL-ROT     SECTION.                                      
075501     MOVE 'IMS-01' TO CURR-IMS-SECTION                                    
075601                                                                          
075701     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
075801            DELIMITED BY SIZE INTO SSA1                                   
075901     MOVE '  GE'                TO GOOD-STATUSCODES                       
076001     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
076101     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
076201     PERFORM IMS-STATUS-CHECK                                             
076301     .                                                                    
076401 IMS-02-GNP-WDK6ARTIKEL SECTION.                                          
076501     MOVE 'IMS-02' TO CURR-IMS-SECTION                                    
076601                                                                          
076701     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
076801            DELIMITED BY SIZE INTO SSA1                                   
076901     MOVE '  GE'                TO GOOD-STATUSCODES                       
077001     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
077101     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
077201     PERFORM IMS-STATUS-CHECK                                             
077301     .                                                                    
077401 IMS-03-GET-WDK7ARTIKEL     SECTION.                                      
077501     MOVE 'IMS-03' TO CURR-IMS-SECTION                                    
077601                                                                          
077701     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
077801            DELIMITED BY SIZE INTO SSA1                                   
077901     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
078001            DELIMITED BY SIZE INTO SSA2                                   
078101     MOVE '  GE'                TO GOOD-STATUSCODES                       
078201     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
078301     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
078401     PERFORM IMS-STATUS-CHECK                                             
078501     .                                                                    
078601 IMS-04-GHU-WDH101 SECTION.                                               
078701     MOVE 'IMS-04' TO CURR-IMS-SECTION                                    
078801                                                                          
078901     STRING                                                               
079001     'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                                
079101     DELIMITED BY SIZE INTO SSA1                                          
079201     MOVE '  GE'                TO GOOD-STATUSCODES                       
079301     CALL CBLTDLI USING GHU INV-PCB INV-ART-WDH101 SSA1                   
079401     MOVE INV-STATUS-CODE TO STATUS-WS                                    
079501     PERFORM IMS-STATUS-CHECK                                             
079601     .                                                                    
079701 IMS-05-GHNP-WDH111   SECTION.                                            
079801     MOVE 'IMS-05' TO CURR-IMS-SECTION                                    
079901                                                                          
080001     STRING                                                               
080101     'WDH111  (WDH111KY>=' W-WDH111KY-MIN-X                               
080201             '&WDH111KY<=' W-WDH111KY-MAX-X ')'                           
080301     DELIMITED BY SIZE INTO SSA1                                          
080401     MOVE '  GE'                TO GOOD-STATUSCODES                       
080501     CALL CBLTDLI USING GHNP INV-PCB INV-WDH111 SSA1                      
080601     MOVE INV-STATUS-CODE TO STATUS-WS                                    
080701     PERFORM IMS-STATUS-CHECK                                             
080801     .                                                                    
080901 IMS-06-REPL-WDH111       SECTION.                                        
081001     MOVE 'IMS-06' TO CURR-IMS-SECTION                                    
081101                                                                          
081201     MOVE '  '            TO GOOD-STATUSCODES                             
081301     CALL CBLTDLI USING REPL INV-PCB INV-WDH111                           
081401     MOVE INV-STATUS-CODE TO STATUS-WS                                    
081501     PERFORM IMS-STATUS-CHECK                                             
081601     .                                                                    
081701 IMS-07-ISRT-WDH1-ROT  SECTION.                                           
081801     MOVE 'IMS-07' TO CURR-IMS-SECTION                                    
081901                                                                          
082001     MOVE 'WDH101 '       TO SSA1                                         
082101     MOVE '  '            TO GOOD-STATUSCODES                             
082201     CALL CBLTDLI USING ISRT INV-PCB INV-ART-WDH101 SSA1                  
082301     MOVE INV-STATUS-CODE TO STATUS-WS                                    
082401     PERFORM IMS-STATUS-CHECK                                             
082501     .                                                                    
082601 IMS-08-ISRT-WDH111 SECTION.                                              
082701     MOVE 'IMS-08' TO CURR-IMS-SECTION                                    
082801                                                                          
082901     MOVE 'WDH111 ' TO SSA1                                               
083001     MOVE '  IINI'          TO GOOD-STATUSCODES                           
083101     CALL CBLTDLI USING ISRT INV-PCB INV-WDH111 SSA1                      
083201     MOVE INV-STATUS-CODE TO STATUS-WS                                    
083301     PERFORM IMS-STATUS-CHECK                                             
083401     .                                                                    
083501 IMS-INSERT-WDH121      SECTION.                                          
083601                                                                          
083701     MOVE 'WDH121 ' TO SSA1                                               
083801     MOVE '  II' TO GOOD-STATUSCODES                                      
083901     CALL CBLTDLI USING ISRT INV-PCB INVL-WDH121   SSA1                   
084001     MOVE INV-STATUS-CODE TO STATUS-WS                                    
084101     PERFORM IMS-STATUS-CHECK                                             
084201     .                                                                    
084301     EJECT                                                                
084401 IMS-STATUS-CHECK   SECTION.                                              
084501                                                                          
084601     SET STATUS-IX TO 1                                                   
084701     SEARCH GOOD-STATUS                                                   
084801       AT END                                                             
084901         CALL FELLOG                                                      
085001     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
085101       CONTINUE                                                           
085201     END-SEARCH                                                           
086001     .                                                                    
