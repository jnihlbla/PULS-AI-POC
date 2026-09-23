000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2014200.                                                
000400 AUTHOR.         GUNNEL ERIKSSON                                          
000500 DATE-WRITTEN.   JANUARI  1989.                                           
000600*    REMARKS                                                              
000800*                                                                         
000900***************************************************************           
001000*                                                             *           
001100*    FUNKTION.                                                *           
001200*                                                             *           
001300*        PROGRAMMET ÄR EN KÖ-BILD                             *           
001400*=>// ANVÄNDS EJ, ANTAL ARTIKLAR PÅ SEQ-INDX BAS WDD2B1 VISAS,*           
001410*=> NYTT,LÄSER NUMERA PÅ SEQ-INDX BAS WDD2E1 MED FLPISK J,N   *           
001500*        MAX 200 LÄSNINGAR PER GÅNG.                          *           
001600***************************************************************           
001610****************************************************************          
001620* 2006-10-20                                                              
001630* VISA FÖRST ALLA ARTIKLAR MED FL PISK = J MED SEQ-E NYCKEL               
001640* SEN ALLA ARTIKLAR MED FL PISK = N                                       
001650****************************************************************          
001700*                                                                         
001800*        TRANSAKTION: W2T142                                              
001900*        MID:         W2I14201                                            
002000*        MOD:         W2O14201                                            
002100*        FORMAT:      W2F142FII0                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002710                                                                          
002800*    -- CHECKED BY WY2000                                                 
002810 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W9012100'.            
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003010 77  WS-LAES-FLPISK              PIC X(1) VALUE SPACE.                    
003100 77  FIX-SW-IDINK-NOLL           PIC X       VALUE 'N'.                   
003200 77  SPRAK-IX                    PIC S9(2)   VALUE +0   COMP SYNC.        
003300 77  RAD-INDX                    PIC S9(9)   VALUE ZERO COMP-3.           
003400 77  MAX-RAD                     PIC S9(2)   VALUE +12  COMP-3.           
003500 77  RAKNARE-ART-WDD2B1          PIC  9(3)   VALUE ZERO.                  
003600 77  MAX-LAS-WDD2B1-INDX         PIC S9(5)   VALUE ZERO COMP SYNC.        
003700 77  SPAR-ANT-ART-WDD2B1         PIC  9(5)   VALUE ZERO.                  
003800 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +1143  COMP SYNC.        
003801 77  TEST-IDINK                  PIC  9(3)   VALUE ZERO.                  
003810                                                                          
003830 01  WS-TIAAVVD                  PIC 9(5).                                
003831 01  FILLER          REDEFINES   WS-TIAAVVD.                              
003840     03  WS-TIAAVV.                                                       
003841         05  FILLER              PIC 9(4).                                
003850     03  FILLER                  PIC 9.                                   
003900     EJECT                                                                
004000     SKIP3                                                                
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
004230     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
004240     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
004250     03  W005INIT                PIC X(8)   VALUE 'W005INIT'.             
004300                                                                          
004400     SKIP3                                                                
004410*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
004420*                                                                         
004430 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
004440     SKIP3                                                                
004450*01 -COPY WMSGINIT                                                        
004460     EJECT                                                                
004470 01  FILLER                      PIC X(16) VALUE 'SPAR-AREA'.             
004471 01  SPAR-AREA.                                                           
004480     03  SPAR-IDTRANS            PIC X(4)    VALUE SPACE.                 
004490     03  SPAR-LAES-NYCKEL        PIC X       VALUE SPACE.                 
004491     03  SPAR-IDINK-ENTER        PIC X(4)    VALUE SPACE.                 
004492     03  SPAR-IDINK-NEXT         PIC X(4)    VALUE SPACE.                 
004493                                                                          
004494     EJECT                                                                
004500*-------------------PARAMETRAR TILL DATUM-OMVANDLING                      
004600 01  WS-AAVV.                                                             
004700     03  WS-AA                  PIC 9(2).                                 
004800     03  WS-VV                  PIC 9(2).                                 
004900     EJECT                                                                
005000*01      -COPY WDATAREA                                                   
005200     EJECT                                                                
005300 01  WS-ARTI01-SEGMENT-SLUT      PIC X(9)  VALUE ALL '-'.                 
005400                                                                          
005500 01  WS-ARTI01-BAS-RAKNADE       PIC X(9).                                
005600     88 WS-ARTI01-SLUTRAKNADE      VALUE '---------'.                     
005700     SKIP3                                                                
005800 01  WS-IN-FAELT-OK              PIC X(1).                                
005900     88 IN-FAELT-OK                           VALUE 'J'.                  
006000     SKIP3                                                                
006100 01  NYCKLAR-TILL-DLI.                                                    
006910     03  W-WDD2B1KY-MIN.                                                  
006920       05  W-IDANSK-MIN        PIC S9(3) COMP-3 VALUE ZERO.               
006930       05  W-IDPROJ-MIN        PIC  X(4)        VALUE SPACE.              
006940       05  W-FLPISK-MIN        PIC  X(1)        VALUE SPACE.              
006950       05  W-DAFINLEV-MIN      PIC  9(8)        VALUE ZERO.               
006960       05  W-IDAO-MIN          PIC  X(10)       VALUE SPACE.              
006970       05  W-IDARTNR-MIN       PIC S9(9) COMP-3 VALUE ZERO.               
006980                                                                          
007000     03  W-WDD2B1KY-MAX.                                                  
007100       05  W-IDANSK-MAX        PIC S9(3) COMP-3 VALUE       ZERO.         
007200       05  W-IDPROJ-MAX        PIC  X(4)        VALUE       SPACE.        
007300       05  FILLER              PIC  X(1)        VALUE HIGH-VALUE.         
007400*      05  FILLER              PIC S9(7) COMP-3 VALUE +9999999.           
007410       05  FILLER              PIC  9(8)        VALUE 99999999.           
007500       05  FILLER              PIC  X(10)       VALUE HIGH-VALUE.         
007600       05  FILLER              PIC S9(9) COMP-3 VALUE +999999999.         
007700                                                                          
007710     03  W-WDD2E1KY-MIN.                                                  
007711       05  W-FLPISK-MIN-E      PIC  X(1)        VALUE LOW-VALUE.          
007720       05  W-IDANSK-MIN-E      PIC S9(3) COMP-3 VALUE ZERO.               
007721       05  W-DAFINLEV-MIN-E    PIC  9(8)        VALUE ZERO.               
007722       05  W-IDINK-MIN-E       PIC  X(4)        VALUE LOW-VALUE.          
007730       05  W-IDPROJ-MIN-E      PIC  X(4)        VALUE LOW-VALUE.          
007770       05  W-IDARTNR-MIN-E     PIC S9(9) COMP-3 VALUE ZERO.               
007780                                                                          
007790     03  W-WDD2E1KY-MAX.                                                  
007791       05  W-FLPISK-MAX-E      PIC  X(1)        VALUE SPACE.              
007792       05  W-IDANSK-MAX-E      PIC S9(3) COMP-3 VALUE -999.               
007793       05  W-DAFINLEV-MAX-E    PIC  9(8)        VALUE 99999999.           
007794       05  W-IDINK-MAX-E       PIC  X(4)        VALUE HIGH-VALUE.         
007795       05  W-IDPROJ-MAX-E      PIC  X(4)        VALUE HIGH-VALUE.         
007796       05  W-IDARTNR-MAX-E     PIC S9(9) COMP-3 VALUE -999999999.         
007810                                                                          
007811     03  W-WDD2E1KY-GU.                                                   
007812       05  W-FLPISK-GU         PIC  X(1)        VALUE SPACE.              
007813       05  W-IDANSK-GU         PIC S9(3) COMP-3 VALUE -999.               
007814       05  W-DAFINLEV-GU       PIC  9(8)        VALUE 99999999.           
007815       05  W-IDINK-GU          PIC  X(4)        VALUE HIGH-VALUE.         
007816       05  W-IDPROJ-GU         PIC  X(4)        VALUE HIGH-VALUE.         
007817       05  W-IDARTNR-GU        PIC S9(9) COMP-3 VALUE -999999999.         
007818                                                                          
007819                                                                          
007820     03  W-IDARTNR-X.                                                     
007900        05  W-IDARTNR         PIC S9(9)     COMP-3.                       
008000                                                                          
008100     03  W-KDSEGKEY-X.                                                    
008200        05  W-KDSEGKEY         PIC X(1)      VALUE '1'.                   
008300                                                                          
008400     03  W-KDANSKQ-X.                                                     
008500        05  W-KDANSKQ          PIC X(1)      VALUE '2'.                   
008600                                                                          
008700 01  ARBETS-FAELT-IN.                                                     
008800     03   WS-IDANSK-MIN         PIC X(3)      VALUE SPACE.                
008900     03   WS-IDANSK-MAX         PIC X(3)      VALUE SPACE.                
009000     03   WS-IDPROJ             PIC X(4)      VALUE SPACE.                
009100     EJECT                                                                
009200 01  FEL-MEDDELANDE.                                                      
009300   03  FEL1.                                                              
009400     05 FILLER                   PIC X(40)                                
009500          VALUE 'NYCKLAR FEL'.                                            
009600     05 FILLER                   PIC X(40)                                
009700          VALUE 'WRONG KEYS'.                                             
009800   03  FILLER REDEFINES FEL1.                                             
009900     05  FEL-1                   PIC X(40)   OCCURS 2.                    
010000                                                                          
010100   03  FEL2.                                                              
010200     05 FILLER                   PIC X(40)                                
010300          VALUE ' ARTIKEL SAKNAS'.                                        
010400     05 FILLER                   PIC X(40)                                
010500          VALUE ' PARTNO MISSING'.                                        
010600   03  FILLER REDEFINES FEL2.                                             
010700     05  FEL-2                   PIC X(40)   OCCURS 2.                    
010800     SKIP3                                                                
010900   03  FEL3.                                                              
011000     05 FILLER                   PIC X(40)                                
011100          VALUE 'UTRÄKNING ANTAL ARTIKLAR FELAKTIG'.                      
011200     05 FILLER                   PIC X(40)                                
011300          VALUE 'QUANT. PART-NUMBER WRONG'.                               
011400   03  FILLER REDEFINES FEL3.                                             
011500     05  FEL-3                   PIC X(40)   OCCURS 2.                    
011600     SKIP3                                                                
011700 01    MEDDELANDE.                                                        
011800   03  MED1.                                                              
011900     05 FILLER                   PIC X(40)                                
012000          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
012100     05 FILLER                   PIC X(40)                                
012200          VALUE 'THIS IS THE FIRST PAGE'.                                 
012300   03  FILLER REDEFINES MED1.                                             
012400     05  MED-1                   PIC X(40)   OCCURS 2.                    
012500                                                                          
012600   03  MED2.                                                              
012700     05 FILLER                   PIC X(40)                                
012800          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
012900     05 FILLER                   PIC X(40)                                
013000          VALUE 'PRESS PF8 FOR MORE LINES'.                               
013100   03  FILLER REDEFINES MED2.                                             
013200     05  MED-2                   PIC X(40)   OCCURS 2.                    
013300                                                                          
013400     EJECT                                                                
013500******************************************************************        
013600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013700******************************************************************        
013800     SKIP3                                                                
013900*01  MID -COPY W2I14201                                                   
014100     EJECT                                                                
014200*01  -COPY WMSGAREA                                                       
014400     EJECT                                                                
014500*03    MOD -COPY W2O14201           -RED MSG-AREA.                        
014700     EJECT                                                                
014800*01  -COPY WMFSAREA                                                       
015000     EJECT                                                                
015100******************************************************************        
015200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015300******************************************************************        
015400 01  IMS-WS.                                                              
015500   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
015600     SKIP3                                                                
015700*                        **** STATUS-KOD FRÅN IMS                         
015800   03  STATUS-WS                 PIC XX.                                  
015900     88  SEGMENT-FINNS                       VALUE '  '.                  
016000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016100     88  BASEN-SLUT                          VALUE 'GB'.                  
016200     SKIP3                                                                
016300   03  GODK-STATUSKODER.                                                  
016400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016500     SKIP3                                                                
016600   03  SSA1                      PIC X(196)  VALUE SPACE.                 
016700   03  SSA2                      PIC X(64)   VALUE SPACE.                 
016800     EJECT                                                                
016900******************************************************************        
017000*                            IMS FUNKTIONSKODER                           
017100******************************************************************        
017200*01    -COPY W0003                                                        
017400     EJECT                                                                
017500*                            DLI INPUT-OUTPUT AREA                        
017600 01  DLI-IO-AREA1.                                                        
017700   03  IO-AREA1                  PIC X(100)  VALUE SPACE.                 
017800     SKIP3                                                                
017900*  03  ARTI01    -COPY WDD2B1  -PRE ARTI01-  -RED IO-AREA1.               
018100     EJECT                                                                
018200 01  DLI-IO-AREA2.                                                        
018300   03  IO-AREA2                  PIC X(700)  VALUE SPACE.                 
018400     SKIP3                                                                
018500*  03  ARTG01    -COPY WDD201  -PRE ARTG01-  -RED IO-AREA2.               
018700     EJECT                                                                
018800 01  DLI-IO-AREA3.                                                        
018900   03  IO-AREA3                  PIC X(200)  VALUE SPACE.                 
019000     SKIP3                                                                
019100*  03  ARTC01    -COPY WDK601  -RED IO-AREA3.                             
019300     EJECT                                                                
019301 01  DLI-IO-AREA4.                                                        
019302   03  IO-AREA4                  PIC X(900)  VALUE SPACE.                 
019303     SKIP3                                                                
019310*  03  ARTC11    -COPY WDK611  -RED IO-AREA4.                             
019320 01  FILLER                     PIC X(16) VALUE 'WDD2E-AREA'.             
019330 01  DLI-IO-WDD2SEQ.                                                      
019340*    03 -COPY WDD2E1                                                      
019600     EJECT                                                                
019700 LINKAGE SECTION.                                                         
019800*01  -COPY W0009     -PRE MSG-                                            
020000     EJECT                                                                
020100*01  -COPY W0008     -PRE WDP7-                                           
020300     05  FILLER        PIC XX.                                            
020400     EJECT                                                                
020500*01  -COPY W0008     -PRE ARTG-                                           
020700     05  FILLER        PIC XX.                                            
020800     EJECT                                                                
020900*01  -COPY W0008     -PRE ARTC-                                           
021100     05  FILLER        PIC XX.                                            
021110*01  -COPY W0008     -PRE WDD2E-                                          
021120     05  FILLER        PIC XX.                                            
021200     EJECT                                                                
021300 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
021310                           ARTG-PCB ARTC-PCB                              
021320                           WDD2E-PCB.                                     
021400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
021410                           ARTG-PCB ARTC-PCB                              
021420                           WDD2E-PCB.                                     
021500                                                                          
021600     PERFORM IMS-GET-MSG                                                  
021700     IF SEGMENT-FINNS                                                     
021800          MOVE MFS-FORMATETS-ATTR TO MOD-ANT-ART-WDD2B1-ATTR              
021900          PERFORM               A-INIT                                    
022000          PERFORM               B-FLYTTA-TILL-WS                          
022100          PERFORM               C-KOLLA-INDATA                            
022200          IF IN-FAELT-OK                                                  
022300                IF MFS-IDPFK = '7'                                        
022400                   MOVE ZERO TO W-IDARTNR-MIN                             
022410                                W-IDARTNR-MIN-E                           
022500                                W-IDARTNR                                 
022600                              MID-ANT-ART-WDD2B1                          
022700                              SPAR-ANT-ART-WDD2B1                         
022800                              MID-IDARTNR-WDD2B1                          
022900                              WS-ARTI01-BAS-RAKNADE                       
023000                   MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                  
023100                ELSE                                                      
023200                  IF MFS-IDPFK = '8'                                      
023300                     PERFORM  D-FIXA-PF8-NYCKEL                           
023400                  ELSE                                                    
023500                     PERFORM  E-FIXA-ENTER-NYCKEL                         
023600                  END-IF                                                  
023700                END-IF                                                    
023800                PERFORM F-LAES-VISA-INFO                                  
023900          ELSE                                                            
024000               MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                       
024100               MOVE +1 TO RAD-INDX                                        
024200               PERFORM MFS-RENSA-MOD-RAD                                  
024300          END-IF                                                          
024400          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
024500          PERFORM IMS-INSERT-MSG                                          
024600     END-IF                                                               
024610**   CALL FELLOG                                                          
024700     MOVE ZERO TO RETURN-CODE                                             
024800     GOBACK                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 A-INIT SECTION.                                                          
025200     IF MSG-DUBBLA-TRANSKODER                                             
025300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I14201                 
025400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
025500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025600       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
025700       MOVE MSG-IDPFK TO MFS-IDPFK                                        
025800     ELSE                                                                 
025900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I14201                  
026000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026200       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
026300     END-IF                                                               
026400                                                                          
026500     IF MFS-IDTRANS NOT = '2142'                                          
026600        MOVE SPACE TO MFS-KDTRTYP                                         
026700        MOVE '7' TO MFS-IDPFK                                             
026710        MOVE 'J' TO WS-LAES-FLPISK                                        
026720                    W-FLPISK-MIN-E                                        
026721                    W-FLPISK-MAX-E                                        
026800        IF MFS-IDTRANS = '2141'                                           
026900           CONTINUE                                                       
027000        ELSE                                                              
027100           PERFORM MFS-RENSA-MID-IN                                       
027200        END-IF                                                            
027201                                                                          
027210     ELSE                                                                 
027220       IF MFS-IDTRANS = '2142'                                            
027222         MOVE ALL '+'           TO MSGI-WMSGINIT                          
027223         MOVE '001'             TO MSGI-KDCALL                            
027224         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
027225         MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                            
027226         MOVE '2142'            TO MSGI-IDTRANS                           
027227         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
027228         MOVE MSGI-SPAR-AREA    TO SPAR-AREA                              
027229         MOVE SPAR-LAES-NYCKEL  TO WS-LAES-FLPISK                         
027230         MOVE WS-LAES-FLPISK    TO W-FLPISK-MIN-E                         
027231                                   W-FLPISK-MAX-E                         
027232                                                                          
027240       END-IF                                                             
027300     END-IF                                                               
027400                                                                          
027500     IF ENGLISH-TEXT                                                      
027600           MOVE +2                       TO SPRAK-IX                      
027700     ELSE                                                                 
027800           MOVE +1                       TO SPRAK-IX                      
027900     END-IF                                                               
028000                                                                          
028100     MOVE LOW-VALUE TO MSG-AREA                                           
028200     MOVE 'W2O14201' TO MFS-IDMOD                                         
028300     MOVE '2142' TO MOD-IDTRANS                                           
028400     PERFORM MFS-RENSA-MOD-IN                                             
028500                                                                          
028600     MOVE ZERO TO MAX-LAS-WDD2B1-INDX                                     
028700     .                                                                    
028800     EJECT                                                                
028900 B-FLYTTA-TILL-WS  SECTION.                                               
029000                                                                          
029100     IF MID-IDANSK-FOM-IN = ALL '+'                                       
029200        MOVE MID-IDANSK-FOM-UT TO WS-IDANSK-MIN                           
029300     ELSE                                                                 
029400        MOVE SPACE TO MFS-KDTRTYP                                         
029500        MOVE '7'             TO MFS-IDPFK                                 
029600        MOVE MID-IDANSK-FOM-IN TO WS-IDANSK-MIN                           
029700     END-IF                                                               
029800     INSPECT WS-IDANSK-MIN REPLACING LEADING SPACE BY ZERO                
029900*                                                                         
030000     IF MID-IDANSK-TOM-IN = ALL '+'                                       
030100        IF MID-IDANSK-FOM-IN  =  ALL '+'                                  
030200           MOVE MID-IDANSK-TOM-UT TO WS-IDANSK-MAX                        
030210*                                    W-IDANSK-MIN-E                       
030300        ELSE                                                              
030400           MOVE MID-IDANSK-FOM-IN TO WS-IDANSK-MAX                        
030500        END-IF                                                            
030600     ELSE                                                                 
030700        MOVE SPACE TO MFS-KDTRTYP                                         
030800        MOVE '7'             TO MFS-IDPFK                                 
030900        MOVE MID-IDANSK-TOM-IN TO WS-IDANSK-MAX                           
031000     END-IF                                                               
031100     INSPECT WS-IDANSK-MAX REPLACING LEADING SPACE BY ZERO                
031200                                                                          
031300     IF MID-IDPROJ-IN = ALL '+'                                           
031400        MOVE MID-IDPROJ-UT TO WS-IDPROJ                                   
031500     ELSE                                                                 
031600        MOVE SPACE TO MFS-KDTRTYP                                         
031700        MOVE '7'             TO MFS-IDPFK                                 
031800        MOVE MID-IDPROJ-IN  TO WS-IDPROJ                                  
031900     END-IF                                                               
031901*    MOVE WS-IDPROJ TO W-IDPROJ-MIN-E                                     
031902                                                                          
031910     IF  MFS-IDPFK = '7'                                                  
031920       MOVE 'J' TO W-FLPISK-MIN-E                                         
031921                   W-FLPISK-MAX-E                                         
031922                   WS-LAES-FLPISK                                         
031940     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 C-KOLLA-INDATA     SECTION.                                              
032300                                                                          
032400     MOVE JA TO WS-IN-FAELT-OK                                            
032500     IF WS-IDANSK-MIN NUMERIC AND WS-IDANSK-MAX NUMERIC                   
032700       IF WS-IDANSK-MIN = ZERO AND WS-IDANSK-MAX = ZERO                   
032800         CONTINUE                                                         
032900       ELSE                                                               
033000         IF  WS-IDANSK-MIN NOT = ZERO                                     
033100         AND WS-IDANSK-MAX = ZERO                                         
033200           MOVE WS-IDANSK-MIN  TO  WS-IDANSK-MAX                          
033300         ELSE                                                             
033400           IF WS-IDANSK-MIN > WS-IDANSK-MAX                               
033500             MOVE NEJ TO WS-IN-FAELT-OK                                   
033600           END-IF                                                         
033700         END-IF                                                           
033800       END-IF                                                             
033900     ELSE                                                                 
034000         MOVE NEJ TO WS-IN-FAELT-OK                                       
034100     END-IF                                                               
034200     MOVE WS-IDANSK-MIN  TO MOD-IDANSK-FOM-UT                             
034300     INSPECT MOD-IDANSK-FOM-UT  REPLACING LEADING ZERO BY SPACE           
034400     MOVE WS-IDANSK-MAX  TO MOD-IDANSK-TOM-UT                             
034500     INSPECT MOD-IDANSK-TOM-UT  REPLACING LEADING ZERO BY SPACE           
034600     MOVE WS-IDPROJ      TO MOD-IDPROJ-UT                                 
034700                                                                          
034800     IF IN-FAELT-OK                                                       
034900       MOVE WS-IDANSK-MIN TO W-IDANSK-MIN                                 
034910                             W-IDANSK-MIN-E                               
034920                                                                          
035000       MOVE WS-IDANSK-MAX TO W-IDANSK-MAX                                 
035010                             W-IDANSK-MAX-E                               
035020                                                                          
035100       IF WS-IDPROJ = SPACE                                               
035200*----------------------INGET PROJ VALT----------------------              
035300          MOVE LOW-VALUE TO W-IDPROJ-MIN                                  
035310                            W-IDPROJ-MIN-E                                
035320                                                                          
035400          MOVE HIGH-VALUE TO W-IDPROJ-MAX                                 
035410                             W-IDPROJ-MAX-E                               
035420                                                                          
035500       ELSE                                                               
035600          MOVE WS-IDPROJ     TO W-IDPROJ-MIN    W-IDPROJ-MAX              
035610                                W-IDPROJ-MIN-E  W-IDPROJ-MAX-E            
035620                                                                          
035700       END-IF                                                             
035800     END-IF                                                               
035900                                                                          
036000     INSPECT MID-ANT-ART-WDD2B1 REPLACING LEADING SPACE BY ZERO           
036100     IF MID-ANT-ART-WDD2B1 NUMERIC                                        
036200        MOVE MID-ANT-ART-WDD2B1 TO SPAR-ANT-ART-WDD2B1                    
036300     ELSE                                                                 
036400        MOVE 99999 TO MID-ANT-ART-WDD2B1                                  
036500        MOVE WS-ARTI01-SEGMENT-SLUT TO MID-IDARTNR-WDD2B1                 
036600                                        WS-ARTI01-BAS-RAKNADE             
036700        MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                              
036800     END-IF                                                               
036900     MOVE    MID-ANT-ART-WDD2B1 TO MOD-ANT-ART-WDD2B1                     
037000     INSPECT MOD-ANT-ART-WDD2B1 REPLACING LEADING ZERO BY SPACE           
037100                                                                          
037200     INSPECT MID-IDARTNR-WDD2B1 REPLACING LEADING SPACE BY ZERO           
037300     IF MID-IDARTNR-WDD2B1 NUMERIC                                        
037400        CONTINUE                                                          
037500     ELSE                                                                 
037600        IF MID-IDARTNR-WDD2B1 = ALL '-'                                   
037700           CONTINUE                                                       
037800        ELSE                                                              
037900          MOVE 99999 TO MID-ANT-ART-WDD2B1                                
038000          MOVE WS-ARTI01-SEGMENT-SLUT TO MID-IDARTNR-WDD2B1               
038100                                        WS-ARTI01-BAS-RAKNADE             
038200          MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                            
038300        END-IF                                                            
038400     END-IF                                                               
038500     MOVE    MID-IDARTNR-WDD2B1 TO MOD-IDARTNR-WDD2B1                     
038600                                   WS-ARTI01-BAS-RAKNADE                  
038700     .                                                                    
038800     EJECT                                                                
038900 D-FIXA-PF8-NYCKEL   SECTION.                                             
039000                                                                          
039100     IF   MID-IDPROJ-PF8 = LOW-VALUE                                      
039200*------------LÅT W-IDPROJ-MIN HA VÄRDET FRÅN C-KOLLA-INDATA SECTIO        
039300        CONTINUE                                                          
039400     ELSE                                                                 
039500        MOVE MID-IDPROJ-PF8 TO W-IDPROJ-GU                                
039600     END-IF                                                               
039700     MOVE MID-FLPISK-PF8 TO W-FLPISK-GU                                   
039710                            W-FLPISK-MIN-E                                
039711                            W-FLPISK-MAX-E                                
039720                                                                          
039800*    MOVE MID-IDAO-PF8   TO W-IDAO-MIN                                    
039810*                           W-IDAO-MIN-GU                                 
039900                                                                          
040000     INSPECT MID-IDANSK-PF8 REPLACING LEADING SPACE BY ZERO               
040100     IF MID-IDANSK-PF8 NUMERIC                                            
040200*       IF   MID-IDANSK-PF8 = ZERO                                        
040300*         MOVE '7'  TO MFS-IDPFK                                          
040400*         MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                           
040500*       ELSE                                                              
040600          MOVE MID-IDANSK-PF8 TO W-IDANSK-GU                              
040700*       END-IF                                                            
040800     ELSE                                                                 
040900        MOVE ZERO TO  W-IDANSK-GU                                         
041000        MOVE '7'  TO MFS-IDPFK                                            
041100        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
041200     END-IF                                                               
041300                                                                          
041400     INSPECT MID-TIFINLEV-PF8 REPLACING LEADING SPACE BY ZERO             
041500     IF MID-TIFINLEV-PF8 NUMERIC                                          
041610        MOVE MID-TIFINLEV-PF8 TO DAT-I-TIDATUM                            
041611        MOVE 'AAMMDD'         TO DAT-KDDATFORM                            
041620        PERFORM FCA-CALL-WDATKONV                                         
041630        IF DAT-KDSVAR-OK                                                  
041640           MOVE DAT-TISEKEL   TO W-DAFINLEV-GU    (1:2)                   
041650           MOVE DAT-TIAAMMDD  TO W-DAFINLEV-GU    (3:6)                   
041660        END-IF                                                            
041700        IF   MID-TIFINLEV-PF8 = ZERO                                      
041710          MOVE ZERO TO W-DAFINLEV-GU                                      
041800          MOVE '7'  TO MFS-IDPFK                                          
041900          MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                           
042000        END-IF                                                            
042100     ELSE                                                                 
042200        MOVE ZERO TO  W-DAFINLEV-MIN                                      
042210                      W-DAFINLEV-GU                                       
042220                                                                          
042300        MOVE '7'  TO MFS-IDPFK                                            
042400        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
042500     END-IF                                                               
042600                                                                          
042700                                                                          
042800     INSPECT MID-IDARTNR-PF8 REPLACING LEADING SPACE BY ZERO              
042900     IF MID-IDARTNR-PF8 NUMERIC                                           
043000        MOVE MID-IDARTNR-PF8 TO W-IDARTNR W-IDARTNR-GU                    
043100        IF   MID-IDARTNR-PF8 = ZERO                                       
043200          MOVE '7'  TO MFS-IDPFK                                          
043300          MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                           
043400        END-IF                                                            
043500     ELSE                                                                 
043600        MOVE ZERO TO W-IDARTNR W-IDARTNR-GU                               
043700        MOVE '7'  TO MFS-IDPFK                                            
043800        MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
043900     END-IF                                                               
043910     MOVE SPAR-IDINK-NEXT TO W-IDINK-GU                                   
043920                                                                          
044000     .                                                                    
044100     EJECT                                                                
044200 E-FIXA-ENTER-NYCKEL SECTION.                                             
044300                                                                          
044400     IF MID-IDPROJ-ENTER = LOW-VALUE                                      
044500*------------LÅT W-IDPROJ-MIN HA VÄRDET FRÅN C-KOLLA-INDATA SECTIO        
044600        CONTINUE                                                          
044700     ELSE                                                                 
044800        MOVE MID-IDPROJ-ENTER TO W-IDPROJ-MIN                             
044810                                 W-IDPROJ-GU                              
044820                                                                          
044900     END-IF                                                               
045000     MOVE MID-FLPISK-ENTER TO W-FLPISK-MIN-E                              
045001                              W-FLPISK-MAX-E                              
045010                              W-FLPISK-GU                                 
045020                                                                          
045100*    MOVE MID-IDAO-ENTER TO W-IDAO-MIN                                    
045110*                           W-IDAO-MIN-E                                  
045120                                                                          
045200                                                                          
045300     INSPECT MID-IDANSK-ENTER REPLACING LEADING SPACE BY ZERO             
045400     IF MID-IDANSK-ENTER NUMERIC                                          
045500*         IF MID-IDANSK-ENTER = ZERO                                      
045600*            CONTINUE                                                     
045700*         ELSE                                                            
045800             MOVE MID-IDANSK-ENTER TO W-IDANSK-MIN                        
045810                                      W-IDANSK-GU                         
045900*         END-IF                                                          
046000     ELSE                                                                 
046100          MOVE ZERO             TO W-IDANSK-MIN                           
046110                                   W-IDANSK-GU                            
046120                                                                          
046200     END-IF                                                               
046300                                                                          
046400     INSPECT MID-TIFINLEV-ENTER REPLACING LEADING SPACE BY ZERO           
046500     IF MID-TIFINLEV-ENTER NUMERIC                                        
046610          MOVE MID-TIFINLEV-ENTER TO DAT-I-TIDATUM                        
046611          MOVE 'AAMMDD'           TO DAT-KDDATFORM                        
046620          PERFORM FCA-CALL-WDATKONV                                       
046630          IF DAT-KDSVAR-OK                                                
046640             MOVE DAT-TISEKEL   TO W-DAFINLEV-MIN   (1:2)                 
046641                                   W-DAFINLEV-GU    (1:2)                 
046642                                                                          
046650             MOVE DAT-TIAAMMDD  TO W-DAFINLEV-MIN   (3:6)                 
046651                                   W-DAFINLEV-GU    (3:6)                 
046652                                                                          
046660          END-IF                                                          
046700     ELSE                                                                 
046800          MOVE ZERO              TO W-DAFINLEV-MIN                        
046810                                    W-DAFINLEV-GU                         
046820                                                                          
046900     END-IF                                                               
047000                                                                          
047100     INSPECT MID-IDARTNR-ENTER REPLACING LEADING SPACE BY ZERO            
047200     IF MID-IDARTNR-ENTER NUMERIC                                         
047300          MOVE MID-IDARTNR-ENTER TO W-IDARTNR W-IDARTNR-MIN               
047310                                              W-IDARTNR-GU                
047320                                                                          
047400     ELSE                                                                 
047500          MOVE ZERO              TO W-IDARTNR W-IDARTNR-MIN               
047510                                              W-IDARTNR-GU                
047520                                                                          
047600     END-IF                                                               
047610     MOVE SPAR-IDINK-ENTER       TO W-IDINK-GU                            
047700     .                                                                    
047800     EJECT                                                                
047900 F-LAES-VISA-INFO SECTION.                                                
048000                                                                          
048100     MOVE +1 TO RAD-INDX                                                  
048200     IF MFS-IDPFK = '7'                                                   
048300        PERFORM S01-LAES-GN-WDD2E                                         
048400     ELSE                                                                 
048401        PERFORM IMS-GU-WDD2E                                              
048600        IF WS-IDPROJ = SPACE                                              
048700           MOVE LOW-VALUE TO W-IDPROJ-MIN-E                               
048800           MOVE HIGH-VALUE TO W-IDPROJ-MAX-E                              
048900        END-IF                                                            
049000     END-IF                                                               
049100                                                                          
049200     IF SEGMENT-FINNS                                                     
049300        PERFORM FA-SPARA-ENTER                                            
049400     ELSE                                                                 
049500        PERFORM FB-NOLLA-ENTER                                            
049600        MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                              
049700     END-IF                                                               
049800                                                                          
049900     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
050000       IF SEGMENT-FINNS                                                   
050100         ADD +1 TO MAX-LAS-WDD2B1-INDX                                    
050200         PERFORM FC-FYLL-I-BILDEN                                         
050311         PERFORM S01-LAES-GN-WDD2E                                        
050400       ELSE                                                               
050500         PERFORM MFS-RENSA-MOD-RAD                                        
050600       END-IF                                                             
050700       ADD +1 TO RAD-INDX                                                 
050800     END-PERFORM                                                          
050900                                                                          
051000     IF SEGMENT-FINNS                                                     
051100        PERFORM FD-SPARA-PF8                                              
051200        MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSINF                             
051300        IF  WS-ARTI01-SLUTRAKNADE                                         
051400            MOVE MFS-STAENG-FAELT TO MOD-ANT-ART-WDD2B1-ATTR              
051500        ELSE                                                              
051600           PERFORM FE-KTR-ANTAL-ART                                       
051700        END-IF                                                            
051800     ELSE                                                                 
051900        PERFORM FF-NOLLA-PF8                                              
052000        MOVE MFS-STAENG-FAELT TO MOD-ANT-ART-WDD2B1-ATTR                  
052100        IF  WS-ARTI01-SLUTRAKNADE                                         
052200           CONTINUE                                                       
052300        ELSE                                                              
052400           MOVE WS-ARTI01-SEGMENT-SLUT TO MOD-IDARTNR-WDD2B1              
052500           MOVE MAX-LAS-WDD2B1-INDX TO MOD-ANT-ART-WDD2B1                 
052600           INSPECT MOD-ANT-ART-WDD2B1 REPLACING LEADING ZERO              
052700                                                    BY SPACE              
052800        END-IF                                                            
052900     END-IF                                                               
052920     MOVE '002'          TO MSGI-KDCALL                                   
052930     MOVE '2142'         TO SPAR-IDTRANS                                  
052931     MOVE WS-LAES-FLPISK TO SPAR-LAES-NYCKEL                              
052940     MOVE SPAR-AREA      TO MSGI-SPAR-AREA                                
052950     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
053000     .                                                                    
053100     EJECT                                                                
053200 FA-SPARA-ENTER      SECTION.                                             
053300                                                                          
053400     MOVE SEQE-IDANSK TO        MOD-IDANSK-ENTER                          
053500     MOVE SEQE-IDPROJ TO            MOD-IDPROJ-ENTER                      
053600     MOVE SEQE-FLPISK TO            MOD-FLPISK-ENTER                      
053700     MOVE SEQE-DAFINLEV (3:6) TO MOD-TIFINLEV-ENTER                       
053800*    MOVE SEQE-IDAO          TO     MOD-IDAO-ENTER                        
053900     MOVE SEQE-IDARTNR TO           MOD-IDARTNR-ENTER                     
053910     MOVE SEQE-IDINK   TO           SPAR-IDINK-ENTER                      
054000                                                                          
054100     .                                                                    
054200     SKIP2                                                                
054300 FB-NOLLA-ENTER       SECTION.                                            
054400                                                                          
054500     MOVE ZERO TO    MOD-IDANSK-ENTER                                     
054600                     MOD-TIFINLEV-ENTER                                   
054700                     MOD-IDARTNR-ENTER                                    
054800                     W-IDARTNR                                            
054900     MOVE 'J'    TO  MOD-FLPISK-ENTER                                     
054901*                    MOD-IDAO-ENTER                                       
054910     MOVE 'J'    TO  WS-LAES-FLPISK                                       
054920                     W-FLPISK-MIN-E                                       
054930                     W-FLPISK-MAX-E                                       
055100     MOVE LOW-VALUE TO MOD-IDPROJ-ENTER                                   
055200     .                                                                    
055300     EJECT                                                                
055400 FC-FYLL-I-BILDEN     SECTION.                                            
055500                                                                          
055600     MOVE NEJ TO FIX-SW-IDINK-NOLL                                        
055700     MOVE SEQE-IDARTNR TO           MOD-IDARTNR(RAD-INDX)                 
055800                                    W-IDARTNR                             
055900     PERFORM IMS-GU-ARTG01                                                
056000*----------------DATUM-KONVERTERING TILL ÅÅVV --------------------        
056100     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
056200                                                                          
057800     IF ARTG01-ART-TIINKOP  = +111111                                     
057900        MOVE 11 TO WS-AA                                                  
058000        MOVE 11 TO WS-VV                                                  
058100        MOVE WS-AAVV          TO MOD-TIINKOP(RAD-INDX)                    
058200     ELSE                                                                 
058300        MOVE ARTG01-ART-TIINKOP TO DAT-I-TIDATUM                          
058400        PERFORM FCA-CALL-WDATKONV                                         
058500        IF DAT-KDSVAR-OK                                                  
058600           MOVE DAT-TIAA-VECKA   TO WS-AA                                 
058700           MOVE DAT-TIVV         TO WS-VV                                 
058800           MOVE WS-AAVV          TO MOD-TIINKOP(RAD-INDX)                 
058900        ELSE                                                              
059000           MOVE ARTG01-ART-TIINKOP TO MOD-TIINKOP(RAD-INDX)               
059100        END-IF                                                            
059200     END-IF                                                               
059300                                                                          
059310     IF ARTG01-ART-TIMOTSI = ZERO                                         
059311        MOVE MFS-RENSA-FAELT  TO MOD-TIMOTSI(RAD-INDX)                    
059312     ELSE                                                                 
059313        MOVE ARTG01-ART-TIMOTSI TO MOD-TIMOTSI(RAD-INDX)                  
059323     END-IF                                                               
059330                                                                          
059400     MOVE ARTG01-ART-IDPROJ TO     MOD-IDPROJ(RAD-INDX)                   
059500     MOVE ARTG01-ART-FLPISK TO     MOD-FLPISK(RAD-INDX)                   
059700     MOVE ARTG01-ART-IDPROJK TO MOD-IDPROJK(RAD-INDX)                     
059701     IF ARTG01-ART-KDKOPTYP = 'R'                                         
059710        MOVE 'REPKÖP'        TO       MOD-TEKOPTYP (RAD-INDX)             
059720     ELSE                                                                 
059721        MOVE 'NYKÖP'         TO       MOD-TEKOPTYP (RAD-INDX)             
059730     END-IF                                                               
059800                                                                          
059900     IF ARTG01-ART-TIREGDAT > ZERO                                        
060210        IF ARTG01-ART-IDINK (1:3) NUMERIC                                 
060220           MOVE ARTG01-ART-IDINK (1:3)     TO TEST-IDINK                  
060230        ELSE                                                              
060240           IF ARTG01-ART-IDINK (2:3) NUMERIC                              
060250              MOVE ARTG01-ART-IDINK (2:3)  TO TEST-IDINK                  
060260           ELSE                                                           
060261              MOVE ZERO                    TO TEST-IDINK                  
060270           END-IF                                                         
060280        END-IF                                                            
060300        IF ( TEST-IDINK >  99 AND < 987 ) OR                              
060310           ( TEST-IDINK > 987 AND < 1000)                                 
060400           MOVE ARTG01-ART-IDINK TO MOD-IDINK(RAD-INDX)                   
060500        END-IF                                                            
060600******************FIX FÖR ARTIKLAR MED INKÖPARE NOLL, ARTG********        
060700        IF ARTG01-ART-IDINK = SPACE                                       
060800           MOVE ARTG01-ART-IDINK TO MOD-IDINK(RAD-INDX)                   
060900           MOVE JA TO FIX-SW-IDINK-NOLL                                   
061000        END-IF                                                            
061100******************************************************************        
061200     END-IF                                                               
061300                                                                          
061400     PERFORM  IMS-GU-ARTC01                                               
061500     IF SEGMENT-FINNS                                                     
061510        MOVE ART-TIFINLV        TO WS-TIAAVVD                             
061600        PERFORM  IMS-GNP-ARTC11                                           
061700        IF SEGMENT-FINNS                                                  
061800           MOVE CLAG-IDPROJUP   TO MOD-IDPROJUP(RAD-INDX)                 
061801           IF CLAG-IDINK (1:3) NUMERIC                                    
061802              MOVE CLAG-IDINK (1:3)           TO TEST-IDINK               
061803           ELSE                                                           
061804              IF CLAG-IDINK (2:3) NUMERIC                                 
061805                 MOVE CLAG-IDINK (2:3)        TO TEST-IDINK               
061806              ELSE                                                        
061807                 MOVE ZERO                    TO TEST-IDINK               
061808              END-IF                                                      
061809           END-IF                                                         
061831           IF MOD-IDINK(RAD-INDX) > SPACE OR                              
061832              FIX-SW-IDINK-NOLL = JA                                      
061833               CONTINUE                                                   
061840           ELSE                                                           
061850              MOVE CLAG-IDINK TO MOD-IDINK(RAD-INDX)                      
061860           END-IF                                                         
061900        END-IF                                                            
062000**      IF MOD-IDINK(RAD-INDX) > SPACE                                    
062100**         CONTINUE                                                       
062200**      ELSE                                                              
062300******************FIX FÖR ARTIKLAR MED INKÖPARE NOLL, ARTG********        
062400**         IF FIX-SW-IDINK-NOLL = JA                                      
062500**            CONTINUE                                                    
062600**         ELSE                                                           
062700******************************************************************        
062900**            IF SEGMENT-FINNS                                            
063000**               MOVE CLAG-IDINK TO MOD-IDINK(RAD-INDX)                   
063100**            END-IF                                                      
063200**         END-IF                                                         
063300**      END-IF                                                            
063310     ELSE                                                                 
063320        MOVE ZERO TO WS-TIAAVVD                                           
063400     END-IF                                                               
063401                                                                          
063402     IF ARTG01-ART-KDKOPTYP = 'R'                                         
063403       MOVE WS-TIAAVV              TO MOD-TIFINLEV(RAD-INDX)              
063404     ELSE                                                                 
063410       IF ARTG01-ART-DAFINLEV = +99999999                                 
063420          MOVE 99 TO WS-AA                                                
063430          MOVE 99 TO WS-VV                                                
063440          MOVE WS-AAVV          TO MOD-TIFINLEV(RAD-INDX)                 
063450       ELSE                                                               
063460          MOVE ARTG01-ART-DAFINLEV (3:6) TO   DAT-I-TIDATUM               
063470          PERFORM FCA-CALL-WDATKONV                                       
063480          IF DAT-KDSVAR-OK                                                
063490             MOVE DAT-TIAA-VECKA   TO WS-AA                               
063491             MOVE DAT-TIVV         TO WS-VV                               
063492             MOVE WS-AAVV          TO MOD-TIFINLEV(RAD-INDX)              
063493          ELSE                                                            
063494             MOVE ARTG01-ART-DAFINLEV (3:6)                               
063495                  TO MOD-TIFINLEV(RAD-INDX)                               
063496          END-IF                                                          
063497       END-IF                                                             
063498     END-IF                                                               
063500     .                                                                    
063600     EJECT                                                                
063700 FCA-CALL-WDATKONV SECTION.                                               
063800     SKIP2                                                                
063900     CALL WDATKONV            USING  DAT-KDDATFORM                        
064000                                     DAT-I-TIDATUM                        
064100                                     DAT-O-TIDATUM                        
064200                                     DAT-KDSVAR                           
064300     .                                                                    
064400     EJECT                                                                
064500 FD-SPARA-PF8 SECTION.                                                    
064600                                                                          
064700     MOVE SEQE-IDANSK TO MOD-IDANSK-PF8                                   
064800     MOVE SEQE-IDPROJ TO MOD-IDPROJ-PF8                                   
064900     MOVE SEQE-FLPISK TO MOD-FLPISK-PF8                                   
064910                         MOD-FLPISK-PF8                                   
064920                         WS-LAES-FLPISK                                   
065000     MOVE SEQE-DAFINLEV (3:6)TO MOD-TIFINLEV-PF8                          
065100*    MOVE SEQE-IDAO          TO MOD-IDAO-PF8                              
065200     MOVE SEQE-IDARTNR TO MOD-IDARTNR-PF8                                 
065201*    PERFORM S03-TESTA-IDINK                                              
065210     MOVE SEQE-IDINK   TO SPAR-IDINK-NEXT                                 
065300     .                                                                    
065400     EJECT                                                                
065500 FE-KTR-ANTAL-ART SECTION.                                                
065600                                                                          
065700******************************************************************        
065800* HÄR SKER EN KONTROLL PÅ HUR MÅNGA ARTIKLAR SOM LIGGER PÅ KÖ             
065900* MED KDANSKQ = 2.                                                        
066000* OM DET ÄR FLER ÄN 200 ARTIKLAR FORSÄTTER PROGRAMMET UPPRÄKNINGEN        
066100* VID NÄSTA TRANS. EX. ENTER, PF8.                                        
066200*                                                                         
066300* BARA NÄR LÄSNINGEN HAR NÅTT 'GE', VISAS ANTALET I MOD-ANT-ART-RD        
066400* FORSÄTTNINGSLÄSNINGEN SKER GENOM ATT MAN LÄSER ARTG01, OCH HÄMTA        
066500* NYCKLARNA TILL SEQ-INDX-WDD2B1.                                         
066600******************************************************************        
066700                                                                          
066800     IF MID-IDARTNR-WDD2B1 = ZERO                                         
066900        MOVE MAX-LAS-WDD2B1-INDX TO RAKNARE-ART-WDD2B1                    
067000     ELSE                                                                 
067100        MOVE MID-IDARTNR-WDD2B1 TO W-IDARTNR                              
067200        MOVE  ZERO TO RAKNARE-ART-WDD2B1                                  
067300        PERFORM IMS-GU-ARTG01                                             
067400        MOVE ARTG01-ART-IDANSK TO     W-IDANSK-MIN                        
067410                                      W-IDANSK-GU                         
067411*                                     W-IDANSK-MIN-E                      
067420                                                                          
067500        MOVE ARTG01-ART-IDPROJ TO     W-IDPROJ-MIN                        
067510                                      W-IDPROJ-GU                         
067511*                                     W-IDPROJ-MIN-E                      
067520                                                                          
067600        MOVE ARTG01-ART-FLPISK TO     W-FLPISK-GU                         
067601*                                     W-FLPISK-MIN-E                      
067610                                                                          
067620                                                                          
067700*       MOVE ARTG01-ART-DAFINLEV (3:6) TO   W-DAFINLEV-MIN                
067701        MOVE ARTG01-ART-DAFINLEV       TO   W-DAFINLEV-MIN                
067710                                            W-DAFINLEV-GU                 
067711*                                           W-DAFINLEV-MIN-E              
067720                                                                          
067800*       MOVE ARTG01-ART-IDAO     TO   W-IDAO-MIN                          
067810*                                     W-IDAO-MIN-E                        
067820                                                                          
067900        MOVE ARTG01-ART-IDARTNR TO W-IDARTNR-MIN                          
067910                                   W-IDARTNR-GU                           
067911*                                  W-IDARTNR-MIN-E                        
067920                                                                          
067930        MOVE ARTG01-ART-IDINK   TO W-IDINK-GU                             
067931*                                  W-IDINK-MIN-E                          
067940                                                                          
068000        PERFORM IMS-GU-WDD2E                                              
068010        IF SEGMENT-FINNS                                                  
068100          IF WS-IDPROJ = SPACE                                            
068200             MOVE LOW-VALUE TO W-IDPROJ-MIN                               
068210                               W-IDPROJ-MIN-E                             
068220                                                                          
068300             MOVE HIGH-VALUE TO W-IDPROJ-MAX                              
068310                                W-IDPROJ-MAX-E                            
068320                                                                          
068400          END-IF                                                          
068410        ELSE                                                              
068420          CALL FELLOG                                                     
068430        END-IF                                                            
068500     END-IF                                                               
068600                                                                          
068700     PERFORM UNTIL (MAX-LAS-WDD2B1-INDX > 180)                            
068800       IF (SEGMENT-FINNS)                                                 
068900          ADD +1 TO RAKNARE-ART-WDD2B1                                    
069000                    MAX-LAS-WDD2B1-INDX                                   
069010          PERFORM S01-LAES-GN-WDD2E                                       
069200       ELSE                                                               
069300          MOVE 200 TO MAX-LAS-WDD2B1-INDX                                 
069400       END-IF                                                             
069500     END-PERFORM                                                          
069600                                                                          
069700     COMPUTE                                                              
069800     SPAR-ANT-ART-WDD2B1 = SPAR-ANT-ART-WDD2B1 +                          
069900                              RAKNARE-ART-WDD2B1                          
070000     MOVE SPAR-ANT-ART-WDD2B1 TO MOD-ANT-ART-WDD2B1                       
070100     INSPECT MOD-ANT-ART-WDD2B1 REPLACING LEADING ZERO                    
070200                                              BY SPACE                    
070300                                                                          
070400     IF SEGMENT-FINNS                                                     
070500        MOVE SEQE-IDARTNR TO MOD-IDARTNR-WDD2B1                           
070600     ELSE                                                                 
070700        MOVE WS-ARTI01-SEGMENT-SLUT TO MOD-IDARTNR-WDD2B1                 
070800        MOVE MFS-STAENG-FAELT TO MOD-ANT-ART-WDD2B1-ATTR                  
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200 FF-NOLLA-PF8         SECTION.                                            
071300                                                                          
071400     MOVE ZERO TO    MOD-IDANSK-PF8                                       
071500                     MOD-TIFINLEV-PF8                                     
071600                     MOD-IDARTNR-PF8                                      
071700                     W-IDARTNR                                            
071800     MOVE 'J'   TO   MOD-FLPISK-PF8                                       
071900*                    MOD-IDAO-PF8                                         
072000     MOVE LOW-VALUE TO MOD-IDPROJ-PF8                                     
072010     MOVE 'J' TO WS-LAES-FLPISK                                           
072020                 W-FLPISK-MIN-E                                           
072021                 W-FLPISK-MAX-E                                           
072030                                                                          
072100     .                                                                    
072200     EJECT                                                                
072300 S01-LAES-GN-WDD2E   SECTION.                                             
072301     PERFORM IMS-GN-WDD2E                                                 
072302     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
072303       IF W-FLPISK-MIN-E = 'J'                                            
072304         MOVE 'N'      TO W-FLPISK-MIN-E                                  
072305                          W-FLPISK-MAX-E                                  
072306                          WS-LAES-FLPISK                                  
072307         PERFORM IMS-GN-WDD2E                                             
072308       END-IF                                                             
072309     END-IF                                                               
072310     .                                                                    
072311     EJECT                                                                
072312*                                                                         
072313*S02-LAES-MIN-WDD2E   SECTION.                                            
072314*    PERFORM IMS-GU-WDD2E                                                 
072315*    IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
072316*      IF W-FLPISK-MIN-E = 'J'                                            
072317*        MOVE 'N'      TO W-FLPISK-MIN-E                                  
072318*                         W-FLPISK-MAX-E                                  
072319*                         WS-LAES-FLPISK                                  
072320*        PERFORM IMS-GN-MIN-KEY-WDD2E                                     
072321*      END-IF                                                             
072322*    END-IF                                                               
072323*    .                                                                    
072324*    EJECT                                                                
072325 S03-TESTA-IDINK SECTION.                                                 
072326     MOVE NEJ TO FIX-SW-IDINK-NOLL                                        
072327     MOVE SEQE-IDARTNR                     TO W-IDARTNR                   
072328                                                                          
072329     PERFORM IMS-GU-ARTG01                                                
072363     IF ARTG01-ART-TIREGDAT > ZERO                                        
072364        IF ARTG01-ART-IDINK (1:3) NUMERIC                                 
072365           MOVE ARTG01-ART-IDINK (1:3)     TO TEST-IDINK                  
072366        ELSE                                                              
072367           IF ARTG01-ART-IDINK (2:3) NUMERIC                              
072368              MOVE ARTG01-ART-IDINK (2:3)  TO TEST-IDINK                  
072369           ELSE                                                           
072370              MOVE ZERO                    TO TEST-IDINK                  
072371           END-IF                                                         
072372        END-IF                                                            
072373        IF ( TEST-IDINK >  99 AND < 987 ) OR                              
072374           ( TEST-IDINK > 987 AND < 1000)                                 
072376           MOVE ARTG01-ART-IDINK TO SPAR-IDINK-NEXT                       
072377        END-IF                                                            
072378******************FIX FÖR ARTIKLAR MED INKÖPARE NOLL, ARTG********        
072379        IF ARTG01-ART-IDINK = SPACE                                       
072380           MOVE ARTG01-ART-IDINK TO SPAR-IDINK-NEXT                       
072381           MOVE JA TO FIX-SW-IDINK-NOLL                                   
072382        END-IF                                                            
072383******************************************************************        
072384     END-IF                                                               
072385                                                                          
072386     PERFORM  IMS-GU-ARTC01                                               
072387     IF SEGMENT-FINNS                                                     
072388                                                                          
072389        PERFORM  IMS-GNP-ARTC11                                           
072390        IF SEGMENT-FINNS                                                  
072391           MOVE CLAG-IDPROJUP   TO MOD-IDPROJUP(RAD-INDX)                 
072392           IF CLAG-IDINK (1:3) NUMERIC                                    
072393              MOVE CLAG-IDINK (1:3)           TO TEST-IDINK               
072394           ELSE                                                           
072395              IF CLAG-IDINK (2:3) NUMERIC                                 
072396                 MOVE CLAG-IDINK (2:3)        TO TEST-IDINK               
072397              ELSE                                                        
072398                 MOVE ZERO                    TO TEST-IDINK               
072399              END-IF                                                      
072400           END-IF                                                         
072404           IF SPAR-IDINK-NEXT > SPACE OR                                  
072405              FIX-SW-IDINK-NOLL = JA                                      
072406               CONTINUE                                                   
072407           ELSE                                                           
072408              MOVE CLAG-IDINK TO SPAR-IDINK-NEXT                          
072409           END-IF                                                         
072411        END-IF                                                            
072425     END-IF                                                               
072428     .                                                                    
072429     EJECT                                                                
072430 MFS-RENSA-MID-IN SECTION.                                                
072440     MOVE MFS-RENSA-FAELT TO MID-IDANSK-FOM-IN                            
072500                             MID-IDANSK-TOM-IN                            
072600                             MID-IDPROJ-IN                                
072700                             MID-ANT-ART-WDD2B1                           
072800     .                                                                    
072900     SKIP2                                                                
073000 MFS-RENSA-MOD-IN SECTION.                                                
073100     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-FOM-IN                            
073200                             MOD-IDANSK-TOM-IN                            
073300                             MOD-IDPROJ-IN                                
073400                             MOD-TEMFSFEL                                 
073500                             MOD-TEMFSINF                                 
073600     .                                                                    
073700     SKIP2                                                                
073800 MFS-RENSA-MOD-RAD SECTION.                                               
073900                                                                          
074000     PERFORM UNTIL RAD-INDX > 12                                          
074100        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR  (RAD-INDX)                   
074200                                MOD-IDPROJ   (RAD-INDX)                   
074300                                MOD-IDPROJK  (RAD-INDX)                   
074400                                MOD-IDPROJUP (RAD-INDX)                   
074500                                MOD-TEKOPTYP (RAD-INDX)                   
074600                                MOD-TIFINLEV (RAD-INDX)                   
074700                                MOD-FLPISK   (RAD-INDX)                   
074800                                MOD-TIINKOP  (RAD-INDX)                   
074900                                MOD-IDINK    (RAD-INDX)                   
075000                                MOD-TIMOTSI  (RAD-INDX)                   
075100        ADD +1 TO RAD-INDX                                                
075200     END-PERFORM                                                          
075300                                                                          
075400     .                                                                    
075500     EJECT                                                                
075600* IMS-SECTIONER                                                           
075700     SKIP3                                                                
075800                                                                          
075900                                                                          
076000 IMS-GET-MSG SECTION.                                                     
076100                                                                          
076200     MOVE '  QC' TO GODK-STATUSKODER                                      
076300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
076400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076500     PERFORM IMS-STATUSKONTROLL                                           
076600     .                                                                    
076700 IMS-INSERT-MSG SECTION.                                                  
076800     IF ENGLISH-TEXT                                                      
076900        MOVE 'N' TO MFS-KDHUVOMR                                          
077000     END-IF                                                               
077100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
077200     MOVE SPACE  TO GODK-STATUSKODER                                      
077300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
077400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     .                                                                    
077700     EJECT                                                                
077710*                                                                         
077800*IMS-GN-MIN-KEY-ARTI01 SECTION.                                           
077900*    STRING 'WLARTI01(WDD2B1KY=>' W-WDD2B1KY-MIN                          
077910*                   '&WDD2B1KY=<' W-WDD2B1KY-MAX                          
078000*                   '&KDANSKQ  =' W-KDANSKQ-X ')'                         
078100*                      DELIMITED BY SIZE INTO SSA1                        
078200*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
078300*    CALL CBLTDLI USING GN     ARTI-PCB DLI-IO-AREA1 SSA1                 
078400*    MOVE   ARTI-STATUS-CODE TO STATUS-WS                                 
078500*    PERFORM IMS-STATUSKONTROLL                                           
078600*    SKIP2                                                                
078700*    .                                                                    
078710 IMS-GU-WDD2E  SECTION.                                                   
078720     STRING 'WDD2E1  (WDD2E1KY= ' W-WDD2E1KY-GU                           
078730                    '&KDANSKQ  =' W-KDANSKQ-X ')'                         
078750                       DELIMITED BY SIZE INTO SSA1                        
078760     MOVE '  GEGB' TO GODK-STATUSKODER                                    
078770     CALL CBLTDLI USING GU WDD2E-PCB DLI-IO-WDD2SEQ SSA1                  
078780     MOVE  WDD2E-STATUS-CODE TO STATUS-WS                                 
078790     PERFORM IMS-STATUSKONTROLL                                           
078791     SKIP2                                                                
078792     .                                                                    
078793 IMS-GN-MIN-KEY-WDD2E  SECTION.                                           
078794     STRING 'WDD2E1  (WDD2E1KY=>' W-WDD2E1KY-MIN                          
078795                    '&WDD2E1KY=<' W-WDD2E1KY-MAX                          
078796                    '&KDANSKQ  =' W-KDANSKQ-X ')'                         
078797                       DELIMITED BY SIZE INTO SSA1                        
078798     MOVE '  GEGB' TO GODK-STATUSKODER                                    
078799     CALL CBLTDLI USING GN WDD2E-PCB DLI-IO-WDD2SEQ SSA1                  
078800     MOVE  WDD2E-STATUS-CODE TO STATUS-WS                                 
078801     PERFORM IMS-STATUSKONTROLL                                           
078802     SKIP2                                                                
078803     .                                                                    
078804*                                                                         
078810*IMS-GN-ARTI01 SECTION.                                                   
078900*    STRING 'WLARTI01(WDD2B1KY=>' W-WDD2B1KY-MIN                          
079000*                   '&WDD2B1KY=<' W-WDD2B1KY-MAX                          
079100*                   '&KDANSKQ = ' W-KDANSKQ-X                             
079200*                   '&IDPROJ  =>' W-IDPROJ-MIN                            
079300*                   '&IDPROJ  <=' W-IDPROJ-MAX ')'                        
079400*                      DELIMITED BY SIZE INTO SSA1                        
079500*    MOVE '  GEGB' TO GODK-STATUSKODER                                    
079600*    CALL CBLTDLI USING GN     ARTI-PCB DLI-IO-AREA1 SSA1                 
079700*    MOVE   ARTI-STATUS-CODE TO STATUS-WS                                 
079800*    PERFORM IMS-STATUSKONTROLL                                           
079900*    .                                                                    
080000*    EJECT                                                                
080010 IMS-GN-WDD2E  SECTION.                                                   
080020     STRING 'WDD2E1  (WDD2E1KY=>' W-WDD2E1KY-MIN                          
080030                    '&WDD2E1KY=<' W-WDD2E1KY-MAX                          
080040                    '&KDANSKQ = ' W-KDANSKQ-X                             
080050                    '&IDPROJ  =>' W-IDPROJ-MIN-E                          
080060                    '&IDPROJ  <=' W-IDPROJ-MAX-E ')'                      
080070                       DELIMITED BY SIZE INTO SSA1                        
080080     MOVE '  GEGB' TO GODK-STATUSKODER                                    
080090     CALL CBLTDLI USING GN    WDD2E-PCB DLI-IO-WDD2SEQ SSA1               
080091     MOVE  WDD2E-STATUS-CODE TO STATUS-WS                                 
080092     PERFORM IMS-STATUSKONTROLL                                           
080093     .                                                                    
080094     EJECT                                                                
080110 IMS-GU-ARTG01      SECTION.                                              
080200     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
080300            DELIMITED BY SIZE INTO SSA1                                   
080400     MOVE '  GE'   TO GODK-STATUSKODER                                    
080500     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA2 SSA1                     
080600     MOVE   ARTG-STATUS-CODE TO STATUS-WS                                 
080700     PERFORM IMS-STATUSKONTROLL                                           
080800     .                                                                    
080900     EJECT                                                                
081000 IMS-GU-ARTC01      SECTION.                                              
081100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
081200            DELIMITED BY SIZE INTO SSA1                                   
081300     MOVE '  GE'   TO GODK-STATUSKODER                                    
081400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1                     
081500     MOVE   ARTC-STATUS-CODE TO STATUS-WS                                 
081600     PERFORM IMS-STATUSKONTROLL                                           
081700     .                                                                    
081800     SKIP1                                                                
081900 IMS-GNP-ARTC11 SECTION.                                                  
082000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
082100            DELIMITED BY SIZE INTO SSA1                                   
082200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
082300            DELIMITED BY SIZE INTO SSA2                                   
082400     MOVE '  GE'   TO GODK-STATUSKODER                                    
082500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA4 SSA1 SSA2               
082600     MOVE   ARTC-STATUS-CODE TO STATUS-WS                                 
082700     PERFORM IMS-STATUSKONTROLL                                           
083900     .                                                                    
084000     EJECT                                                                
084010 IMS-GU-WDK611 SECTION.                                                   
084020     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
084030            DELIMITED BY SIZE INTO SSA1                                   
084040     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
084050            DELIMITED BY SIZE INTO SSA2                                   
084060     MOVE '  GE'   TO GODK-STATUSKODER                                    
084070     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA4 SSA1 SSA2               
084080     MOVE   ARTC-STATUS-CODE TO STATUS-WS                                 
084090     PERFORM IMS-STATUSKONTROLL                                           
084091     .                                                                    
084092     EJECT                                                                
084100 IMS-STATUSKONTROLL SECTION.                                              
084200                                                                          
084300     SET STATUS-IX TO 1                                                   
084400     SEARCH GODK-STATUS                                                   
084410       AT END                                                             
084420         CALL FELLOG                                                      
084500       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
084600     END-SEARCH                                                           
084700     .                                                                    
