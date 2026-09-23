000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2612000.                                    
000300 AUTHOR.                     IDK INGVAR CARLSSON.                         
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN.               JANUARI 1979.                                
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        PROGRAMMET, SOM ÄR EN SB, LÄSER                                  
001200*        ORDERINGÅNGSREGISTRET WDL8 (SEGMENTEN -01,                       
001300*        -11) OCH SKAPAR POSTER PÅ UTFILEN W26121                         
001400*        FÖR LTK-UPPFÖLJNING OCH LAGERBALANSERING.                        
001500*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*--------------------------------------- POSTER INNEHÅLLANDE              
002400*                                        ORDERINGÅNGSINFORMATION          
002500*                                        FÖR LTK-UPPFÖLJNING OCH          
002600*                                        LAGERBALANSERING                 
002700*                                        OUTPUT                           
002800                                                                          
002900     SELECT W26121 ASSIGN UT-S-W26120D1.                                  
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 FILE SECTION.                                                            
003300     SKIP2                                                                
003400 FD  W26121                                                               
003500     RECORDING F                                                          
003600     BLOCK 0                                                              
003700     LABEL RECORD STANDARD.                                               
003800*01  POST -COPY W261225    -PRE U21- -L                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300*--------------------------------------- KONSTANTER                       
004400 01  KONSTANTER.                                                          
004500     05  JA                  PIC X       VALUE 'J'.                       
004600     05  NEJ                 PIC X       VALUE 'N'.                       
004700     SKIP3                                                                
004800*--------------------------------------- INDEXFÄLT                        
004900 01  INDEXFALT.                                                           
005000     05  XOI                  PIC S9(9)               COMP SYNC.          
005010     03  IX-W                 PIC S9(9)               COMP-3.             
005020     03  IX-UT                PIC S9(9)               COMP-3.             
005030     03  IX                   PIC S9(9)               COMP-3.             
005100     SKIP3                                                                
005200*--------------------------------------- ALLMÄNNA ARBETSAREOR             
005300 01  W.                                                                   
005400     05  W-DDATUM-AAP         PIC S9(3).                                  
005500     05  FILLER               REDEFINES W-DDATUM-AAP.                     
005600         10  W-DDATUM-AA      PIC 9(2).                                   
005700         10  W-DDATUM-P       PIC 9.                                      
005900     05  W-OI-AAP             PIC S9(3).                                  
006000     05  FILLER REDEFINES W-OI-AAP.                                       
006100         10  W-OI-AA          PIC 99.                                     
006200         10  W-OI-P           PIC 9.                                      
006201     03  MAX-ANT-PERIODER                                                 
006202                         PIC S9(9)   VALUE +16       COMP SYNC.           
006203     SKIP3                                                                
006220 01  WS-TIAAAAP              PIC 9(5).                                    
006230 01  FILLER REDEFINES WS-TIAAAAP.                                         
006240     03  FILLER                  PIC  9(2).                               
006250     03  WS-TIAAP                PIC  9(3).                               
006260     03  WS-TIAAP-RED            REDEFINES WS-TIAAP.                      
006270         05 WS-TIAAP-AAR           PIC  99.                               
006280         05 WS-TIAAP-PERIOD        PIC 9.                                 
006290 01  FILLER REDEFINES WS-TIAAAAP.                                         
006291     03  WS-TIAAAA               PIC 9(4).                                
006292     03  FILLER                  PIC 9.                                   
006293                                                                          
006294 01  W-PERIOD-AAAAP              PIC 9(5).                                
006295 01  FILLER REDEFINES W-PERIOD-AAAAP.                                     
006296     03  W-PERIOD-TISEKEL    PIC 9(2).                                    
006297     03  W-PERIOD-AAP        PIC 9(3).                                    
006298     03  W-PER               REDEFINES W-PERIOD-AAP.                      
006299         05  W-PERIOD-AA     PIC 9(2).                                    
006300         05  W-PERIOD-P      PIC 9(1).                                    
006301 01  FILLER REDEFINES W-PERIOD-AAAAP.                                     
006302     03  W-PERIOD-AAAA       PIC 9(4).                                    
006303     03  FILLER              PIC 9(1).                                    
006304                                                                          
006305 01  UTPOSTTAB.                                                           
006306   02  FILLER   OCCURS 16.                                                
006307     03  W-SISTA-PER-AAAAP       PIC 9(5).                                
006308     03  FILLER   REDEFINES W-SISTA-PER-AAAAP.                            
006309         05  W-SISTA-PER-AAAA    PIC 9(4).                                
006310         05  W-SISTA-PER-P       PIC 9.                                   
006311     03  FILLER   REDEFINES W-SISTA-PER-AAAAP.                            
006312         05  W-SISTA-PER-TISEKEL PIC 9(2).                                
006313         05  W-SISTA-PER-AAP     PIC 9(3).                                
006320     SKIP3                                                                
006400*--------------------------------------- SWITCHAR.                        
006500                                                                          
006600 01  SW.                                                                  
006700     05  SW-FORSTA-ARTIKEL   PIC X       VALUE 'J'.                       
006710     05  SW-ORDING-ARTIKEL   PIC X       VALUE 'N'.                       
006800     SKIP3                                                                
006900*--------------------------------------- GENERELLA SUBRUTINER             
007000                                                                          
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
007300     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
007400     05  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
007500     05  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
007600     EJECT                                                                
007700*--------------------------------------- PARAMETRAR TILL DATKORT          
007800                                                                          
007900 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W26120'.                  
008000                                                                          
008100 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
008200                                                                          
008300*01  -COPY WDATKORT                                                       
008500     EJECT                                                                
008600*--------------------------------------- PARAMETRAR TILL POSTSUM          
008700                                                                          
008800*01  -COPY W0005      -PRE POSTSUM-                                       
009000     EJECT                                                                
009010*    -COPY W221PERT                                                       
009020     EJECT                                                                
009100*--------------------------------------- AREA FÖR W26121-POST             
009200                                                                          
009300*01  AREA -COPY W261225    -PRE U21-                                      
009500     EJECT                                                                
009600*--------------------------------------- NOLLAREA FÖR W26121              
009700                                                                          
009800*01  NOLLAREA -COPY W261225    -PRE U21- -L                               
010000     EJECT                                                                
010100 01  DLI-IO-AREA.                                                         
010110     03  IO-AREA             PIC X(2500).                                 
010200     SKIP3                                                                
010300*03       -COPY WDL801    -RED IO-AREA.                                   
010500     EJECT                                                                
010600*03       -COPY WDL811    -RED IO-AREA.                                   
011100     EJECT                                                                
011200 01  IMS-WS.                                                              
011300     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
011400*----------------------------------STATUSKODER FRÅN IMS                   
011500     03  STATUS-WS   PIC XX.                                              
011600         88  SEGMENT-FINNS       VALUE '  '.                              
011700         88  SEGMENT-SLUT        VALUE 'GB'.                              
011800                                                                          
011900     03  GODK-STATUSKODER.                                                
012000      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
012100                                                                          
012200     EJECT                                                                
012300*----------------------------------IMS-CALL FUNKTIONER                    
012400*01              -COPY W0003                                              
012600                                                                          
012700     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900*01    -COPY W0008         -PRE WDL8-                                     
013100          05  FILLER      PIC   XX.                                       
013200     EJECT                                                                
013300 PROCEDURE DIVISION USING WDL8-PCB.                                       
013400     ENTRY 'CBLTDLI'  USING WDL8-PCB.                                     
013410                                                                          
013500     PERFORM A-INIT                                                       
013510                                                                          
013600     PERFORM IMS-GET-WDL8                                                 
013700     PERFORM UNTIL SEGMENT-SLUT                                           
013800         EVALUATE WDL8-SEG-NAME-FB                                        
013900            WHEN 'WDL801  '                                               
014000                  PERFORM B-BEHANDLA-OIARTIKEL                            
014300            WHEN 'WDL811  '                                               
014400                  PERFORM C-FLYTTA-OIANT-INFO                             
014500         END-EVALUATE                                                     
014600         PERFORM IMS-GET-WDL8                                             
014700     END-PERFORM                                                          
014710                                                                          
014800     PERFORM Z-FINIT                                                      
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300******************************************************************        
015400*                                                                *        
015500*    INITIERING                                                  *        
015600*    ÖPPNA FIL, LÄS DATUMKORT, BILDA NOLLPOST                    *        
015700*                                                                *        
015800******************************************************************        
015900 A-INIT       SECTION.                                                    
016000*                                                                         
016100     OPEN OUTPUT W26121                                                   
016200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016300     MOVE D-AAR      TO W-DDATUM-AA                                       
016400     MOVE D-PERIOD   TO W-DDATUM-P                                        
016500*                                                                         
016600     IF W-DDATUM-P = 1                                                    
016700         IF W-DDATUM-AA = 00                                              
016800             MOVE 99 TO W-DDATUM-AA                                       
016900         ELSE                                                             
017000             COMPUTE W-DDATUM-AA = W-DDATUM-AA - 1                        
017100         END-IF                                                           
017101         MOVE 8 TO W-DDATUM-P                                             
017102     ELSE                                                                 
017103         COMPUTE W-DDATUM-P = W-DDATUM-P - 1                              
017104     END-IF                                                               
017110*                                                                         
017120     MOVE W-DDATUM-AA      TO W-PERIOD-AA                                 
017130     MOVE W-DDATUM-P       TO W-PERIOD-P                                  
017140     IF W-PERIOD-AA > 50                                                  
017150        MOVE 19   TO W-PERIOD-TISEKEL                                     
017160     ELSE                                                                 
017161        MOVE 20   TO W-PERIOD-TISEKEL                                     
017170     END-IF                                                               
017200*                                                                         
017300     MOVE SPACE TO U21-POST                                               
017400     MOVE 225 TO U21-IDPTYP                                               
017500     MOVE 1 TO XOI                                                        
017600     PERFORM UNTIL XOI > 16                                               
017700         MOVE ZERO TO U21-OTRA-PROGNOSPAV-C1 (XOI)                        
017800                      U21-OTRA-DIVERSE-C1 (XOI)                           
018300         ADD +1 TO XOI                                                    
018400     END-PERFORM                                                          
018500     MOVE 1 TO XOI                                                        
018600     PERFORM UNTIL XOI > 8                                                
018700         MOVE ZERO TO U21-OING-PROGNOSPAV-C1 (XOI)                        
018800                      U21-OING-DIVERSE-C1 (XOI)                           
018900                      U21-OING-SATS-C1 (XOI)                              
019000                      U21-OING-SDC-C2 (XOI)                               
019100                      U21-OING-NDC-C2 (XOI)                               
019200                      U21-OING-REFILL (XOI)                               
019300         ADD +1 TO XOI                                                    
019400     END-PERFORM                                                          
019500                                                                          
019600     MOVE U21-AREA TO U21-NOLLAREA                                        
019700                                                                          
019800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
019900     MOVE 225 TO POSTSUM-TRANSTYP                                         
020000     MOVE 'W26121' TO POSTSUM-FDNAMN                                      
020100     MOVE 'W26120D1' TO POSTSUM-DDNAMN2                                   
020101                                                                          
020110     PERFORM AA-INIT-PERIODVECKO-TABELL                                   
020120     .                                                                    
020130     EJECT                                                                
020140 AA-INIT-PERIODVECKO-TABELL   SECTION.                                    
020150******************************************************************        
020160*                                                                *        
020170*    TIDSANGIVELSE FÖR DE 16 SISTA PERIODERNA BERÄKNAS           *        
020180*                                                                *        
020190******************************************************************        
020191     SKIP3                                                                
020192     MOVE 1 TO IX                                                         
020193     PERFORM UNTIL IX > MAX-ANT-PERIODER                                  
020194         MOVE W-PERIOD-AAAAP TO W-SISTA-PER-AAAAP (IX)                    
020196         SUBTRACT 1 FROM W-PERIOD-AAP                                     
020197         IF  W-PERIOD-P = ZERO                                            
020198             MOVE 8          TO W-PERIOD-P                                
020200             SUBTRACT 1 FROM W-PERIOD-AAAA                                
020201         END-IF                                                           
020202         ADD 1 TO IX                                                      
020203     END-PERFORM                                                          
020204                                                                          
020205     DISPLAY 'UTPOSTTAB ' UTPOSTTAB                                       
020210     EJECT                                                                
020300******************************************************************        
020400*                                                                *        
020500*    BEHANDLA OIARTIKEL                                          *        
020600*    AVSLUTA GAMLA OIARTIKELN                                    *        
020700*    HÄMTA IDARTNR   FRÅN OIARTIKELSEGMENTET (WDL801) FÖR NYA    *        
020800*    OIARTIKELN                                                  *        
020900*                                                                *        
021000******************************************************************        
021100                                                                          
021200                                                                          
021300     .                                                                    
021400 B-BEHANDLA-OIARTIKEL SECTION.                                            
021500                                                                          
021520                                                                          
021600     IF  SW-FORSTA-ARTIKEL = JA                                           
021700         MOVE NEJ TO SW-FORSTA-ARTIKEL                                    
021800     ELSE                                                                 
021810       IF SW-ORDING-ARTIKEL = JA                                          
021900         PERFORM S01-SKRIV-U21                                            
021910         MOVE NEJ      TO SW-ORDING-ARTIKEL                               
021920       END-IF                                                             
022000     END-IF                                                               
022100                                                                          
022200     MOVE U21-NOLLAREA TO U21-AREA                                        
022300     MOVE ART-IDARTNR  TO U21-IDARTNR                                     
022310     .                                                                    
022400     EJECT                                                                
022500******************************************************************        
022600*                                                                *        
022700*    FLYTTA OI-   INFO                                           *        
022800*    HÄMTA UPPGIFTER FRÅN ANTALREDOVISNINGSSEGMENTET (WDL811)    *        
022900*                                                                *        
023000******************************************************************        
023100                                                                          
023200                                                                          
023400 C-FLYTTA-OIANT-INFO SECTION.                                             
023410                                                                          
023420     IF AAR-TIAAAA = W-SISTA-PER-AAAA (1) OR                              
023430        AAR-TIAAAA = W-SISTA-PER-AAAA (8) OR                              
023431        AAR-TIAAAA = W-SISTA-PER-AAAA (16)                                
023440        MOVE AAR-TIAAAA TO W-PERIOD-AAAA                                  
023450        MOVE 1 TO W-PERIOD-P                                              
023460        PERFORM UNTIL W-PERIOD-P > 8                                      
023470           MOVE 1 TO IX                                                   
023480           PERFORM UNTIL IX > MAX-ANT-PERIODER                            
023490               IF  W-PERIOD-AAAAP = W-SISTA-PER-AAAAP (IX)                
023491                   MOVE IX TO IX-UT                                       
023492                   MOVE JA TO SW-ORDING-ARTIKEL                           
023493                   PERFORM CA-UPPDAT-OT                                   
023494                   IF IX < 9                                              
023495                      PERFORM CB-UPPDAT-OI                                
023496                   END-IF                                                 
023497                   MOVE MAX-ANT-PERIODER TO IX                            
023499               END-IF                                                     
023500               ADD 1 TO IX                                                
023501           END-PERFORM                                                    
023502           ADD +1 TO W-PERIOD-P                                           
023503        END-PERFORM                                                       
023504     END-IF                                                               
023505     .                                                                    
023506     EJECT                                                                
023507 CA-UPPDAT-OT   SECTION.                                                  
023508                                                                          
023527     MOVE PER-VECKA-FOM (W-PERIOD-P) TO IX-W                              
023528                                                                          
023529     PERFORM UNTIL IX-W > PER-VECKA-TOM (W-PERIOD-P)                      
023530                                                                          
023531       ADD AAR-KVOT-PROG (IX-W) TO U21-OTRA-PROGNOSPAV-C1 (IX-UT)         
023532       ADD AAR-KVOT-DIV  (IX-W) TO U21-OTRA-DIVERSE-C1    (IX-UT)         
023533                                                                          
023534       ADD +1 TO IX-W                                                     
023535                                                                          
023536     END-PERFORM                                                          
023537     .                                                                    
023538     EJECT                                                                
023539 CB-UPPDAT-OI   SECTION.                                                  
023540                                                                          
023550     MOVE PER-VECKA-FOM (W-PERIOD-P) TO IX-W                              
023560     PERFORM UNTIL IX-W > PER-VECKA-TOM (W-PERIOD-P)                      
023570                                                                          
023580       ADD AAR-KVOI-PROG (IX-W)  TO U21-OING-PROGNOSPAV-C1 (IX-UT)        
023590       ADD AAR-KVOI-DIV  (IX-W)  TO U21-OING-DIVERSE-C1    (IX-UT)        
023600       ADD AAR-KVOI-SATS (IX-W)  TO U21-OING-SATS-C1       (IX-UT)        
023700       ADD AAR-KVOI-SDC  (IX-W)  TO U21-OING-SDC-C2        (IX-UT)        
023800       ADD AAR-KVOI-NDC  (IX-W)  TO U21-OING-NDC-C2        (IX-UT)        
023810       ADD AAR-KVOI-REFILL(IX-W) TO U21-OING-REFILL        (IX-UT)        
023900                                                                          
024000       ADD +1 TO IX-W                                                     
024100                                                                          
024200     END-PERFORM                                                          
028300     .                                                                    
028301     EJECT                                                                
028304******************************************************************        
028305*                                                                *        
028306*    AVSLUTNING                                                  *        
028307*    BEHANDLA SISTA OIARTIKELN                                   *        
028308*    STÄNG FIL, SKRIV UT POSTSUMS RÄKNEVERK                      *        
028309*                                                                *        
028310******************************************************************        
028311                                                                          
028312 Z-FINIT      SECTION.                                                    
028313                                                                          
028314     IF SW-ORDING-ARTIKEL = JA                                            
028315        PERFORM S01-SKRIV-U21                                             
028316     END-IF                                                               
028317                                                                          
028318     CLOSE W26121                                                         
028319                                                                          
028320     MOVE 'S' TO POSTSUM-OPKOD                                            
028321     CALL POSTSUM USING POSTSUM-PARM                                      
028322     .                                                                    
028323     EJECT                                                                
028324******************************************************************        
028325*                                                                *        
028330*    SKRIV W26121                                                *        
028340*    SKRIV POST PÅ W26121, ADDERA TILL POSTRÄKNEVERK             *        
028350*                                                                *        
028360******************************************************************        
028370                                                                          
028400 S01-SKRIV-U21 SECTION.                                                   
028500     WRITE U21-POST FROM U21-AREA                                         
028600                                                                          
028700     CALL POSTSUM USING POSTSUM-PARM                                      
028800     .                                                                    
028810     EJECT                                                                
028900 IMS-GET-WDL8 SECTION.                                                    
029000     SKIP3                                                                
029100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
029200     CALL CBLTDLI USING GN WDL8-PCB DLI-IO-AREA                           
029300     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
029400     PERFORM IMS-STATUSKONTROLL                                           
029500     .                                                                    
029600 IMS-STATUSKONTROLL SECTION.                                              
029700     SKIP3                                                                
029800     SET STATUS-IX TO 1                                                   
029900     SEARCH GODK-STATUS AT END CALL FELLOG                                
030000       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
030100       CONTINUE                                                           
030200     END-SEARCH                                                           
030300     .                                                                    
