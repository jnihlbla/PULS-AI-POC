000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1211200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   JUNI 1986.                                               
000500         REMARKS.                                                         
000600*        PROGRAMMET LÄSER IGENOM WDD3 OCH SKRIVER EN POST                 
000700*        FÖR VARJE BENÄMNINGSNUMMER MED KDBENSTAT 1, 2 ELLER              
000800*        3 SOM SAKNAR ARTIKEL-SEGMENT.                                    
000900*                                                                         
001000*        ÄNDRAT TILL SB/COBOL-II    880803       G.ERIKSSON               
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.                                                            
001500         SELECT W12113         ASSIGN TO UT-S-W12112D1.                   
001600                                                                          
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900 FILE SECTION.                                                            
002000 FD  W12113                                                               
002100     LABEL RECORD STANDARD                                                
002200     RECORDING MODE V                                                     
002300     BLOCK CONTAINS 0 RECORDS.                                            
002400*01  UT-POST   -COPY W12113               -L.                             
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  PROGRAM-NAMN    PIC X(8)   VALUE '1211200'.                          
003100 77  JA              PIC X(1)   VALUE 'J'.                                
003200 77  NEJ             PIC X(1)   VALUE 'N'.                                
003300 77  FLSKRIV         PIC X(1)   VALUE 'N'.                                
003310                                                                          
003320 01  SPAR-FOM-DATUM           PIC 9(6).                                   
003400                                                                          
003500 01  DYNAMISKA-SUBPGM.                                                    
003600     03  POSTSUM     PIC X(8)    VALUE 'POSTSUM '.                        
003700     03  CBLTDLI     PIC X(8)    VALUE 'CBLTDLI '.                        
003800     03  FELLOG      PIC X(8)    VALUE 'FELLOG'.                          
003810     03  WZ20DAYS    PIC X(8)    VALUE 'WZ20DAYS'.                        
003811                                                                          
003820 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
003830*   -COPY WZ20DAYS                                                        
003900     EJECT                                                                
004000 01  FILLER          PIC X(24)   VALUE 'UT-AREA-START'.                   
004100                                                                          
004200*01  AREA    -COPY W12113       -PRE UT-.                                 
004300     EJECT                                                                
004400*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
004500*                                                                         
004600*01    -COPY W0005           -PRE POSTSUM-                                
004700     EJECT                                                                
004800 01  IMS-WS.                                                              
004900     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
005000*----------------------------------STATUSKODER FRÅN IMS                   
005100     03  STATUS-WS   PIC XX.                                              
005200         88  SEGMENT-FINNS       VALUE '  '.                              
005300         88  SEGMENT-SLUT        VALUE 'GB'.                              
005400                                                                          
005500     03  GODK-STATUSKODER.                                                
005600      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
005700                                                                          
005800     EJECT                                                                
005900*----------------------------------IMS-CALL FUNKTIONER                    
006000*01  -COPY W0003                                                          
006100     EJECT                                                                
006200 01  DLI-IO-AREA.                                                         
006300     03  IO-AREA            PIC X(100).                                   
006400*03  WLBENA01    -COPY WDD301        -RED IO-AREA.                        
006410*03  WLBENA11    -COPY WDD311        -RED IO-AREA.                        
006500     EJECT                                                                
006630 LINKAGE SECTION.                                                         
006700                                                                          
006800*01    -COPY W0008      -PRE WDD3-                                        
006900          05  FILLER  PIC XX.                                             
007000     EJECT                                                                
007100 PROCEDURE DIVISION USING  WDD3-PCB.                                      
007200     ENTRY 'CBLTDLI' USING WDD3-PCB.                                      
007300     PERFORM A-INIT                                                       
007400     PERFORM IMS-GET-WDD3                                                 
007500     PERFORM UNTIL SEGMENT-SLUT                                           
007600         EVALUATE WDD3-SEG-NAME-FB                                        
007700            WHEN 'WDD301  '                                               
007800                   PERFORM B-SKRIV-FOREGAENDE-IDBENNR                     
007900                   IF BEN-TIUPPDAT-STOP < SPAR-FOM-DATUM                  
008000                      PERFORM C-SPARA-IDBENNR                             
008100                      MOVE JA TO FLSKRIV                                  
008200                   END-IF                                                 
008300                                                                          
008400            WHEN 'WDD311  '                                               
008500                   IF TEXT-IDSKYLT = 'S '                                 
008501                      MOVE ';'        TO UT-BENA-BEART(1:1)               
008502                      MOVE TEXT-BEART TO UT-BENA-BEART(2:24)              
008503                   END-IF                                                 
008510                                                                          
008520            WHEN 'WDD312  '                                               
008530                   MOVE NEJ TO FLSKRIV                                    
008600                                                                          
008700         END-EVALUATE                                                     
008800         PERFORM IMS-GET-WDD3                                             
008900     END-PERFORM                                                          
009000     PERFORM B-SKRIV-FOREGAENDE-IDBENNR                                   
009100     PERFORM Z-FINIT                                                      
009200     MOVE ZERO TO RETURN-CODE                                             
009300     GOBACK                                                               
009400     .                                                                    
009500     EJECT                                                                
009600 A-INIT SECTION.                                                          
009700                                                                          
009800     OPEN OUTPUT W12113                                                   
009900                                                                          
010000     MOVE PROGRAM-NAMN    TO POSTSUM-PROGNAMN                             
010100                                                                          
010200     MOVE ZERO            TO UT-BENA-IDBENNR                              
010210     MOVE SPACE           TO UT-BENA-BEART                                
010300                                                                          
010301*    BERÄKNA DATUM 6 MÅNADER BAKÅT RENSNINGSGRÄNS                         
010310     ACCEPT SPAR-FOM-DATUM FROM DATE                                      
010320                                                                          
010330     MOVE  'YYMMDD'            TO DAYS-KDDATFMT1                          
010340     MOVE  'YYMMDD'            TO DAYS-KDDATFMT2                          
010350     MOVE   SPAR-FOM-DATUM     TO DAYS-TIDATE1                            
010351     MOVE  -180                TO DAYS-KVDAYS                             
010360     MOVE   SPACE              TO DAYS-TIDATE2                            
010370                                  DAYS-IDCALEND                           
010380     CALL     WZ20DAYS USING DAYS-WZ20DAYS                                
010390                                                                          
010391     MOVE     DAYS-TIDATE2(1:6)          TO SPAR-FOM-DATUM                
010400     .                                                                    
010500     EJECT                                                                
010600 B-SKRIV-FOREGAENDE-IDBENNR SECTION.                                      
010700                                                                          
010800     IF FLSKRIV = JA                                                      
010900        WRITE UT-POST FROM UT-AREA                                        
011000        MOVE NEJ TO FLSKRIV                                               
011100                                                                          
011200        MOVE 'W12112'        TO POSTSUM-FDNAMN                            
011300        MOVE 'W12112D2'      TO POSTSUM-DDNAMN2                           
011400        MOVE SPACE           TO POSTSUM-TRANSTYP                          
011500        CALL POSTSUM USING POSTSUM-PARM                                   
011600     END-IF                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 C-SPARA-IDBENNR SECTION.                                                 
012000                                                                          
012100     MOVE BEN-IDBENNR TO UT-BENA-IDBENNR                                  
012110     MOVE SPACE       TO UT-BENA-BEART                                    
012200     .                                                                    
012300     EJECT                                                                
012400 Z-FINIT SECTION.                                                         
012500                                                                          
012600     CLOSE W12113                                                         
012700                                                                          
012800     MOVE 'S'             TO POSTSUM-OPKOD                                
012900     CALL POSTSUM USING POSTSUM-PARM                                      
013000     EJECT                                                                
013100     .                                                                    
013200 IMS-GET-WDD3 SECTION.                                                    
013300     SKIP3                                                                
013400     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
013500     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-AREA                           
013600     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
013700     PERFORM IMS-STATUSKONTROLL                                           
013800     .                                                                    
013900 IMS-STATUSKONTROLL SECTION.                                              
014000     SKIP3                                                                
014100     SET STATUS-IX TO 1                                                   
014200     SEARCH GODK-STATUS AT END CALL FELLOG                                
014300       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
014400       CONTINUE                                                           
014500     END-SEARCH                                                           
014600     .                                                                    
