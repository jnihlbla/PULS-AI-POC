000100 ID DIVISION.                                                             
000200 PROGRAM-ID.        W2360200.                                             
000300*AUTHOR.            ANDERSON RALPH.                                       
000400*DATE-WRITTEN.      JANUARI 1979.                                         
000500*                                                                         
000600*    REMARKS                                                              
000700*                    PROGRAMET LÄSER WDD9. AVGÖR MED  HJÄLP               
000800*                    AV DATUMKORT DE ARTIKLAR SOM HAR AVROP               
000900*                    INOM AKTUELLT TIDSINTERVALL MED AVSEEN-              
001000*                    DE PÅ LEVERANSAVVIKELSE(TID).                        
001100*                                                                         
001200     SKIP3                                                                
001300*                    PROGRAMMET ÄNDRAT TILL SB/COBOL-11 890824   G        
001400*                                                                         
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900     SELECT W23601 ASSIGN TO UT-S-W23602D1.                               
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 FILE SECTION.                                                            
002300     SKIP3                                                                
002400 FD  W23601                                                               
002500     LABEL RECORD STANDARD                                                
002600     BLOCK CONTAINS 0 RECORDS                                             
002700     RECORDING F.                                                         
002800*                                                                         
002900 01  UTTRANS.                                                             
003000     03  UT-IDARTNR          PIC S9(9)  COMP-3.                           
003100     SKIP2                                                                
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP3                                                                
003401*    -COPY WY2000W3                                                       
003410     SKIP3                                                                
003420 01  FELTEXT.                                                             
003430     03  FILLER              PIC X(8)    VALUE 'FELTEXT'.                 
003440     03  FELTEXT-STR         PIC X(72)   VALUE SPACE.                     
003500 01  PARAMETRAR-TILL-DATUMKORT.                                           
003600     03  PROGRAM-NAMN        PIC X(6)    VALUE 'W23602'.                  
003700     03  DATUMKORT-ID        PIC X(6)    VALUE 'WDATUM'.                  
003800     SKIP2                                                                
003900*    -COPY WDATKORT                                                       
004100     EJECT                                                                
004200 01  DS-IDARTNR              PIC  9(9)           VALUE ZERO.              
004201 01  WS-IDARTNR              PIC S9(9)   COMP-3  VALUE +0.                
004210 01  WS-TIAAVV               PIC 9(4)            VALUE ZERO.              
004300*                                                                         
004400 01  SKRIV-SW                PIC X       VALUE 'N'.                       
004500     88  NYTT-IDARTNR                    VALUE 'N'.                       
004600*                                                                         
004700 01  KONSTANTER.                                                          
004800     03  NY-ARTIKEL          PIC X       VALUE 'N'.                       
004900     03  KLART               PIC X       VALUE 'K'.                       
005000     SKIP2                                                                
005100 01  TIAAVV-MAX              PIC 9(5).                                    
005200 01  FILLER REDEFINES TIAAVV-MAX.                                         
005300     03  FILLER              PIC 9.                                       
005400     03  WS-MAX-AA           PIC 99.                                      
005500     03  WS-MAX-VV           PIC 99.                                      
005600     EJECT                                                                
005700 01  GENERELLA-SUBPROGRAM.                                                
005800     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
005900     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
006000     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
006100     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
006110     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
006120     EJECT                                                                
006130*01      -COPY WDATAREA.                                                  
006140     EJECT                                                                
006300     SKIP3                                                                
006400*01  -COPY W0005    -PRE POSTSUM-                                         
006600 01  IMS-WS.                                                              
006700     03 FILLER       PIC X(16) VALUE 'IMS-WS'.                            
006800*----------------------------------STATUSKODER FRÅN IMS                   
006900     03  STATUS-WS   PIC XX.                                              
007000         88  SEGMENT-FINNS       VALUE '  '.                              
007100         88  SEGMENT-SLUT        VALUE 'GB'.                              
007200                                                                          
007300     03  GODK-STATUSKODER.                                                
007400      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
007500                                                                          
007600     EJECT                                                                
007700*----------------------------------IMS-CALL FUNKTIONER                    
007800*01              -COPY W0003                                              
008000                                                                          
008100     EJECT                                                                
008200 01  IO-AREA     PIC X(150).                                              
008300*                                                                         
008400*01  A   -COPY   WDD901     -PRE WDD901-     -RED IO-AREA                 
008600*                                                                         
008700*01  B   -COPY   WDD905     -PRE WDD905-     -RED IO-AREA                 
008900     EJECT                                                                
009000 LINKAGE SECTION.                                                         
009100*01    -COPY W0008         -PRE WDD9-                                     
009300          05  FILLER      PIC   XX.                                       
009400     EJECT                                                                
009500 PROCEDURE DIVISION USING WDD9-PCB.                                       
009600     ENTRY 'CBLTDLI'  USING WDD9-PCB.                                     
009700     PERFORM S01-INIT                                                     
009800     PERFORM IMS-GET-WDD9                                                 
009900     PERFORM UNTIL SEGMENT-SLUT                                           
010000         EVALUATE WDD9-SEG-NAME-FB                                        
010100             WHEN   'WDD901  '                                            
010200                 PERFORM A-SPARA-ARTNR                                    
010300             WHEN   'WDD905  '                                            
010400                 PERFORM B-GRANSKA-AVROP                                  
010500         END-EVALUATE                                                     
010600         PERFORM IMS-GET-WDD9                                             
010700     END-PERFORM                                                          
010800     PERFORM S02-FINIT                                                    
010900     MOVE ZERO TO RETURN-CODE                                             
011000     GOBACK                                                               
011100     .                                                                    
011200     EJECT                                                                
011300 A-SPARA-ARTNR SECTION.                                                   
011400*                                                                         
011500     MOVE WDD901-IDARTNR TO WS-IDARTNR DS-IDARTNR                         
011600     MOVE NY-ARTIKEL TO SKRIV-SW                                          
011700*                                                                         
011800     .                                                                    
011900 B-GRANSKA-AVROP SECTION.                                                 
012000*                                                                         
012002     MOVE WDD905-TIAVRDAT-INL  TO DAT-I-TIDATUM                           
012003     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
012004     CALL WDATKONV USING          DAT-KDDATFORM                           
012005                                  DAT-I-TIDATUM                           
012006                                  DAT-O-TIDATUM                           
012007                                  DAT-KDSVAR                              
012008     IF DAT-KDSVAR-FEL                                                    
012009       MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT-STR                 
012010       DISPLAY FELTEXT ' ' DS-IDARTNR                                     
012011*FIX !!                                                                   
012012*      MOVE 0252               TO WS-TIAAVV                               
012013*      MOVE WS-TIAAVV          TO TMP1-YYWW                               
012014*FIX !!  SKIP FELLOG                                                      
012015       CALL FELLOG                                                        
012016     ELSE                                                                 
012017       MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                               
012018       MOVE WS-TIAAVV          TO TMP1-YYWW                               
012019     END-IF                                                               
012020     MOVE TIAAVV-MAX           TO TMP2-YYWW                               
012030     PERFORM WY2000P3                                                     
012100     IF NYTT-IDARTNR AND                                                  
012200         TMP1-YYWW  < TMP2-YYWW                                           
012300                 WRITE UTTRANS  FROM WS-IDARTNR                           
012400                 CALL POSTSUM USING POSTSUM-PARM                          
012500                 MOVE KLART TO SKRIV-SW                                   
012600     END-IF                                                               
012700                                                                          
012800     .                                                                    
012900 S01-INIT SECTION.                                                        
013000*                                                                         
013100     OPEN OUTPUT W23601                                                   
013200*                                                                         
013300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
013400*                                                                         
013500     MOVE D-AAR TO WS-MAX-AA                                              
013600     MOVE D-VECKA TO WS-MAX-VV                                            
013700     ADD 1 TO WS-MAX-VV                                                   
013800*                                                                         
013900     MOVE 'W23602D1' TO POSTSUM-DDNAMN2                                   
014000     MOVE 'W23601  ' TO POSTSUM-FDNAMN                                    
014100     MOVE SPACE      TO POSTSUM-TRANSID                                   
014200*                                                                         
014300     .                                                                    
014400 S02-FINIT SECTION.                                                       
014500*                                                                         
014600     MOVE 'S' TO POSTSUM-OPKOD                                            
014700     CALL POSTSUM USING POSTSUM-PARM                                      
014800*                                                                         
014900     CLOSE W23601                                                         
015000     .                                                                    
015100 IMS-GET-WDD9 SECTION.                                                    
015200     SKIP3                                                                
015300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015400     CALL CBLTDLI USING GN WDD9-PCB IO-AREA                               
015500     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
015600     PERFORM IMS-STATUSKONTROLL                                           
015700     .                                                                    
015800 IMS-STATUSKONTROLL SECTION.                                              
015900     SKIP3                                                                
016000     SET STATUS-IX TO 1                                                   
016100     SEARCH GODK-STATUS AT END CALL FELLOG                                
016200       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
016300       CONTINUE                                                           
016400     END-SEARCH                                                           
016500     .                                                                    
016510     EJECT                                                                
016600*    -COPY WY2000P3                                                       
