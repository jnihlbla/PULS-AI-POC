000100*********************************************                             
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4765300.                                                
000400 AUTHOR.         MOGREN STINA.                                            
000500 DATE-WRITTEN.   02/09/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        INGÅR I SOP-RUTIN W476D5                                         
001000*        PROGRAMMET LÄSER W47653 , SORTERADE POSTER                       
001100*        POSTER TILL VIPS W476RIK/RIL/RIM/RIN/RIO/RIZ/RIP                 
001200*        MED INFO FRÅN WDE4 BL.A.                                         
001300*        LIKA POSTER PLOCKAS BORT                                         
001400*                                                                         
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- POSTER FRÅN W4766800 SOM SKA DUBLETTKOLLAS                 
002500     SELECT W47651                     ASSIGN TO W47653D1.                
002600     SKIP2                                                                
002700*          --- DUBLETTRENSAD UTFIL                                        
002800     SELECT W47653                     ASSIGN TO W47653D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W47651                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W4765201     -L.                                               
003900     SKIP3                                                                
004000 FD  W47653                                                               
004100     RECORDING       V                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  POST   -COPY W461RIK1   -PRE RIK-  -L.                               
004400*01  POST   -COPY W461RILN   -PRE RIL-  -L.                               
004500*01  POST   -COPY W461RIM2   -PRE RIM-  -L.                               
004600*01  POST   -COPY W461RINN   -PRE RIN-  -L.                               
004700*01  POST   -COPY W461RIO2   -PRE RIO-  -L.                               
004800*01  POST   -COPY W461RIPN   -PRE RIP-  -L.                               
004900*01  POST   -COPY W461RIZN   -PRE RIZ-  -L.                               
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W4765300'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700 77  W47651-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W47651                       VALUE 'J'.                   
005900     EJECT                                                                
006000 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
006100                                                                          
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETRAR TILL ABEND                                            
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400*    --NYCKLAR FÖR HOP-SORTERING                                          
007500 01  W-IDPTYP                   PIC X(3)   VALUE SPACE.                   
007600 01  W-IDFAKT                   PIC S9(7)  VALUE ZERO  COMP-3.            
007700 01  W-IDDISTR                  PIC S9(5)  VALUE ZERO  COMP-3.            
007800 01  W-IDKUNDNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
007900 01  W-IDORDER                  PIC S9(7)  VALUE ZERO  COMP-3.            
008000 01  W-IDKUNDNR-S               PIC S9(7)  VALUE ZERO  COMP-3.            
008100 01  W-IDPRODNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
008200 01  W-IDKOLLI                  PIC  9(5)  VALUE ZERO.                    
008300 01  W-IDPURAD                  PIC S9(5)  VALUE ZERO  COMP-3.            
008400                                                                          
008500 01  TEST-IDDISTR               PIC 9(5) COMP-3.                          
008600*01  FILLER  -COPY   WWDIST07   -RED TEST-IDDISTR.                        
008700     EJECT                                                                
008800*01  FILLER  -COPY   WWDIST35   -RED TEST-IDDISTR.                        
008900     EJECT                                                                
009000                                                                          
009100*    --- PARAMETRAR TILL POSTSUM                                          
009200*                                                                         
009300*01  -COPY W0005   -PRE  POSTSUM-                                         
009400     EJECT                                                                
009500 01  IN-AREA-START               PIC X(16)   VALUE                        
009600                                 'IN-AREA-START  '.                       
009700 01  IN-AREA.                                                             
009800*    03  -COPY W4765201                                                   
009900     EJECT                                                                
010000 01  UT-AREA-START               PIC X(16)   VALUE                        
010100                                 'UT-AREA-START  '.                       
010200 01  FILLER.                                                              
010300 03  UT-AREA                     PIC X(500) VALUE SPACE.                  
010400*    OBS SKALL INNEHÅLLA LÄNGDEN PÅ LÄNGSTA COPYTEXTEN NEDAN              
010500*    FÖR TILLFÄLLET W461RIO2 211 LÅNG                                     
010600*    LÄNGDEN ÄR TILLTAGEN FÖR ATT SLIPPA FREKVENTA ÄNDRINGAR              
010700     SKIP2                                                                
010800*03  -COPY W461RIK1   -RED UT-AREA                                        
010900*                                                                         
011000*03  -COPY W461RILN   -RED UT-AREA                                        
011100*                                                                         
011200*03  -COPY W461RIM2   -RED UT-AREA                                        
011300*                                                                         
011400*03  -COPY W461RINN   -RED UT-AREA                                        
011500*                                                                         
011600*03  -COPY W461RIPN   -RED UT-AREA                                        
011700*                                                                         
011800*03  -COPY W461RIO2   -RED UT-AREA                                        
011900*                                                                         
012000*03  -COPY W461RIZN   -RED UT-AREA                                        
012100     EJECT                                                                
012200 PROCEDURE DIVISION.                                                      
012300 MAIN SECTION.                                                            
012400                                                                          
012500                                                                          
012600     PERFORM A-INIT                                                       
012700                                                                          
012800     PERFORM S01-LAES-W47651                                              
012900     IF NOT END-OF-W47651                                                 
013000       MOVE RJX-FILLER         TO UT-AREA                                 
013100       PERFORM S11-SKRIV-W47653                                           
013200       PERFORM S10-SPARA-ID                                               
013300     END-IF                                                               
013400     PERFORM UNTIL END-OF-W47651                                          
013500                                                                          
013600                                                                          
013700       PERFORM D-TESTA-SKAPA-POSTER                                       
013800                                                                          
013900                                                                          
014000       PERFORM S01-LAES-W47651                                            
014100     END-PERFORM                                                          
014200                                                                          
014300                                                                          
014400     PERFORM Z-FINIT                                                      
014500                                                                          
014600     MOVE ZERO TO RETURN-CODE                                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN INPUT  W47651                                                   
015300     OPEN OUTPUT W47653                                                   
015400                                                                          
015500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015700     EJECT                                                                
015800 D-TESTA-SKAPA-POSTER  SECTION.                                           
015900                                                                          
016000     IF W-IDFAKT     = RJX-IDFAKT     AND                                 
016100        W-IDDISTR    = RJX-IDDISTR    AND                                 
016200        W-IDKUNDNR   = RJX-IDKUNDNR   AND                                 
016300        W-IDPRODNR   = RJX-IDPRODNR   AND                                 
016400        W-IDKOLLI    = RJX-IDKOLLI    AND                                 
016500        W-IDORDER    = RJX-IDORDER    AND                                 
016600        W-IDKUNDNR-S = RJX-IDKUNDNR-S AND                                 
016700        W-IDPURAD    = RJX-IDPURAD    AND                                 
016800        W-IDPTYP     = RJX-IDPTYP                                         
016900                                                                          
017000       MOVE W-IDDISTR TO TEST-IDDISTR                                     
017100       IF DIST07-MEXICO               OR                                  
017200          DIST07-MEXICO-RET-DISCR     OR                                  
017300          DIST35-MX-CDC-RETURNS       OR                                  
017310          DIST07-BRAZIL               OR                                  
017320          DIST07-BRAZIL-RET-DISCR     OR                                  
017330          DIST35-BR-CDC-RETURNS                                           
017400*RC IS IT OK TO ADD BR HERE??                                             
017410*                                                                         
017500*  ADD HERE ANY OTHER FUTURE DELIVERIES FROM DC.53                        
017600*                                                                         
017700         IF RJX-IDPTYP = 'RIO'                                            
017800           MOVE RJX-FILLER          TO UT-AREA                            
017900           PERFORM S11-SKRIV-W47653                                       
018000         ELSE                                                             
018100           CONTINUE                                                       
018200         END-IF                                                           
018300       ELSE                                                               
018400         CONTINUE                                                         
018500       END-IF                                                             
018600     ELSE                                                                 
018700        MOVE RJX-FILLER             TO UT-AREA                            
018800        PERFORM S11-SKRIV-W47653                                          
018900        PERFORM S10-SPARA-ID                                              
019000     END-IF                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 Z-FINIT SECTION.                                                         
019400     CLOSE W47651                                                         
019500           W47653                                                         
019600                                                                          
019700     MOVE 'S' TO POSTSUM-OPKOD                                            
019800     CALL POSTSUM USING POSTSUM-PARM                                      
019900     .                                                                    
020000     EJECT                                                                
020100 S01-LAES-W47651  SECTION.                                                
020200     READ W47651 INTO IN-AREA                                             
020300     AT END                                                               
020400        SET END-OF-W47651 TO TRUE                                         
020500                                                                          
020600     NOT AT END                                                           
020700        MOVE 'W47653'       TO POSTSUM-FDNAMN                             
020800        MOVE 'W47653D1'     TO POSTSUM-DDNAMN2                            
020900        MOVE RJX-IDPTYP     TO POSTSUM-TRANSTYP                           
021000        CALL POSTSUM USING POSTSUM-PARM                                   
021100     END-READ                                                             
021200     .                                                                    
021300     EJECT                                                                
021400 S10-SPARA-ID  SECTION.                                                   
021500                                                                          
021600     MOVE RJX-IDFAKT                 TO W-IDFAKT                          
021700     MOVE RJX-IDDISTR                TO W-IDDISTR                         
021800     MOVE RJX-IDKUNDNR               TO W-IDKUNDNR                        
021900     MOVE RJX-IDPRODNR               TO W-IDPRODNR                        
022000     MOVE RJX-IDKOLLI                TO W-IDKOLLI                         
022100     MOVE RJX-IDORDER                TO W-IDORDER                         
022200     MOVE RJX-IDKUNDNR-S             TO W-IDKUNDNR-S                      
022300     MOVE RJX-IDPURAD                TO W-IDPURAD                         
022400     MOVE RJX-IDPTYP                 TO W-IDPTYP                          
022500     .                                                                    
022600     EJECT                                                                
022700 S11-SKRIV-W47653 SECTION.                                                
022800                                                                          
022900     EVALUATE RJX-IDPTYP                                                  
023000       WHEN 'RIK'                                                         
023100         WRITE RIK-POST   FROM RIK-W461RIK1                               
023200       WHEN 'RIL'                                                         
023300         WRITE RIL-POST   FROM RIL-W461RILN-CTX                           
023400       WHEN 'RIM'                                                         
023500         WRITE RIM-POST   FROM RIM-W461RIM2-CTX                           
023600       WHEN 'RIN'                                                         
023700         WRITE RIN-POST   FROM RIN-W461RINN-CTX                           
023800       WHEN 'RIO'                                                         
023900         WRITE RIO-POST   FROM RIO-W461RIO2                               
024000       WHEN 'RIZ'                                                         
024100         WRITE RIP-POST   FROM RIP-W461RIPN-CTX                           
024200*   RIP O RIZ HAR BYTT PLATS I SORT.NYCKELN FÖR RÄTT SORTERAT             
024300       WHEN 'RIP'                                                         
024400         WRITE RIZ-POST   FROM RIZ-W461RIZN-CTX                           
024500     END-EVALUATE                                                         
024600                                                                          
024700     MOVE RIO-IDPTYP        TO POSTSUM-TRANSTYP                           
024800     MOVE 'W47653'          TO POSTSUM-FDNAMN                             
024900     MOVE 'W47653D2'        TO POSTSUM-DDNAMN2                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
