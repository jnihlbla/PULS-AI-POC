000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5120200.                                                
000300                                                                          
000400 AUTHOR.         KARL JOHAN HANSSON.                                      
000500 DATE-WRITTEN.   MARS     1995.                                           
000600                                                                          
000700*    REMARKS.                                                             
000800*      FUNKTION: SKAPAR FIL MED TOTAL KALKYLPÅLÄGGET ALLA DC              
000900*                                                                         
001000*        INDATA  ARTIKELFILER ÖVER WDK6 WDK7 SALDO OCH PÅLÄGG             
001100*                                                                         
001200*        UTDATA  FIL W51202 MED 4 TOTALER                                 
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800     SELECT  W51054 ASSIGN TO UT-S-W51202D1.                              
001900     SELECT  W01160 ASSIGN TO UT-S-W51202D2.                              
002000     SELECT  W51202 ASSIGN TO UT-S-W51202D3.                              
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400     SKIP2                                                                
002500 FD  W51054                                                               
002600     RECORDING F                                                          
002700     BLOCK CONTAINS 0.                                                    
002800                                                                          
002900*01  -COPY W51054 -L.                                                     
003000     SKIP3                                                                
003100 FD  W01160                                                               
003200     RECORDING F                                                          
003300     BLOCK CONTAINS 0.                                                    
003400                                                                          
003500*01  -COPY W01160 -L.                                                     
003600     SKIP2                                                                
003700 FD  W51202                                                               
003800     RECORDING V                                                          
003900     BLOCK CONTAINS 0.                                                    
004000                                                                          
004100 01  UT-RAD              PIC X(84).                                       
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 01  IDPGM               PIC X(8)    VALUE 'W5120200'.                    
004600                                                                          
004700 77  NEJ                 PIC X     VALUE 'N'.                             
004800 77  JA                  PIC X     VALUE 'J'.                             
004900                                                                          
005000 01  WORK-AREA.                                                           
005100     03  W-KVANT         PIC S9(5)            COMP-3.                     
005200     03  W-SUDIRL        PIC S9(9)V99         COMP-3.                     
005300     03  W-SUDMTRL       PIC S9(9)V99         COMP-3.                     
005400     03  W-SUOVRP        PIC S9(9)V99         COMP-3.                     
005500     03  W-SUMTOTAL      PIC S9(9)V99         COMP-3.                     
005600                                                                          
005700 01  W51054-EOF          PIC X         VALUE 'N'.                         
005800 01  W01160-EOF          PIC X         VALUE 'N'.                         
005900                                                                          
006000 01  GEN-SUBPROGR.                                                        
006100     03  POSTSUM         PIC X(8)      VALUE 'POSTSUM'.                   
006200                                                                          
006300 01  BLANKRAD.                                                            
006400     03  FILLER          PIC X(20)     VALUE SPACE.                       
006500                                                                          
006600 01  UTRAD.                                                               
006700     03  UT-BEN          PIC X(20)     VALUE SPACE.                       
006800     03  UT-SUMMA        PIC -Z(9)9.99 VALUE ZERO.                        
006900                                                                          
007000 01  UTRAD2.                                                              
007100     03  FILLER          PIC X(33)     VALUE                              
007200          'MÅNADSSLUTETS TOTALA KALKYLPÅLÄGG'.                            
007300                                                                          
007400*    -COPY W0005     -PRE POSTSUM-.                                       
007500 01  W51054-TRANSID.                                                      
007600     03  FILLER                PIC X(6)  VALUE 'W51054'.                  
007700     03  FILLER                PIC X(8)  VALUE 'W51202D1'.                
007800     03  54-TRANSTYP           PIC X(4)  VALUE '    '.                    
007900 01  W01160-TRANSID.                                                      
008000     03  FILLER                PIC X(6)  VALUE 'W01160'.                  
008100     03  FILLER                PIC X(8)  VALUE 'W51202D2'.                
008200     03  FILLER                PIC X(4)  VALUE ' 60 '.                    
008300     EJECT                                                                
008400 01  FILLER              PIC X(16) VALUE 'W51054-AREA'.                   
008500*01  AREA    -COPY W51054  -PRE 54-                                       
008600     EJECT                                                                
008700 01  FILLER              PIC X(16) VALUE 'CLAG-AREA'.                     
008800*01  CLAG-AREA -COPY W01160                                               
008900     EJECT                                                                
009000 PROCEDURE DIVISION.                                                      
009100                                                                          
009200     PERFORM A-INIT                                                       
009300                                                                          
009400     PERFORM S01-LAS-W51054                                               
009500     PERFORM S02-LAS-W01160                                               
009600     PERFORM UNTIL W51054-EOF = JA OR W01160-EOF = JA                     
009700       IF 54-IDARTNR = CLAG-IDARTNR                                       
009800         PERFORM B-SUMMERA                                                
009900         PERFORM S01-LAS-W51054                                           
010000       ELSE                                                               
010100         IF 54-IDARTNR < CLAG-IDARTNR                                     
010200           PERFORM S01-LAS-W51054                                         
010300         ELSE                                                             
010400           PERFORM S02-LAS-W01160                                         
010500         END-IF                                                           
010600       END-IF                                                             
010700     END-PERFORM                                                          
010800                                                                          
010900     PERFORM Z-FINIT                                                      
011000                                                                          
011100     MOVE +0 TO RETURN-CODE                                               
011200     GOBACK                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 A-INIT SECTION.                                                          
011600                                                                          
011700     OPEN INPUT  W51054                                                   
011800                 W01160                                                   
011900          OUTPUT W51202                                                   
012000     MOVE ZERO           TO W-KVANT                                       
012100                            W-SUDIRL                                      
012200                            W-SUDMTRL                                     
012300                            W-SUOVRP                                      
012400                                                                          
012500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012600     .                                                                    
012700     EJECT                                                                
012800 B-SUMMERA SECTION.                                                       
012900                                                                          
013000     COMPUTE W-KVANT = 54-KVAKS + 54-KVAKS-PAV + 54-KVEFRS +              
013100                           54-KVLS                                        
013200     IF W-KVANT NOT = 0                                                   
013300       COMPUTE W-SUDIRL  = W-SUDIRL  + (W-KVANT * CLAG-PRDIRLON)          
013400       COMPUTE W-SUDMTRL = W-SUDMTRL + (W-KVANT * CLAG-PRDMTRL )          
013500       COMPUTE W-SUOVRP  = W-SUOVRP  + (W-KVANT * CLAG-PROVRPAL)          
013600     END-IF                                                               
013700     .                                                                    
013800     EJECT                                                                
013900 Z-FINIT SECTION.                                                         
014000                                                                          
014100     COMPUTE W-SUMTOTAL = W-SUDIRL + W-SUDMTRL + W-SUOVRP                 
014200                                                                          
014300     WRITE UT-RAD  FROM UTRAD2                                            
014400     WRITE UT-RAD  FROM BLANKRAD                                          
014500                                                                          
014600     MOVE 'DIREKT LÖN'          TO UT-BEN                                 
014700     MOVE W-SUDIRL              TO UT-SUMMA                               
014800     WRITE UT-RAD  FROM UTRAD                                             
014900                                                                          
015000     MOVE 'DIREKT MATERIAL '    TO UT-BEN                                 
015100     MOVE W-SUDMTRL             TO UT-SUMMA                               
015200     WRITE UT-RAD  FROM UTRAD                                             
015300                                                                          
015400     MOVE 'ÖVRIGA OMKOSTNADER'  TO UT-BEN                                 
015500     MOVE W-SUOVRP              TO UT-SUMMA                               
015600     WRITE UT-RAD  FROM UTRAD                                             
015700                                                                          
015800     MOVE 'TOTALT '             TO UT-BEN                                 
015900     MOVE W-SUMTOTAL            TO UT-SUMMA                               
016000     WRITE UT-RAD  FROM UTRAD                                             
016100                                                                          
016200     MOVE 'S'   TO POSTSUM-OPKOD                                          
016300     CALL POSTSUM USING POSTSUM-PARM                                      
016400                                                                          
016500     CLOSE W51054                                                         
016600           W01160                                                         
016700           W51202                                                         
016800     .                                                                    
016900     SKIP3                                                                
017000 S01-LAS-W51054  SECTION.                                                 
017100                                                                          
017200     READ W51054 INTO 54-AREA                                             
017300     AT END                                                               
017400       MOVE JA              TO W51054-EOF                                 
017500     NOT AT END                                                           
017600       MOVE 54-IDDC         TO 54-TRANSTYP                                
017700       MOVE W51054-TRANSID  TO POSTSUM-TRANSID                            
017800       CALL POSTSUM USING POSTSUM-PARM                                    
017900     END-READ                                                             
018000     .                                                                    
018100     SKIP3                                                                
018200 S02-LAS-W01160  SECTION.                                                 
018300                                                                          
018400     READ W01160 INTO CLAG-W01160                                         
018500     AT END                                                               
018600       MOVE JA              TO W01160-EOF                                 
018700     NOT AT END                                                           
018800       MOVE W01160-TRANSID  TO POSTSUM-TRANSID                            
018900       CALL POSTSUM USING POSTSUM-PARM                                    
019000     END-READ                                                             
019100     .                                                                    
