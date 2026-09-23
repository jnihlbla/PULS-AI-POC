000300                                                                          
000400 ID DIVISION.                                                             
000500                                                                          
000600 PROGRAM-ID.      W4883200.                                               
001000*AUTHOR.          E RINGQVIST.                                            
001100*DATE-WRITTEN.    MAJ 1984.                                               
001200                                                                          
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700                                                                          
001800*        PROGRAMMET LÄSER IGENOM SALDOBASEN (WDD8) OCH                    
001900*        SKRIVER EN POST FÖR VARJE SEGMENT SOM ÄR EN HÖGLAGER-            
002000*        ARTIKEL. D.V.S ATT BUFFERTADRESSEN ÄR 1 FÖR ADBUFFOMR.           
002100*        OMGJORT TILL SB MARS 1987 LASSE C.                               
002200                                                                          
002300     EJECT                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900                                                                          
003000*- - - - - - - - - - - - - - UTFIL:                                       
003100     SELECT W48833-UT                    ASSIGN TO UT-S-W48832D1.         
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600 FD  W48833-UT                                                            
003700     RECORDING      F                                                     
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  POST  -COPY W488031    -PRE W48833- -L.                              
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4883200'.            
005200                                                                          
005300*- - - - - - - - - - - - - - GENERELLA KONSTANTER                         
005400                                                                          
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005710*      --- VALID IDDC CODES                                               
005720*                                                                         
005730*01    -COPY WWDC99                                                       
005740       EJECT                                                              
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006200     EJECT                                                                
006300                                                                          
006400                                                                          
006500 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
006600                                                                          
006700 01  IMS-WS.                                                              
006800                                                                          
006900   03  STATUS-WS                 PIC X(2).                                
007000      88  SEGMENT-FINNS                      VALUE '  '.                  
007100      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
007200      88  SEGMENT-SLUT                       VALUE 'GB'.                  
007300                                                                          
007400   03 GODK-STATUSKODER.                                                   
007500      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
007600                                                                          
007700     EJECT                                                                
007800*- - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                      
007900                                                                          
008000*    -COPY W0005       -PRE POSTSUM-                                      
008200     EJECT                                                                
008300*    -COPY W0003                                                          
008500     EJECT                                                                
008600 01  FILLER                      PIC X(24)  VALUE                         
008700                                            'UTSP01-AREA'.                
008800                                                                          
008900*01        -COPY W488031    -PRE UT-                                      
009100     EJECT                                                                
009200                                                                          
009300 01  FILLER                      PIC X(16)  VALUE 'IO-AREA   '.           
009400 01  IO-AREA.                                                             
009500   03 IO-AREA1                   PIC X(50).                               
009600                                                                          
009700*  03  WDD801 -PRE  ARTD-    -COPY WDD801     -RED IO-AREA1.              
009900     EJECT                                                                
010000*  03  WDD811 -PRE  ARTD-    -COPY WDD811     -RED IO-AREA1.              
010200     EJECT                                                                
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500*01 -COPY W0008        -PRE BUFF-                                         
010700      05 FILLER                  PIC X(1).                                
010800     EJECT                                                                
010900 PROCEDURE DIVISION USING BUFF-PCB.                                       
011000     ENTRY 'DLITCBL' USING BUFF-PCB.                                      
011100                                                                          
011200     PERFORM A-INIT                                                       
011300     PERFORM IMS-GET-WDD8                                                 
011400                                                                          
011500     PERFORM UNTIL SEGMENT-SLUT                                           
011700       EVALUATE BUFF-SEG-NAME-FB                                          
011800       WHEN 'WDD801'                                                      
011900         MOVE ARTD-ART-IDARTNR TO UT-IDARTNR                              
012000       WHEN 'WDD811'                                                      
012010         MOVE ARTD-SALDO-IDDC      TO WS-IDDC                             
012100         IF CDC-SE                                                        
012200           IF ARTD-SALDO-ADBUFFOMR = 1                                    
012300                AND ARTD-SALDO-ADBUFFGANG = 0                             
012400                  AND ARTD-SALDO-ADBUFFPL = 0                             
012500             IF ARTD-SALDO-KVBUFF-F > 0                                   
012600                 OR ARTD-SALDO-KVBUFF-OF > 0                              
012700               MOVE ARTD-SALDO-IDDC                                       
012800                             TO UT-IDDC                                   
012900               MOVE ARTD-SALDO-ADBUFFOMR                                  
013000                             TO UT-ADBUFFOMR                              
013100               MOVE ARTD-SALDO-KVBUFF-F                                   
013200                             TO UT-KVBUFF-F                               
013300               MOVE ARTD-SALDO-KVBUFF-OF                                  
013400                            TO UT-KVBUFF-OF                               
013500               MOVE ARTD-SALDO-KVKOLLI-F                                  
013600                             TO UT-KVKOLLI-F                              
013700               MOVE ARTD-SALDO-KVKOLLI-OF                                 
013800                            TO UT-KVKOLLI-OF                              
013900               PERFORM S01-SKRIV-UT                                       
014000             END-IF                                                       
014100           END-IF                                                         
014200         END-IF                                                           
014300       END-EVALUATE                                                       
014400       PERFORM IMS-GET-WDD8                                               
014500     END-PERFORM                                                          
014600     PERFORM Z-FINIT                                                      
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     CONTINUE.                                                            
015000     EJECT                                                                
015100                                                                          
015200 A-INIT SECTION.                                                          
015300                                                                          
015400     OPEN OUTPUT W48833-UT                                                
015500     MOVE '031' TO UT-IDPTYP                                              
015600                                                                          
015700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     CONTINUE.                                                            
015900     EJECT                                                                
016000 S01-SKRIV-UT SECTION.                                                    
016100                                                                          
016200     WRITE W48833-POST FROM UT-W488031                                    
016300                                                                          
016400     MOVE 'W48833'   TO POSTSUM-FDNAMN                                    
016500     MOVE 'W48832D1' TO POSTSUM-DDNAMN2                                   
016600     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
016700     CALL POSTSUM USING POSTSUM-PARM                                      
016800     CONTINUE.                                                            
016900     EJECT                                                                
017000 Z-FINIT SECTION.                                                         
017100                                                                          
017200     CLOSE W48833-UT                                                      
017300                                                                          
017400     MOVE 'S' TO POSTSUM-OPKOD                                            
017500     CALL POSTSUM USING POSTSUM-PARM                                      
017600     CONTINUE.                                                            
017700     EJECT                                                                
017800 IMS-GET-WDD8 SECTION.                                                    
017900                                                                          
018000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
018100     CALL CBLTDLI USING GN BUFF-PCB IO-AREA                               
018200     MOVE BUFF-STATUS-CODE TO STATUS-WS                                   
018300     PERFORM IMS-STATUSKONTROLL                                           
018400     CONTINUE.                                                            
018500                                                                          
018600 IMS-STATUSKONTROLL SECTION.                                              
018700                                                                          
018800     SET STATUS-IX TO 1                                                   
018900     SEARCH GODK-STATUS AT END CALL FELLOG                                
019000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
019100     END-SEARCH                                                           
019200     CONTINUE                                                             
019300            CONTINUE.                                                     
019400 IMS-STATUSKONTROLL-EXIT. EXIT.                                           
019500     CONTINUE.                                                            
