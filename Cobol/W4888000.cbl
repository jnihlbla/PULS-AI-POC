000300                                                                          
000400 ID DIVISION.                                                             
000500                                                                          
000600 PROGRAM-ID.      W4888000.                                               
001000*AUTHOR.          T JOHANSSON.                                            
001100*DATE-WRITTEN.    AUG 1985.                                               
001200                                                                          
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700                                                                          
001800*        PROGRAMMET LÄSER IGENOM SALDOBASEN (WDD8) OCH                    
001900*        SKRIVER EN POST FÖR VARJE SEGMENT.                               
002000*        OMGJORT TILL SB MARS 1987 LASSE C.                               
002100                                                                          
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900*- - - - - - - - - - - - - - UTFIL:                                       
003000     SELECT W48881                       ASSIGN TO UT-S-W48880D1.         
003100     EJECT                                                                
003110     SELECT W48882                       ASSIGN TO UT-S-W48880D2.         
003120     EJECT                                                                
003200 DATA DIVISION.                                                           
003300                                                                          
003400 FILE SECTION.                                                            
003500 FD  W48881                                                               
003600     RECORDING      F                                                     
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900*01  POST  -COPY W48881     -PRE W48881- -L.                              
004100     EJECT                                                                
004110 FD  W48882                                                               
004120     RECORDING      F                                                     
004130     BLOCK CONTAINS 0.                                                    
004140                                                                          
004150*01  POST  -COPY W48882     -PRE W48882- -L.                              
004160     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004201*    -- CHECKED BY WY2000                                                 
004210     SKIP3                                                                
004300 77  IDPGM                       PIC X(8)    VALUE 'W4888000'.            
005100                                                                          
005200*- - - - - - - - - - - - - - GENERELLA KONSTANTER                         
005300                                                                          
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006100     EJECT                                                                
006200                                                                          
006300 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
006400                                                                          
006500 01  IMS-WS.                                                              
006600                                                                          
006700   03  STATUS-WS                 PIC X(2).                                
006800      88  SEGMENT-FINNS                      VALUE '  '.                  
006900      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
007000      88  SEGMENT-SLUT                       VALUE 'GB'.                  
007100                                                                          
007200   03 GODK-STATUSKODER.                                                   
007300      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
007400                                                                          
007500     EJECT                                                                
007600*                         PARAMETRAR TILL POSTSUM                         
007700                                                                          
007800*          -COPY W0005      -PRE POSTSUM-                                 
008000     EJECT                                                                
008100*          -COPY W0003                                                    
008300     EJECT                                                                
008400 01  FILLER                      PIC X(24)  VALUE                         
008500                                            'UTSP01-AREA'.                
008600                                                                          
008700*01  AREA  -COPY W48881     -PRE UT-                                      
008900     EJECT                                                                
009000                                                                          
009010*01  AREA  -COPY W48882     -PRE UT1-                                     
009020     EJECT                                                                
009030                                                                          
009100 01  IO-AREA.                                                             
009200   03 IO-AREA1                   PIC X(100).                              
009300                                                                          
009400*  03  WDD801 -PRE  ARTD-   -COPY WDD801     -RED IO-AREA1.               
009600     EJECT                                                                
009700*  03  WDD811 -PRE  ARTD-   -COPY WDD811     -RED IO-AREA1.               
009900     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100                                                                          
010200*01 -COPY W0008      -PRE SALDO-                                          
010400       05 FILLER                  PIC X(1).                               
010500     EJECT                                                                
010600 PROCEDURE DIVISION USING SALDO-PCB.                                      
010700     ENTRY 'DLITCBL' USING SALDO-PCB.                                     
010800                                                                          
010900     PERFORM A-INIT                                                       
011000     PERFORM IMS-GET-WDD8                                                 
011100                                                                          
011200     PERFORM UNTIL SEGMENT-SLUT                                           
011400       EVALUATE SALDO-SEG-NAME-FB                                         
011500       WHEN 'WDD801'                                                      
011600         MOVE ARTD-ART-IDARTNR TO UT-IDARTNR                              
011610                                  UT1-IDARTNR                             
011700       WHEN 'WDD811'                                                      
011800         MOVE ARTD-SALDO-IDDC       TO UT-IDDC                            
011810                                       UT1-IDDC                           
011900         MOVE ARTD-SALDO-ADBUFFOMR  TO UT-ADBUFFOMR                       
011901                                       UT1-ADBUFFOMR                      
011910         MOVE ARTD-SALDO-DABUFPAF   TO UT-DABUFPAF                        
011920                                       UT1-DABUFPAF                       
012000         MOVE ARTD-SALDO-ADBUFFGANG TO UT-ADBUFFGANG                      
012010                                       UT1-ADBUFFGANG                     
012100         MOVE ARTD-SALDO-ADBUFFPL   TO UT-ADBUFFPL                        
012110                                       UT1-ADBUFFPL                       
012200         MOVE ARTD-SALDO-KVBUFF-F   TO UT-KVBUFF-F                        
012210                                       UT1-KVBUFF-F                       
012300         MOVE ARTD-SALDO-KVBUFF-OF  TO UT-KVBUFF-OF                       
012310                                       UT1-KVBUFF-OF                      
012400         MOVE ARTD-SALDO-KVKOLLI-F  TO UT-KVKOLLI-F                       
012410                                       UT1-KVKOLLI-F                      
012500         MOVE ARTD-SALDO-KVKOLLI-OF TO UT-KVKOLLI-OF                      
012510                                       UT1-KVKOLLI-OF                     
012600         MOVE ARTD-SALDO-KDBRIST    TO UT-KDBRIST                         
012610                                       UT1-KDBRIST                        
012700         MOVE ARTD-SALDO-KDPAF      TO UT-KDPAF                           
012800                                       UT1-KDPAF                          
012900         PERFORM S01-SKRIV-UT                                             
012910         PERFORM S02-SKRIV-UT1                                            
013000       END-EVALUATE                                                       
013100       PERFORM IMS-GET-WDD8                                               
013200     END-PERFORM                                                          
013300     PERFORM Z-FINIT                                                      
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     CONTINUE.                                                            
013700     EJECT                                                                
013800                                                                          
013900 A-INIT SECTION.                                                          
014000                                                                          
014100     OPEN OUTPUT W48881                                                   
014110                 W48882                                                   
014200                                                                          
014300     MOVE '081' TO UT-IDPTYP                                              
014310                   UT1-IDPTYP                                             
014400                                                                          
014500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014600     CONTINUE.                                                            
014700     EJECT                                                                
014800 S01-SKRIV-UT SECTION.                                                    
014900                                                                          
015000     WRITE W48881-POST FROM UT-W48881                                     
015100                                                                          
015200     MOVE 'W48881'   TO POSTSUM-FDNAMN                                    
015300     MOVE 'W48880D1' TO POSTSUM-DDNAMN2                                   
015400     MOVE UT-IDPTYP  TO POSTSUM-TRANSTYP                                  
015500                                                                          
015600     CALL POSTSUM USING POSTSUM-PARM                                      
015700     CONTINUE.                                                            
015800     EJECT                                                                
015810 S02-SKRIV-UT1 SECTION.                                                   
015820                                                                          
015830     WRITE W48882-POST FROM UT1-W48882                                    
015840                                                                          
015850     MOVE 'W48882'   TO POSTSUM-FDNAMN                                    
015860     MOVE 'W48880D2' TO POSTSUM-DDNAMN2                                   
015870     MOVE UT1-IDPTYP  TO POSTSUM-TRANSTYP                                 
015880                                                                          
015890     CALL POSTSUM USING POSTSUM-PARM                                      
015891     CONTINUE.                                                            
015892     EJECT                                                                
015900 Z-FINIT SECTION.                                                         
016000                                                                          
016100     CLOSE W48881                                                         
016110           W48882                                                         
016200                                                                          
016300     MOVE 'S' TO POSTSUM-OPKOD                                            
016400                                                                          
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600     CONTINUE.                                                            
016700     EJECT                                                                
016800 IMS-GET-WDD8 SECTION.                                                    
016900                                                                          
017000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
017100     CALL CBLTDLI USING GN SALDO-PCB IO-AREA                              
017200     MOVE SALDO-STATUS-CODE TO STATUS-WS                                  
017300     PERFORM IMS-STATUSKONTROLL                                           
017400     CONTINUE.                                                            
017500                                                                          
017600 IMS-STATUSKONTROLL SECTION.                                              
017700                                                                          
017800     SET STATUS-IX TO 1                                                   
017900     SEARCH GODK-STATUS AT END CALL FELLOG                                
018000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
018100     END-SEARCH                                                           
018200     CONTINUE                                                             
018300            CONTINUE.                                                     
018400 IMS-STATUSKONTROLL-EXIT. EXIT.                                           
018500     CONTINUE.                                                            
