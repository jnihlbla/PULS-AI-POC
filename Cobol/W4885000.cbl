000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.      W4885000.                                               
000400*              PROGRAM CONVERTED BY                                       
000500*              COBOL CONVERSION AID PO 5785-ABJ                           
000600*              CONVERSION DATE 05/25/91 17:38:44.                         
000700*AUTHOR.          E RINGQVIST.                                            
000800*DATE-WRITTEN.    MAJ 1984.                                               
000900*REMARKS.                                                                 
001000*    FUNKTION:                                                            
001100*        PROGRAMMET LÄSER IGENOM SALDOBASEN (WDD8) OCH                    
001200*        SKRIVER EN POST FÖR VARJE HÖGLAGER POST.                         
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*- - - - - - - - - - - - - - UTFIL50:                                     
002200     SELECT W48850                       ASSIGN TO UT-S-W48850D2.         
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700 FD  W48850                                                               
002800     RECORDING      F                                                     
002900     BLOCK CONTAINS 0.                                                    
003000                                                                          
003100*01  POST  -COPY W4885601   -PRE W48850- -L.                              
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600     SKIP3                                                                
003700 77  PROGRAM-NAMN                PIC X(8) VALUE 'W4885000'.               
003800*- - - - - - - - - - - - - - GENERELLA KONSTANTER                         
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300*      --- VALID IDDC CODES                                               
004400*                                                                         
004500*01    -COPY WWDC99                                                       
004600       EJECT                                                              
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
004900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005100     EJECT                                                                
005200                                                                          
005300 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
005400                                                                          
005500 01  IMS-WS.                                                              
005600                                                                          
005700   03  STATUS-WS                 PIC X(2).                                
005800      88  SEGMENT-FINNS                      VALUE '  '.                  
005900      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
006000      88  SEGMENT-SLUT                       VALUE 'GB'.                  
006100                                                                          
006200   03 GODK-STATUSKODER.                                                   
006300      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
006400                                                                          
006500     EJECT                                                                
006600*- - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                      
006700                                                                          
006800*    -COPY W0005       -PRE POSTSUM-                                      
006900     EJECT                                                                
007000                                                                          
007100*    -COPY W0003                                                          
007200     EJECT                                                                
007300 01  FILLER                      PIC X(24)  VALUE                         
007400                                            'UT50-AREA'.                  
007500*01  AREA  -COPY W4885601   -PRE UT50-                                    
007600                                                                          
007700     EJECT                                                                
007800                                                                          
007900                                                                          
008000 01  IO-AREA.                                                             
008100   03 IO-AREA1                   PIC X(100).                              
008200                                                                          
008300   03  WDD801 -PRE  ARTD-   -COPY WDD801     -RED IO-AREA1.               
008400     EJECT                                                                
008500   03  WDD811 -PRE  ARTD-   -COPY WDD811     -RED IO-AREA1.               
008600     EJECT                                                                
008700                                                                          
008800 LINKAGE SECTION.                                                         
008900                                                                          
009000*01 -COPY W0008      -PRE SALDO-                                          
009100      05 FILLER                     PIC X(1).                             
009200                                                                          
009300     EJECT                                                                
009400 PROCEDURE DIVISION USING SALDO-PCB.                                      
009500     ENTRY 'DLITCBL' USING SALDO-PCB.                                     
009600                                                                          
009700     PERFORM A-INIT                                                       
009800     PERFORM IMS-GET-WDD8                                                 
009900                                                                          
010000     PERFORM UNTIL SEGMENT-SLUT                                           
010100       EVALUATE SALDO-SEG-NAME-FB                                         
010200       WHEN 'WDD801'                                                      
010300         MOVE ARTD-ART-IDARTNR TO UT50-IDARTNR                            
010400       WHEN 'WDD811'                                                      
010500         MOVE ARTD-SALDO-IDDC     TO WS-IDDC                              
010600         IF CDC-SE AND                                                    
010700            ARTD-SALDO-ADBUFFOMR = +1                                     
010800            MOVE +0        TO UT50-ADLAGOMR                               
010900                              UT50-ADGANG                                 
011000                              UT50-ADPLATS                                
011100                              UT50-KVPB-TOT                               
011200                              UT50-KVLS-CDC                               
011300                              UT50-VLARTNTO                               
011400            MOVE SPACES    TO UT50-ADINLOMR-BOA                           
011500                              UT50-BEART                                  
011600            PERFORM S02-SKRIV-W48850                                      
011700         END-IF                                                           
011800       END-EVALUATE                                                       
011900                                                                          
012000       PERFORM IMS-GET-WDD8                                               
012100     END-PERFORM                                                          
012200                                                                          
012300     PERFORM Z-FINIT                                                      
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
012800 A-INIT SECTION.                                                          
012900                                                                          
013000     OPEN OUTPUT W48850                                                   
013100                                                                          
013200     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
013300     .                                                                    
013400     EJECT                                                                
013500 S02-SKRIV-W48850 SECTION.                                                
013600                                                                          
013700     WRITE W48850-POST FROM UT50-W4885601                                 
013800                                                                          
013900     MOVE 'W48850'   TO POSTSUM-FDNAMN                                    
014000     MOVE 'W48850D2' TO POSTSUM-DDNAMN2                                   
014100     MOVE '050'      TO POSTSUM-TRANSTYP                                  
014200     CALL POSTSUM USING POSTSUM-PARM                                      
014300     .                                                                    
014400     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
014600                                                                          
014700     CLOSE W48850                                                         
014800                                                                          
014900*- - - - - - - - - - - - - - - -  SKRIV UT ANTAL SKRIVNA POSTER           
015000                                                                          
015100     MOVE 'S' TO POSTSUM-OPKOD                                            
015200     CALL POSTSUM USING POSTSUM-PARM                                      
015300     .                                                                    
015400     EJECT                                                                
015500                                                                          
015600 IMS-GET-WDD8 SECTION.                                                    
015700                                                                          
015800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015900     CALL CBLTDLI USING GN SALDO-PCB IO-AREA                              
016000     MOVE SALDO-STATUS-CODE TO STATUS-WS                                  
016100     PERFORM IMS-STATUSKONTROLL                                           
016200     .                                                                    
016300                                                                          
016400 IMS-STATUSKONTROLL SECTION.                                              
016500                                                                          
016600     SET STATUS-IX TO 1                                                   
016700     SEARCH GODK-STATUS AT END CALL FELLOG                                
016800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
016900     END-SEARCH                                                           
017000     .                                                                    
