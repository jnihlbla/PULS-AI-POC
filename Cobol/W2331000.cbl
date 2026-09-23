000100 SKIP3                                                                    
000200 ID DIVISION.                                                             
000300 SKIP2                                                                    
000400 PROGRAM-ID.         W2331000.                                            
000500*AUTHOR.             URBAN ZACKRISSON.                                    
000600*DATE-WRITTEN.       NOV 1984.                                            
000700*DATE-COMPILED.                                                           
000800                                                                          
000900*   FUNKTION:                                                             
001000*                    FRÅN FILEN W23305 UTSELEKTERAS DE POSTER             
001100*                    SOM SKA SKRIVAS PÅ FILEN W23311,                     
001200*                    PV-INKÖPARE                                          
001300*                    FÖR DESSA POSTER GÄLLER ATT:                         
001400                                                                          
001500*                    - POSTTYP = 310, 320, 330 ELLER 340                  
001600*                      FÖR PT 320 DESSUTOM ATT MOTTAGET ANTAL             
001700*                      EJ = AVISERAT ANTAL                                
001800                                                                          
001900*                    - REDOVISNINGSTYP = 0                                
002000                                                                          
002100*                    - INKÖPARNUMMER = 100 - 299 (PV)                     
002900*                                                                         
003000 EJECT                                                                    
003100 ENVIRONMENT DIVISION.                                                    
003200 SKIP2                                                                    
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600 SKIP2                                                                    
003700     SELECT  W23305  ASSIGN  TO UT-S-W23310D1.                            
003800     SELECT  W23311  ASSIGN  TO UT-S-W23310D2.                            
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200 SKIP2                                                                    
004300 FILE SECTION.                                                            
004400 SKIP3                                                                    
004500 FD  W23305                                                               
004600     RECORDING F                                                          
004700     BLOCK CONTAINS 0.                                                    
004800                                                                          
004900*01      -COPY  W211310 -L                                                
005000 SKIP3                                                                    
005100 FD  W23311                                                               
005200     RECORDING F                                                          
005300     BLOCK CONTAINS 0.                                                    
005400                                                                          
005500*01  W23311-POST   -COPY W211310 -L                                       
005600 SKIP3                                                                    
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                   PIC X(8)            VALUE 'W2331000'.        
006700 77  JA                      PIC X               VALUE 'J'.               
006800 77  NEJ                     PIC X               VALUE 'N'.               
006900 77  W23305-EOF              PIC X               VALUE 'N'.               
007000     SKIP3                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200     03  POSTSUM             PIC X(8)            VALUE 'POSTSUM'.         
007300     EJECT                                                                
007400*01  -COPY W0005  -PRE POSTSUM-                                           
007500     EJECT                                                                
007600 01  WORK-AREA.                                                           
007700*    03  -COPY   W211310  -PRE    WORK-.                                  
007800     EJECT                                                                
007900 PROCEDURE DIVISION.                                                      
008000                                                                          
008100     PERFORM A-INIT                                                       
008200                                                                          
008300     PERFORM S01-LAS-W23305                                               
008400     PERFORM UNTIL                                                        
008500      NOT ( W23305-EOF = NEJ )                                            
008600       IF WORK-IDPTYP = +310 OR +330 OR +340 OR                           
008700       ((WORK-IDPTYP = +320) AND                                          
008800       (WORK-KVMOTANT NOT = WORK-KVAVIS))                                 
008900         IF WORK-KDRT = 0 OR 10                                           
009000***        OBS      KDPKINR = IDINK                                       
009100           IF (WORK-KDPKINR > 099 AND < 790) OR                           
009200              (WORK-KDPKINR > 799 AND < 987) OR                           
009300              (WORK-KDPKINR > 987 AND < 1000)                             
009400             PERFORM S02-SKRIV-W23311-POST                                
009900           END-IF                                                         
010000         END-IF                                                           
010100       END-IF                                                             
010200       PERFORM S01-LAS-W23305                                             
010300     END-PERFORM                                                          
010400     PERFORM Z-FINIT                                                      
010500                                                                          
010600     MOVE ZERO TO RETURN-CODE                                             
010700     GOBACK                                                               
010800     CONTINUE.                                                            
010900     EJECT                                                                
011000 A-INIT SECTION.                                                          
011100     OPEN INPUT W23305                                                    
011200          OUTPUT W23311                                                   
011400                                                                          
011500     CONTINUE.                                                            
011600     SKIP3                                                                
011700 S01-LAS-W23305 SECTION.                                                  
011800                                                                          
011900     READ W23305 INTO WORK-AREA                                           
012000     AT END MOVE JA TO W23305-EOF                                         
012100     END-READ                                                             
012200                                                                          
012300     IF W23305-EOF = NEJ                                                  
012400       MOVE 'W23305'    TO POSTSUM-FDNAMN                                 
012500       MOVE 'W23310D1'  TO POSTSUM-DDNAMN2                                
012600       MOVE WORK-IDPTYP TO POSTSUM-TRANSTYP                               
012700       CALL POSTSUM USING POSTSUM-PARM                                    
012800     END-IF                                                               
012900     CONTINUE.                                                            
013000     EJECT                                                                
013100 S02-SKRIV-W23311-POST SECTION.                                           
013200                                                                          
013300     WRITE W23311-POST FROM WORK-AREA                                     
013400     MOVE 'W23311'    TO POSTSUM-FDNAMN                                   
013500     MOVE 'W23310D2'  TO POSTSUM-DDNAMN2                                  
013600     MOVE WORK-IDPTYP TO POSTSUM-TRANSTYP                                 
013700     CALL POSTSUM USING POSTSUM-PARM                                      
013800     CONTINUE.                                                            
013900     SKIP3                                                                
014900 Z-FINIT SECTION.                                                         
015000     CLOSE W23305                                                         
015100           W23311                                                         
015300     MOVE 'S' TO POSTSUM-OPKOD                                            
015400     CALL POSTSUM USING POSTSUM-PARM                                      
015500     CONTINUE.                                                            
