000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W5102600.                                    
000300 AUTHOR.                     CHRISTINA BRUHN.                             
000400 DATE-WRITTEN.               OKTOBER  1992.                               
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION.                                                            
000900                                                                          
001000*    LADDAR WDK1 MHA BMC-UTILITY OCH FIL W51025.                          
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.                                                            
001500     SELECT  W51025          ASSIGN      W51026D1.                        
001600     SELECT  SORTFIL         ASSIGN      W51026DS.                        
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900 FILE SECTION.                                                            
002000     SKIP3                                                                
002100 FD  W51025                                                               
002200     LABEL RECORDS STANDARD                                               
002300     RECORDING      F                                                     
002400     BLOCK CONTAINS 0.                                                    
002500                                                                          
002600*01  POST  -COPY WDK101     -PRE LADD-  -L.                               
002800     SKIP2                                                                
002900 SD  SORTFIL                                                              
003000     LABEL RECORDS STANDARD                                               
003100     RECORDING      F                                                     
003200     BLOCK CONTAINS 0.                                                    
003300                                                                          
003400 01  SORTFIL-AREA.                                                        
003500     03  SORTFIL-RANDOMKEY               PIC X(4).                        
003600     03  SORT-POST.                                                       
003700       06 SORTFIL-WDK1KEY                PIC S9(9) COMP-3.                
003800       06 FILLER                         PIC X(430).                      
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W5102600'.                
004300 77  JA                      PIC X       VALUE 'J'.                       
004400 77  NEJ                     PIC X       VALUE 'N'.                       
004500 77  W51025-EOF              PIC X       VALUE 'N'.                       
004600 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
004700 77  RKOD                    PIC S9(4)   VALUE +0 COMP SYNC.              
004800 77  DATABASE                PIC X(4)    VALUE 'WDK1'.                    
004900                                                                          
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100     03  ABEND               PIC X(8)    VALUE 'ABEND  '.                 
005200     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
005300     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
005400     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
005500     03  W015RAND            PIC X(8)    VALUE 'W015RAND'.                
005600     EJECT                                                                
005700 01  FILLER                  PIC X(16)   VALUE 'POSTSUMAREA'.             
005800*01  -COPY W0005      -PRE POSTSUM-.                                      
006000     EJECT                                                                
006100 01  IN-AREA-START           PIC X(16)   VALUE 'IN-AREA'.                 
006200     SKIP2                                                                
006300 01  IN-AREA.                                                             
006400*03  POST -COPY WDK101      -PRE IN-.                                     
006600     EJECT                                                                
006700*    ---- ARBETS-AREOR FÖR IMSSEKTIONERNA                                 
006800 01  FILLER                 PIC X(16)  VALUE 'IMS-WS'.                    
006900     SKIP2                                                                
007000*    ---- STATUSKOD FRÅN IMS                                              
007100 01  STATUS-WS              PIC XX.                                       
007200   88  SEGMENT-FINNS               VALUE '  '.                            
007300   88  SEGMENT-SAKNAS              VALUE 'GE'.                            
007400     SKIP2                                                                
007500 01  GODK-STATUSKODER.                                                    
007600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
007700     SKIP2                                                                
007800 01  SSA1                   PIC X(32).                                    
007900     EJECT                                                                
008000*01      -COPY W0003.                                                     
008200     EJECT                                                                
008300 01  FILLER                 PIC X(16)  VALUE 'DLI-IO-AREA'.               
008400 01  DLI-IO-AREA.                                                         
008700*    03 POST  -COPY WDK101  -PRE WDK101-                                  
008900     EJECT                                                                
009000 LINKAGE SECTION.                                                         
009100     SKIP2                                                                
009200*    -COPY W0008 -PRE WDK1-.                                              
009400          05  FILLER         PIC XX.                                      
009500     EJECT                                                                
009600 PROCEDURE DIVISION  USING WDK1-PCB.                                      
009700     ENTRY 'DLITCBL' USING WDK1-PCB.                                      
009800                                                                          
009900     PERFORM A-INIT                                                       
010000                                                                          
010100     SORT SORTFIL ASCENDING SORTFIL-RANDOMKEY                             
010200                            SORTFIL-WDK1KEY                               
010300          INPUT PROCEDURE B-INPUT                                         
010400          OUTPUT PROCEDURE C-OUTPUT-LADDA-WDK1                            
010500     IF SORT-RETURN NOT = ZERO                                            
010600        DISPLAY 'FEL I SORTEN'                                            
010700        MOVE +33 TO RKOD                                                  
010800        CALL ABEND USING RKOD                                             
010900     END-IF                                                               
011000                                                                          
011100     PERFORM Z-FINIT                                                      
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600 A-INIT SECTION.                                                          
011700                                                                          
011800     OPEN INPUT  W51025                                                   
011900                                                                          
012000     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
012100     MOVE NEJ          TO W51025-EOF                                      
012200     MOVE NEJ          TO SORTFIL-EOF                                     
012300*    MOVE +260000      TO SORT-FILE-SIZE                                  
012400     .                                                                    
012500     SKIP2                                                                
012600 B-INPUT SECTION.                                                         
012700                                                                          
012800     PERFORM UNTIL W51025-EOF = JA                                        
012900                                                                          
013000       READ W51025 INTO SORT-POST                                         
013100           AT END     MOVE JA TO W51025-EOF                               
013200       END-READ                                                           
013300       IF W51025-EOF = NEJ                                                
013400         CALL W015RAND USING SORTFIL-WDK1KEY                              
013500                             SORTFIL-RANDOMKEY                            
013600                             DATABASE                                     
013700         MOVE 'W51025'   TO POSTSUM-FDNAMN                                
013800         MOVE 'W51026D1' TO POSTSUM-DDNAMN2                               
013900         MOVE 'LADD'     TO POSTSUM-TRANSTYP                              
014000         CALL POSTSUM USING POSTSUM-PARM                                  
014100         RELEASE SORTFIL-AREA                                             
014200       END-IF                                                             
014300     END-PERFORM                                                          
014400     SKIP2                                                                
014500     .                                                                    
014600 C-OUTPUT-LADDA-WDK1 SECTION.                                             
014700                                                                          
014800     PERFORM S01-RETURN-SORTFIL                                           
014900                                                                          
015000     PERFORM UNTIL SORTFIL-EOF = JA                                       
015100        MOVE SORT-POST TO WDK101-POST                                     
015200        PERFORM IMS-ISRT-WDK101                                           
015300        PERFORM S01-RETURN-SORTFIL                                        
015400     END-PERFORM                                                          
015500     .                                                                    
015600     EJECT                                                                
015700 S01-RETURN-SORTFIL SECTION.                                              
015800                                                                          
015900     RETURN SORTFIL AT END                                                
016000            MOVE JA TO SORTFIL-EOF                                        
016100     END-RETURN                                                           
016200     .                                                                    
016300     EJECT                                                                
016400 Z-FINIT SECTION.                                                         
016500     SKIP2                                                                
016600                                                                          
016700     CLOSE W51025                                                         
016800                                                                          
016900     MOVE 'S' TO POSTSUM-OPKOD                                            
017000     CALL POSTSUM USING POSTSUM-PARM                                      
017100     .                                                                    
017200     EJECT                                                                
017300*    ---- IMS SEKTIONER                                                   
017400     SKIP2                                                                
017500 IMS-ISRT-WDK101 SECTION.                                                 
017600*                                                                         
017700     MOVE 'WDK101 '   TO SSA1                                             
017800     MOVE '  '     TO GODK-STATUSKODER                                    
017900     CALL CBLTDLI USING ISRT WDK1-PCB DLI-IO-AREA SSA1                    
018000     MOVE WDK1-STATUS-CODE TO STATUS-WS                                   
018100     PERFORM IMS-STATUSKONTROLL                                           
018200     SKIP2                                                                
018300     .                                                                    
018400     EJECT                                                                
018500 IMS-STATUSKONTROLL SECTION.                                              
018600*                                                                         
018700     SET STATUS-IX TO 1                                                   
018800     SEARCH GODK-STATUS AT END CALL FELLOG                                
018900         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
019000         CONTINUE                                                         
019100     END-SEARCH                                                           
019200     .                                                                    
