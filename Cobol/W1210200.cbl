000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1210200.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   AUGUSTI 1985.                                            
000500         REMARKS.                                                         
000600*        PROGRAMMET SOM ÄR EN EXIT TILL HJÄLPPROGRAMMET IMS               
000700*        FAST SCAN UTILITY (FSU) LÄSER IGENOM WDD3 OCH SKAPAR EN          
000800*        UTPOST FÖR VARJE ARTIKEL MED FLFELHOMO = J, ELLER                
000900*        KDBENSTAT = 1 ,2 ELLER 3.                                        
001000*                                                                         
001100*        ÄNDRAT : 880803 TOL  SB/OS.VS COBOL       G.ERIKSSON             
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600         SELECT W12102         ASSIGN TO UT-S-W12102D1.                   
001700                                                                          
001800     EJECT                                                                
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100 FD  W12102                                                               
002200     LABEL RECORD STANDARD                                                
002300     RECORDING MODE F                                                     
002400     BLOCK CONTAINS 0 RECORDS.                                            
002500                                                                          
002600                                                                          
002700*01  POST    -COPY W12102        -PRE UT-  -L.                            
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200******************************************************************        
003300*    S W I T C H A R                                                      
003400******************************************************************        
003500                                                                          
003600 01  SWITCHAR.                                                            
003700     05  SW-TRAEFF       PIC X(01)   VALUE 'N'.                           
003800                                                                          
003900******************************************************************        
004000*    K O N S T A N T E R                                                  
004100******************************************************************        
004200 01  PROGRAM-NAMN        PIC X(8)   VALUE 'W1210200'.                     
004300                                                                          
004400 01  KONSTANTER.                                                          
004500     05  JA              PIC X(01)   VALUE 'J'.                           
004600     05  NEJ             PIC X(01)   VALUE 'N'.                           
004700     05  S-KONSTANT      PIC X(01)   VALUE 'S'.                           
004800                                                                          
004900*------------------------------------------------------                   
005000 01  DYNAMISKA-SUBPGM.                                                    
005100    03  CBLTDLI          PIC X(8)   VALUE 'CBLTDLI'.                      
005200    03  POSTSUM          PIC X(8)   VALUE 'POSTSUM'.                      
005300    03  FELLOG           PIC X(8)   VALUE 'FELLOG'.                       
005400*-------------------------------- ARBETSAREOR IMS SECTIONEN.              
005500 01  IMS-WS.                                                              
005600     03  FILLER          PIC X(8)    VALUE 'IMS-WS'.                      
005700*---------------------------------STATUS-KODER FRÅN IMS.                  
005800     03  STATUS-WS       PIC XX.                                          
005900        88  SEGMENT-FINNS            VALUE '  '.                          
006000        88  SEGMENT-SLUT             VALUE 'GB'.                          
006100                                                                          
006200     03  GODK-STATUSKODER.                                                
006300        05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.             
006400                                                                          
006500     EJECT                                                                
006600*---------------------------------IMS-CALL FUNKTIONER                     
006700*01            -COPY W0003CCCC0                                           
006800*++INCLUDE W0003CCCC0                                                     
006900     EJECT                                                                
007000*---------------------------------PARAMETRAR TILL POST-SUM                
007100*01            -COPY W0005CCCC0 -PRE POSTSUM-                             
007200*++INCLUDE W0005CCCC0                                                     
007300     EJECT                                                                
007400 01  DLI-IO-AREA.                                                         
007500     03  IO-AREA            PIC X(150).                                   
007600     EJECT                                                                
007700*03  SEGM01      -COPY WDD301     -PRE WDD301- -RED IO-AREA.              
007800*++INCLUDE WDD301CCC0                                                     
007900     EJECT                                                                
008000*03  SEGM02      -COPY WDD311     -PRE WDD311- -RED IO-AREA.              
008100*++INCLUDE WDD311CCC0                                                     
008200     EJECT                                                                
008300*03  SEGM03      -COPY WDD312     -PRE WDD312- -RED IO-AREA.              
008400*++INCLUDE WDD312CCC0                                                     
008500     EJECT                                                                
008600******************************************************************        
008700*    U T P O S T E N                                                      
008800******************************************************************        
008900*01  AREA        -COPY W12102     -PRE UTPOST-                            
009100     EJECT                                                                
009200                                                                          
009300 LINKAGE SECTION.                                                         
009400                                                                          
009500*01     -COPY W0008      -PRE WDD3-                                       
009700        05  FILLER  PIC XX.                                               
009800     EJECT                                                                
009900 PROCEDURE DIVISION USING   WDD3-PCB.                                     
010000     ENTRY 'CBLTDLI' USING  WDD3-PCB.                                     
010100     PERFORM A-INIT                                                       
010200     PERFORM IMS-GET-WDD3                                                 
010300     PERFORM UNTIL SEGMENT-SLUT                                           
010400         EVALUATE WDD3-SEG-NAME-FB                                        
010500            WHEN  'WDD301  '                                              
010600                   MOVE SPACE TO UTPOST-BENAEMNING-BEART                  
010700                   MOVE ZERO  TO UTPOST-BENAEMNING-TIUPPDAT               
010800                                 UTPOST-BENAEMNING-KDFEL                  
010900                   IF SW-TRAEFF = JA                                      
011000                      MOVE NEJ        TO SW-TRAEFF                        
011100                   END-IF                                                 
011200                                                                          
011300                   IF WDD301-BEN-KDBENSTAT = 0 OR 1 OR 2                  
011400                      MOVE JA         TO SW-TRAEFF                        
011500                      MOVE 1          TO UTPOST-BENAEMNING-KDFEL          
011600                   ELSE                                                   
011700                      IF WDD301-BEN-KDBENSTAT = 3                         
011800                         MOVE JA      TO SW-TRAEFF                        
011900                         MOVE 3       TO UTPOST-BENAEMNING-KDFEL          
012000                      END-IF                                              
012100                   END-IF                                                 
012200            WHEN  'WDD311  '                                              
012300                IF WDD311-TEXT-IDSKYLT = 'S  '                            
012400                   MOVE WDD311-TEXT-BEART  TO                             
012500                                     UTPOST-BENAEMNING-BEART              
012600                                                                          
012700                   MOVE WDD311-TEXT-TIUPPDAT TO                           
012800                                     UTPOST-BENAEMNING-TIUPPDAT           
012900                END-IF                                                    
013000            WHEN  'WDD312  '                                              
013100               IF UTPOST-BENAEMNING-KDFEL = 3                             
013200                   CONTINUE                                               
013300               ELSE                                                       
013400                   IF WDD312-ART-FLFELHOMO = 'J'                          
013500                      MOVE JA              TO SW-TRAEFF                   
013600                      MOVE 2         TO UTPOST-BENAEMNING-KDFEL           
013700                   END-IF                                                 
013800               END-IF                                                     
013900              MOVE WDD312-ART-IDARTNR TO UTPOST-BENAEMNING-IDARTNR        
014000                                                                          
014100              IF SW-TRAEFF = JA   AND                                     
014200                 UTPOST-BENAEMNING-IDARTNR > ZERO                         
014300                 PERFORM S01-SKRIV-UTPOST                                 
014400              END-IF                                                      
014500                                                                          
014600         END-EVALUATE                                                     
014700         PERFORM IMS-GET-WDD3                                             
014800     END-PERFORM                                                          
014900     PERFORM Z-FINIT                                                      
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500     OPEN OUTPUT W12102                                                   
015600                                                                          
015700     MOVE PROGRAM-NAMN    TO POSTSUM-PROGNAMN                             
015800                                                                          
015900     MOVE ZERO            TO UTPOST-BENAEMNING-IDARTNR                    
016000                             UTPOST-BENAEMNING-KDFEL                      
016100                             UTPOST-BENAEMNING-TIUPPDAT                   
016200     MOVE SPACE           TO UTPOST-BENAEMNING-BEART                      
016300     .                                                                    
016400     EJECT                                                                
016500 Z-FINIT SECTION.                                                         
016600                                                                          
016700     CLOSE W12102                                                         
016800                                                                          
016900     MOVE S-KONSTANT      TO POSTSUM-OPKOD                                
017000     CALL POSTSUM USING POSTSUM-PARM                                      
017100     .                                                                    
017200     EJECT                                                                
017300 S01-SKRIV-UTPOST SECTION.                                                
017400                                                                          
017500     WRITE UT-POST FROM UTPOST-AREA                                       
017600                                                                          
017700     MOVE 'W12102'        TO POSTSUM-FDNAMN                               
017800     MOVE 'W12102D1'      TO POSTSUM-DDNAMN2                              
017900     MOVE SPACE           TO POSTSUM-TRANSTYP                             
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100                                                                          
018200     MOVE ZERO            TO UTPOST-BENAEMNING-IDARTNR                    
018300     IF UTPOST-BENAEMNING-KDFEL = 2                                       
018400        MOVE ZERO TO UTPOST-BENAEMNING-KDFEL                              
018500        MOVE NEJ  TO SW-TRAEFF                                            
018600     END-IF                                                               
018700     .                                                                    
018800 IMS-GET-WDD3 SECTION.                                                    
018900     SKIP3                                                                
019000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
019100     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-AREA                           
019200     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
019300     PERFORM IMS-STATUSKONTROLL                                           
019400     .                                                                    
019500 IMS-STATUSKONTROLL SECTION.                                              
019600     SKIP3                                                                
019700     SET STATUS-IX TO 1                                                   
019800     SEARCH GODK-STATUS AT END CALL FELLOG                                
019900       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
020000       CONTINUE                                                           
020100     END-SEARCH                                                           
020200     .                                                                    
