000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID. W4788400.                                                    
000800 AUTHOR. LARS DAHLQVIST/ERIK KÅREBY.                                      
000900                                                                          
001000 DATE-WRITTEN. 31 OKT 73/DEC 1981.                                        
001100                                                                          
001200 DATE-COMPILED.                                                           
001300*REMARKS.                                                                 
001400*        PROGRAMMET SORTERAR W47880 PÅ ARTIKELNR OCH DC                   
001500*        OM IDENTITETEN ÄR LIKA SUMMERAS LEVERERAT ANTAL.                 
001600 EJECT                                                                    
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SELECT  W47880  ASSIGN  TO  UT-S-W47884D1.                           
002300     SELECT  W47884  ASSIGN  TO  UT-S-W47884D2.                           
002400     SELECT  SORTFIL ASSIGN  TO  UT-S-W47884DS.                           
002500 EJECT                                                                    
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800 SKIP2                                                                    
002900 FD  W47880                                                               
003000     RECORDING F                                                          
003100     BLOCK CONTAINS 0                                                     
003200     LABEL RECORDS STANDARD                                               
003300     DATA RECORD IS  IN-W4788002.                                         
003400                                                                          
003500*01  W4788002   -PRE IN-  -COPY W4788002.                                 
003700 SKIP2                                                                    
003800 FD  W47884                                                               
003900     RECORDING F                                                          
004000     BLOCK CONTAINS 0                                                     
004100     LABEL RECORD STANDARD                                                
004200     DATA RECORD IS  UT-W4788002.                                         
004300                                                                          
004400*01  W4788002   -PRE UT-  -COPY W4788002.                                 
004600 SKIP2                                                                    
004700 SD  SORTFIL                                                              
004800     DATA RECORD IS  S1-W4788002.                                         
004900                                                                          
005000*01  W4788002   -PRE S1-  -COPY W4788002.                                 
005200 SKIP2                                                                    
005300 WORKING-STORAGE SECTION.                                                 
005301                                                                          
005310*    -- CHECKED BY WY2000                                                 
005400 01  PROGRAM-NAMN                  PIC X(8)   VALUE 'W4758400'.           
005500                                                                          
005501*****  GENERELLA KONSTANTER                                               
005510 77  JA          PIC X   VALUE 'J'.                                       
005520 77  NEJ         PIC X   VALUE 'N'.                                       
005521                                                                          
005530*****  END-OF-FILE-SWITCHAR                                               
005600 77  INFIL-EOF   PIC X   VALUE 'N'.                                       
005800                                                                          
005900*01  W4788002   -PRE W1-  -COPY W4788002.                                 
006100 EJECT                                                                    
006200 PROCEDURE DIVISION.                                                      
006300 A-STYR SECTION.                                                          
006400     SORT SORTFIL ON ASCENDING                                            
006500                         S1-IDARTNR                                       
006600                         S1-IDDC                                          
006700             USING   W47880                                               
006800             OUTPUT PROCEDURE B-OUTPUT.                                   
006900     CLOSE W47884.                                                        
007000     MOVE  0  TO  RETURN-CODE                                             
007100     GOBACK                                                               
007110     .                                                                    
007200 SKIP2                                                                    
007300 B-OUTPUT SECTION.                                                        
007400     OPEN OUTPUT W47884                                                   
007500     MOVE ZERO TO   W1-IDARTNR                                            
007700                    W1-KVLEVART                                           
007710     MOVE SPACE TO  W1-IDDC                                               
007800     PERFORM D-LAES                                                       
007900     PERFORM UNTIL INFIL-EOF = JA                                         
007910       PERFORM C-BEARBETA                                                 
007911       PERFORM D-LAES                                                     
007920     END-PERFORM                                                          
008000     PERFORM E-SKRIV                                                      
008100     .                                                                    
008300 SKIP2                                                                    
008400 C-BEARBETA SECTION.                                                      
008500*****             SUMMERA POSTER MED SAMMA ARTIKELNR OCH DC               
008700     IF  W1-IDARTNR = S1-IDARTNR     AND                                  
008800         W1-IDDC    = S1-IDDC                                             
008900             PERFORM  F-SUMMERA                                           
009000     ELSE                                                                 
009100             PERFORM  E-SKRIV                                             
009200             MOVE S1-W4788002 TO W1-W4788002                              
009210     END-IF                                                               
009600     .                                                                    
009610 SKIP2                                                                    
009700 D-LAES SECTION.                                                          
009800*****                    LÄS SORTFIL                                      
009900                                                                          
010000     RETURN  SORTFIL AT END MOVE JA TO INFIL-EOF                          
010100     .                                                                    
010300 SKIP2                                                                    
010400 E-SKRIV SECTION.                                                         
010500*****                    SKRIV W47884                                     
010600     IF  W1-KVLEVART     > ZERO                                           
010700             WRITE UT-W4788002 FROM  W1-W4788002                          
010710     END-IF                                                               
010800     .                                                                    
011000 SKIP2                                                                    
011100 F-SUMMERA SECTION.                                                       
011200*****                    SUMMERA LEVERERAT ANTAL                          
011300     ADD S1-KVLEVART     TO  W1-KVLEVART                                  
011400     .                                                                    
