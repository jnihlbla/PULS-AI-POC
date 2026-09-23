000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3300200.                                                 
000400 AUTHOR.        RONNY STENHOLM                                            
000500 DATE-WRITTEN.  AUG 1989.                                                 
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        FÅNGAR FÖRSÄLJNING.                                              
001000*        PROGRAMMET LÄSER TVÅ FILER MED FAKTUROR KONVERTERAR              
001100*        DATUMET OCH BYTER POSTTYP. FILERNA LÄGGS DÄREFTER                
001200*        SAMMAN TILL EN FIL (W33003).                                     
001300*        PERIODICITET : VECKA                                             
001400*                                                                         
001500*    SUBPROGRAM:                                                          
001600*        WDATKONV - UTFÖR KONVERTERING AV DATUM ÅÅMMDD TILL ÅÅVV          
001700*                                                                         
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*    --- INFILER:                                                         
002600                                                                          
002700     SELECT W4758V                       ASSIGN TO W33002D1.              
002800                                                                          
002900     SELECT W3710G                       ASSIGN TO W33002D2.              
003000                                                                          
003200     SKIP2                                                                
003300*    --- UTFIL:                                                           
003400*           --- UPPDATERAT REGISTER:                                      
003500     SELECT W33003                       ASSIGN TO W33002D4.              
003600     SKIP2                                                                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W4758V                                                               
004300     LABEL RECORD   STANDARD                                              
004400     RECORDING      F                                                     
004500     BLOCK CONTAINS 0.                                                    
004600     SKIP2                                                                
004700*    -COPY W330099      -L.                                               
004900     SKIP2                                                                
005000     EJECT                                                                
005100 FD  W3710G                                                               
005200     LABEL RECORD   STANDARD                                              
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500     SKIP2                                                                
005600*    -COPY W330099      -L.                                               
005800     SKIP2                                                                
005900     EJECT                                                                
006900 FD  W33003                                                               
007000     LABEL RECORD   STANDARD                                              
007100     RECORDING      V                                                     
007200     BLOCK CONTAINS 0.                                                    
007300     SKIP2                                                                
007400*01  POST -COPY W330100     -PRE UTREG-  -L.                              
007600     EJECT                                                                
007700 WORKING-STORAGE SECTION.                                                 
007800     SKIP2                                                                
007801                                                                          
007810*    -- CHECKED BY WY2000                                                 
007900 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3300200'.            
008000*    --- FLAGGOR                                                          
008100 77  W4758V-EOF-SW                 PIC X(1)    VALUE 'N'.                 
008200     88  END-OF-W4758V                       VALUE 'J'.                   
008300 77  W3710G-EOF-SW               PIC X(1)    VALUE 'N'.                   
008400     88  END-OF-W3710G                       VALUE 'J'.                   
008700     SKIP3                                                                
008800                                                                          
008900 01  WS-AAVVD.                                                            
009000*                                                                         
009100     03  WS-AAVV                 PIC 9(4).                                
009200     03  FILLER                  PIC 9(1).                                
009300     EJECT                                                                
009310 01  FILLER REDEFINES WS-AAVVD.                                           
009320     03  FILLER                  PIC 9(2).                                
009321     03  WS-VECKA                PIC 9(2).                                
009330     03  FILLER                  PIC 9(1).                                
009400                                                                          
009500 01  DYNAMISKA-SUBPROGRAM.                                                
009600*                                                                         
009700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009900     SKIP2                                                                
010000*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
010100*                                                                         
010200*01  -COPY W0005      -PRE  POSTSUM-                                      
010400*- - - - - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                 
010500*                                                                         
010600*01  -COPY WDATAREA                                                       
010800     EJECT                                                                
010900 01  UT-AREA-START                PIC X(24)   VALUE                       
011000                                            'UT-AREA-START  '.            
011100     SKIP3                                                                
011200 01  UT-AREA.                                                             
011300     03  FILLER                   PIC X(25).                              
011400     SKIP2                                                                
011500*01  FILLER  -PRE UT-  -COPY W330100    -RED UT-AREA                      
011700     EJECT                                                                
011800 01  IN-AREA-START                PIC X(24) VALUE                         
011900                                            'IN-AREA-START  '.            
012000 01  IN-AREA.                                                             
012100     03  FILLER                   PIC X(26).                              
012200     SKIP2                                                                
012300*01  FILLER  -PRE  IN-  -COPY W330099    -RED IN-AREA                     
012500     EJECT                                                                
012600                                                                          
012700 PROCEDURE DIVISION.                                                      
012800     SKIP2                                                                
012900 STYR SECTION.                                                            
013000     PERFORM A-INIT                                                       
013100     PERFORM D-LAS-W475                                                   
013200     PERFORM UNTIL END-OF-W4758V                                          
013300*---------PRARTNTO INNEHÅLLER STYCKPRIS, DÄRFÖR MULTIPLICERAS             
013400*---------PRARTNTO MED KVLEVART.                                          
013500       COMPUTE IN-PRARTNTO = IN-PRARTNTO * IN-KVLEVART                    
013600                                                                          
013700       PERFORM B-BEHANDLA-POST                                            
013800       PERFORM C-SKRIV-POST                                               
013900       PERFORM D-LAS-W475                                                 
014000     END-PERFORM                                                          
014100     PERFORM E-LAS-W371                                                   
014200     PERFORM UNTIL END-OF-W3710G                                          
014300*---------PRARTNTO INNEHÅLLER TOT FSGPRIS, DÄRFÖR MULTIPLICERAS           
014400*---------INGET HÄR.                                                      
014500       PERFORM B-BEHANDLA-POST                                            
014600       PERFORM C-SKRIV-POST                                               
014700       PERFORM E-LAS-W371                                                 
014800     END-PERFORM                                                          
015600     PERFORM Z-FINIT                                                      
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200     SKIP2                                                                
016300     OPEN INPUT W4758V                                                    
016400                W3710G                                                    
016500                                                                          
016600     SKIP2                                                                
016700     OPEN OUTPUT W33003                                                   
016800     SKIP2                                                                
016900     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
017000     .                                                                    
017100     EJECT                                                                
017200                                                                          
017300 B-BEHANDLA-POST  SECTION.                                                
017400     SKIP2                                                                
017500     MOVE IN-TIFAKT TO DAT-I-TIDATUM                                      
017600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
017700     CALL WDATKONV USING DAT-KDDATFORM                                    
017800                         DAT-I-TIDATUM                                    
017900                         DAT-O-TIDATUM                                    
018000                         DAT-KDSVAR                                       
018100     MOVE DAT-TIAAVVD TO WS-AAVVD                                         
018120                                                                          
018200*    MOVE WS-AAVV     TO UT-TIFSGVV                                       
018201     IF WS-VECKA NOT = 53                                                 
018202       MOVE WS-AAVV     TO UT-TIFSGVV                                     
018203     ELSE                                                                 
018204       MOVE 52          TO WS-VECKA                                       
018205       MOVE WS-AAVV     TO UT-TIFSGVV                                     
018206     END-IF                                                               
018300                                                                          
018400     MOVE '100'         TO UT-IDPTYP                                      
018500     MOVE IN-IDARTNR    TO UT-IDARTNR                                     
018600     MOVE IN-IDDISTR    TO UT-IDDISTR                                     
018700     MOVE IN-KVLEVART   TO UT-KVLEVART                                    
018800     MOVE IN-PRARTNTO   TO UT-PRARTNTO                                    
018900     MOVE IN-KDPRTYP    TO UT-KDPRTYP                                     
019000     MOVE IN-KDORDKL    TO UT-KDORDKL                                     
019100     .                                                                    
019200                                                                          
019300                                                                          
019400 C-SKRIV-POST  SECTION.                                                   
019500     SKIP2                                                                
019600     WRITE UTREG-POST FROM UT-AREA.                                       
019700     MOVE SPACE TO POSTSUM-TRANSTYP                                       
019800     MOVE 'W33003' TO POSTSUM-FDNAMN                                      
019900     MOVE 'W33002D4' TO POSTSUM-DDNAMN2                                   
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400 D-LAS-W475  SECTION.                                                     
020500     SKIP2                                                                
020600     READ W4758V INTO IN-AREA                                             
020700     AT END                                                               
020800         SET END-OF-W4758V TO TRUE                                        
020900     END-READ                                                             
021000     IF NOT END-OF-W4758V                                                 
021100       MOVE SPACE TO POSTSUM-TRANSTYP                                     
021200       MOVE 'W4758V' TO POSTSUM-FDNAMN                                    
021300       MOVE 'W33002D1' TO POSTSUM-DDNAMN2                                 
021400       CALL POSTSUM USING POSTSUM-PARM                                    
021500     END-IF                                                               
021600     .                                                                    
021700                                                                          
021800                                                                          
021900 E-LAS-W371  SECTION.                                                     
022000     SKIP2                                                                
022100     READ W3710G INTO IN-AREA                                             
022200     AT END                                                               
022300         SET END-OF-W3710G TO TRUE                                        
022400     END-READ                                                             
022500     IF NOT END-OF-W3710G                                                 
022600       MOVE SPACE TO POSTSUM-TRANSTYP                                     
022700       MOVE 'W3710G' TO POSTSUM-FDNAMN                                    
022800       MOVE 'W33002D2' TO POSTSUM-DDNAMN2                                 
022900       CALL POSTSUM USING POSTSUM-PARM                                    
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300                                                                          
024900 Z-FINIT SECTION.                                                         
025000     SKIP2                                                                
025100     CLOSE W4758V                                                         
025200           W3710G                                                         
025400           W33003                                                         
025500     MOVE 'S' TO POSTSUM-OPKOD                                            
025600     CALL POSTSUM USING POSTSUM-PARM                                      
025700     .                                                                    
025800     SKIP2                                                                
025900     EJECT                                                                
