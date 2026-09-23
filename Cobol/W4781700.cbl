000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4781700.                                                 
000400 AUTHOR.        ANNA-LENA ANDERSSON/BRA                                   
000500 DATE-WRITTEN.  JUNI 1980                                                 
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        PROGRAMMET SKAPAR EN SORTERAD OCH KOMPLETERAD FIL MED            
001200*        PERSONKOD OCH PERSONENS SIGNATUR.                                
001800*                                                                         
001810*      **OBS. PERSONKOD OCH SIGNATUR BLANKAS ALLTID FOM 13/6 '99          
001820*             EFTERSOM DE UPPGIFTERNA INTE ANVÄNDS LÄNGRE.                
001830*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016    - OM RETURKOD FRÅN SORT                                 
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002310 CONFIGURATION SECTION.                                                   
002320 SPECIAL-NAMES.                                                           
002330     ALPHABET Y2000 IS X'05' THRU X'09' X'00' THRU X'04'.                 
002340                                                                          
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - INFIL:                                           
002900*                        - -  FIL MED SELEKTERADE POSTER UR               
003000*                             KOLLIREGISTRET                              
003100     SELECT W47815-UPPF                  ASSIGN TO UT-S-W47817D1.         
003200     SKIP2                                                                
003300*- - - - - - - - - - - - UTFIL:                                           
003400*                        - -  KOMPLETTERAD FIL TILL LISTPROGRAMMET        
003500*                             W47818                                      
003600     SELECT W47817                       ASSIGN TO UT-S-W47817D2.         
003700     SKIP2                                                                
003800*- - - - - - - - - - - - SORTFIL:                                         
003900     SELECT SORTFIL                      ASSIGN TO UT-S-W47817DS.         
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W47815-UPPF                                                          
004600     LABEL RECORD   STANDARD                                              
004700     RECORDING      F                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
005000*    -COPY W47815       -L.                                               
005200     SKIP2                                                                
005300 FD  W47817                                                               
005400     LABEL RECORD   STANDARD                                              
005500     RECORDING      F                                                     
005600     BLOCK CONTAINS 0.                                                    
005700     SKIP2                                                                
005800*01  UTPOST -COPY W47817       -L.                                        
006000     SKIP2                                                                
006100 SD  SORTFIL                                                              
006200     RECORDING F.                                                         
006300*01  POST   -COPY W47815     -PRE SORT-                                   
006310*Y2K-SORT                                                                 
006400     04 FILLER REDEFINES SORT-TIBEGPAC.                                   
006410        05  SORT-DECADE            PIC X.                                 
006420        05  SORT-SMALL-DATE        PIC S9(5) COMP-3.                      
006500     04 SORT-KDSORT1               PIC S9(1).                             
006600*                                  ***  SÄTTS TILL 0 OM                   
006700*                                  ***  SVERIGE-DISTRIKT                  
006800*                                  ***  1 FÖR ÖVRIGA                      
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007001                                                                          
007010*    -- CHECKED BY WY2000                                                 
007100*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
007200 77  PROGRAM-NAMN                PIC X(8)  VALUE 'W4781700'.              
007300     SKIP2                                                                
007400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
007500                                                                          
007600 77  JA                          PIC X(1)    VALUE 'J'.                   
007700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007800     SKIP2                                                                
007900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
008000                                                                          
008100 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
008200 77  UPPF-EOF                    PIC X(1)    VALUE 'N'.                   
008300     SKIP2                                                                
008400                                                                          
008500 77  ARB-KDORDKL                 PIC S9(1)   COMP-3 VALUE +0.             
008600                                                                          
008700     EJECT                                                                
008800 01  DISTRIKT-FAELT.                                                      
008900     03  IDDISTR-WS              PIC S9(5)   COMP-3.                      
009000     03  DISTRIKT-TYP            PIC X(3).                                
009100         88  EXPORT-DISTRIKT                 VALUE 'EXP'.                 
009200         88  SVERIGE-DISTRIKT                VALUE 'SVE'.                 
009201*                                                                         
009202 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
009203     SKIP3                                                                
009204*01  FILLER   -COPY WWDIST03    -RED TEST-IDDISTR.                        
009205     SKIP3                                                                
009206*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
009210     SKIP3                                                                
009220*01  FILLER   -COPY WWDIST20    -RED TEST-IDDISTR.                        
009300     SKIP3                                                                
009310*      --- VALID IDDC CODES                                               
009320*                                                                         
009330*01    -COPY WWDC99                                                       
009340       EJECT                                                              
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009700     03  W4781710                PIC X(8)    VALUE 'W4781710'.            
009800     SKIP3                                                                
009900*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
010000                                                                          
010100 01  RETURKODER.                                                          
010200   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010300   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010400   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010500     EJECT                                                                
010600*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
010700                                                                          
010800*01  -COPY W0005       -PRE  POSTSUM-.                                    
011000     EJECT                                                                
011100*01  AREA  -PRE UT-     -COPY W47817                                      
011300     EJECT                                                                
011400*01  AREA  -PRE WSORT-  -COPY W47815                                      
011600     04 WSORT-KDSORT1            PIC S9(1).                               
011700*                                ***  SÄTTS TILL +0 OM                    
011800*                                ***  SVERIGE-DISTRIKT                    
011900*                                ***  TILL +1 FÖR ÖVRIGA                  
012000     EJECT                                                                
014200 PROCEDURE DIVISION.                                                      
014300     SKIP2                                                                
014600     PERFORM A-INIT                                                       
014700                                                                          
014800     SORT SORTFIL ASCENDING                                               
014900                  SORT-IDDC                                               
015000                  SORT-KDSORT1                                            
015100                  SORT-IDTTYP                                             
015200                  SORT-DECADE                                             
015210                  SORT-SMALL-DATE                                         
015300                  SORT-IDDISTR                                            
015400                  SORT-IDKUNDNR                                           
015500                  SORT-IDPRODNR                                           
015600                  SORT-IDKOLLI                                            
015700                  DESCENDING                                              
015800                  SORT-IDKUNDRF                                           
015810                  COLLATING SEQUENCE Y2000                                
015900          INPUT PROCEDURE B-INNAN-SORTERING                               
016000          OUTPUT PROCEDURE C-EFTER-SORTERING                              
016100     SKIP2                                                                
016200     IF SORT-RETURN > ZERO                                                
016300        DISPLAY '***  W4781700  - FEL VID SORTERING'                      
016400        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
016500     ELSE                                                                 
016600                                                                          
016700        PERFORM Z-FINIT                                                   
016800        MOVE ZERO TO RETURN-CODE                                          
016900        GOBACK                                                            
017000                                                                          
017100     END-IF                                                               
017110     .                                                                    
017200     EJECT                                                                
017300 A-INIT SECTION.                                                          
017400                                                                          
017500     OPEN INPUT  W47815-UPPF                                              
017600     OPEN OUTPUT W47817                                                   
017700     SKIP2                                                                
017800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
017810     .                                                                    
017900     EJECT                                                                
018000 B-INNAN-SORTERING SECTION.                                               
018100                                                                          
018200     PERFORM S02-LAS-OSORTERAD-W47815                                     
018300                                                                          
018400     PERFORM UNTIL UPPF-EOF = JA                                          
018500                                                                          
018510        MOVE WSORT-IDDC    TO WS-IDDC                                     
018600        IF SVERIGE-DISTRIKT AND CDC-SE                                    
018700            MOVE +0    TO WSORT-KDSORT1                                   
018800        ELSE                                                              
018900            MOVE +1    TO WSORT-KDSORT1                                   
019000        END-IF                                                            
019100                                                                          
019200        RELEASE SORT-POST  FROM WSORT-AREA                                
019300        PERFORM S02-LAS-OSORTERAD-W47815                                  
019400     END-PERFORM                                                          
019410     .                                                                    
019500     EJECT                                                                
019600 C-EFTER-SORTERING SECTION.                                               
019700                                                                          
019800     PERFORM S01-LAS-SORTERAD-W47815                                      
019900                                                                          
020000     PERFORM UNTIL SORTFIL-EOF = JA                                       
020100         MOVE WSORT-AREA TO UT-AREA                                       
020200         IF SORTFIL-EOF = NEJ                                             
020300             PERFORM S03-SAETT-PERSONKOD                                  
020400         END-IF                                                           
020500                                                                          
020600         WRITE UTPOST FROM UT-AREA                                        
020700         PERFORM S01-LAS-SORTERAD-W47815                                  
020800     END-PERFORM                                                          
020810     .                                                                    
020900     EJECT                                                                
021000 S01-LAS-SORTERAD-W47815 SECTION.                                         
021100                                                                          
021200     RETURN SORTFIL   INTO WSORT-AREA                                     
021300                      AT END MOVE JA TO SORTFIL-EOF                       
021400     END-RETURN                                                           
021500     IF SORTFIL-EOF = NEJ                                                 
021600                                                                          
021700        MOVE 'SORTFIL'           TO POSTSUM-FDNAMN                        
021800        MOVE 'W47817DS'          TO POSTSUM-DDNAMN2                       
021900        MOVE WSORT-IDTTYP        TO POSTSUM-TRANSTYP                      
022000        CALL POSTSUM   USING POSTSUM-PARM                                 
022100                                                                          
022200     END-IF                                                               
022210     .                                                                    
022300     EJECT                                                                
022400 S02-LAS-OSORTERAD-W47815 SECTION.                                        
022500                                                                          
022600     READ W47815-UPPF INTO WSORT-AREA                                     
022700                      AT END MOVE JA TO UPPF-EOF                          
022800     END-READ                                                             
022900     IF UPPF-EOF = NEJ                                                    
023000                                                                          
023100        MOVE 'W47815-UPPF'       TO POSTSUM-FDNAMN                        
023200        MOVE 'W47817D1'          TO POSTSUM-DDNAMN2                       
023300        MOVE WSORT-IDTTYP        TO POSTSUM-TRANSTYP                      
023400        CALL POSTSUM   USING POSTSUM-PARM                                 
023500                                                                          
023600        MOVE WSORT-IDDISTR       TO IDDISTR-WS                            
023610                                    TEST-IDDISTR                          
023700        PERFORM S04-DISTRIKT-TYP                                          
023800     END-IF                                                               
023810     .                                                                    
023900     EJECT                                                                
024000 S03-SAETT-PERSONKOD SECTION.                                             
024100                                                                          
025200     MOVE SPACE                  TO UT-BEPERSON                           
025300     MOVE SPACE                  TO UT-BETELNR                            
025500     .                                                                    
025600     EJECT                                                                
025700 S04-DISTRIKT-TYP  SECTION.                                               
025800                                                                          
025900     IF NOT DIST03-SVERIGE   OR                                           
026000            DIST18-SKROT     OR                                           
026010            DIST20-EMBALLAGE                                              
026100        MOVE 'EXP' TO DISTRIKT-TYP                                        
026200     ELSE                                                                 
026300        MOVE 'SVE' TO DISTRIKT-TYP                                        
026400     END-IF                                                               
026410     .                                                                    
026500 Z-FINIT SECTION.                                                         
026600                                                                          
026700     CLOSE W47817 W47815-UPPF                                             
026800     SKIP2                                                                
026900*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
027000*                                    SKRIVNA POSTER                       
027100                                                                          
027200     MOVE 'S' TO POSTSUM-OPKOD                                            
027300     CALL POSTSUM USING POSTSUM-PARM                                      
027400     .                                                                    
