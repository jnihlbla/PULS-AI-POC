000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2363500.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        FÖRBEREDER LISTA FÖR LEVERANSPRECISION                           
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INLEV GÅGNA VECKAN                                         
002500     SELECT W23635                     ASSIGN TO W23635D1.                
002600     SKIP2                                                                
002700*          --- FRISERADE INLEV G V                                        
002800     SELECT W23636                     ASSIGN TO W23635D2.                
002900     SKIP2                                                                
003000*          --- SORTERINGSFIL                                              
003100     SELECT SORTFIL                    ASSIGN TO W23635DS.                
003200     EJECT                                                                
003300*          --- FRISERADE INLEV G V                                        
003400     SELECT W23636X                    ASSIGN TO W23635D3.                
003500     SKIP2                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W23635                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W23635      -L.                                                
004500     SKIP3                                                                
004600 FD  W23636                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W23636 -PRE  UT-  -L.                                     
005100     SKIP2                                                                
005200 SD  SORTFIL.                                                             
005300                                                                          
005400*01  POST -COPY W23635      -PRE SORT-                                    
005500     EJECT                                                                
005600 FD  W23636X                                                              
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  POST -COPY W23636X -PRE  UTX-  -L.                                   
006100     SKIP2                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W2363500'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700                                                                          
006800 77  W23635-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W23635                       VALUE 'J'.                   
007000                                                                          
007100 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007200     88  END-OF-SORTFIL                      VALUE 'J'.                   
007300                                                                          
007400 01  ARBETSAREOR.                                                         
007500     03  WS-AAAAVVD              PIC 9(7).                                
007600     03  FILLER REDEFINES WS-AAAAVVD.                                     
007700         05  FILLER              PIC 9(2).                                
007800         05  WS-AAVVD            PIC 9(5).                                
007900     03  FILLER REDEFINES WS-AAAAVVD.                                     
008000         05  WS-AAAAVV           PIC 9(6).                                
008100         05  WS-TILEVDAG         PIC 9(1).                                
008200     03  PAH-TIAVIDAT            PIC 9(7).                                
008300     EJECT                                                                
008400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES DAGENS-DATUM.                                       
008600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009400     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
009500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009600     SKIP2                                                                
009700*    --- PARAMETRAR TILL ABEND                                            
009800                                                                          
009900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010200     SKIP2                                                                
010300 01  FELTEXT.                                                             
010400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL POSTSUM                                          
010800*                                                                         
010900*01  -COPY W0005   -PRE  POSTSUM-                                         
011000     EJECT                                                                
011100*01  -COPY WORKAREA                                                       
011200     EJECT                                                                
011300*01  -COPY WDATAREA                                                       
011400     EJECT                                                                
011500 01  IN-AREA-START               PIC X(24)   VALUE                        
011600                                 'IN-AREA-START  '.                       
011700     SKIP2                                                                
011800                                                                          
011900*01  AREA -COPY W23635     -PRE IN-                                       
012000     EJECT                                                                
012100 01  UT-AREA-START               PIC X(24)   VALUE                        
012200                                 'UT-AREA-START  '.                       
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W23636     -PRE UT-                                       
012600     EJECT                                                                
012700 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
012800                                  'SORTWS-AREA-START  '.                  
012900     SKIP2                                                                
013000                                                                          
013100*01  AREA -COPY W23635      -PRE SORTWS-                                  
013200 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
013300     EJECT                                                                
013400 01  UTX-AREA-START               PIC X(24)   VALUE                       
013410                                 'UTX-AREA-START  '.                      
013420     SKIP2                                                                
013430                                                                          
013440*01  AREA -COPY W23636X     -PRE UTX-                                     
013500 PROCEDURE DIVISION.                                                      
013600 MAIN SECTION.                                                            
013700     SKIP2                                                                
013800                                                                          
013900     PERFORM A-INIT                                                       
014000                                                                          
014100     SORT SORTFIL ASCENDING KEY SORT-IDLEVNR                              
014200                                SORT-IDLOPNRM                             
014300                                SORT-IDARTNR                              
014400                  INPUT  PROCEDURE B-SORT-INPUT                           
014500                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
014600                                                                          
014700     IF SORT-RETURN NOT = 0                                               
014800       MOVE SORT-RETURN TO SORT-RETURN-X                                  
014900       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
015000       DELIMITED BY SIZE INTO FELTEXT-STR                                 
015100       DISPLAY FELTEXT                                                    
015200       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
015300       PERFORM S99-ABEND                                                  
015400     ELSE                                                                 
015500       PERFORM Z-FINIT                                                    
015600                                                                          
015700       MOVE ZERO TO RETURN-CODE                                           
015800       GOBACK                                                             
015900     END-IF                                                               
016000                                                                          
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     OPEN INPUT  W23635                                                   
016600                                                                          
016700     OPEN OUTPUT W23636                                                   
016710                 W23636X                                                  
016800     SKIP2                                                                
016900     ACCEPT DAGENS-DATUM  FROM DATE                                       
017000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017100     .                                                                    
017200     EJECT                                                                
017300 B-SORT-INPUT  SECTION.                                                   
017400                                                                          
017500     PERFORM S01-LAES-W23635                                              
017600     PERFORM UNTIL END-OF-W23635                                          
017700                                                                          
017800         MOVE IN-AREA TO SORTWS-AREA                                      
017900         PERFORM S31-SORT-RELEASE                                         
018000                                                                          
018100       PERFORM S01-LAES-W23635                                            
018200     END-PERFORM                                                          
018300     .                                                                    
018400     EJECT                                                                
018500 C-SORT-OUTPUT SECTION.                                                   
018600     SKIP2                                                                
018700     PERFORM S32-SORT-RETURN                                              
018800     PERFORM UNTIL END-OF-SORTFIL                                         
018900       MOVE SORTWS-AREA TO UT-AREA                                        
018910                                                                          
018911       PERFORM CA-FIXA-DATUM                                              
018920       MOVE UT-IDARTNR     TO UTX-IDARTNR                                 
018930       MOVE UT-IDLEVNR     TO UTX-IDLEVNR                                 
018940       MOVE UT-IDLOPNRM    TO UTX-IDLOPNRM                                
018950       MOVE UT-TIAVIDAT    TO UTX-TIAVIDAT                                
018960       MOVE UT-KVAVIS      TO UTX-KVAVIS                                  
018970       MOVE UT-TIUPPDAT    TO UTX-TIUPPDAT                                
018980       MOVE UT-KDEFFMAN    TO UTX-KDEFFMAN                                
018990       MOVE UT-KVDAGAR-TT  TO UTX-KVDAGAR-TT                              
018991       MOVE UT-KVDAGAR-INLEV  TO UTX-KVDAGAR-INLEV                        
018992       MOVE UT-BEFT        TO UTX-BEFT                                    
018993       MOVE UT-IDANSK      TO UTX-IDANSK                                  
018994       MOVE UT-DAAVROP-AVS TO UTX-DAAVROP-AVS                             
018995       MOVE UT-TILEVDAG    TO UTX-TILEVDAG                                
018996       MOVE UT-KVAVROP-AVB TO UTX-KVAVROP-AVB                             
018997       MOVE UT-TIAVROP-AAMMDD  TO UTX-TIAVROP-AAMMDD                      
018998       MOVE UT-KVDAGAR     TO UTX-KVDAGAR                                 
019000                                                                          
019200       PERFORM S11-SKRIV-W23636                                           
019210       PERFORM S12-SKRIV-W23636X                                          
019300                                                                          
019400       PERFORM S32-SORT-RETURN                                            
019500     END-PERFORM                                                          
019600     .                                                                    
019700     EJECT                                                                
019800 CA-FIXA-DATUM SECTION.                                                   
019900     SKIP2                                                                
020000     MOVE SORTWS-DAAVROP-AVS   TO WS-AAAAVV                               
020100     MOVE SORTWS-TILEVDAG      TO WS-TILEVDAG                             
020200     MOVE 'AAVVD'              TO DAT-KDDATFORM                           
020300     MOVE WS-AAVVD             TO DAT-I-TIDATUM                           
020400                                                                          
020500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
020600                     DAT-O-TIDATUM DAT-KDSVAR                             
020700                                                                          
020800     IF DAT-KDSVAR-OK                                                     
020900       MOVE DAT-TIAAMMDD       TO UT-TIAVROP-AAMMDD                       
021000     ELSE                                                                 
021100       DISPLAY 'FEL I DATKONV 1 '                                         
021200       PERFORM S99-ABEND                                                  
021300     END-IF                                                               
021400                                                                          
021500     IF UT-TIAVROP-AAMMDD = UT-TIAVIDAT                                   
021600        MOVE ZERO              TO UT-KVDAGAR                              
021700     ELSE                                                                 
021800      IF UT-TIAVROP-AAMMDD > UT-TIAVIDAT AND                              
021900         UT-TIAVROP-AAMMDD < 500000                                       
022000        MOVE 001               TO WORK-KDCALL                             
022100        MOVE '11'              TO WORK-IDDC                               
022200        MOVE UT-TIAVIDAT       TO WORK-TIAAMMDD-FOM                       
022300        MOVE UT-TIAVROP-AAMMDD TO WORK-TIAAMMDD-TOM                       
022400        CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR         
022500        IF WORK-KDSVAR-FEL                                                
022600           DISPLAY ' FEL I WORKDAY 1 '                                    
022700           DISPLAY ' UT-TIAVROP-AAMMDD ' UT-TIAVROP-AAMMDD                
022800           MOVE UT-TIAVIDAT TO PAH-TIAVIDAT                               
022900           DISPLAY ' UT-TIAVIDAT ' PAH-TIAVIDAT                           
023000           PERFORM S99-ABEND                                              
023100        END-IF                                                            
023200        MOVE WORK-KVWORKD      TO UT-KVDAGAR                              
023300        COMPUTE UT-KVDAGAR = -1 * UT-KVDAGAR                              
023400      ELSE                                                                
023500        MOVE 001               TO WORK-KDCALL                             
023600        MOVE '11'              TO WORK-IDDC                               
023700        MOVE UT-TIAVIDAT       TO WORK-TIAAMMDD-TOM                       
023800        MOVE UT-TIAVROP-AAMMDD TO WORK-TIAAMMDD-FOM                       
023900        CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR         
024000        IF WORK-KDSVAR-FEL                                                
024100           DISPLAY ' FEL I WORKDAY 2 '                                    
024200           PERFORM S99-ABEND                                              
024300        END-IF                                                            
024400        MOVE WORK-KVWORKD      TO UT-KVDAGAR                              
024500      END-IF                                                              
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 Z-FINIT SECTION.                                                         
025000     CLOSE W23635                                                         
025100           W23636                                                         
025110           W23636X                                                        
025200     SKIP2                                                                
025300     MOVE 'S' TO POSTSUM-OPKOD                                            
025400     CALL POSTSUM USING POSTSUM-PARM                                      
025500     .                                                                    
025600     EJECT                                                                
025700 S01-LAES-W23635  SECTION.                                                
025800     READ W23635 INTO IN-AREA                                             
025900     AT END                                                               
026000        SET END-OF-W23635 TO TRUE                                         
026100                                                                          
026200     NOT AT END                                                           
026300        MOVE 'W23635'   TO POSTSUM-FDNAMN                                 
026400        MOVE 'W23635D1' TO POSTSUM-DDNAMN2                                
026500        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
026600        CALL POSTSUM USING POSTSUM-PARM                                   
026700     END-READ                                                             
026800     .                                                                    
026900     EJECT                                                                
027000 S11-SKRIV-W23636 SECTION.                                                
027100                                                                          
027200     WRITE UT-POST FROM UT-AREA                                           
027300                                                                          
027400     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
027500     MOVE 'W23636'   TO POSTSUM-FDNAMN                                    
027600     MOVE 'W23635D2' TO POSTSUM-DDNAMN2                                   
027700     CALL POSTSUM USING POSTSUM-PARM                                      
027800     .                                                                    
027900     EJECT                                                                
027910 S12-SKRIV-W23636X SECTION.                                               
027920                                                                          
027930     WRITE UTX-POST FROM UTX-AREA                                         
027940                                                                          
027950     MOVE 'UTX'       TO POSTSUM-TRANSTYP                                 
027960     MOVE 'W23636X'   TO POSTSUM-FDNAMN                                   
027970     MOVE 'W23635D3' TO POSTSUM-DDNAMN2                                   
027980     CALL POSTSUM USING POSTSUM-PARM                                      
027990     .                                                                    
027991     EJECT                                                                
028000 S31-SORT-RELEASE  SECTION.                                               
028100                                                                          
028200     RELEASE SORT-POST FROM SORTWS-AREA                                   
028300     .                                                                    
028400     EJECT                                                                
028500 S32-SORT-RETURN  SECTION.                                                
028600                                                                          
028700     RETURN SORTFIL INTO SORTWS-AREA                                      
028800     AT END                                                               
028900         SET END-OF-SORTFIL TO TRUE                                       
029000     .                                                                    
029100     EJECT                                                                
029200 S99-ABEND SECTION.                                                       
029300                                                                          
029400     SKIP2                                                                
029500     MOVE 'S' TO POSTSUM-OPKOD                                            
029600     CALL POSTSUM USING POSTSUM-PARM                                      
029700     CALL ABEND USING RKOD-ABEND                                          
029800     .                                                                    
