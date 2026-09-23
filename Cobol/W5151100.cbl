000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W5151100.                                        
000300 AUTHOR.                 ANDERS HENRIKSSON                                
000400 DATE-WRITTEN.           OKOBER 2017.                                     
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       POSTER PÅ FIL FRÅN SAP LÄSES OCH AV URVAL SKAPAS 2 OLIKA          
000900*       FILER. EN FÖR VECKORAPPORTEN W517-SYSTEMET OCH EN FIL             
001000*       SOM BILLIT TAR HAND OM.                                           
001100*                                                                         
001200*       A20 OCH A21 VECKORAPPORT FIL                                      
001300*       ÖVRIGA POSTER BILLIT FIL                                          
001400*                                                                         
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*    ---- INFIL:                                                          
002500*                            - KONTROLLTABELLER I SAP                     
002600     SELECT  W51512        ASSIGN  W51511D1.                              
002700*                                                                         
002800*    ---- UTFIL1                                                          
002900*                            - UPPDATERINGSPOSTER TILL BILLIT             
003000     SELECT  W51513        ASSIGN  W51511D2.                              
003100*    ---- UTFIL2                                                          
003200*                            - TOFS-POSTER TILL                           
003300     SELECT W51511         ASSIGN  W51511D3.                              
003400*                                                                         
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W51512                                                               
004000     LABEL RECORD STANDARD                                                
004100     RECORDING  V                                                         
004200     BLOCK CONTAINS 0.                                                    
004300                                                                          
004400 01  IN-POST        PIC X(1050).                                          
004500     SKIP3                                                                
004600 FD  W51513                                                               
004700     LABEL RECORD STANDARD                                                
004800     RECORDING  F                                                         
004900     BLOCK CONTAINS 0.                                                    
005000                                                                          
005100 01  UT-POST1  PIC X(1050).                                               
005200     SKIP3                                                                
005300 FD  W51511                                                               
005400     LABEL RECORD STANDARD                                                
005500     RECORDING  F                                                         
005600     BLOCK CONTAINS 0.                                                    
005700                                                                          
005800 01  UT-POST2  PIC X(430).                                                
005900     SKIP3                                                                
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300     SKIP3                                                                
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  PROGRAM-NAMN            PIC X(8) VALUE 'W5151100'.                   
006700                                                                          
006800 77  JA                      PIC X       VALUE 'J'.                       
006900 77  NEJ                     PIC X       VALUE 'N'.                       
007000                                                                          
007100                                                                          
007200 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
007300 01  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.                
007400 01  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.                
007500                                                                          
007600                                                                          
007700 01  SPAR-ANALOPNR1-4        PIC 9(4)    VALUE 9999.                      
007800 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
007900                                                                          
008000 01  DAGENS-TIAAVV-GRP.                                                   
008100     03  DAGENS-TIAA         PIC 9(2).                                    
008200     03  DAGENS-TIVV         PIC 9(2).                                    
008300 01  DAGENS-TIAAVV REDEFINES DAGENS-TIAAVV-GRP  PIC 9(4).                 
008400                                                                          
008500 01  SWITCHAR.                                                            
008600     03  INPOST-EOF-SW       PIC X       VALUE 'N'.                       
008700       88  INPOST-EOF                    VALUE 'J'.                       
008800     EJECT                                                                
008900*01  -COPY W0005  -PRE POSTSUM-.                                          
009000     EJECT                                                                
009100 01  IN-TRANSID.                                                          
009200     03  FILLER                 PIC X(6) VALUE 'W51511'.                  
009300     03  FILLER                 PIC X(8) VALUE 'W51511D1'.                
009400     03  IN-TRANSTYP            PIC X(4) VALUE '    '.                    
009500                                                                          
009600 01  UT-TRANSID.                                                          
009700     03  FILLER                 PIC X(6) VALUE 'W51514'.                  
009800     03  FILLER                 PIC X(8) VALUE 'W51511D2'.                
009900     03  UT-TRANSTYP            PIC X(4) VALUE '    '.                    
010000     EJECT                                                                
010100 01  FILLER                     PIC X(8) VALUE 'WDATAREA'.                
010200*01  -COPY WDATAREA                                                       
010300     EJECT                                                                
010400 01  FILLER                  PIC X(16) VALUE 'IN-AREA IN-AREA '.          
010500                                                                          
010600 01  IN-AREA.                                                             
010700     03  IN-RECTYP                PIC X(3).                               
010800     03  IN-KONTO                 PIC X(1047).                            
010900     EJECT                                                                
011000                                                                          
011100 01  FILLER                  PIC X(16) VALUE 'UT-AREA    '.               
011200 01  UT-AREA1                PIC X(1050).                                 
011300     EJECT                                                                
011400                                                                          
011500 01  FILLER                  PIC X(16) VALUE 'UT-AREA    '.               
011600 01  UT-AREA2                PIC X(430).                                  
011700     EJECT                                                                
011800                                                                          
011900 PROCEDURE DIVISION.                                                      
012000                                                                          
012100 STYR SECTION.                                                            
012200     PERFORM A-INIT                                                       
012300     PERFORM S01-LAS-INFIL                                                
012400                                                                          
012500     PERFORM UNTIL INPOST-EOF                                             
012600       EVALUATE IN-RECTYP                                                 
012700         WHEN 'A20'  PERFORM B-SKAPA-TOFS                                 
012800         WHEN 'A21'  PERFORM B-SKAPA-TOFS                                 
012900         WHEN OTHER  PERFORM C-SKAPA-OVRIGT                               
013000       END-EVALUATE                                                       
013100       PERFORM S01-LAS-INFIL                                              
013200     END-PERFORM                                                          
013300                                                                          
013400     PERFORM Z-FINIT                                                      
013500     MOVE ZERO TO RETURN-CODE                                             
013600     GOBACK                                                               
013700     .                                                                    
013800     EJECT                                                                
013900                                                                          
014000 A-INIT       SECTION.                                                    
014100                                                                          
014200     OPEN INPUT  W51512                                                   
014300          OUTPUT W51513                                                   
014400          OUTPUT W51511                                                   
014500                                                                          
014600     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014700     .                                                                    
014800     EJECT                                                                
014900                                                                          
015000 B-SKAPA-TOFS SECTION.                                                    
015100     MOVE IN-AREA TO UT-AREA2                                             
015200     PERFORM S03-SKRIV-TOFS                                               
015300     .                                                                    
015400     EJECT                                                                
015500 C-SKAPA-OVRIGT SECTION.                                                  
015600     MOVE IN-AREA TO UT-AREA1                                             
015700     PERFORM S02-SKRIV-SAP-POST                                           
015800     .                                                                    
015900     EJECT                                                                
016000                                                                          
016100 Z-FINIT   SECTION.                                                       
016200     CLOSE  W51512                                                        
016300            W51513                                                        
016400            W51511                                                        
016500     MOVE 'S' TO POSTSUM-OPKOD                                            
016600     CALL POSTSUM USING POSTSUM-PARM                                      
016700     .                                                                    
016800     EJECT                                                                
016900                                                                          
017000 S01-LAS-INFIL    SECTION.                                                
017100     READ W51512 INTO IN-AREA                                             
017200     AT END                                                               
017300         MOVE JA TO INPOST-EOF-SW                                         
017400     NOT AT END                                                           
017500         MOVE 'IN-POST'    TO POSTSUM-TRANSTYP                            
017600         MOVE 'W51511'     TO POSTSUM-FDNAMN                              
017700         MOVE 'W51511D1'   TO POSTSUM-DDNAMN2                             
017800         CALL POSTSUM USING POSTSUM-PARM                                  
017900     END-READ                                                             
018000     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 S02-SKRIV-SAP-POST SECTION.                                              
018400     WRITE UT-POST1 FROM UT-AREA1                                         
018500                                                                          
018600     MOVE 'UT-SAP '    TO POSTSUM-TRANSTYP                                
018700     MOVE 'W51513'     TO POSTSUM-FDNAMN                                  
018800     MOVE 'W51511D2'   TO POSTSUM-DDNAMN2                                 
018900     CALL POSTSUM USING POSTSUM-PARM                                      
019000     .                                                                    
019100     EJECT                                                                
019200                                                                          
019300 S03-SKRIV-TOFS SECTION.                                                  
019400     WRITE UT-POST2 FROM UT-AREA2                                         
019500                                                                          
019600     MOVE 'UT-TOFS'    TO POSTSUM-TRANSTYP                                
019700     MOVE 'W51511'     TO POSTSUM-FDNAMN                                  
019800     MOVE 'W51511D3'   TO POSTSUM-DDNAMN2                                 
019900     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
