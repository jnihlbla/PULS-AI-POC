000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W5611100.                                        
000300 AUTHOR.                 PRERNA DADHICH                                   
000400 DATE-WRITTEN.           DECMBER 2017.                                    
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
001500*                                                                         
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*    ---- INFIL:                                                          
002400*                            - KONTROLLTABELLER I SAP                     
002500     SELECT  W56112        ASSIGN  W56111D1.                              
002600*                                                                         
002700*    ---- UTFIL1                                                          
002800*                            - UPPDATERINGSPOSTER TILL BILLIT             
002900     SELECT  W56113        ASSIGN  W56111D2.                              
003000*    ---- UTFIL2                                                          
003100*                            - TOFS-POSTER TILL                           
003200     SELECT W56111         ASSIGN  W56111D3.                              
003300*                                                                         
003400 DATA DIVISION.                                                           
003500                                                                          
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W56112                                                               
003900     LABEL RECORD STANDARD                                                
004000     RECORDING  V                                                         
004100     BLOCK CONTAINS 0.                                                    
004200                                                                          
004300 01  IN-POST        PIC X(1050).                                          
004400     SKIP3                                                                
004500 FD  W56113                                                               
004600     LABEL RECORD STANDARD                                                
004700     RECORDING  F                                                         
004800     BLOCK CONTAINS 0.                                                    
004900                                                                          
005000 01  UT-POST1  PIC X(1050).                                               
005100     SKIP3                                                                
005200 FD  W56111                                                               
005300     LABEL RECORD STANDARD                                                
005400     RECORDING  F                                                         
005500     BLOCK CONTAINS 0.                                                    
005600                                                                          
005700 01  UT-POST2  PIC X(430).                                                
005800     SKIP3                                                                
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100                                                                          
006200     SKIP3                                                                
006300                                                                          
006400*    -- CHECKED BY WY2000                                                 
006500 77  PROGRAM-NAMN            PIC X(8) VALUE 'W5611100'.                   
006600                                                                          
006700 77  JA                      PIC X       VALUE 'J'.                       
006800 77  NEJ                     PIC X       VALUE 'N'.                       
006900                                                                          
007000                                                                          
007100 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
007200 01  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.                
007300 01  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.                
007400                                                                          
007500                                                                          
007600 01  SPAR-ANALOPNR1-4        PIC 9(4)    VALUE 9999.                      
007700 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
007800                                                                          
007900 01  DAGENS-TIAAVV-GRP.                                                   
008000     03  DAGENS-TIAA         PIC 9(2).                                    
008100     03  DAGENS-TIVV         PIC 9(2).                                    
008200 01  DAGENS-TIAAVV REDEFINES DAGENS-TIAAVV-GRP  PIC 9(4).                 
008300                                                                          
008400 01  SWITCHAR.                                                            
008500     03  INPOST-EOF-SW       PIC X       VALUE 'N'.                       
008600       88  INPOST-EOF                    VALUE 'J'.                       
008700     EJECT                                                                
008800*01  -COPY W0005  -PRE POSTSUM-.                                          
008900     EJECT                                                                
009000 01  IN-TRANSID.                                                          
009100     03  FILLER                 PIC X(6) VALUE 'W56111'.                  
009200     03  FILLER                 PIC X(8) VALUE 'W56111D1'.                
009300     03  IN-TRANSTYP            PIC X(4) VALUE '    '.                    
009400                                                                          
009500 01  UT-TRANSID.                                                          
009600     03  FILLER                 PIC X(6) VALUE 'W56114'.                  
009700     03  FILLER                 PIC X(8) VALUE 'W56111D2'.                
009800     03  UT-TRANSTYP            PIC X(4) VALUE '    '.                    
009900     EJECT                                                                
010000 01  FILLER                     PIC X(8) VALUE 'WDATAREA'.                
010100*01  -COPY WDATAREA                                                       
010200     EJECT                                                                
010300 01  FILLER                  PIC X(16) VALUE 'IN-AREA IN-AREA '.          
010400                                                                          
010500 01  IN-AREA.                                                             
010600     03  IN-RECTYP                PIC X(3).                               
010700     03  IN-KONTO                 PIC X(1047).                            
010800     EJECT                                                                
010900                                                                          
011000 01  FILLER                  PIC X(16) VALUE 'UT-AREA    '.               
011100 01  UT-AREA1                PIC X(1050).                                 
011200     EJECT                                                                
011300                                                                          
011400 01  FILLER                  PIC X(16) VALUE 'UT-AREA    '.               
011500 01  UT-AREA2                PIC X(430).                                  
011600     EJECT                                                                
011700                                                                          
011800 PROCEDURE DIVISION.                                                      
011900                                                                          
012000 STYR SECTION.                                                            
012100     PERFORM A-INIT                                                       
012200     PERFORM S01-LAS-INFIL                                                
012300                                                                          
012400     PERFORM UNTIL INPOST-EOF                                             
012500       EVALUATE IN-RECTYP                                                 
012600         WHEN 'A20'  PERFORM B-SKAPA-TOFS                                 
012700         WHEN 'A21'  PERFORM B-SKAPA-TOFS                                 
012800         WHEN OTHER  PERFORM C-SKAPA-OVRIGT                               
012900       END-EVALUATE                                                       
013000       PERFORM S01-LAS-INFIL                                              
013100     END-PERFORM                                                          
013200                                                                          
013300     PERFORM Z-FINIT                                                      
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800                                                                          
013900 A-INIT       SECTION.                                                    
014000                                                                          
014100     OPEN INPUT  W56112                                                   
014200          OUTPUT W56113                                                   
014300          OUTPUT W56111                                                   
014400                                                                          
014500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
014600     .                                                                    
014700     EJECT                                                                
014800                                                                          
014900 B-SKAPA-TOFS SECTION.                                                    
015000     MOVE IN-AREA TO UT-AREA2                                             
015100     PERFORM S03-SKRIV-TOFS                                               
015200     .                                                                    
015300     EJECT                                                                
015400 C-SKAPA-OVRIGT SECTION.                                                  
015500     MOVE IN-AREA TO UT-AREA1                                             
015600     PERFORM S02-SKRIV-SAP-POST                                           
015700     .                                                                    
015800     EJECT                                                                
015900                                                                          
016000 Z-FINIT   SECTION.                                                       
016100     CLOSE  W56112                                                        
016200            W56113                                                        
016300            W56111                                                        
016400     MOVE 'S' TO POSTSUM-OPKOD                                            
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600     .                                                                    
016700     EJECT                                                                
016800                                                                          
016900 S01-LAS-INFIL    SECTION.                                                
017000     READ W56112 INTO IN-AREA                                             
017100     AT END                                                               
017200         MOVE JA TO INPOST-EOF-SW                                         
017300     NOT AT END                                                           
017400         MOVE 'IN-POST'    TO POSTSUM-TRANSTYP                            
017500         MOVE 'W56111'     TO POSTSUM-FDNAMN                              
017600         MOVE 'W56111D1'   TO POSTSUM-DDNAMN2                             
017700         CALL POSTSUM USING POSTSUM-PARM                                  
017800     END-READ                                                             
017900     .                                                                    
018000     EJECT                                                                
018100                                                                          
018200 S02-SKRIV-SAP-POST SECTION.                                              
018300     WRITE UT-POST1 FROM UT-AREA1                                         
018400                                                                          
018500     MOVE 'UT-SAP '    TO POSTSUM-TRANSTYP                                
018600     MOVE 'W56113'     TO POSTSUM-FDNAMN                                  
018700     MOVE 'W56111D2'   TO POSTSUM-DDNAMN2                                 
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018900     .                                                                    
019000     EJECT                                                                
019100                                                                          
019200 S03-SKRIV-TOFS SECTION.                                                  
019300     WRITE UT-POST2 FROM UT-AREA2                                         
019400                                                                          
019500     MOVE 'UT-TOFS'    TO POSTSUM-TRANSTYP                                
019600     MOVE 'W56111'     TO POSTSUM-FDNAMN                                  
019700     MOVE 'W56111D3'   TO POSTSUM-DDNAMN2                                 
019800     CALL POSTSUM USING POSTSUM-PARM                                      
019900     .                                                                    
