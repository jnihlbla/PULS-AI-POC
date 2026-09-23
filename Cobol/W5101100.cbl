000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W5101100.                                        
000300 AUTHOR.                 MARKUS ASPFJÄLL                                  
000400 DATE-WRITTEN.           NOVEMBER 1998.                                   
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       POSTER PÅ FIL FRÅN SAP LÄSES OCH AV URVAL SKAPAS 2 OLIKA          
000900*       FILER. EN FÖR VECKORAPPORTEN W517-SYSTEMET OCH EN FIL             
001000*       SOM BILLIT TAR HAND OM.                                           
001100*                                                                         
001200*       A20 OCH A21 VECKORAPPORT FIL                                      
001300*       ÖVRIGA POSTER BILLIT FIL                                          
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*    ---- INFIL:                                                          
002300*                            - KONTROLLTABELLER I SAP                     
002400     SELECT  W51012        ASSIGN  W51011D1.                              
002500*                                                                         
002600*    ---- UTFIL1                                                          
002700*                            - UPPDATERINGSPOSTER TILL BILLIT             
002800     SELECT  W51013        ASSIGN  W51011D2.                              
002900*    ---- UTFIL2                                                          
003000*                            - TOFS-POSTER TILL                           
003100     SELECT W51011         ASSIGN  W51011D3.                              
003200*                                                                         
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W51012                                                               
003800     LABEL RECORD STANDARD                                                
003900     RECORDING  V                                                         
004000     BLOCK CONTAINS 0.                                                    
004100                                                                          
004210 01  IN-POST        PIC X(1050).                                          
004300     SKIP3                                                                
004400 FD  W51013                                                               
004500     LABEL RECORD STANDARD                                                
004600     RECORDING  F                                                         
004700     BLOCK CONTAINS 0.                                                    
004800                                                                          
004900 01  UT-POST1  PIC X(1050).                                               
005000     SKIP3                                                                
005100 FD  W51011                                                               
005200     LABEL RECORD STANDARD                                                
005300     RECORDING  F                                                         
005400     BLOCK CONTAINS 0.                                                    
005500                                                                          
005600 01  UT-POST2  PIC X(430).                                                
005700     SKIP3                                                                
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100     SKIP3                                                                
006200                                                                          
006300*    -- CHECKED BY WY2000                                                 
006400 77  PROGRAM-NAMN            PIC X(8) VALUE 'W5101100'.                   
006500                                                                          
006600 77  JA                      PIC X       VALUE 'J'.                       
006700 77  NEJ                     PIC X       VALUE 'N'.                       
006800                                                                          
006900                                                                          
007000 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
007100 01  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.                
007200 01  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.                
007300                                                                          
007400                                                                          
007500 01  SPAR-ANALOPNR1-4        PIC 9(4)    VALUE 9999.                      
007600 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
007700                                                                          
007800 01  DAGENS-TIAAVV-GRP.                                                   
007900     03  DAGENS-TIAA         PIC 9(2).                                    
008000     03  DAGENS-TIVV         PIC 9(2).                                    
008100 01  DAGENS-TIAAVV REDEFINES DAGENS-TIAAVV-GRP  PIC 9(4).                 
008200                                                                          
008300 01  SWITCHAR.                                                            
008400     03  INPOST-EOF-SW       PIC X       VALUE 'N'.                       
008500       88  INPOST-EOF                    VALUE 'J'.                       
008600     EJECT                                                                
008700*01  -COPY W0005  -PRE POSTSUM-.                                          
008800     EJECT                                                                
008900 01  IN-TRANSID.                                                          
009000     03  FILLER                 PIC X(6) VALUE 'W51011'.                  
009100     03  FILLER                 PIC X(8) VALUE 'W51011D1'.                
009200     03  IN-TRANSTYP            PIC X(4) VALUE '    '.                    
009300                                                                          
009400 01  UT-TRANSID.                                                          
009500     03  FILLER                 PIC X(6) VALUE 'W51014'.                  
009600     03  FILLER                 PIC X(8) VALUE 'W51011D2'.                
009700     03  UT-TRANSTYP            PIC X(4) VALUE '    '.                    
009800     EJECT                                                                
009900 01  FILLER                     PIC X(8) VALUE 'WDATAREA'.                
010000*01  -COPY WDATAREA                                                       
010100     EJECT                                                                
010200 01  FILLER                  PIC X(16) VALUE 'IN-AREA IN-AREA '.          
010300                                                                          
010400 01  IN-AREA.                                                             
010500     03  IN-RECTYP                PIC X(3).                               
010610     03  IN-KONTO                 PIC X(1047).                            
013300     EJECT                                                                
013400                                                                          
013500 01  FILLER                  PIC X(16) VALUE 'UT-AREA    '.               
013600 01  UT-AREA1                PIC X(1050).                                 
016100     EJECT                                                                
016101                                                                          
016110 01  FILLER                  PIC X(16) VALUE 'UT-AREA    '.               
016120 01  UT-AREA2                PIC X(430).                                  
016130     EJECT                                                                
016200                                                                          
016700 PROCEDURE DIVISION.                                                      
016800                                                                          
016900 STYR SECTION.                                                            
017000     PERFORM A-INIT                                                       
017100     PERFORM S01-LAS-INFIL                                                
017200                                                                          
017300     PERFORM UNTIL INPOST-EOF                                             
017400       EVALUATE IN-RECTYP                                                 
018700         WHEN 'A20'  PERFORM B-SKAPA-TOFS                                 
018701         WHEN 'A21'  PERFORM B-SKAPA-TOFS                                 
018702         WHEN OTHER  PERFORM C-SKAPA-OVRIGT                               
018800       END-EVALUATE                                                       
018900       PERFORM S01-LAS-INFIL                                              
019000     END-PERFORM                                                          
019100                                                                          
019200     PERFORM Z-FINIT                                                      
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700                                                                          
019800 A-INIT       SECTION.                                                    
020900                                                                          
021000     OPEN INPUT  W51012                                                   
021100          OUTPUT W51013                                                   
021200          OUTPUT W51011                                                   
021300                                                                          
021400     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
021500     .                                                                    
021600     EJECT                                                                
021700                                                                          
027600 B-SKAPA-TOFS SECTION.                                                    
027700     MOVE IN-AREA TO UT-AREA2                                             
027800     PERFORM S03-SKRIV-TOFS                                               
027900     .                                                                    
028000     EJECT                                                                
028010 C-SKAPA-OVRIGT SECTION.                                                  
028020     MOVE IN-AREA TO UT-AREA1                                             
028030     PERFORM S02-SKRIV-SAP-POST                                           
028040     .                                                                    
028050     EJECT                                                                
028100                                                                          
028200 Z-FINIT   SECTION.                                                       
028300     CLOSE  W51012                                                        
028400            W51013                                                        
028500            W51011                                                        
028600     MOVE 'S' TO POSTSUM-OPKOD                                            
028700     CALL POSTSUM USING POSTSUM-PARM                                      
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100 S01-LAS-INFIL    SECTION.                                                
029200     READ W51012 INTO IN-AREA                                             
029300     AT END                                                               
029400         MOVE JA TO INPOST-EOF-SW                                         
029500     NOT AT END                                                           
029600         MOVE 'IN-POST'    TO POSTSUM-TRANSTYP                            
029700         MOVE 'W51011'     TO POSTSUM-FDNAMN                              
029800         MOVE 'W51011D1'   TO POSTSUM-DDNAMN2                             
029900         CALL POSTSUM USING POSTSUM-PARM                                  
030000     END-READ                                                             
030100     .                                                                    
030200     EJECT                                                                
030300                                                                          
030400 S02-SKRIV-SAP-POST SECTION.                                              
030500     WRITE UT-POST1 FROM UT-AREA1                                         
030600                                                                          
030700     MOVE 'UT-SAP '    TO POSTSUM-TRANSTYP                                
030800     MOVE 'W51013'     TO POSTSUM-FDNAMN                                  
030900     MOVE 'W51011D2'   TO POSTSUM-DDNAMN2                                 
031000     CALL POSTSUM USING POSTSUM-PARM                                      
031100     .                                                                    
031200     EJECT                                                                
031300                                                                          
031400 S03-SKRIV-TOFS SECTION.                                                  
031500     WRITE UT-POST2 FROM UT-AREA2                                         
031600                                                                          
031700     MOVE 'UT-TOFS'    TO POSTSUM-TRANSTYP                                
031800     MOVE 'W51011'     TO POSTSUM-FDNAMN                                  
031900     MOVE 'W51011D3'   TO POSTSUM-DDNAMN2                                 
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     .                                                                    
