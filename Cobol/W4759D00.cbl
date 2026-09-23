000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4759D00.                                                
000300 AUTHOR.         ARUP DATTA                                               
000400 DATE-WRITTEN.   DEC 2013.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THE PGM                                                          
000900*        - READS INPUT FILE WITH INTRASTAT ITEMS TO TRANSPORT             
001000*        - CREATES DATA RECORDS FOR ITEMS TO VOLVO TRANSPORT              
001100*          (EXCEL).                                                       
001200*        - TEMPORARY SOLUTION                                             
001300*          TO DISTRIBUTION & PRINT BY USING WZ01SEND                      
001400*                                                                         
001500*                                                                         
001600*    CHANGE LOG:                                                          
001700*                                                                         
001800*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001900*      ----------------------------------------------------------         
002000*      15/10/12 - REDDY RAHUL     - ADD STAT NUMBER AND WEIGHT            
002100*                                   IN INTRASTE FILES.                    
002200*                                   E'TRACKER 10265098                    
002300*                                                                         
002400*      21/12/28 - CAMELIA O,      - ADD VAT CODE, COUNTRY OF OR.          
002500*                                   AND TRANS. CODE                       
002600*                                   IN INTRASTE FILES.                    
002700*                                   STORY 2523293                         
002800*                                                                         
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500*          --- INFIL                                                      
003600     SELECT W4759A                     ASSIGN TO W4759DD1.                
003700*          --- UTFIL                                                      
003800     SELECT W4759D                     ASSIGN TO W4759DD2.                
003900                                                                          
004000 DATA DIVISION.                                                           
004100                                                                          
004200 FILE SECTION.                                                            
004300 FD  W4759A                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY A7290A01     -L.                                               
004800                                                                          
004900     EJECT                                                                
005000 FD  W4759D                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400 01  UT-POST       PIC X(250).                                            
005500     EJECT                                                                
005600                                                                          
005700 WORKING-STORAGE SECTION.                                                 
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W4759D00'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006600 77  WS-LAST-DAY                 PIC X(10)        VALUE SPACE.            
006700 77  WS-INV-NUM                  PIC X(23)        VALUE SPACE.            
006800 77  WS-DAT-YYYY                 PIC 9(4)         VALUE 0.                
006900 77  WS-DAT-MM                   PIC 9(2)         VALUE 0.                
007000 77  WS-DAT-DD                   PIC 9(2)         VALUE 0.                
007100                                                                          
007200 77  W4759A-EOF-SW               PIC X       VALUE 'N'.                   
007300     88  END-OF-W4759A                       VALUE 'J'.                   
007400                                                                          
007500 01  W-ANTAL-FORMAT              PIC -Z(11)9.                             
007600                                                                          
007700 01  W-VARDE-FORMAT              PIC -Z(12)9.99.                          
007800                                                                          
007900 01  W-IDSTATNR-FORMAT           PIC -Z(09).                              
008000                                                                          
008010 01  W-KDINTTYP-FORMAT           PIC -Z(01)9.                             
008020                                                                          
008100 01  W-VKARTTOT-FORMAT           PIC -Z(15)9.99.                          
008200                                                                          
008300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008400 01  FILLER REDEFINES DAGENS-DATUM.                                       
008500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008800                                                                          
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009200                                                                          
009300 01  FELTEXT.                                                             
009400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009600     EJECT                                                                
009700                                                                          
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100                                                                          
010200 01  IN-AREA-START               PIC X(24)   VALUE                        
010300                                 'IN-AREA-START  '.                       
010400*01  AREA -COPY A7290A01   -PRE IN-                                       
010500     EJECT                                                                
010600                                                                          
010700 01  UT-AREA-START           PIC X(24)   VALUE                            
010800                                 'UT-AREA-START '.                        
010900 01  UT-AREA                 PIC X(250).                                  
011000     EJECT                                                                
011100                                                                          
011200 PROCEDURE DIVISION.                                                      
011300                                                                          
011400 MAIN SECTION.                                                            
011500                                                                          
011600     PERFORM A-INIT                                                       
011700                                                                          
011800     PERFORM S11-READ-W4759A                                              
011900     PERFORM UNTIL END-OF-W4759A                                          
012000       IF IN-POSTTYP = '01'                                               
012100          MOVE IN-FILDATUM(1:4) TO WS-DAT-YYYY                            
012200          MOVE IN-FILDATUM(5:2) TO WS-DAT-MM                              
012300          MOVE IN-FILDATUM(7:2) TO WS-DAT-DD                              
012400          STRING WS-DAT-DD '.'                                            
012500                 WS-DAT-MM '.'                                            
012600                 WS-DAT-YYYY                                              
012700          DELIMITED BY SIZE   INTO WS-LAST-DAY                            
012800       ELSE                                                               
012900         IF IN-VARDE OF IN-ARTIKEL-AREA = 0                               
013000            CONTINUE                                                      
013100         ELSE                                                             
013200            PERFORM B-CREATE-PUT-LINE                                     
013300            PERFORM S12-WRITE-W4759D                                      
013400         END-IF                                                           
013500       END-IF                                                             
013600       PERFORM S11-READ-W4759A                                            
013700                                                                          
013800     END-PERFORM                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500                                                                          
014600 A-INIT SECTION.                                                          
014700     OPEN INPUT W4759A                                                    
014800     OPEN OUTPUT W4759D                                                   
014900     ACCEPT DAGENS-DATUM FROM DATE                                        
015000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 B-CREATE-PUT-LINE SECTION.                                               
015500                                                                          
016000     MOVE SPACE          TO UT-AREA                                       
017000                                                                          
017100     MOVE IN-VARDE OF IN-ARTIKEL-AREA                                     
017200                         TO W-VARDE-FORMAT                                
017300     INSPECT W-VARDE-FORMAT REPLACING ALL '.' BY ','                      
017400     MOVE IN-ANTAL OF IN-ARTIKEL-AREA                                     
017500                         TO W-ANTAL-FORMAT                                
017600     MOVE IN-IDSTATNR    TO W-IDSTATNR-FORMAT                             
017610     MOVE IN-KDINTTYP    TO W-KDINTTYP-FORMAT                             
017700     COMPUTE W-VKARTTOT-FORMAT = (IN-VKARTTOT / 1000)                     
017800     INSPECT W-VKARTTOT-FORMAT REPLACING ALL '.' BY ','                   
017900                                                                          
018000     STRING 'D'                                 '-'                       
018100            IN-LANDKOD   OF IN-ARTIKEL-AREA     '-'                       
018200            '1441001'                           '-'                       
018300            WS-LAST-DAY                                                   
018400     DELIMITED BY SIZE INTO WS-INV-NUM                                    
018500                                                                          
018600     STRING 'SE'                                ';'                       
018700            'S1'                                ';'                       
018800            'V1'                                ';'                       
018900            WS-LAST-DAY                         ';'                       
019000            WS-INV-NUM                          ';'                       
019100            '1441001'                           ';'                       
019200            ' '                                 ';'                       
019300            ' '                                 ';'                       
019400            ' '                                 ';'                       
019501            IN-IDVAT     OF IN-ARTIKEL-AREA     ';'                       
019600            IN-LANDKOD   OF IN-ARTIKEL-AREA     ';'                       
019700            '1'                                 ';'                       
019800            ' '                                 ';'                       
019900            ' '                                 ';'                       
020000            ' '                                 ';'                       
020100            'SEK'                               ';'                       
020200            ' '                                 ';'                       
020300            ' '                                 ';'                       
020400            ' '                                 ';'                       
020500            ' '                                 ';'                       
020600            ' '                                 ';'                       
020700            ' '                                 ';'                       
020800            ' '                                 ';'                       
020900            ' '                                 ';'                       
021000            ' '                                 ';'                       
021100            ' '                                 ';'                       
021200            ' '                                 ';'                       
021300            ' '                                 ';'                       
021400            ' '                                 ';'                       
021500            ' '                                 ';'                       
021600            'i'                                 ';'                       
021700            ' '                                 ';'                       
021800            ' '                                 ';'                       
021900            ' '                                 ';'                       
022000            ' '                                 ';'                       
022100            ' '                                 ';'                       
022200            ' '                                 ';'                       
022300            ' '                                 ';'                       
022400            ' '                                 ';'                       
022500            ' '                                 ';'                       
022600            ' '                                 ';'                       
022700       IN-ARTIKELNR      OF IN-ARTIKEL-AREA     ';'                       
022800            ' '                                 ';'                       
022900       W-IDSTATNR-FORMAT                        ';'                       
023001       IN-KDARTURS       OF IN-ARTIKEL-AREA     ';'                       
023100            ' '                                 ';'                       
023201       w-KDINTTYP-FORMAT                        ';'                       
023300       W-ANTAL-FORMAT                           ';'                       
023400            ' '                                 ';'                       
023500       W-VKARTTOT-FORMAT                        ';'                       
023600       W-ANTAL-FORMAT                           ';'                       
023700            ' '                                 ';'                       
023800       W-VARDE-FORMAT                           ';'                       
023900     DELIMITED BY SIZE INTO UT-AREA                                       
024000     .                                                                    
024100     EJECT                                                                
024200                                                                          
024300 Z-FINIT SECTION.                                                         
024400     CLOSE W4759A W4759D                                                  
024500                                                                          
024600     MOVE 'S' TO POSTSUM-OPKOD                                            
024700     CALL POSTSUM USING POSTSUM-PARM                                      
024800     .                                                                    
024900     EJECT                                                                
025000                                                                          
025100 S11-READ-W4759A SECTION.                                                 
025200     READ W4759A INTO IN-AREA                                             
025300     AT END                                                               
025400       SET END-OF-W4759A TO TRUE                                          
025500                                                                          
025600     NOT AT END                                                           
025700       MOVE 'W4759A'   TO POSTSUM-FDNAMN                                  
025800       MOVE 'W4759DD1' TO POSTSUM-DDNAMN2                                 
025900       MOVE SPACE      TO POSTSUM-TRANSTYP                                
026000       CALL POSTSUM USING POSTSUM-PARM                                    
026100     END-READ                                                             
026200     .                                                                    
026300     EJECT                                                                
026400                                                                          
026500 S12-WRITE-W4759D SECTION.                                                
026600                                                                          
026700     WRITE UT-POST FROM UT-AREA                                           
026800                                                                          
026900     MOVE 'W4759D'   TO POSTSUM-FDNAMN                                    
027000     MOVE 'W4759DD2' TO POSTSUM-DDNAMN2                                   
027100     CALL POSTSUM USING POSTSUM-PARM                                      
027200     .                                                                    
027300     EJECT                                                                
027400                                                                          
