000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6117B00.                                                
000300 AUTHOR.         ARUP DATTA                                               
000400 DATE-WRITTEN.   DEC 2013.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THE PGM                                                          
000900*        - READS INPUT FILE WITH INTRASTAT ITEMS TO TRANSPORT             
001000*        - CREATES DATA RECORDS FOR INTRASTAT ITEMS (EXCEL)               
001100*        - TEMPORARY SOLUTION                                             
001200*          TO DISTRIBUTION & PRINT BY USING WZ01SEND                      
001300*                                                                         
001400*                                                                         
001500*    CHANGE LOG:                                                          
001600*                                                                         
001700*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
001800*      ----------------------------------------------------------         
001900*      15/10/12 - REDDY RAHUL     - ADD STAT NUMBER AND WEIGHT            
002000*                                   IN INTRASTE FILES.                    
002100*                                   E'TRACKER 10265098                    
002200*                                                                         
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900*          --- INFIL                                                      
003000     SELECT W61172                     ASSIGN TO W6117BD1.                
003100*          --- UTFIL                                                      
003200     SELECT W6117B                     ASSIGN TO W6117BD2.                
003300                                                                          
003400 DATA DIVISION.                                                           
003500                                                                          
003600 FILE SECTION.                                                            
003700 FD  W61172                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY A7290A01     -L.                                               
004200                                                                          
004300     EJECT                                                                
004400 FD  W6117B                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  UT-POST       PIC X(250).                                            
004900     EJECT                                                                
005000                                                                          
005100 WORKING-STORAGE SECTION.                                                 
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W6117B00'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005910 77  WS-LAST-DAY                 PIC X(10)        VALUE SPACE.            
005920 77  WS-INV-NUM                  PIC X(23)        VALUE SPACE.            
005930 77  WS-DAT-YYYY                 PIC 9(4)         VALUE 0.                
005940 77  WS-DAT-MM                   PIC 9(2)         VALUE 0.                
005950 77  WS-DAT-DD                   PIC 9(2)         VALUE 0.                
006000                                                                          
006100 77  W61172-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W61172                       VALUE 'J'.                   
006300                                                                          
006400 01  W-ANTAL-FORMAT              PIC -Z(11)9.                             
006500                                                                          
006600 01  W-VARDE-FORMAT              PIC -Z(12)9.99.                          
006700                                                                          
006800 01  W-IDSTATNR-FORMAT           PIC -Z(09).                              
006900                                                                          
007000 01  W-VKARTTOT-FORMAT           PIC -Z(15)9.99.                          
007100                                                                          
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES DAGENS-DATUM.                                       
007400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007700                                                                          
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008100                                                                          
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500     EJECT                                                                
008600                                                                          
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*01  -COPY W0005   -PRE  POSTSUM-                                         
008900     EJECT                                                                
009000                                                                          
009100 01  IN-AREA-START               PIC X(24)   VALUE                        
009200                                 'IN-AREA-START  '.                       
009300*01  AREA -COPY A7290A01   -PRE IN-                                       
009400     EJECT                                                                
009500                                                                          
009600 01  UT-AREA-START           PIC X(24)   VALUE                            
009700                                 'UT-AREA-START '.                        
009800 01  UT-AREA                 PIC X(250).                                  
009900     EJECT                                                                
010000                                                                          
010100 PROCEDURE DIVISION.                                                      
010200                                                                          
010300 MAIN SECTION.                                                            
010400                                                                          
010500     PERFORM A-INIT                                                       
010600                                                                          
010700     PERFORM S11-READ-W61172                                              
010800     PERFORM UNTIL END-OF-W61172                                          
010900       IF IN-POSTTYP = '01'                                               
011210         MOVE IN-FILDATUM(1:4) TO WS-DAT-YYYY                             
011220         MOVE IN-FILDATUM(5:2) TO WS-DAT-MM                               
011230         MOVE IN-FILDATUM(7:2) TO WS-DAT-DD                               
011240         STRING WS-DAT-DD '.'                                             
011250                WS-DAT-MM '.'                                             
011260                WS-DAT-YYYY                                               
011270         DELIMITED BY SIZE   INTO WS-LAST-DAY                             
011300       ELSE                                                               
011310         IF IN-VARDE OF IN-ARTIKEL-AREA = 0                               
011320            CONTINUE                                                      
011330         ELSE                                                             
011400            PERFORM B-CREATE-PUT-LINE                                     
011410            PERFORM S12-WRITE-W6117B                                      
011600         END-IF                                                           
011700       END-IF                                                             
011710                                                                          
011800       PERFORM S11-READ-W61172                                            
011900     END-PERFORM                                                          
012000                                                                          
012100                                                                          
012200     PERFORM Z-FINIT                                                      
012300     MOVE ZERO TO RETURN-CODE                                             
012400     GOBACK                                                               
012500     .                                                                    
012600     EJECT                                                                
012700                                                                          
012800 A-INIT SECTION.                                                          
012900     OPEN INPUT W61172                                                    
013000     OPEN OUTPUT W6117B                                                   
013100     ACCEPT DAGENS-DATUM FROM DATE                                        
013200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013300     .                                                                    
013400     EJECT                                                                
013500                                                                          
016500 B-CREATE-PUT-LINE SECTION.                                               
016600                                                                          
016700     MOVE SPACE          TO UT-AREA                                       
016800                                                                          
016900     MOVE IN-VARDE OF IN-ARTIKEL-AREA                                     
017000                         TO W-VARDE-FORMAT                                
017010     INSPECT W-VARDE-FORMAT REPLACING ALL '.' BY ','                      
017100     MOVE IN-ANTAL OF IN-ARTIKEL-AREA                                     
017200                         TO W-ANTAL-FORMAT                                
017300     MOVE IN-IDSTATNR    TO W-IDSTATNR-FORMAT                             
017500                                                                          
018470     COMPUTE W-VKARTTOT-FORMAT = (IN-VKARTTOT / 1000)                     
018480     INSPECT W-VKARTTOT-FORMAT REPLACING ALL '.' BY ','                   
018490                                                                          
018491     STRING 'A'                                 '-'                       
018492            IN-LANDKOD   OF IN-ARTIKEL-AREA     '-'                       
018493            '1441003'                           '-'                       
018494            WS-LAST-DAY                                                   
018495     DELIMITED BY SIZE INTO WS-INV-NUM                                    
018496                                                                          
018497     STRING 'SE'                                ';'                       
018498            'S1'                                ';'                       
018499            'V1'                                ';'                       
018500            WS-LAST-DAY                         ';'                       
018501            WS-INV-NUM                          ';'                       
018502            '1441003'                           ';'                       
018503            ' '                                 ';'                       
018504            ' '                                 ';'                       
018505            ' '                                 ';'                       
018506            ' '                                 ';'                       
018507            IN-LANDKOD   OF IN-ARTIKEL-AREA     ';'                       
018508            '1'                                 ';'                       
018509            ' '                                 ';'                       
018510            ' '                                 ';'                       
018511            ' '                                 ';'                       
018512            ' '                                 ';'                       
018513            'SEK'                               ';'                       
018514            ' '                                 ';'                       
018515            ' '                                 ';'                       
018516            ' '                                 ';'                       
018517            ' '                                 ';'                       
018518            ' '                                 ';'                       
018519            ' '                                 ';'                       
018520            ' '                                 ';'                       
018521            ' '                                 ';'                       
018522            ' '                                 ';'                       
018523            ' '                                 ';'                       
018524            ' '                                 ';'                       
018525            ' '                                 ';'                       
018526            ' '                                 ';'                       
018527            ' '                                 ';'                       
018528            ' '                                 ';'                       
018529            ' '                                 ';'                       
018541       IN-ARTIKELNR      OF IN-ARTIKEL-AREA     ';'                       
018542            ' '                                 ';'                       
018543       W-IDSTATNR-FORMAT                        ';'                       
018544            'SE'                                ';'                       
018546            '1'                                 ';'                       
018547       W-ANTAL-FORMAT                           ';'                       
018548            ' '                                 ';'                       
018549       W-VKARTTOT-FORMAT                        ';'                       
018550       W-ANTAL-FORMAT                           ';'                       
018551            ' '                                 ';'                       
018552       W-VARDE-FORMAT                           ';'                       
018553     DELIMITED BY SIZE INTO UT-AREA                                       
018560     .                                                                    
018600     EJECT                                                                
018700                                                                          
018800 Z-FINIT SECTION.                                                         
018900     CLOSE W61172 W6117B                                                  
019000                                                                          
019100     MOVE 'S' TO POSTSUM-OPKOD                                            
019200     CALL POSTSUM USING POSTSUM-PARM                                      
019300     .                                                                    
019400     EJECT                                                                
019500                                                                          
019600 S11-READ-W61172 SECTION.                                                 
019700     READ W61172 INTO IN-AREA                                             
019800     AT END                                                               
019900       SET END-OF-W61172 TO TRUE                                          
020000                                                                          
020100     NOT AT END                                                           
020200       MOVE 'W61172'   TO POSTSUM-FDNAMN                                  
020300       MOVE 'W6117BD1' TO POSTSUM-DDNAMN2                                 
020400       MOVE SPACE      TO POSTSUM-TRANSTYP                                
020500       CALL POSTSUM USING POSTSUM-PARM                                    
020600     END-READ                                                             
020700     .                                                                    
020800     EJECT                                                                
020900                                                                          
021000 S12-WRITE-W6117B SECTION.                                                
021100                                                                          
021200     WRITE UT-POST FROM UT-AREA                                           
021300                                                                          
021400     MOVE 'W6117B'   TO POSTSUM-FDNAMN                                    
021500     MOVE 'W6117BD2' TO POSTSUM-DDNAMN2                                   
021600     CALL POSTSUM USING POSTSUM-PARM                                      
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
