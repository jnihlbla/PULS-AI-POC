000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1162800.                                                
000400*AUTHOR.         KENT JEBSEN                                              
000500*DATE-WRITTEN.   APRIL 1998.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*    URSPRUNGLIGEN KOPIERAT FRÅN W33541                                   
000900*                                                                         
001000*                                                                         
001100*    FUNKTION:                                                            
001200*       PROGRAMMET LÄSER FIL INNEHÅLLANDE ARTIKELINFO TOTALFIL.           
001300*       AV DENNA FIL SKAPAS "LAGOM STOR" UTFIL                            
001400*       SOM SKICKAS VIA DISTRIBUTION PRINT.                               
001500*       EV. RESTERANDE POSTER SKRIVS SOM NY GENERATION AV INFILEN.        
001600*       OM RESTERANDE POSTER FINNS LÄMNAS RETURKOD 8.RETURKODEN           
001700*       TESTAS SEDAN I JCL OCH OM DEN ÄR 8 BESTÄLLS JOBBET                
001800*       IGEN OCH DEN NYA GENERATIONEN TAS IN FÖR ATT                      
001900*       KUNNA SKICKA RESTERANDE POSTER OSV.                               
002000*                                                                         
002100*       INGÅR I BESTÄLLNINGSRUTIN W116S2.                                 
002200*                                                                         
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- FIL MED ARTIKELINFO                                        
003200*          --- POSTER ATT SÄNDA MED DISTRIBUTION PRINT                    
003300     SELECT W11628I                    ASSIGN TO W11628D1.                
003400     SKIP2                                                                
003500*          --- POSTER KVAR ATT SÄNDA EFTER DENNA SÄNDNING.                
003600     SELECT W11628U                    ASSIGN TO W11628D2.                
003700     SKIP2                                                                
003800*          --- 'DEL-FIL' ATT SÄNDA VIA DISTRIBUTION PRINT                 
003900     SELECT W11628V                    ASSIGN TO W11628D3.                
004000     SKIP2                                                                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W11628I                                                              
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900     SKIP2                                                                
005000*01  -COPY W11620B       -L.                                              
005100 FD  W11628U                                                              
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400     SKIP2                                                                
005500*01  POST -COPY W11620B  -PRE  UT-  -L.                                   
005600     SKIP3                                                                
005700 FD  W11628V                                                              
005800     RECORDING       V                                                    
005900     BLOCK CONTAINS  0.                                                   
006000     SKIP2                                                                
006100*01  POST -COPY W11620  -PRE  UT-VIPS- -L.                                
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400     SKIP2                                                                
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                       PIC X(8)    VALUE 'W1162800'.            
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000 77  WS-POSTRAKNARE              PIC 9(5)    VALUE ZERO.                  
007100 77  MAX-POSTER                  PIC 9(5)    VALUE 60000.                 
007200 77  RETURKOD                    PIC S9(2)   COMP-3 VALUE ZERO.           
007300                                                                          
007400                                                                          
007500 77  W11628I-EOF-SW              PIC X       VALUE 'N'.                   
007600     88  END-OF-W11628I                      VALUE 'J'.                   
007700     EJECT                                                                
007800 77  W11628-EOF-SW              PIC X       VALUE 'N'.                    
007900     88  END-OF-W11628                      VALUE 'J'.                    
008000     EJECT                                                                
008010 01  WS-DAP-LINE1.                                                        
008021     03  FILLER                  PIC X(133)  VALUE                        
008040                                 '¤DAPPARTINFO'.                          
008050 01  WS-DAP-LINE2.                                                        
008061     03  FILLER                  PIC X(4)    VALUE '¤DAP'.                
008070     03  WS-DAP-IDLANDX2         PIC X(2)    VALUE SPACE.                 
008071     03  WS-DAP-IDLOPNR          PIC X(2)    VALUE SPACE.                 
008080     03  FILLER                  PIC X(125)  VALUE SPACE.                 
008090     EJECT                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900 01  IN-AREA-START               PIC X(24)   VALUE                        
009000                                 'IN-AREA-START  '.                       
009100     SKIP2                                                                
009200 01  IN-AREA.                                                             
009300     03  FILLER                  PIC X(400).                              
009400*01  FILLER -COPY W11620B       -PRE IN-   -RED  IN-AREA                  
009500     EJECT                                                                
009600 01  UT-AREA-START               PIC X(24)   VALUE                        
009700                                 'UT-AREA-START  '.                       
009800     SKIP2                                                                
009900 01  UT-AREA.                                                             
010000     03  FILLER                  PIC X(400).                              
010100*01  FILLER -COPY W11620B       -PRE UT-   -RED  UT-AREA                  
010200     EJECT                                                                
010300 01  UT-VIPS-AREA-START          PIC X(24)   VALUE                        
010400                                 'UT-VIPS-AREA-START  '.                  
010500     SKIP2                                                                
010600 01  UT-VIPS-AREA.                                                        
010700     03  FILLER                  PIC X(400).                              
010800*01  FILLER -COPY W11620        -PRE UT-VIPS-  -RED  UT-VIPS-AREA         
010900     EJECT                                                                
011000 PROCEDURE DIVISION.                                                      
011100                                                                          
011200     PERFORM A-INIT                                                       
011300     PERFORM S02-LAES-W11628I                                             
011400     MOVE +1 TO WS-POSTRAKNARE                                            
011500                                                                          
011600     PERFORM UNTIL WS-POSTRAKNARE > MAX-POSTER OR                         
011700                   END-OF-W11628I                                         
011800                                                                          
011900       PERFORM B-FLYTTA-INPOST-TILL-VCOMFIL                               
012000                                                                          
012100       PERFORM S02-LAES-W11628I                                           
012200       ADD +1 TO WS-POSTRAKNARE                                           
012300     END-PERFORM                                                          
012400                                                                          
012500     IF END-OF-W11628I                                                    
012600       MOVE ZERO TO RETURKOD                                              
012700     ELSE                                                                 
012800       MOVE +8   TO RETURKOD                                              
012900                                                                          
013000       PERFORM UNTIL END-OF-W11628I                                       
013100                                                                          
013200         PERFORM C-FLYTTA-INPOST-TILL-UTFIL                               
013300                                                                          
013400         PERFORM S02-LAES-W11628I                                         
013500       END-PERFORM                                                        
013600     END-IF                                                               
013700                                                                          
013800     PERFORM Z-FINIT                                                      
013900                                                                          
014000     MOVE RETURKOD TO RETURN-CODE                                         
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500                                                                          
014700     OPEN INPUT  W11628I                                                  
014800                                                                          
014900     OPEN OUTPUT W11628U                                                  
015000                 W11628V                                                  
015100                                                                          
015200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015300     .                                                                    
015400     EJECT                                                                
015500 B-FLYTTA-INPOST-TILL-VCOMFIL SECTION.                                    
015600                                                                          
015800     IF  IN-IDLANDX2 = WS-DAP-IDLANDX2                                    
015801     AND IN-IDLOPNR  = WS-DAP-IDLOPNR                                     
015802       CONTINUE                                                           
015803     ELSE                                                                 
015804       MOVE IN-IDLANDX2      TO WS-DAP-IDLANDX2                           
015805       MOVE IN-IDLOPNR       TO WS-DAP-IDLOPNR                            
015810       WRITE UT-VIPS-POST  FROM WS-DAP-LINE1                              
015820       MOVE SPACE            TO UT-VIPS-POST                              
015830                                                                          
015850       WRITE UT-VIPS-POST  FROM WS-DAP-LINE2                              
015860       MOVE SPACE            TO UT-VIPS-POST                              
015870     END-IF                                                               
015880                                                                          
015890     MOVE IN-IDPTYP          TO UT-VIPS-IDPTYP                            
015891     MOVE IN-IDARTNR20       TO UT-VIPS-IDARTNR20                         
015892     MOVE IN-IDFKNGRP        TO UT-VIPS-IDFKNGRP                          
015893     MOVE IN-KDSRA           TO UT-VIPS-KDSRA                             
015894     MOVE IN-KVQPACK-0       TO UT-VIPS-KVQPACK-0                         
015895     MOVE IN-KDARTURS-NUM    TO UT-VIPS-KDARTURS-NUM                      
015896     MOVE IN-KDPRODSL        TO UT-VIPS-KDPRODSL                          
015897     MOVE IN-VLARTNTO        TO UT-VIPS-VLARTNTO                          
015898     MOVE IN-VKART           TO UT-VIPS-VKART                             
015899     MOVE IN-KDVSOP          TO UT-VIPS-KDVSOP                            
015900     MOVE IN-IDSTATNR        TO UT-VIPS-IDSTATNR                          
015901     MOVE IN-KDSORT          TO UT-VIPS-KDSORT                            
015902     MOVE IN-KDERS           TO UT-VIPS-KDERS                             
015903     MOVE IN-KDBPSR          TO UT-VIPS-KDBPSR                            
015904     MOVE IN-KDBBCL          TO UT-VIPS-KDBBCL                            
015905     MOVE IN-IDLEVNR         TO UT-VIPS-IDLEVNR                           
015906     MOVE IN-KDAGE           TO UT-VIPS-KDAGE                             
015907     MOVE IN-IDPROJ          TO UT-VIPS-IDPROJ                            
015908     MOVE IN-PRARTSJK        TO UT-VIPS-PRARTSJK                          
015909     MOVE IN-PRARTSTD        TO UT-VIPS-PRARTSTD                          
015910     MOVE IN-BEART-L1        TO UT-VIPS-BEART-L1                          
015911     MOVE IN-BEART-L2        TO UT-VIPS-BEART-L2                          
015912     MOVE IN-KDPSLLOC        TO UT-VIPS-KDPSLLOC                          
015913     MOVE IN-IDLEVNR-LOC     TO UT-VIPS-IDLEVNR-LOC                       
015914     MOVE IN-KDSTANAUTG      TO UT-VIPS-KDSTANAUTG                        
015915     MOVE IN-FLOVRLAG        TO UT-VIPS-FLOVRLAG                          
015916     MOVE IN-FLSOFTWARE      TO UT-VIPS-FLSOFTWARE                        
015917     MOVE IN-DADATUM         TO UT-VIPS-DADATUM                           
015918     MOVE IN-IDLEVNR-DUBLETT TO UT-VIPS-IDLEVNR-DUBLETT                   
015919     MOVE IN-IDLEVNR-LOC-DUBLETT TO UT-VIPS-IDLEVNR-LOC-DUBLETT           
015920     MOVE IN-KDTIPPR         TO UT-VIPS-KDTIPPR                           
015921     MOVE IN-IDKAT(01)       TO UT-VIPS-IDKAT(01)                         
015922     MOVE IN-IDKAT(02)       TO UT-VIPS-IDKAT(02)                         
015923     MOVE IN-IDKAT(03)       TO UT-VIPS-IDKAT(03)                         
015924     MOVE IN-BELEVART        TO UT-VIPS-BELEVART                          
015925     MOVE IN-FLGEMFMC        TO UT-VIPS-FLGEMFMC                          
015926     MOVE IN-IDPROJUP        TO UT-VIPS-IDPROJUP                          
015927     MOVE IN-BEARTEXT        TO UT-VIPS-BEARTEXT                          
015928     MOVE IN-TIURPROD        TO UT-VIPS-TIURPROD                          
015929                                                                          
015930     PERFORM S14-SKRIV-W11628V                                            
016000     .                                                                    
016100     EJECT                                                                
016200                                                                          
016300 C-FLYTTA-INPOST-TILL-UTFIL SECTION.                                      
016400                                                                          
016500     MOVE IN-W11620B         TO UT-W11620B                                
016621                                                                          
016630     PERFORM S11-SKRIV-W11628U                                            
016700     .                                                                    
016800     EJECT                                                                
016900                                                                          
017000 Z-FINIT SECTION.                                                         
017100                                                                          
017200     CLOSE                                                                
017300           W11628I                                                        
017400           W11628U                                                        
017500           W11628V                                                        
017600                                                                          
017700     MOVE 'S' TO POSTSUM-OPKOD                                            
017800     CALL POSTSUM USING POSTSUM-PARM                                      
017900     .                                                                    
018000     EJECT                                                                
018100 S02-LAES-W11628I SECTION.                                                
018200                                                                          
018300     READ W11628I INTO IN-AREA                                            
018400     AT END                                                               
018500        SET END-OF-W11628I TO TRUE                                        
018600                                                                          
018700     NOT AT END                                                           
018800        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
018900        MOVE 'W11683'   TO POSTSUM-FDNAMN                                 
019000        MOVE 'W11628D1' TO POSTSUM-DDNAMN2                                
019100        CALL POSTSUM USING POSTSUM-PARM                                   
019200     END-READ                                                             
019300     .                                                                    
019400     EJECT                                                                
019500 S11-SKRIV-W11628U SECTION.                                               
019600                                                                          
019700     WRITE UT-POST FROM UT-AREA                                           
019800                                                                          
019900     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
020000     MOVE 'W11628'   TO POSTSUM-FDNAMN                                    
020100     MOVE 'W11628D2' TO POSTSUM-DDNAMN2                                   
020200     CALL POSTSUM USING POSTSUM-PARM                                      
020300     .                                                                    
020400     EJECT                                                                
020500 S14-SKRIV-W11628V SECTION.                                               
020600                                                                          
020700     WRITE UT-VIPS-POST FROM UT-VIPS-AREA                                 
020800                                                                          
020900     MOVE 'WA1'      TO POSTSUM-TRANSTYP                                  
021000     MOVE SPACE      TO POSTSUM-FDNAMN                                    
021100     MOVE 'W11628D3' TO POSTSUM-DDNAMN2                                   
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     .                                                                    
021400     EJECT                                                                
