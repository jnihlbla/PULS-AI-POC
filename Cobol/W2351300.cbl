000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2351300.                                                
000400*AUTHOR.         INGER STENING.                                           
000500*DATE-WRITTEN.   DEC 2013.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR LISTA MED AVROP SOM SELEKTERATS PÅ BILD 2423.             
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          ---                                                            
002600     SELECT W2351301                   ASSIGN TO W23513D1.                
002700*          ---                                                            
002800     SELECT W2351302                   ASSIGN TO W23513D2.                
002900*          --- UT-FIL                                                     
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W2351301                                                             
003600     LABEL RECORD STANDARD                                                
003700     RECORDING V                                                          
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  POST -COPY W2351102  -L.                                             
004100                                                                          
004200 FD  W2351302                                                             
004300     LABEL RECORD STANDARD                                                
004400     RECORDING V                                                          
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700 01  UT-POST                 PIC X(120).                                  
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W2351300'.            
005410 77  YES                         PIC X       VALUE 'Y'.                   
005510 77  NOO                         PIC X       VALUE 'N'.                   
005600 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
005700 77  ANTAL-IX                    PIC S9(5)   VALUE +0.                    
005800     SKIP2                                                                
005900                                                                          
006000                                                                          
006100                                                                          
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500                                                                          
006600 77  W2351301-EOF-SW             PIC X       VALUE 'N'.                   
006700     88  END-OF-W2351301                     VALUE 'J'.                   
006800                                                                          
006900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007000 01  FILLER REDEFINES DAGENS-DATUM.                                       
007100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007400     EJECT                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL POSTSUM                                          
008200*                                                                         
008300*01  -COPY W0005   -PRE  POSTSUM-                                         
008400     EJECT                                                                
008500 01  IN-AREA-START               PIC X(24)   VALUE                        
008600                                             'IN-AREA-START'.             
008700     SKIP2                                                                
008800*01  AREA -COPY W2351102   -PRE IN-                                       
008900*                                                                         
009000                                                                          
009100 01  UT-AREA-1.                                                           
009200     03  FILLER               PIC X(4)    VALUE 'PROC'.                   
009300     03  FILLER               PIC X       VALUE X'05'.                    
009310     03  FILLER               PIC X(5)    VALUE 'SUPPL'.                  
009500     03  FILLER               PIC X       VALUE X'05'.                    
009511     03  FILLER               PIC X(9)    VALUE 'SUPPL-SHP'.              
009520     03  FILLER               PIC X       VALUE X'05'.                    
009600     03  FILLER               PIC X(5)    VALUE 'DC'.                     
009700     03  FILLER               PIC X       VALUE X'05'.                    
009800     03  FILLER               PIC X(13)   VALUE 'SUPPL PART NO'.          
009900     03  FILLER               PIC X       VALUE X'05'.                    
010000     03  FILLER               PIC X(7)    VALUE 'PART NO'.                
010100     03  FILLER               PIC X       VALUE X'05'.                    
010200     03  FILLER               PIC X(8)    VALUE 'CALL OFF'.               
010300     03  FILLER               PIC X       VALUE X'05'.                    
010400     03  FILLER               PIC X(8)    VALUE 'QUANTITY'.               
010500     03  FILLER               PIC X       VALUE X'05'.                    
010600     03  FILLER               PIC X(8)    VALUE 'SUP CODE'.               
010700     03  FILLER               PIC X       VALUE X'05'.                    
010710     03  FILLER               PIC X(9)    VALUE 'SS-MARKET'.              
010720     03  FILLER               PIC X       VALUE X'05'.                    
010800     03  FILLER               PIC X(9)    VALUE 'AGREEMENT'.              
010900     03  FILLER               PIC X       VALUE X'05'.                    
011000     EJECT                                                                
011100 01  UT-AREA-2.                                                           
011200     03  UT-IDANSK            PIC Z(2)9   VALUE ZERO.                     
011300     03  FILLER               PIC X       VALUE X'05'.                    
011400     03  UT-IDLEVNR           PIC X(5)    VALUE SPACE.                    
011500     03  FILLER               PIC X       VALUE X'05'.                    
011510     03  UT-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                    
011520     03  FILLER               PIC X       VALUE X'05'.                    
011600     03  UT-IDDC              PIC X(2)    VALUE SPACE.                    
011700     03  FILLER               PIC X       VALUE X'05'.                    
011800     03  UT-BELEV             PIC X(35)   VALUE SPACE.                    
011900     03  FILLER               PIC X       VALUE X'05'.                    
012000     03  UT-IDARTNR           PIC Z(8)9   VALUE ZERO.                     
012100     03  FILLER               PIC X       VALUE X'05'.                    
012200     03  UT-AVROPS-DAT        PIC Z(6)9   VALUE ZERO.                     
012300     03  FILLER               PIC X       VALUE X'05'.                    
012400     03  UT-KVAVROP           PIC Z(6)9   VALUE ZERO.                     
012500     03  FILLER               PIC X       VALUE X'05'.                    
012600     03  UT-KDERS             PIC Z(2)9   VALUE ZERO.                     
012700     03  FILLER               PIC X       VALUE X'05'.                    
012710     03  UT-FLERSDAT-VIPS     PIC X       VALUE SPACE.                    
012720     03  FILLER               PIC X       VALUE X'05'.                    
012800     03  UT-KDAVT             PIC 9(1)    VALUE ZERO.                     
012900     03  FILLER               PIC X       VALUE X'05'.                    
013000     EJECT                                                                
013100 01  UT-AREA-3.                                                           
013200     03  FILLER                  PIC X(33)                                
013300                       VALUE '25000 AVTAL VISAS MER AVTAL FINNS'.         
013400     EJECT                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600     SKIP3                                                                
013700 PROCEDURE DIVISION.                                                      
013800                                                                          
013900     PERFORM A-INIT                                                       
014000     PERFORM S01-LAES-W2351301                                            
014100     MOVE +1 TO ANTAL-IX                                                  
014200     PERFORM UNTIL END-OF-W2351301 OR ANTAL-IX > 25000                    
014300                                                                          
014400       PERFORM B-BEHANDLA-POSTER                                          
014500       ADD +1 TO ANTAL-IX                                                 
014600       PERFORM S01-LAES-W2351301                                          
014700                                                                          
014800     END-PERFORM                                                          
014900     IF ANTAL-IX >= 25000                                                 
015000       WRITE UT-POST FROM UT-AREA-3                                       
015100     END-IF                                                               
015200     PERFORM Z-FINIT                                                      
015300                                                                          
015400     MOVE ZERO TO RETURN-CODE                                             
015500     GOBACK                                                               
015600     .                                                                    
015700     EJECT                                                                
015800 A-INIT SECTION.                                                          
015900     SKIP2                                                                
016000                                                                          
016100     ACCEPT DAGENS-DATUM FROM DATE                                        
016200                                                                          
016300     OPEN INPUT  W2351301                                                 
016400     OPEN OUTPUT W2351302                                                 
016500     WRITE UT-POST FROM UT-AREA-1                                         
016600                                                                          
016700                                                                          
016800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     .                                                                    
017000     EJECT                                                                
017100 B-BEHANDLA-POSTER SECTION.                                               
017200                                                                          
017300     MOVE IN-IDARTNR         TO UT-IDARTNR                                
017400     MOVE IN-BELEV           TO UT-BELEV                                  
017500     MOVE IN-IDLEVNR         TO UT-IDLEVNR                                
017510     MOVE IN-IDLEVNR-SHIP    TO UT-IDLEVNR-SHIP                           
017600     MOVE IN-IDDC            TO UT-IDDC                                   
017700     MOVE IN-KVAVROP         TO UT-KVAVROP                                
017800     MOVE IN-AVROPS-DAT      TO UT-AVROPS-DAT                             
017900     MOVE IN-KDERS           TO UT-KDERS                                  
017910     IF IN-TIERSDAT-VIPS > ZERO                                           
017920        MOVE YES             TO UT-FLERSDAT-VIPS                          
017930     ELSE                                                                 
017931        MOVE NOO             TO UT-FLERSDAT-VIPS                          
017940     END-IF                                                               
018000     MOVE IN-KDAVT           TO UT-KDAVT                                  
018200     MOVE IN-IDANSK          TO UT-IDANSK                                 
018300     PERFORM S02-SKRIV-W2351302                                           
018400     .                                                                    
018500     EJECT                                                                
018600                                                                          
018700 Z-FINIT SECTION.                                                         
018800                                                                          
018900     CLOSE W2351301                                                       
019000           W2351302                                                       
019100     SKIP2                                                                
019200     MOVE 'S' TO POSTSUM-OPKOD                                            
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019400                                                                          
019500     .                                                                    
019600     EJECT                                                                
019700 S01-LAES-W2351301 SECTION.                                               
019800     SKIP2                                                                
019900     READ W2351301 INTO IN-AREA                                           
020000     AT END                                                               
020100        SET END-OF-W2351301 TO TRUE                                       
020200                                                                          
020300     NOT AT END                                                           
020400        MOVE 'W2351301' TO POSTSUM-FDNAMN                                 
020500        MOVE 'W23513D1' TO POSTSUM-DDNAMN2                                
020600        CALL POSTSUM USING POSTSUM-PARM                                   
020700     END-READ                                                             
020800     .                                                                    
020900     EJECT                                                                
021000 S02-SKRIV-W2351302 SECTION.                                              
021100                                                                          
021200     WRITE UT-POST FROM UT-AREA-2                                         
021300     .                                                                    
021400     EJECT                                                                
