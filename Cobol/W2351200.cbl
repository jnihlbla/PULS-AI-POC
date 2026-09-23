000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2351200.                                                
000400*AUTHOR.         INGER STENING.                                           
000500*DATE-WRITTEN.   DEC 2013.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR LISTA MED LEV.BESK. SOM SELEKTERATS PÅ BILD 2423.         
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
002600     SELECT W2351201                   ASSIGN TO W23512D1.                
002700*          ---                                                            
002800     SELECT W2351202                   ASSIGN TO W23512D2.                
002900*          --- UT-FIL                                                     
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W2351201                                                             
003600     LABEL RECORD STANDARD                                                
003700     RECORDING V                                                          
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  POST -COPY W2351101  -L.                                             
004100                                                                          
004200 FD  W2351202                                                             
004300     LABEL RECORD STANDARD                                                
004400     RECORDING F                                                          
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700 01  UT-POST                 PIC X(500).                                  
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005100                                                                          
005200*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W2351200'.            
005400 77  YES                         PIC X       VALUE 'Y'.                   
005500 77  NOO                         PIC X       VALUE 'N'.                   
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
006600 77  W2351201-EOF-SW             PIC X       VALUE 'N'.                   
006700     88  END-OF-W2351201                     VALUE 'J'.                   
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
008800*01  AREA -COPY W2351101   -PRE IN-                                       
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
009800     03  FILLER               PIC X(7)    VALUE 'PART NO'.                
009900     03  FILLER               PIC X       VALUE X'05'.                    
010000     03  FILLER               PIC X(8)    VALUE 'EXT TEXT'.               
010100     03  FILLER               PIC X       VALUE X'05'.                    
010200     03  FILLER               PIC X(16)   VALUE                           
010300                                          'TEXT INFO ENDING'.             
010400     03  FILLER               PIC X       VALUE X'05'.                    
010500     03  FILLER               PIC X(13)   VALUE 'DELIVERY INFO'.          
010600     03  FILLER               PIC X       VALUE X'05'.                    
010700     03  FILLER               PIC X(12)   VALUE 'PROMISED QTY'.           
010800     03  FILLER               PIC X       VALUE X'05'.                    
010900     03  FILLER               PIC X(10)   VALUE 'PREADVICED'.             
011000     03  FILLER               PIC X       VALUE X'05'.                    
011100     03  FILLER               PIC X(9)    VALUE 'STOCK BAL'.              
011200     03  FILLER               PIC X       VALUE X'05'.                    
011300     03  FILLER               PIC X(2)    VALUE 'AK'.                     
011400     03  FILLER               PIC X       VALUE X'05'.                    
011500     03  FILLER               PIC X(2)    VALUE 'BO'.                     
011600     03  FILLER               PIC X       VALUE X'05'.                    
011700     03  FILLER               PIC X(13)   VALUE 'NEXT CALL OFF'.          
011800     03  FILLER               PIC X       VALUE X'05'.                    
011900     03  FILLER               PIC X(8)    VALUE 'QUANTITY'.               
012000     03  FILLER               PIC X       VALUE X'05'.                    
012100     03  FILLER               PIC X(9)    VALUE 'AGREEMENT'.              
012200     03  FILLER               PIC X       VALUE X'05'.                    
012300     03  FILLER               PIC X(8)    VALUE 'SUP CODE'.               
012400     03  FILLER               PIC X       VALUE X'05'.                    
012410     03  FILLER               PIC X(9)    VALUE 'SS-MARKET'.              
012420     03  FILLER               PIC X       VALUE X'05'.                    
012500     EJECT                                                                
012600 01  UT-AREA-2.                                                           
012700     03  UT-IDANSK            PIC Z(3)    VALUE ZERO.                     
012800     03  FILLER               PIC X       VALUE X'05'.                    
012900     03  UT-IDLEVNR           PIC X(5)    VALUE SPACE.                    
013000     03  FILLER               PIC X       VALUE X'05'.                    
013010     03  UT-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                    
013020     03  FILLER               PIC X       VALUE X'05'.                    
013100     03  UT-IDDC              PIC X(5)    VALUE SPACE.                    
013200     03  FILLER               PIC X       VALUE X'05'.                    
013300     03  UT-IDARTNR           PIC Z(8)9   VALUE ZERO.                     
013400     03  FILLER               PIC X       VALUE X'05'.                    
013500     03  UT-TELEVBSK-COMP     PIC X(323)  VALUE SPACE.                    
013600     03  FILLER               PIC X       VALUE X'05'.                    
013700     03  UT-TIBORT            PIC Z(7)    VALUE ZERO.                     
013800     03  FILLER               PIC X       VALUE X'05'.                    
013900     03  UT-DALEVBSK-AVS      PIC Z(7)9   VALUE ZERO.                     
014000     03  FILLER               PIC X       VALUE X'05'.                    
014100     03  UT-KVAVIS-BSKKVAR    PIC Z(6)9   VALUE ZERO.                     
014200     03  FILLER               PIC X       VALUE X'05'.                    
014300     03  UT-KVART-FORAVIS     PIC Z(7)    VALUE ZERO.                     
014400     03  FILLER               PIC X       VALUE X'05'.                    
014500     03  UT-KVLS              PIC Z(7)    VALUE ZERO.                     
014600     03  FILLER               PIC X       VALUE X'05'.                    
014700     03  UT-KVAKS             PIC Z(7)    VALUE ZERO.                     
014800     03  FILLER               PIC X       VALUE X'05'.                    
014900     03  UT-KVROS             PIC Z(7)    VALUE ZERO.                     
015000     03  FILLER               PIC X       VALUE X'05'.                    
015100     03  UT-AVROPS-DAT        PIC Z(7)    VALUE ZERO.                     
015200     03  FILLER               PIC X       VALUE X'05'.                    
015300     03  UT-KVAVROP           PIC Z(7)    VALUE ZERO.                     
015400     03  FILLER               PIC X       VALUE X'05'.                    
015500     03  UT-KDAVT             PIC Z(1)    VALUE ZERO.                     
015600     03  FILLER               PIC X       VALUE X'05'.                    
015700     03  UT-KDERS             PIC Z(2)9   VALUE ZERO.                     
015800     03  FILLER               PIC X       VALUE X'05'.                    
015810     03  UT-FLERSDAT-VIPS     PIC X       VALUE SPACE.                    
015820     03  FILLER               PIC X       VALUE X'05'.                    
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016100     SKIP3                                                                
016200 PROCEDURE DIVISION.                                                      
016300                                                                          
016400     PERFORM A-INIT                                                       
016500     PERFORM S01-LAES-W2351201                                            
016600     PERFORM UNTIL END-OF-W2351201                                        
016700                                                                          
016800       PERFORM B-BEHANDLA-POSTER                                          
016900       PERFORM S01-LAES-W2351201                                          
017000                                                                          
017100     END-PERFORM                                                          
017200     PERFORM Z-FINIT                                                      
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800 A-INIT SECTION.                                                          
017900     SKIP2                                                                
018000                                                                          
018100     ACCEPT DAGENS-DATUM FROM DATE                                        
018200                                                                          
018300     OPEN INPUT  W2351201                                                 
018400     OPEN OUTPUT W2351202                                                 
018500     WRITE UT-POST FROM UT-AREA-1                                         
018600                                                                          
018700                                                                          
018800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018900     .                                                                    
019000     EJECT                                                                
019100 B-BEHANDLA-POSTER SECTION.                                               
019200                                                                          
019300     MOVE IN-IDANSK          TO UT-IDANSK                                 
019400     MOVE IN-IDLEVNR         TO UT-IDLEVNR                                
019410     MOVE IN-IDLEVNR-SHIP    TO UT-IDLEVNR-SHIP                           
019500     MOVE IN-IDDC            TO UT-IDDC                                   
019600     MOVE IN-IDARTNR         TO UT-IDARTNR                                
019700     MOVE IN-TELEVBSK-COMP   TO UT-TELEVBSK-COMP                          
019800     MOVE IN-TIBORT          TO UT-TIBORT                                 
019900     MOVE IN-DALEVBSK-AVS    TO UT-DALEVBSK-AVS                           
020000     MOVE IN-KVAVIS-BSKKVAR  TO UT-KVAVIS-BSKKVAR                         
020100     MOVE IN-KVART-FORAVIS   TO UT-KVART-FORAVIS                          
020200     MOVE IN-KVLS            TO UT-KVLS                                   
020300     MOVE IN-KVAKS           TO UT-KVAKS                                  
020400     MOVE IN-KVROS           TO UT-KVROS                                  
020500     MOVE IN-AVROPS-DAT      TO UT-AVROPS-DAT                             
020600     MOVE IN-KVAVROP         TO UT-KVAVROP                                
020700     MOVE IN-KDAVT           TO UT-KDAVT                                  
020800     MOVE IN-KDERS           TO UT-KDERS                                  
020810     IF IN-TIERSDAT-VIPS > ZERO                                           
020820        MOVE YES             TO UT-FLERSDAT-VIPS                          
020830     ELSE                                                                 
020840        MOVE NOO             TO UT-FLERSDAT-VIPS                          
020850     END-IF                                                               
020900     PERFORM S02-SKRIV-W2351202                                           
021000     .                                                                    
021100     EJECT                                                                
021200                                                                          
021300 Z-FINIT SECTION.                                                         
021400                                                                          
021500     CLOSE W2351201                                                       
021600           W2351202                                                       
021700     SKIP2                                                                
021800     MOVE 'S' TO POSTSUM-OPKOD                                            
021900     CALL POSTSUM USING POSTSUM-PARM                                      
022000                                                                          
022100     .                                                                    
022200     EJECT                                                                
022300 S01-LAES-W2351201 SECTION.                                               
022400     SKIP2                                                                
022500     READ W2351201 INTO IN-AREA                                           
022600     AT END                                                               
022700        SET END-OF-W2351201 TO TRUE                                       
022800                                                                          
022900     NOT AT END                                                           
023000        MOVE 'W2351201' TO POSTSUM-FDNAMN                                 
023100        MOVE 'W23512D1' TO POSTSUM-DDNAMN2                                
023200        CALL POSTSUM USING POSTSUM-PARM                                   
023300     END-READ                                                             
023400     .                                                                    
023500     EJECT                                                                
023600 S02-SKRIV-W2351202 SECTION.                                              
023700                                                                          
023800     WRITE UT-POST FROM UT-AREA-2                                         
023900     .                                                                    
024000     EJECT                                                                
