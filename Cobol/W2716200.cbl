000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2716200.                                                
000300 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000400 DATE-WRITTEN.   MAJ 1997.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                PROGRAMMET LÄSER RESTORDER OCH SUMMERAR                  
001000*                ORDERRADER PER DC/ARTIKEL FÖR ATT SENARE                 
001100*                SKRIVA LISTAN 'TOPP 100 RESTORDER' PER LAND              
001200*                I ÄLDSTAORDNING                                          
001300*                ENBART ÄLDSTA RESTORDERN PER ARTIKEL VISAS               
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800     SELECT W27162IN                   ASSIGN TO W27162D1.                
002900*                                                                         
003000     SELECT W27162UT                   ASSIGN TO W27162D2.                
003100     SKIP2                                                                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700                                                                          
003800 FD  W27162IN                                                             
003900     RECORD CONTAINS 76 CHARACTERS                                        
004000     LABEL RECORD STANDARD                                                
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300 01  IN-POST                 PIC X(76).                                   
004400                                                                          
004500     SKIP3                                                                
004600                                                                          
004700 FD  W27162UT                                                             
004800     RECORD CONTAINS 80 CHARACTERS                                        
004900     LABEL RECORD STANDARD                                                
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200 01  UT-POST                 PIC X(80).                                   
005300                                                                          
005400*                                                                         
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W2716200'.            
005900 77  DC-IX                       PIC S9(4)   VALUE +0  COMP SYNC.         
006000 77  MAX-TAB-IX                  PIC S9(4)   VALUE +10 COMP SYNC.         
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300     SKIP2                                                                
006400 01  WS.                                                                  
006600     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
006700     03  WS-ANTAL-ORDRAD         PIC 9(7)    VALUE ZERO.                  
006900     03  WS-ANTAL-QTY            PIC 9(7)    VALUE ZERO.                  
007800     03  WS-DARODAT              PIC S9(09)  VALUE ZERO.                  
007900 01  W-IDDCTEXT-MSGI.                                                     
008000     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
008100     03  W-IDDC-MSGI         PIC X(2).                                    
008200     EJECT                                                                
008300 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
008400                                                                          
008500 01  FILLER          PIC X(16)   VALUE 'SPAR-FAELT START'.                
008600 01  SPAR-ANTAL-KOLLI            PIC S9(7)   VALUE ZERO  COMP-3.          
008700 01  SPAR-ANTAL-RADER            PIC S9(7)   VALUE ZERO  COMP-3.          
008800 01  SPAR-IDKUNDNR               PIC S9(7)   VALUE ZERO  COMP-3.          
008900 01  SPAR-IDLBBET                PIC X(12)   VALUE SPACE.                 
009000 01  SPAR-IDFAKT                 PIC S9(7)   VALUE ZERO  COMP-3.          
009100 01  SPAR-IDKOLLI                PIC S9(5)   VALUE ZERO  COMP-3.          
009200 01  SPAR-KDKOLLI                PIC X(8)    VALUE SPACE.                 
009300 01  SPAR-IDKUNDRF               PIC X(10)   VALUE SPACE.                 
009400 01  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO  COMP-3.          
009500 01  SPAR-KDFRAKT                PIC S9(3)   VALUE ZERO  COMP-3.          
009600 01  SPAR-KDVALISO               PIC X(3)    VALUE SPACE.                 
009700 01  SPAR-TIFAKT                 PIC S9(7)   VALUE ZERO  COMP-3.          
009800 01  SPAR-TIBERANK               PIC S9(7)   VALUE ZERO  COMP-3.          
009900 01  SPAR-PRKURS              PIC S9(6)V9(1) VALUE ZERO  COMP-3.          
010000                                                                          
010100 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
010200 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
010300                                                                          
010400 01  WS-ARBETSAREA.                                                       
010500*    DATE + TIME  FÖR SKAPANDE AV INLEVERANSNUMMER                        
010600     03  WS-TIAAMMDDTTMMSSTH     PIC 9(14)  VALUE ZERO.                   
010700     03  FILLER REDEFINES WS-TIAAMMDDTTMMSSTH.                            
010800         05  WS-TIAAMMDD-DATE    PIC 9(6).                                
010900         05  WS-TTMMSSTH-TIME    PIC 9(8).                                
011000     03  WS-IDINLEV              PIC S9(15) VALUE ZERO COMP-3.            
011100     03  WS-IDFAKT               PIC  9(7) VALUE ZERO.                    
011200                                                                          
011300     EJECT                                                                
011400 01  FELTEXT.                                                             
011500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011700                                                                          
011800 77  W27162-EOF-SW               PIC X       VALUE 'N'.                   
011900     88  END-OF-W27162                       VALUE 'J'.                   
012000     EJECT                                                                
012100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012200 01  FILLER REDEFINES DAGENS-DATUM.                                       
012300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012600     SKIP3                                                                
012700 01  WS-AAMMDD                   PIC 9(6).                                
012800 01  FILLER REDEFINES WS-AAMMDD.                                          
012900     03  WS-AA               PIC 9(2).                                    
013000     03  FILLER              PIC 9(4).                                    
013100                                                                          
013200 01  DYNAMISKA-SUBPROGRAM.                                                
013300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
013400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013500     EJECT                                                                
013600 01  IN-AREA-START               PIC X(24)   VALUE                        
013700                                             'IN-AREA-START'.             
013800     SKIP2                                                                
013900*01  AREA -COPY W27161     -PRE IN-                                       
014000                                                                          
014100     EJECT                                                                
014200 01  UT-AREA-START              PIC X(24)   VALUE                         
014300                                 'UT-AREA-START  '.                       
014400     SKIP2                                                                
014500*01  AREA -COPY W27162     -PRE UT-                                       
014600                                                                          
014700                                                                          
014800     EJECT                                                                
014900 PROCEDURE DIVISION.                                                      
015000                                                                          
015100     PERFORM A-INIT                                                       
015200                                                                          
015300     PERFORM S01-LAES-W27162IN                                            
015400                                                                          
015500     PERFORM UNTIL END-OF-W27162                                          
015600                                                                          
016120       MOVE IN-IDDC          TO UT-IDDC                                   
016130       MOVE IN-IDLANDX2      TO UT-IDLANDX2                               
016200       MOVE IN-DARODAT       TO WS-DARODAT                                
016300                                UT-DARODAT                                
016400       MOVE IN-IDARTNR       TO WS-IDARTNR                                
016500                                UT-IDARTNR                                
016600       MOVE IN-BEART         TO UT-BEART                                  
016700       MOVE IN-IDFKNGRP      TO UT-IDFKNGRP                               
016800       MOVE IN-FREEZECODE    TO UT-FREEZECODE                             
016900       MOVE IN-IDPERSON-BUY  TO UT-IDPERSON-BUY                           
017000       MOVE IN-KVBEART       TO UT-KVBEART                                
017100       MOVE IN-PRAVCOST      TO UT-PRAVCOST                               
017200       MOVE IN-KVPB-REF      TO UT-KVPB-REF                               
017300       MOVE IN-ONHAND        TO UT-ONHAND                                 
017400       MOVE IN-AVAIL-CDC     TO UT-AVAIL-CDC                              
017500       MOVE IN-KDERS         TO UT-KDERS                                  
017600       MOVE IN-KVAKS-SDC     TO UT-KVAKS-SDC                              
017700                                                                          
017800       PERFORM UNTIL END-OF-W27162                                        
017900       OR NOT (IN-IDARTNR    = WS-IDARTNR                                 
018000       AND IN-DARODAT        = WS-DARODAT)                                
018100                                                                          
018200         ADD +1              TO WS-ANTAL-ORDRAD                           
018300         ADD IN-KVBEART-Q    TO WS-ANTAL-QTY                              
018400                                                                          
018500         PERFORM S01-LAES-W27162IN                                        
018600       END-PERFORM                                                        
018700                                                                          
018800       MOVE WS-ANTAL-ORDRAD  TO UT-ANTAL-ORDRAD                           
018900       MOVE WS-ANTAL-QTY     TO UT-ANTAL-QTY                              
018910       MOVE ZERO             TO WS-ANTAL-ORDRAD                           
018920                                WS-ANTAL-QTY                              
019000                                                                          
019100       PERFORM S02-SKRIV-UTFIL                                            
019200                                                                          
019300     END-PERFORM                                                          
019400                                                                          
019500                                                                          
019600                                                                          
019700     PERFORM Z-FINIT                                                      
019800                                                                          
019900     MOVE ZERO TO RETURN-CODE                                             
020000     GOBACK                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 A-INIT SECTION.                                                          
020400     SKIP2                                                                
020500                                                                          
020600     OPEN INPUT  W27162IN                                                 
020700     OPEN OUTPUT W27162UT                                                 
020800     .                                                                    
020900     EJECT                                                                
021000                                                                          
021100 Z-FINIT SECTION.                                                         
021200                                                                          
021300                                                                          
021400     CLOSE W27162IN                                                       
021500           W27162UT                                                       
021600     .                                                                    
021700     EJECT                                                                
021800 S01-LAES-W27162IN SECTION.                                               
021900     SKIP2                                                                
022000     READ W27162IN           INTO IN-AREA                                 
022100     AT END                                                               
022200        MOVE JA TO W27162-EOF-SW                                          
022300                                                                          
022400     END-READ                                                             
022500     .                                                                    
022600     EJECT                                                                
022700 S02-SKRIV-UTFIL SECTION.                                                 
022800     SKIP2                                                                
022900     WRITE UT-POST                    FROM UT-AREA                        
023000     .                                                                    
